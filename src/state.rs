use std::collections::HashMap;
use std::io::sink;

use swc_common::{
    errors::{EmitterWriter, Handler, HANDLER},
    sync::Lrc,
    FileName, SourceMap, GLOBALS,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::{
    ArrayLit, Callee, Expr, ExprOrSpread, Ident, KeyValueProp, Lit, MemberExpr, MemberProp, Module,
    ModuleItem, ObjectLit, Prop, PropName, PropOrSpread,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Any,
    Number,
    String,
    Boolean,
    Null,
    Array(Box<TypeInfo>),
    Object(HashMap<String, TypeInfo>),
}

impl TypeInfo {
    pub fn describe(&self) -> String {
        match self {
            TypeInfo::Any => "any".to_string(),
            TypeInfo::Number => "number".to_string(),
            TypeInfo::String => "string".to_string(),
            TypeInfo::Boolean => "boolean".to_string(),
            TypeInfo::Null => "null".to_string(),
            TypeInfo::Array(inner) => format!("{}[]", inner.describe()),
            TypeInfo::Object(props) => {
                if props.is_empty() {
                    "Record<string, any>".to_string()
                } else {
                    let fields = props
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name, ty.describe()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{{ {} }}", fields)
                }
            }
        }
    }

    pub fn element_type(&self) -> TypeInfo {
        match self {
            TypeInfo::Array(inner) => (*inner.clone()).clone(),
            _ => TypeInfo::Any,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StateVariable {
    pub name: String,
    pub ty: TypeInfo,
}

#[derive(Debug, Clone)]
pub struct LoopBinding {
    pub collection: String,
    pub collection_type: TypeInfo,
    pub item: Option<StateVariable>,
    pub index: Option<StateVariable>,
}

#[derive(Debug, Clone)]
pub struct ManifoldState {
    pub properties: HashMap<String, TypeInfo>,
}

impl ManifoldState {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
        }
    }

    pub fn set_property(&mut self, name: String, ty: TypeInfo) {
        self.properties.insert(name, ty);
    }

    pub fn property_type(&self, name: &str) -> Option<&TypeInfo> {
        self.properties.get(name)
    }
}

pub type ManifoldStates = HashMap<String, ManifoldState>;

pub fn parse_states_from_scripts(text: &str) -> ManifoldStates {
    let mut states: ManifoldStates = HashMap::new();

    for script in extract_script_blocks(text) {
        if let Some(module) = parse_script_module(&script) {
            for item in module.body {
                if let Some(definition) = analyze_module_item(item) {
                    let name = definition.name.unwrap_or_else(|| "default".to_string());
                    let mut state = states.remove(&name).unwrap_or_else(ManifoldState::new);
                    for (prop, ty) in definition.properties {
                        state.set_property(prop, ty);
                    }
                    states.insert(name, state);
                }
            }
        }
    }

    states
}

pub fn resolve_identifier_type(
    identifier: &str,
    state: Option<&ManifoldState>,
    locals: &HashMap<String, TypeInfo>,
) -> Option<TypeInfo> {
    let mut segments = identifier
        .split('.')
        .map(|segment| segment.trim())
        .filter(|s| !s.is_empty());
    let first = segments.next()?;

    let mut current = if let Some(local) = locals.get(first) {
        local.clone()
    } else {
        state?.property_type(first)?.clone()
    };

    for segment in segments {
        current = match current {
            TypeInfo::Object(ref props) => props.get(segment).cloned().unwrap_or(TypeInfo::Any),
            TypeInfo::Array(ref inner) => {
                if segment == "length" {
                    TypeInfo::Number
                } else {
                    (*inner.clone()).clone()
                }
            }
            _ => TypeInfo::Any,
        };
    }

    Some(current)
}

fn extract_script_blocks(text: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let lower = text.to_lowercase();
    let mut offset = 0;

    while let Some(start_rel) = lower[offset..].find("<script") {
        let start = offset + start_rel;
        let tag_close_rel = match lower[start..].find('>') {
            Some(index) => index,
            None => break,
        };
        let content_start = start + tag_close_rel + 1;
        let end_rel = match lower[content_start..].find("</script") {
            Some(index) => index,
            None => break,
        };
        let content_end = content_start + end_rel;
        blocks.push(text[content_start..content_end].to_string());

        let after_end = match lower[content_end..].find('>') {
            Some(index) => content_end + index + 1,
            None => content_end,
        };
        offset = after_end;
    }

    blocks
}

fn parse_script_module(script: &str) -> Option<Module> {
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_emitter(
        false,
        false,
        Box::new(EmitterWriter::new(Box::new(sink()), None, false, false)),
    );

    GLOBALS.set(&Default::default(), || {
        HANDLER.set(&handler, || {
            let fm = cm.new_source_file(FileName::Anon, script.to_string());
            let lexer = Lexer::new(
                Syntax::Typescript(TsConfig {
                    tsx: false,
                    dts: false,
                    ..Default::default()
                }),
                EsVersion::Es2022,
                StringInput::from(&*fm),
                None,
            );
            let mut parser = Parser::new_from(lexer);
            match parser.parse_module() {
                Ok(module) => Some(module),
                Err(err) => {
                    err.into_diagnostic(&handler).emit();
                    None
                }
            }
        })
    })
}

#[derive(Default)]
struct StateDefinition {
    name: Option<String>,
    properties: HashMap<String, TypeInfo>,
}

fn analyze_module_item(item: ModuleItem) -> Option<StateDefinition> {
    match item {
        ModuleItem::Stmt(stmt) => match stmt {
            swc_ecma_ast::Stmt::Decl(decl) => match decl {
                swc_ecma_ast::Decl::Var(var_decl) => {
                    for declarator in var_decl.decls {
                        if let Some(init) = declarator.init {
                            if let Some(def) = analyze_expression(init.as_ref()) {
                                return Some(def);
                            }
                        }
                    }
                    None
                }
                _ => None,
            },
            _ => None,
        },
        ModuleItem::ModuleDecl(_) => None,
    }
}

fn analyze_expression(expr: &Expr) -> Option<StateDefinition> {
    if let Expr::Call(call) = expr {
        if let Callee::Expr(callee) = &call.callee {
            if let Expr::Member(member) = &**callee {
                if property_name(member)? == "build" {
                    let mut definition = StateDefinition::default();
                    if let Some(arg) = call.args.get(0) {
                        if let Some(name) = string_from_expr(&arg.expr) {
                            definition.name = Some(name);
                        }
                    }
                    collect_chain(&member.obj, &mut definition)?;
                    return Some(definition);
                }
            }
        }
    }
    None
}

fn collect_chain(expr: &Expr, definition: &mut StateDefinition) -> Option<()> {
    match expr {
        Expr::Call(call) => {
            if let Callee::Expr(callee) = &call.callee {
                if let Expr::Member(member) = &**callee {
                    let method = property_name(member)?;
                    match method.as_str() {
                        "add" => {
                            if let Some(ExprOrSpread {
                                expr: prop_expr, ..
                            }) = call.args.get(0)
                            {
                                if let Some(prop_name) = string_from_expr(prop_expr) {
                                    let ty = call
                                        .args
                                        .get(1)
                                        .and_then(|arg| infer_type_from_expr(&arg.expr))
                                        .unwrap_or(TypeInfo::Any);
                                    definition.properties.insert(prop_name, ty);
                                }
                            }
                            collect_chain(&member.obj, definition)
                        }
                        "create" => {
                            if definition.name.is_none() {
                                if let Some(ExprOrSpread {
                                    expr: name_expr, ..
                                }) = call.args.get(0)
                                {
                                    if let Some(name) = string_from_expr(name_expr) {
                                        definition.name = Some(name);
                                    }
                                }
                            }
                            Some(())
                        }
                        _ => collect_chain(&member.obj, definition),
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr::Member(member) => collect_chain(&member.obj, definition),
        Expr::Ident(ident) => {
            if ident.sym == *"Manifold" {
                Some(())
            } else {
                Some(())
            }
        }
        _ => Some(()),
    }
}

fn property_name(member: &MemberExpr) -> Option<String> {
    match &member.prop {
        MemberProp::Ident(ident) => Some(ident.sym.to_string()),
        _ => None,
    }
}

fn string_from_expr(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Lit(Lit::Str(str_lit)) => Some(str_lit.value.to_string()),
        Expr::Lit(Lit::Bool(b)) => Some(b.value.to_string()),
        Expr::Ident(Ident { sym, .. }) => Some(sym.to_string()),
        _ => None,
    }
}

fn infer_type_from_expr(expr: &Expr) -> Option<TypeInfo> {
    let ty = match expr {
        Expr::Lit(lit) => match lit {
            Lit::Str(_) => TypeInfo::String,
            Lit::Num(_) => TypeInfo::Number,
            Lit::Bool(_) => TypeInfo::Boolean,
            Lit::Null(_) => TypeInfo::Null,
            Lit::BigInt(_) => TypeInfo::Number,
            Lit::Regex(_) => TypeInfo::String,
            _ => TypeInfo::Any,
        },
        Expr::Array(array) => infer_array_type(array),
        Expr::Object(object) => infer_object_type(object),
        _ => TypeInfo::Any,
    };
    Some(ty)
}

fn infer_array_type(array: &ArrayLit) -> TypeInfo {
    let mut element: Option<TypeInfo> = None;
    for elem in &array.elems {
        if let Some(expr_or_spread) = elem {
            if expr_or_spread.spread.is_some() {
                return TypeInfo::Any;
            }
            if let Some(value) = infer_type_from_expr(&expr_or_spread.expr) {
                element = match element {
                    None => Some(value),
                    Some(existing) => {
                        if existing == value {
                            Some(existing)
                        } else {
                            return TypeInfo::Any;
                        }
                    }
                };
            }
        }
    }
    TypeInfo::Array(Box::new(element.unwrap_or(TypeInfo::Any)))
}

fn infer_object_type(object: &ObjectLit) -> TypeInfo {
    let mut props = HashMap::new();
    for prop in &object.props {
        if let PropOrSpread::Prop(prop) = prop {
            if let Prop::KeyValue(KeyValueProp { key, value }) = &**prop {
                if let Some(name) = property_name_from_prop(key) {
                    if let Some(value_type) = infer_type_from_expr(value) {
                        props.insert(name, value_type);
                    }
                }
            }
        }
    }
    TypeInfo::Object(props)
}

fn property_name_from_prop(name: &PropName) -> Option<String> {
    match name {
        PropName::Ident(ident) => Some(ident.sym.to_string()),
        PropName::Str(str_lit) => Some(str_lit.value.to_string()),
        _ => None,
    }
}
