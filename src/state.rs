use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::sink;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

use swc_common::{
    errors::{EmitterWriter, Handler, HANDLER},
    sync::Lrc,
    FileName, SourceMap, Span, GLOBALS,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::{
    self as ast, ArrayLit, BindingIdent, Callee, ExportSpecifier, Expr, ExprOrSpread, Ident,
    ImportSpecifier, KeyValueProp, Lit, MemberExpr, MemberProp, Module, ModuleExportName,
    ModuleItem, ObjectLit, Pat, Prop, PropName, PropOrSpread,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Any,
    Unknown,
    Number,
    String,
    Boolean,
    Null,
    Array(Box<TypeInfo>),
    Object(HashMap<String, TypeInfo>),
    Set(Box<TypeInfo>),
    Map(Box<TypeInfo>, Box<TypeInfo>),
    Tuple(Vec<TypeInfo>),
    Promise(Box<TypeInfo>),
    Union(Vec<TypeInfo>),
    Named(String),
}

impl TypeInfo {
    pub fn from_union(types: Vec<TypeInfo>) -> TypeInfo {
        let mut flat: Vec<TypeInfo> = Vec::new();
        for ty in types {
            match ty {
                TypeInfo::Union(inner) => flat.extend(inner),
                other => flat.push(other),
            }
        }

        if flat.iter().any(|ty| matches!(ty, TypeInfo::Any)) {
            return TypeInfo::Any;
        }

        let mut unique: Vec<TypeInfo> = Vec::new();
        for ty in flat {
            if !unique.iter().any(|existing| existing == &ty) {
                unique.push(ty);
            }
        }

        if unique.is_empty() {
            TypeInfo::Any
        } else if unique.len() == 1 {
            unique.into_iter().next().unwrap()
        } else {
            TypeInfo::Union(unique)
        }
    }

    pub fn promise_resolution_type(&self) -> Option<TypeInfo> {
        match self {
            TypeInfo::Promise(inner) => Some((**inner).clone()),
            TypeInfo::Union(options) => {
                let mut collected = Vec::new();
                for option in options {
                    if let Some(resolved) = option.promise_resolution_type() {
                        collected.push(resolved);
                    }
                }
                if collected.is_empty() {
                    None
                } else {
                    Some(TypeInfo::from_union(collected))
                }
            }
            _ => None,
        }
    }

    pub fn describe(&self) -> String {
        match self {
            TypeInfo::Any => "any".to_string(),
            TypeInfo::Unknown => "unknown".to_string(),
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
                    format!("{{ {fields} }}")
                }
            }
            TypeInfo::Set(inner) => format!("Set<{}>", inner.describe()),
            TypeInfo::Map(key, value) => {
                format!("Map<{}, {}>", key.describe(), value.describe())
            }
            TypeInfo::Tuple(items) => {
                let parts = items.iter().map(|ty| ty.describe()).collect::<Vec<_>>();
                format!("[{}]", parts.join(", "))
            }
            TypeInfo::Promise(inner) => format!("Promise<{}>", inner.describe()),
            TypeInfo::Union(types) => {
                if types.is_empty() {
                    "never".to_string()
                } else {
                    types
                        .iter()
                        .map(|ty| ty.describe())
                        .collect::<Vec<_>>()
                        .join(" | ")
                }
            }
            TypeInfo::Named(name) => name.clone(),
        }
    }

    pub fn element_type(&self) -> TypeInfo {
        match self {
            TypeInfo::Array(inner) => (*inner.clone()).clone(),
            TypeInfo::Set(inner) => (*inner.clone()).clone(),
            TypeInfo::Map(key, value) => {
                TypeInfo::Tuple(vec![(*value.clone()).clone(), (*key.clone()).clone()])
            }
            TypeInfo::Object(props) => {
                let value_union = if props.is_empty() {
                    TypeInfo::Any
                } else {
                    TypeInfo::from_union(props.values().cloned().collect())
                };
                TypeInfo::Tuple(vec![value_union, TypeInfo::String])
            }
            TypeInfo::Tuple(items) => TypeInfo::Tuple(items.clone()),
            TypeInfo::Union(items) => {
                let mut collected = Vec::new();
                for item in items {
                    let elem = item.element_type();
                    if !matches!(elem, TypeInfo::Any) {
                        collected.push(elem);
                    }
                }
                if collected.is_empty() {
                    TypeInfo::Any
                } else if collected.iter().all(|ty| ty == &collected[0]) {
                    collected[0].clone()
                } else {
                    TypeInfo::Any
                }
            }
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
    pub items: Vec<StateVariable>,
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

#[derive(Debug, Clone)]
pub struct DefinitionLocation {
    pub uri: Url,
    pub line: u32,
    pub character: u32,
    pub length: u32,
}

pub type DefinitionIndex = HashMap<(String, String), DefinitionLocation>;
type BindingMap = HashMap<String, Arc<Expr>>;

#[derive(Clone)]
struct ImportBinding {
    specifier: String,
    kind: ImportKind,
}

#[derive(Clone)]
enum ImportKind {
    Default,
    Named(String),
}

#[derive(Default, Clone)]
struct ModuleContext {
    id: usize,
    bindings: BindingMap,
    imports: HashMap<String, ImportBinding>,
    exports: HashMap<String, Arc<Expr>>,
    default_export: Option<Arc<Expr>>,
}

#[derive(Clone)]
struct ModuleEnv {
    context: Arc<ModuleContext>,
    base_dir: Option<PathBuf>,
}

#[derive(Clone)]
struct FileHandle {
    path: PathBuf,
    content: Arc<String>,
    line_index: Arc<crate::lineindex::LineIndex>,
}

impl FileHandle {
    fn new(path: PathBuf, content: String) -> Self {
        let content_arc = Arc::new(content);
        let line_index = Arc::new(crate::lineindex::LineIndex::new(content_arc.as_str()));
        Self {
            path,
            content: content_arc,
            line_index,
        }
    }
}

struct CachedFile {
    handle: FileHandle,
    module: Option<Arc<Module>>,
    context: Option<Arc<ModuleContext>>,
}

impl CachedFile {
    fn new(path: PathBuf, content: String) -> Self {
        Self {
            handle: FileHandle::new(path, content),
            module: None,
            context: None,
        }
    }
}

#[derive(Default)]
struct FileCache {
    files: HashMap<PathBuf, CachedFile>,
}

impl FileCache {
    fn get_handle(&self, path: &Path) -> Option<FileHandle> {
        self.files.get(path).map(|entry| entry.handle.clone())
    }

    fn resolve_script(&mut self, specifier: &str, base: &Path) -> Option<FileHandle> {
        if !(specifier.starts_with('.') || specifier.starts_with('/')) {
            return None;
        }

        let joined = if specifier.starts_with('/') {
            PathBuf::from(specifier)
        } else {
            base.join(specifier)
        };

        let candidates = vec![
            joined.clone(),
            joined.with_extension("ts"),
            joined.with_extension("js"),
            joined.with_extension("mjs"),
            joined.with_extension("cjs"),
            joined.join("index.ts"),
            joined.join("index.js"),
            joined.join("index.mjs"),
            joined.join("index.cjs"),
        ];

        for candidate in candidates {
            let canonical = canonical_or(candidate.clone());
            if let Some(handle) = self.get_handle(&canonical) {
                return Some(handle);
            }
            if let Ok(content) = fs::read_to_string(&candidate) {
                let canonical = canonical_or(candidate);
                let cached = CachedFile::new(canonical.clone(), content);
                let handle = cached.handle.clone();
                self.files.insert(canonical.clone(), cached);
                return Some(handle);
            }
        }

        None
    }

    fn ensure_module(&mut self, path: &Path) -> Option<Arc<Module>> {
        if let Some(entry) = self.files.get_mut(path) {
            if let Some(module) = &entry.module {
                return Some(module.clone());
            }
            if let Some(parsed) = parse_script_module(&entry.handle.content) {
                let arc = Arc::new(parsed);
                entry.module = Some(arc.clone());
                return Some(arc);
            }
        }
        None
    }

    fn ensure_context(&mut self, path: &Path) -> Option<Arc<ModuleContext>> {
        if let Some(entry) = self.files.get_mut(path) {
            if let Some(context) = &entry.context {
                return Some(context.clone());
            }
            let module = entry.module.clone().or_else(|| {
                if let Some(parsed) = parse_script_module(&entry.handle.content) {
                    let arc = Arc::new(parsed);
                    entry.module = Some(arc.clone());
                    Some(arc)
                } else {
                    None
                }
            })?;
            let context = Arc::new(collect_module_context(module.as_ref()));
            entry.context = Some(context.clone());
            return Some(context);
        }
        None
    }
}

pub fn parse_states_from_document(
    text: &str,
    base_dir: Option<&Path>,
) -> (ManifoldStates, Vec<String>) {
    let mut states: ManifoldStates = HashMap::new();
    let (inline_scripts, srcs) = extract_script_blocks_and_srcs(text);

    let default_base = std::env::current_dir().ok();
    let base = base_dir.or(default_base.as_deref());
    let mut unresolved: Vec<String> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut file_cache = FileCache::default();

    // Parse inline scripts, follow imports
    for script in inline_scripts {
        if let Some(module) = parse_script_module(&script) {
            let context = Arc::new(collect_module_context(&module));
            let env = ModuleEnv {
                context,
                base_dir: base.map(Path::to_path_buf),
            };
            process_module_for_states(
                &module,
                env.clone(),
                &mut states,
                &mut file_cache,
                &mut unresolved,
            );
            if base.is_some() {
                follow_imports(
                    &module,
                    &env,
                    &mut states,
                    &mut visited,
                    &mut unresolved,
                    0,
                    &mut file_cache,
                );
            }
        }
    }

    // Parse classic/extern scripts referenced via src
    for src in srcs {
        if let Some(base) = base {
            if let Some(handle) = file_cache.resolve_script(&src, base) {
                if visited.insert(handle.path.clone()) {
                    if let Some(module) = file_cache.ensure_module(&handle.path) {
                        let context = match file_cache.ensure_context(&handle.path) {
                            Some(ctx) => ctx,
                            None => continue,
                        };
                        let base_dir = handle.path.parent().map(Path::to_path_buf);
                        let env = ModuleEnv { context, base_dir };
                        process_module_for_states(
                            module.as_ref(),
                            env.clone(),
                            &mut states,
                            &mut file_cache,
                            &mut unresolved,
                        );
                        follow_imports(
                            module.as_ref(),
                            &env,
                            &mut states,
                            &mut visited,
                            &mut unresolved,
                            0,
                            &mut file_cache,
                        );
                    }
                }
            } else {
                unresolved.push(src);
            }
        } else {
            // No base directory; record as unresolved so we can inform the user
            unresolved.push(src);
        }
    }

    (states, unresolved)
}

pub fn build_definition_index(
    html_text: &str,
    html_path: Option<&Path>,
) -> (DefinitionIndex, Vec<String>) {
    let mut index: DefinitionIndex = HashMap::new();
    let (inline_scripts, srcs, starts) = extract_script_blocks_srcs_with_starts(html_text);
    let html_uri = html_path.and_then(|p| Url::from_file_path(p).ok());
    let html_li = crate::lineindex::LineIndex::new(html_text);

    let default_base = html_path
        .and_then(|p| p.parent().map(Path::to_path_buf))
        .or_else(|| std::env::current_dir().ok())
        .unwrap_or_else(|| PathBuf::from("."));

    let mut unresolved: Vec<String> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut file_cache = FileCache::default();

    // Inline scripts: parse and harvest definitions; positions map to HTML URI
    for (script, start) in inline_scripts.iter().zip(starts.iter()) {
        if let Some(module) = parse_script_module(script) {
            // Collect property names per state
            let mut defs = Vec::<(String, PropertyDefinition)>::new();
            let bindings = collect_variable_bindings(&module);
            for item in &module.body {
                if let Some(definition) = analyze_module_item(item, &bindings) {
                    let state_name = definition.name.unwrap_or_else(|| "default".to_string());
                    for prop in definition.properties {
                        defs.push((state_name.clone(), prop));
                    }
                }
            }
            // Simple text search to locate property occurrences
            for (state_name, prop) in defs {
                let mut recorded = false;
                if let Some(span) = prop.span {
                    if let Some((start_rel, end_rel)) = literal_body_range(span, script) {
                        if let Some(uri) = html_uri.clone() {
                            let absolute = *start + start_rel;
                            let p = html_li.position_at(absolute);
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(script, start_rel, end_rel),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(method_pos) = script
                    .find(&format!("add(\"{}\"", prop.name))
                    .or_else(|| script.find(&format!(".add(\"{}\"", prop.name)))
                    .or_else(|| script.find(&format!("add('{}'", prop.name)))
                    .or_else(|| script.find(&format!(".add('{}'", prop.name)))
                {
                    let quote_offset = script[method_pos..]
                        .find('"')
                        .or_else(|| script[method_pos..].find('\''));
                    if let Some(quote_rel) = quote_offset {
                        let prop_start_rel = method_pos + quote_rel + 1;
                        let absolute = *start + prop_start_rel;
                        let p = html_li.position_at(absolute);
                        if let Some(uri) = html_uri.clone() {
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: prop.name.len() as u32,
                                },
                            );
                        }
                    }
                }
            }

            // Follow ESM imports from this inline module to discover external definitions
            collect_defs_from_imports(
                &module,
                &default_base,
                &mut index,
                &mut visited,
                &mut unresolved,
                0,
                &mut file_cache,
            );
        }
    }

    // External scripts via src
    for src in srcs {
        if let Some(handle) = file_cache.resolve_script(&src, &default_base) {
            if visited.insert(handle.path.clone()) {
                if let Some(module) = file_cache.ensure_module(&handle.path) {
                    harvest_definitions_from_module(
                        module.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        &mut index,
                    );
                    collect_defs_from_imports(
                        module.as_ref(),
                        handle.path.parent().unwrap_or(&default_base),
                        &mut index,
                        &mut visited,
                        &mut unresolved,
                        0,
                        &mut file_cache,
                    );
                }
            }
        } else {
            unresolved.push(src);
        }
    }

    (index, unresolved)
}

pub fn build_state_name_index(
    html_text: &str,
    html_path: Option<&Path>,
) -> (HashMap<String, DefinitionLocation>, Vec<String>) {
    let mut index: HashMap<String, DefinitionLocation> = HashMap::new();
    let (inline_scripts, srcs, starts) = extract_script_blocks_srcs_with_starts(html_text);
    let html_uri = html_path.and_then(|p| Url::from_file_path(p).ok());
    let html_li = crate::lineindex::LineIndex::new(html_text);

    let default_base = html_path
        .and_then(|p| p.parent().map(|pp| pp.to_path_buf()))
        .or_else(|| std::env::current_dir().ok())
        .unwrap_or_else(|| PathBuf::from("."));

    let mut unresolved: Vec<String> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut file_cache = FileCache::default();

    // Inline scripts
    for (script, start) in inline_scripts.iter().zip(starts.iter()) {
        if let Some(module) = parse_script_module(script) {
            // Collect state names
            let mut names: Vec<(String, Option<Span>)> = Vec::new();
            let bindings = collect_variable_bindings(&module);
            for item in &module.body {
                if let Some(definition) = analyze_module_item(item, &bindings) {
                    if let Some(name) = definition.name {
                        names.push((name, definition.name_span));
                    }
                }
            }
            for (name, span) in names {
                let mut recorded = false;
                if let Some(span) = span {
                    if let Some((start_rel, end_rel)) = literal_body_range(span, script) {
                        if let Some(uri) = html_uri.clone() {
                            let absolute = *start + start_rel;
                            let p = html_li.position_at(absolute);
                            index.insert(
                                name.clone(),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(script, start_rel, end_rel),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(pos) = find_create_call(script, &name) {
                    if let Some(uri) = html_uri.clone() {
                        let absolute = *start + pos;
                        let p = html_li.position_at(absolute);
                        index.insert(
                            name.clone(),
                            DefinitionLocation {
                                uri,
                                line: p.line,
                                character: p.character,
                                length: name.len() as u32,
                            },
                        );
                    }
                }
            }

            collect_state_names_from_imports(
                &module,
                &default_base,
                &mut index,
                &mut visited,
                &mut unresolved,
                0,
                &mut file_cache,
            );
        }
    }

    // External scripts via src
    for src in srcs {
        if let Some(handle) = file_cache.resolve_script(&src, &default_base) {
            if visited.insert(handle.path.clone()) {
                if let Some(module) = file_cache.ensure_module(&handle.path) {
                    harvest_state_names_from_module(
                        module.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        &mut index,
                    );
                    collect_state_names_from_imports(
                        module.as_ref(),
                        handle.path.parent().unwrap_or(&default_base),
                        &mut index,
                        &mut visited,
                        &mut unresolved,
                        0,
                        &mut file_cache,
                    );
                }
            }
        } else {
            unresolved.push(src);
        }
    }

    (index, unresolved)
}

#[cfg(test)]
mod tests {
    use super::*;

    const HTML: &str = r#"
        <div data-mf-register>
            <button :onclick="count++">Increment</button>
            <button :onclick="items.push(item)">Add</button>
        </div>
        <div data-mf-register="secondary">
            <p>${message}</p>
        </div>
        <script type="module">
            const state = Manifold.create()
                .add("count", 0)
                .add("items", [])
                .add("message", "hi")
                .build();

            const secondary = Manifold.create("secondary")
                .add("message", "from secondary")
                .build();
        </script>
    "#;

    #[test]
    fn definition_index_includes_inline_state_properties() {
        let (index, unresolved) =
            build_definition_index(HTML, Some(Path::new("/virtual/test.html")));
        assert!(unresolved.is_empty());

        let key = ("default".to_string(), "count".to_string());
        let definition = index.get(&key).expect("count definition present");
        assert!(definition.length >= 5);
    }

    #[test]
    fn state_name_index_tracks_custom_register_values() {
        let (state_map, unresolved) =
            build_state_name_index(HTML, Some(Path::new("/virtual/test.html")));
        assert!(unresolved.is_empty());
        assert!(state_map.contains_key("secondary"));
    }

    #[test]
    fn add_with_object_literal_registers_properties() {
        const OBJECT_HTML: &str = r#"
            <div data-mf-register>
                <script type="module">
                    const state = Manifold.create()
                        .add({ addTodo, markTodoDone })
                        .build();

                    function addTodo(title) {
                        console.log(title);
                    }

                    function markTodoDone(id) {
                        console.log(id);
                    }
                </script>
            </div>
        "#;
        let (states, unresolved) = parse_states_from_document(OBJECT_HTML, None);
        assert!(unresolved.is_empty());
        let default = states.get("default").expect("default state");
        assert!(default.property_type("addTodo").is_some());
        assert!(default.property_type("markTodoDone").is_some());
    }

    #[test]
    fn add_with_object_literal_method_definitions_registers_properties() {
        const METHOD_HTML: &str = r#"
            <div data-mf-register>
                <script type="module">
                    const state = Manifold.create()
                        .add({
                            addTodo(title) {
                                console.log(title);
                            },
                            markTodoDone(id) {
                                console.log(id);
                            },
                            get remaining() {
                                return 0;
                            },
                            set selected(value) {
                                console.log(value);
                            }
                        })
                        .build();
                </script>
            </div>
        "#;
        let (states, unresolved) = parse_states_from_document(METHOD_HTML, None);
        assert!(unresolved.is_empty());
        let default = states.get("default").expect("default state");
        assert!(default.property_type("addTodo").is_some());
        assert!(default.property_type("markTodoDone").is_some());
        assert!(default.property_type("remaining").is_some());
        assert!(default.property_type("selected").is_some());
    }

    #[test]
    fn derive_registers_properties() {
        const DERIVE_HTML: &str = r#"
            <div data-mf-register>
                <script type="module">
                    const builder = State.create()
                        .add("count", 2)
                        .derive("doubleCount", (store) => store.count * 2);
                    const state = builder.build();
                </script>
            </div>
        "#;
        let (states, unresolved) = parse_states_from_document(DERIVE_HTML, None);
        assert!(unresolved.is_empty());
        let default = states.get("default").expect("default state");
        assert!(default.property_type("doubleCount").is_some());
    }

    #[test]
    fn builder_split_across_modules_registers_properties() {
        let dir = tempfile::tempdir().expect("temp dir");
        let builder_js = dir.path().join("builder.js");
        let entry_js = dir.path().join("entry.js");

        fs::write(
            &builder_js,
            r#"
                const builder = Manifold
                    .create()
                    .add("todos", [])
                    .add("addTodo", () => {})
                    .add("markTodoDone", () => {});

                export default builder;
            "#,
        )
        .expect("write builder");

        fs::write(
            &entry_js,
            r#"
                import builder from "./builder.js";

                const state = builder.build();
                export default state;
            "#,
        )
        .expect("write entry");

        const HTML: &str = r#"
            <div data-mf-register>
                <script type="module" src="./entry.js"></script>
                <button :onclick="addTodo('x')">Add</button>
            </div>
        "#;

        let (states, unresolved) = parse_states_from_document(HTML, Some(dir.path()));
        assert!(unresolved.is_empty());
        let default = states.get("default").expect("default");
        assert!(default.property_type("todos").is_some());
        assert!(default.property_type("addTodo").is_some());
        assert!(default.property_type("markTodoDone").is_some());
    }
}

fn find_create_call(content: &str, name: &str) -> Option<usize> {
    [
        format!("create(\"{name}\")"),
        format!(".create(\"{name}\")"),
        format!("create('{name}')"),
        format!(".create('{name}')"),
    ]
    .into_iter()
    .find_map(|p| content.find(&p))
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
        current = resolve_member_type(&current, segment);
    }

    Some(current)
}

pub fn resolve_member_type(current: &TypeInfo, segment: &str) -> TypeInfo {
    match current {
        TypeInfo::Object(props) => props.get(segment).cloned().unwrap_or(TypeInfo::Any),
        TypeInfo::Set(_) => {
            if segment == "size" {
                TypeInfo::Number
            } else {
                TypeInfo::Any
            }
        }
        TypeInfo::Map(_, _) => {
            if segment == "size" {
                TypeInfo::Number
            } else {
                TypeInfo::Any
            }
        }
        TypeInfo::Array(inner) => {
            if segment == "length" {
                TypeInfo::Number
            } else {
                (*inner.clone()).clone()
            }
        }
        TypeInfo::Tuple(items) => {
            if segment == "length" {
                TypeInfo::Number
            } else if let Ok(index) = segment.parse::<usize>() {
                items.get(index).cloned().unwrap_or(TypeInfo::Any)
            } else {
                TypeInfo::Any
            }
        }
        TypeInfo::Union(options) => {
            let mut collected = Vec::new();
            for option in options {
                let ty = resolve_member_type(option, segment);
                if !matches!(ty, TypeInfo::Any) {
                    collected.push(ty);
                }
            }

            if collected.is_empty() {
                TypeInfo::Any
            } else {
                TypeInfo::from_union(collected)
            }
        }
        TypeInfo::Unknown => TypeInfo::Unknown,
        TypeInfo::Any => TypeInfo::Any,
        TypeInfo::Promise(_) => TypeInfo::Any,
        TypeInfo::Named(_) => TypeInfo::Any,
        TypeInfo::Null => TypeInfo::Null,
        TypeInfo::Boolean => TypeInfo::Any,
        TypeInfo::Number => TypeInfo::Any,
        TypeInfo::String => TypeInfo::Any,
    }
}

fn extract_script_blocks_and_srcs(text: &str) -> (Vec<String>, Vec<String>) {
    let mut blocks: Vec<String> = Vec::new();
    let mut srcs: Vec<String> = Vec::new();
    let lower = text.to_lowercase();
    let mut offset = 0;

    while let Some(start_rel) = lower[offset..].find("<script") {
        let start = offset + start_rel;
        let tag_close_rel = match lower[start..].find('>') {
            Some(index) => index,
            None => break,
        };
        // Extract within the tag to search for src attributes
        let tag_open = &text[start..start + tag_close_rel + 1];
        if let Some(src_val) = find_attribute_value(tag_open, "src") {
            srcs.push(src_val);
        }

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

    (blocks, srcs)
}

fn extract_script_blocks_srcs_with_starts(text: &str) -> (Vec<String>, Vec<String>, Vec<usize>) {
    let mut blocks: Vec<String> = Vec::new();
    let mut srcs: Vec<String> = Vec::new();
    let mut starts: Vec<usize> = Vec::new();
    let lower = text.to_lowercase();
    let mut offset = 0;

    while let Some(start_rel) = lower[offset..].find("<script") {
        let start = offset + start_rel;
        let tag_close_rel = match lower[start..].find('>') {
            Some(index) => index,
            None => break,
        };
        let tag_open = &text[start..start + tag_close_rel + 1];
        if let Some(src_val) = find_attribute_value(tag_open, "src") {
            srcs.push(src_val);
        }
        let content_start = start + tag_close_rel + 1;
        let end_rel = match lower[content_start..].find("</script") {
            Some(index) => index,
            None => break,
        };
        let content_end = content_start + end_rel;
        starts.push(content_start);
        blocks.push(text[content_start..content_end].to_string());
        let after_end = match lower[content_end..].find('>') {
            Some(index) => content_end + index + 1,
            None => content_end,
        };
        offset = after_end;
    }

    (blocks, srcs, starts)
}

fn find_attribute_value(tag_open: &str, attr: &str) -> Option<String> {
    // naive attribute extractor for src="..." or src='...'
    let lower = tag_open.to_ascii_lowercase();
    let needle = format!("{attr}=");
    let mut i = 0usize;
    while let Some(pos) = lower[i..].find(&needle) {
        let idx = i + pos + needle.len();
        let bytes = tag_open.as_bytes();
        if idx >= bytes.len() {
            break;
        }
        let quote = bytes[idx];
        if quote == b'"' || quote == b'\'' {
            let rest = &tag_open[idx + 1..];
            if let Some(end) = rest.find(quote as char) {
                return Some(rest[..end].to_string());
            }
        }
        i = idx + 1;
    }
    None
}

fn process_module_for_states(
    module: &Module,
    env: ModuleEnv,
    states: &mut ManifoldStates,
    file_cache: &mut FileCache,
    unresolved: &mut Vec<String>,
) {
    for item in &module.body {
        if let Some(definition) = analyze_module_item_with_env(item, &env, file_cache, unresolved) {
            let name = definition.name.unwrap_or_else(|| "default".to_string());
            let mut state = states.remove(&name).unwrap_or_else(ManifoldState::new);
            for prop in definition.properties {
                state.set_property(prop.name, prop.ty);
            }
            states.insert(name, state);
        }
    }
}

fn follow_imports(
    module: &Module,
    env: &ModuleEnv,
    states: &mut ManifoldStates,
    visited: &mut HashSet<PathBuf>,
    unresolved: &mut Vec<String>,
    depth: usize,
    cache: &mut FileCache,
) {
    if depth > 16 {
        return; // reasonable safety cap
    }
    let imports = collect_import_paths(module);
    let Some(base) = env.base_dir.as_deref() else {
        unresolved.extend(imports);
        return;
    };
    for import in imports {
        if let Some(handle) = cache.resolve_script(&import, base) {
            if visited.insert(handle.path.clone()) {
                if let Some(child) = cache.ensure_module(&handle.path) {
                    let context = match cache.ensure_context(&handle.path) {
                        Some(ctx) => ctx,
                        None => continue,
                    };
                    let next_base = handle
                        .path
                        .parent()
                        .map(Path::to_path_buf)
                        .or_else(|| env.base_dir.clone());
                    let child_env = ModuleEnv {
                        context,
                        base_dir: next_base,
                    };
                    process_module_for_states(
                        child.as_ref(),
                        child_env.clone(),
                        states,
                        cache,
                        unresolved,
                    );
                    follow_imports(
                        child.as_ref(),
                        &child_env,
                        states,
                        visited,
                        unresolved,
                        depth + 1,
                        cache,
                    );
                }
            }
        } else {
            unresolved.push(import);
        }
    }
}

fn collect_import_paths(module: &Module) -> Vec<String> {
    let mut paths = Vec::new();
    for item in &module.body {
        if let ModuleItem::ModuleDecl(decl) = item {
            match decl {
                swc_ecma_ast::ModuleDecl::Import(import) => {
                    paths.push(import.src.value.to_string());
                }
                swc_ecma_ast::ModuleDecl::ExportNamed(named) => {
                    if let Some(src) = &named.src {
                        paths.push(src.value.to_string());
                    }
                }
                swc_ecma_ast::ModuleDecl::ExportAll(all) => {
                    paths.push(all.src.value.to_string());
                }
                _ => {}
            }
        }
    }
    paths
}

fn canonical_or(path: PathBuf) -> PathBuf {
    fs::canonicalize(&path).unwrap_or(path)
}

fn harvest_definitions_from_module(
    module: &Module,
    content: &str,
    file_path: &Path,
    li: &crate::lineindex::LineIndex,
    index: &mut DefinitionIndex,
) {
    let bindings = collect_variable_bindings(module);
    for item in &module.body {
        if let Some(definition) = analyze_module_item(item, &bindings) {
            let state_name = definition.name.unwrap_or_else(|| "default".to_string());
            for prop in definition.properties {
                let mut recorded = false;
                if let Some(span) = prop.span {
                    if let Some((start, end)) = literal_body_range(span, content) {
                        if let Ok(uri) = Url::from_file_path(file_path) {
                            let p = li.position_at(start);
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(content, start, end),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(method_pos) = content
                    .find(&format!("add(\"{}\"", prop.name))
                    .or_else(|| content.find(&format!(".add(\"{}\"", prop.name)))
                    .or_else(|| content.find(&format!("add('{}'", prop.name)))
                    .or_else(|| content.find(&format!(".add('{}'", prop.name)))
                {
                    let quote_offset = content[method_pos..]
                        .find('"')
                        .or_else(|| content[method_pos..].find('\''));
                    if let Some(quote_rel) = quote_offset {
                        let prop_start = method_pos + quote_rel + 1;
                        if let Ok(uri) = Url::from_file_path(file_path) {
                            let p = li.position_at(prop_start);
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: prop.name.len() as u32,
                                },
                            );
                        }
                    }
                }
            }
        }
    }
}

fn collect_variable_bindings(module: &Module) -> BindingMap {
    fn register_from_var_decl(var_decl: &ast::VarDecl, bindings: &mut BindingMap) {
        for declarator in &var_decl.decls {
            if let (Pat::Ident(BindingIdent { id, .. }), Some(init)) =
                (&declarator.name, &declarator.init)
            {
                bindings.insert(id.sym.to_string(), Arc::new((**init).clone()));
            }
        }
    }

    let mut bindings: BindingMap = HashMap::new();
    for item in &module.body {
        match item {
            ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Var(var_decl))) => {
                register_from_var_decl(var_decl, &mut bindings);
            }
            ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::ExportDecl(
                swc_ecma_ast::ExportDecl {
                    decl: swc_ecma_ast::Decl::Var(var_decl),
                    ..
                },
            )) => {
                register_from_var_decl(var_decl, &mut bindings);
            }
            _ => {}
        }
    }
    bindings
}

fn collect_module_context(module: &Module) -> ModuleContext {
    let bindings = collect_variable_bindings(module);
    let imports = collect_import_bindings(module);
    let (exports, default_export) = collect_export_bindings(module, &bindings);

    ModuleContext {
        id: module as *const Module as usize,
        bindings,
        imports,
        exports,
        default_export,
    }
}

fn collect_import_bindings(module: &Module) -> HashMap<String, ImportBinding> {
    let mut imports = HashMap::new();
    for item in &module.body {
        if let ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::Import(import)) = item {
            for specifier in &import.specifiers {
                match specifier {
                    ImportSpecifier::Default(default_spec) => {
                        imports.insert(
                            default_spec.local.sym.to_string(),
                            ImportBinding {
                                specifier: import.src.value.to_string(),
                                kind: ImportKind::Default,
                            },
                        );
                    }
                    ImportSpecifier::Named(named_spec) => {
                        let imported = named_spec
                            .imported
                            .as_ref()
                            .map(|name| match name {
                                ModuleExportName::Ident(ident) => ident.sym.to_string(),
                                ModuleExportName::Str(str_lit) => str_lit.value.to_string(),
                            })
                            .unwrap_or_else(|| named_spec.local.sym.to_string());
                        imports.insert(
                            named_spec.local.sym.to_string(),
                            ImportBinding {
                                specifier: import.src.value.to_string(),
                                kind: ImportKind::Named(imported),
                            },
                        );
                    }
                    ImportSpecifier::Namespace(_) => {
                        // Namespace imports are not yet resolved for builder chains
                    }
                }
            }
        }
    }
    imports
}

fn collect_export_bindings(
    module: &Module,
    bindings: &BindingMap,
) -> (HashMap<String, Arc<Expr>>, Option<Arc<Expr>>) {
    let mut exports = HashMap::new();
    let mut default_export: Option<Arc<Expr>> = None;

    for item in &module.body {
        if let ModuleItem::ModuleDecl(decl) = item {
            match decl {
                swc_ecma_ast::ModuleDecl::ExportDecl(export_decl) => {
                    if let swc_ecma_ast::Decl::Var(var_decl) = &export_decl.decl {
                        for declarator in &var_decl.decls {
                            if let Pat::Ident(BindingIdent { id, .. }) = &declarator.name {
                                if let Some(expr) = bindings.get(&id.sym.to_string()) {
                                    exports.insert(id.sym.to_string(), expr.clone());
                                }
                            }
                        }
                    }
                }
                swc_ecma_ast::ModuleDecl::ExportNamed(named) => {
                    if named.src.is_none() {
                        for specifier in &named.specifiers {
                            if let ExportSpecifier::Named(named_spec) = specifier {
                                let exported_name = named_spec
                                    .exported
                                    .as_ref()
                                    .map(|name| match name {
                                        ModuleExportName::Ident(ident) => ident.sym.to_string(),
                                        ModuleExportName::Str(str_lit) => str_lit.value.to_string(),
                                    })
                                    .unwrap_or_else(|| match &named_spec.orig {
                                        ModuleExportName::Ident(ident) => ident.sym.to_string(),
                                        ModuleExportName::Str(str_lit) => str_lit.value.to_string(),
                                    });
                                let local_name = match &named_spec.orig {
                                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                                    ModuleExportName::Str(str_lit) => str_lit.value.to_string(),
                                };
                                if let Some(expr) = bindings.get(&local_name) {
                                    exports.insert(exported_name, expr.clone());
                                }
                            }
                        }
                    }
                }
                swc_ecma_ast::ModuleDecl::ExportDefaultExpr(default_expr) => {
                    default_export = resolve_export_expression(&default_expr.expr, bindings);
                }
                _ => {}
            }
        }
    }

    (exports, default_export)
}

fn resolve_export_expression(expr: &Expr, bindings: &BindingMap) -> Option<Arc<Expr>> {
    match expr {
        Expr::Ident(ident) => bindings.get(&ident.sym.to_string()).cloned(),
        _ => Some(Arc::new(expr.clone())),
    }
}

fn resolve_import_binding(
    current_env: &ModuleEnv,
    binding: &ImportBinding,
    file_cache: &mut FileCache,
    unresolved: &mut Vec<String>,
) -> Option<(Arc<Expr>, ModuleEnv)> {
    let base = match current_env.base_dir.as_deref() {
        Some(base) => base,
        None => {
            unresolved.push(binding.specifier.clone());
            return None;
        }
    };

    let handle = match file_cache.resolve_script(&binding.specifier, base) {
        Some(handle) => handle,
        None => {
            unresolved.push(binding.specifier.clone());
            return None;
        }
    };

    file_cache.ensure_module(&handle.path)?;
    let context = file_cache.ensure_context(&handle.path)?;

    let expr = match &binding.kind {
        ImportKind::Default => context.default_export.clone(),
        ImportKind::Named(name) => context.exports.get(name).cloned(),
    }?;

    let base_dir = handle.path.parent().map(Path::to_path_buf);
    let env = ModuleEnv { context, base_dir };

    Some((expr, env))
}

fn collect_defs_from_imports(
    module: &Module,
    base: &Path,
    index: &mut DefinitionIndex,
    visited: &mut HashSet<PathBuf>,
    unresolved: &mut Vec<String>,
    depth: usize,
    cache: &mut FileCache,
) {
    if depth > 16 {
        return;
    }
    let imports = collect_import_paths(module);
    for import in imports {
        if let Some(handle) = cache.resolve_script(&import, base) {
            if visited.insert(handle.path.clone()) {
                if let Some(child) = cache.ensure_module(&handle.path) {
                    harvest_definitions_from_module(
                        child.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        index,
                    );
                    let next_base = handle.path.parent().unwrap_or(base);
                    collect_defs_from_imports(
                        child.as_ref(),
                        next_base,
                        index,
                        visited,
                        unresolved,
                        depth + 1,
                        cache,
                    );
                }
            }
        } else {
            unresolved.push(import);
        }
    }
}

fn harvest_state_names_from_module(
    module: &Module,
    content: &str,
    file_path: &Path,
    li: &crate::lineindex::LineIndex,
    index: &mut HashMap<String, DefinitionLocation>,
) {
    let bindings = collect_variable_bindings(module);
    for item in &module.body {
        if let Some(definition) = analyze_module_item(item, &bindings) {
            if let Some(name) = definition.name {
                let mut recorded = false;
                if let Some(span) = definition.name_span {
                    if let Some((start, end)) = literal_body_range(span, content) {
                        if let Ok(uri) = Url::from_file_path(file_path) {
                            let p = li.position_at(start);
                            index.insert(
                                name.clone(),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(content, start, end),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(pos) = find_create_call(content, &name) {
                    if let Ok(uri) = Url::from_file_path(file_path) {
                        let p = li.position_at(pos);
                        index.insert(
                            name.clone(),
                            DefinitionLocation {
                                uri,
                                line: p.line,
                                character: p.character,
                                length: name.len() as u32,
                            },
                        );
                    }
                }
            }
        }
    }
}

fn collect_state_names_from_imports(
    module: &Module,
    base: &Path,
    index: &mut HashMap<String, DefinitionLocation>,
    visited: &mut HashSet<PathBuf>,
    unresolved: &mut Vec<String>,
    depth: usize,
    cache: &mut FileCache,
) {
    if depth > 16 {
        return;
    }
    let imports = collect_import_paths(module);
    for import in imports {
        if let Some(handle) = cache.resolve_script(&import, base) {
            if visited.insert(handle.path.clone()) {
                if let Some(child) = cache.ensure_module(&handle.path) {
                    harvest_state_names_from_module(
                        child.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        index,
                    );
                    let next_base = handle.path.parent().unwrap_or(base);
                    collect_state_names_from_imports(
                        child.as_ref(),
                        next_base,
                        index,
                        visited,
                        unresolved,
                        depth + 1,
                        cache,
                    );
                }
            }
        } else {
            unresolved.push(import);
        }
    }
}

fn span_to_byte_range(span: Span) -> Option<(usize, usize)> {
    let lo = span.lo.0;
    let hi = span.hi.0;
    if hi < lo {
        return None;
    }
    let start = lo.checked_sub(1)? as usize;
    let len = (hi - lo) as usize;
    let end = start + len;
    Some((start, end))
}

fn literal_body_range(span: Span, text: &str) -> Option<(usize, usize)> {
    let (start, end) = span_to_byte_range(span)?;
    if end > text.len() {
        return None;
    }
    if end <= start {
        return None;
    }
    let bytes = text.as_bytes();
    if end - start >= 2 {
        let first = bytes.get(start)?;
        let last = bytes.get(end - 1)?;
        if (*first == b'"' && *last == b'"') || (*first == b'\'' && *last == b'\'') {
            return Some((start + 1, end - 1));
        }
        if *first == b'`' && *last == b'`' {
            return Some((start + 1, end - 1));
        }
    }
    Some((start, end))
}

fn slice_char_length(text: &str, start: usize, end: usize) -> u32 {
    text[start..end].chars().count() as u32
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

#[derive(Clone)]
struct PropertyDefinition {
    name: String,
    ty: TypeInfo,
    span: Option<Span>,
}

#[derive(Default)]
struct StateDefinition {
    name: Option<String>,
    name_span: Option<Span>,
    properties: Vec<PropertyDefinition>,
}

fn analyze_module_item_with_env(
    item: &ModuleItem,
    env: &ModuleEnv,
    file_cache: &mut FileCache,
    unresolved: &mut Vec<String>,
) -> Option<StateDefinition> {
    match item {
        ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Var(var_decl)))
        | ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::ExportDecl(
            swc_ecma_ast::ExportDecl {
                decl: swc_ecma_ast::Decl::Var(var_decl),
                ..
            },
        )) => {
            for declarator in &var_decl.decls {
                if let Some(init) = declarator.init.as_ref() {
                    if let Some(def) =
                        analyze_expression_with_env(init.as_ref(), env, file_cache, unresolved)
                    {
                        return Some(def);
                    }
                }
            }
            None
        }
        ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::ExportDefaultExpr(default_expr)) => {
            analyze_expression_with_env(&default_expr.expr, env, file_cache, unresolved)
        }
        ModuleItem::Stmt(_) | ModuleItem::ModuleDecl(_) => None,
    }
}

fn analyze_expression_with_env(
    expr: &Expr,
    env: &ModuleEnv,
    file_cache: &mut FileCache,
    unresolved: &mut Vec<String>,
) -> Option<StateDefinition> {
    if let Expr::Call(call) = expr {
        if let Callee::Expr(callee) = &call.callee {
            if let Expr::Member(member) = &**callee {
                if property_name(member)? == "build" {
                    let mut definition = StateDefinition::default();
                    if let Some(arg) = call.args.first() {
                        if let Some((name, span)) = string_from_expr(&arg.expr) {
                            definition.name = Some(name);
                            definition.name_span = span;
                        }
                    }
                    let mut visited = HashSet::new();
                    collect_chain_with_env(
                        &member.obj,
                        &mut definition,
                        env,
                        file_cache,
                        unresolved,
                        &mut visited,
                    )?;
                    return Some(definition);
                }
            }
        }
    }
    None
}

fn collect_chain_with_env(
    expr: &Expr,
    definition: &mut StateDefinition,
    env: &ModuleEnv,
    file_cache: &mut FileCache,
    unresolved: &mut Vec<String>,
    visited: &mut HashSet<(usize, String)>,
) -> Option<()> {
    match expr {
        Expr::Call(call) => {
            if let Callee::Expr(callee) = &call.callee {
                if let Expr::Member(member) = &**callee {
                    let method = property_name(member)?;
                    match method.as_str() {
                        "add" => {
                            if let Some(ExprOrSpread {
                                expr: prop_expr, ..
                            }) = call.args.first()
                            {
                                if let Some((prop_name, span)) = string_from_expr(prop_expr) {
                                    let ty = call
                                        .args
                                        .get(1)
                                        .and_then(|arg| infer_type_from_expr(&arg.expr))
                                        .unwrap_or(TypeInfo::Any);
                                    definition.properties.push(PropertyDefinition {
                                        name: prop_name,
                                        ty,
                                        span,
                                    });
                                } else if let Expr::Object(obj) = prop_expr.as_ref() {
                                    collect_properties_from_object_literal(obj, definition);
                                }
                            }
                            collect_chain_with_env(
                                &member.obj,
                                definition,
                                env,
                                file_cache,
                                unresolved,
                                visited,
                            )
                        }
                        "derive" => {
                            if let Some((prop_name, span)) = call
                                .args
                                .first()
                                .and_then(|arg| string_from_expr(&arg.expr))
                            {
                                definition.properties.push(PropertyDefinition {
                                    name: prop_name,
                                    ty: TypeInfo::Any,
                                    span,
                                });
                            }
                            collect_chain_with_env(
                                &member.obj,
                                definition,
                                env,
                                file_cache,
                                unresolved,
                                visited,
                            )
                        }
                        "create" => {
                            if definition.name.is_none() {
                                if let Some(ExprOrSpread {
                                    expr: name_expr, ..
                                }) = call.args.first()
                                {
                                    if let Some((name, span)) = string_from_expr(name_expr) {
                                        definition.name = Some(name);
                                        definition.name_span = span;
                                    }
                                }
                            }
                            Some(())
                        }
                        _ => collect_chain_with_env(
                            &member.obj,
                            definition,
                            env,
                            file_cache,
                            unresolved,
                            visited,
                        ),
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr::Member(member) => collect_chain_with_env(
            &member.obj,
            definition,
            env,
            file_cache,
            unresolved,
            visited,
        ),
        Expr::Ident(ident) => {
            let name = ident.sym.to_string();
            let key = (env.context.id, name.clone());
            if !visited.insert(key) {
                return Some(());
            }
            if let Some(bound) = env.context.bindings.get(&name) {
                collect_chain_with_env(
                    bound.as_ref(),
                    definition,
                    env,
                    file_cache,
                    unresolved,
                    visited,
                )
            } else if let Some(binding) = env.context.imports.get(&name) {
                if let Some((expr, import_env)) =
                    resolve_import_binding(env, binding, file_cache, unresolved)
                {
                    collect_chain_with_env(
                        expr.as_ref(),
                        definition,
                        &import_env,
                        file_cache,
                        unresolved,
                        visited,
                    )
                } else {
                    Some(())
                }
            } else {
                Some(())
            }
        }
        _ => Some(()),
    }
}

fn analyze_module_item(item: &ModuleItem, bindings: &BindingMap) -> Option<StateDefinition> {
    match item {
        ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Var(var_decl)))
        | ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::ExportDecl(
            swc_ecma_ast::ExportDecl {
                decl: swc_ecma_ast::Decl::Var(var_decl),
                ..
            },
        )) => {
            for declarator in &var_decl.decls {
                if let Some(init) = declarator.init.as_ref() {
                    if let Some(def) = analyze_expression(init.as_ref(), bindings) {
                        return Some(def);
                    }
                }
            }
            None
        }
        ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::ExportDefaultExpr(default_expr)) => {
            analyze_expression(&default_expr.expr, bindings)
        }
        ModuleItem::Stmt(_) | ModuleItem::ModuleDecl(_) => None,
    }
}

fn analyze_expression(expr: &Expr, bindings: &BindingMap) -> Option<StateDefinition> {
    if let Expr::Call(call) = expr {
        if let Callee::Expr(callee) = &call.callee {
            if let Expr::Member(member) = &**callee {
                if property_name(member)? == "build" {
                    let mut definition = StateDefinition::default();
                    if let Some(arg) = call.args.first() {
                        if let Some((name, span)) = string_from_expr(&arg.expr) {
                            definition.name = Some(name);
                            definition.name_span = span;
                        }
                    }
                    let mut visited = HashSet::new();
                    collect_chain(&member.obj, &mut definition, bindings, &mut visited)?;
                    return Some(definition);
                }
            }
        }
    }
    None
}

fn collect_chain(
    expr: &Expr,
    definition: &mut StateDefinition,
    bindings: &BindingMap,
    visited: &mut HashSet<String>,
) -> Option<()> {
    match expr {
        Expr::Call(call) => {
            if let Callee::Expr(callee) = &call.callee {
                if let Expr::Member(member) = &**callee {
                    let method = property_name(member)?;
                    match method.as_str() {
                        "add" => {
                            if let Some(ExprOrSpread {
                                expr: prop_expr, ..
                            }) = call.args.first()
                            {
                                if let Some((prop_name, span)) = string_from_expr(prop_expr) {
                                    let ty = call
                                        .args
                                        .get(1)
                                        .and_then(|arg| infer_type_from_expr(&arg.expr))
                                        .unwrap_or(TypeInfo::Any);
                                    definition.properties.push(PropertyDefinition {
                                        name: prop_name,
                                        ty,
                                        span,
                                    });
                                } else if let Expr::Object(obj) = prop_expr.as_ref() {
                                    collect_properties_from_object_literal(obj, definition);
                                }
                            }
                            collect_chain(&member.obj, definition, bindings, visited)
                        }
                        "derive" => {
                            if let Some((prop_name, span)) = call
                                .args
                                .first()
                                .and_then(|arg| string_from_expr(&arg.expr))
                            {
                                definition.properties.push(PropertyDefinition {
                                    name: prop_name,
                                    ty: TypeInfo::Any,
                                    span,
                                });
                            }
                            collect_chain(&member.obj, definition, bindings, visited)
                        }
                        "create" => {
                            if definition.name.is_none() {
                                if let Some(ExprOrSpread {
                                    expr: name_expr, ..
                                }) = call.args.first()
                                {
                                    if let Some((name, span)) = string_from_expr(name_expr) {
                                        definition.name = Some(name);
                                        definition.name_span = span;
                                    }
                                }
                            }
                            Some(())
                        }
                        _ => collect_chain(&member.obj, definition, bindings, visited),
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr::Member(member) => collect_chain(&member.obj, definition, bindings, visited),
        Expr::Ident(ident) => {
            let name = ident.sym.to_string();
            if !visited.insert(name.clone()) {
                return Some(());
            }
            if let Some(bound) = bindings.get(&name) {
                collect_chain(bound.as_ref(), definition, bindings, visited)
            } else {
                Some(())
            }
        }
        _ => Some(()),
    }
}

fn collect_properties_from_object_literal(object: &ObjectLit, definition: &mut StateDefinition) {
    for entry in &object.props {
        let PropOrSpread::Prop(prop) = entry else {
            continue;
        };
        match &**prop {
            Prop::KeyValue(KeyValueProp { key, value }) => {
                if let Some(name) = property_name_from_prop(key) {
                    let ty = infer_type_from_expr(value).unwrap_or(TypeInfo::Any);
                    definition.properties.push(PropertyDefinition {
                        name,
                        ty,
                        span: span_from_prop_name(key),
                    });
                }
            }
            Prop::Shorthand(ident) => {
                definition.properties.push(PropertyDefinition {
                    name: ident.sym.to_string(),
                    ty: TypeInfo::Any,
                    span: Some(ident.span),
                });
            }
            Prop::Method(method) => {
                if let Some(name) = property_name_from_prop(&method.key) {
                    definition.properties.push(PropertyDefinition {
                        name,
                        ty: TypeInfo::Any,
                        span: span_from_prop_name(&method.key).or(Some(method.function.span)),
                    });
                }
            }
            Prop::Getter(getter) => {
                if let Some(name) = property_name_from_prop(&getter.key) {
                    definition.properties.push(PropertyDefinition {
                        name,
                        ty: TypeInfo::Any,
                        span: span_from_prop_name(&getter.key).or(Some(getter.span)),
                    });
                }
            }
            Prop::Setter(setter) => {
                if let Some(name) = property_name_from_prop(&setter.key) {
                    definition.properties.push(PropertyDefinition {
                        name,
                        ty: TypeInfo::Any,
                        span: span_from_prop_name(&setter.key).or(Some(setter.span)),
                    });
                }
            }
            _ => {}
        }
    }
}

fn property_name(member: &MemberExpr) -> Option<String> {
    match &member.prop {
        MemberProp::Ident(ident) => Some(ident.sym.to_string()),
        _ => None,
    }
}

fn string_from_expr(expr: &Expr) -> Option<(String, Option<Span>)> {
    match expr {
        Expr::Lit(Lit::Str(str_lit)) => Some((str_lit.value.to_string(), Some(str_lit.span))),
        Expr::Lit(Lit::Bool(b)) => Some((b.value.to_string(), Some(b.span))),
        Expr::Ident(Ident { sym, span, .. }) => Some((sym.to_string(), Some(*span))),
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
        Expr::TsAs(ts_as) => infer_type_from_ts_type(&ts_as.type_ann).unwrap_or(TypeInfo::Any),
        Expr::TsTypeAssertion(assertion) => {
            infer_type_from_ts_type(&assertion.type_ann).unwrap_or(TypeInfo::Any)
        }
        Expr::TsConstAssertion(assertion) => {
            infer_type_from_expr(&assertion.expr).unwrap_or(TypeInfo::Any)
        }
        Expr::TsNonNull(non_null) => infer_type_from_expr(&non_null.expr).unwrap_or(TypeInfo::Any),
        _ => TypeInfo::Any,
    };
    Some(ty)
}

fn infer_array_type(array: &ArrayLit) -> TypeInfo {
    let mut element: Option<TypeInfo> = None;
    for expr_or_spread in array.elems.iter().flatten() {
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

fn span_from_prop_name(name: &PropName) -> Option<Span> {
    match name {
        PropName::Ident(ident) => Some(ident.span),
        PropName::Str(str_lit) => Some(str_lit.span),
        PropName::Num(num_lit) => Some(num_lit.span),
        PropName::Computed(comp) => Some(comp.span),
        PropName::BigInt(big) => Some(big.span),
    }
}

fn infer_type_from_ts_type(ts_type: &ast::TsType) -> Option<TypeInfo> {
    use ast::TsKeywordTypeKind::*;

    match ts_type {
        ast::TsType::TsKeywordType(keyword) => Some(match keyword.kind {
            TsStringKeyword => TypeInfo::String,
            TsNumberKeyword => TypeInfo::Number,
            TsBooleanKeyword => TypeInfo::Boolean,
            TsNullKeyword => TypeInfo::Null,
            TsAnyKeyword => TypeInfo::Any,
            TsUnknownKeyword => TypeInfo::Unknown,
            TsUndefinedKeyword => TypeInfo::Named("undefined".to_string()),
            TsVoidKeyword => TypeInfo::Named("void".to_string()),
            TsNeverKeyword => TypeInfo::Named("never".to_string()),
            TsObjectKeyword => TypeInfo::Object(HashMap::new()),
            TsBigIntKeyword => TypeInfo::Number,
            TsSymbolKeyword => TypeInfo::Named("symbol".to_string()),
            _ => TypeInfo::Any,
        }),
        ast::TsType::TsUnionOrIntersectionType(union) => match union {
            ast::TsUnionOrIntersectionType::TsUnionType(union_type) => {
                let mut members = Vec::new();
                for ty in &union_type.types {
                    if let Some(info) = infer_type_from_ts_type(ty) {
                        members.push(info);
                    }
                }
                if members.is_empty() {
                    Some(TypeInfo::Any)
                } else {
                    Some(TypeInfo::from_union(members))
                }
            }
            ast::TsUnionOrIntersectionType::TsIntersectionType(_) => Some(TypeInfo::Any),
        },
        ast::TsType::TsArrayType(array_type) => {
            let inner = infer_type_from_ts_type(&array_type.elem_type)?;
            Some(TypeInfo::Array(Box::new(inner)))
        }
        ast::TsType::TsTypeRef(type_ref) => {
            let name = ts_entity_name_to_string(&type_ref.type_name);
            let generic_args: Vec<TypeInfo> = type_ref
                .type_params
                .as_ref()
                .map(|params| {
                    params
                        .params
                        .iter()
                        .filter_map(|param| infer_type_from_ts_type(param))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();

            match name.as_str() {
                "Promise" => {
                    let inner = generic_args.first().cloned().unwrap_or(TypeInfo::Unknown);
                    Some(TypeInfo::Promise(Box::new(inner)))
                }
                "Array" | "ReadonlyArray" => {
                    let inner = generic_args.first().cloned().unwrap_or(TypeInfo::Any);
                    Some(TypeInfo::Array(Box::new(inner)))
                }
                "Set" | "ReadonlySet" => {
                    let inner = generic_args.first().cloned().unwrap_or(TypeInfo::Any);
                    Some(TypeInfo::Set(Box::new(inner)))
                }
                "Map" | "ReadonlyMap" => {
                    let key = generic_args.first().cloned().unwrap_or(TypeInfo::Any);
                    let value = generic_args.get(1).cloned().unwrap_or(TypeInfo::Any);
                    Some(TypeInfo::Map(Box::new(key), Box::new(value)))
                }
                "Record" => {
                    let key = generic_args.first().cloned().unwrap_or(TypeInfo::String);
                    let value = generic_args.get(1).cloned().unwrap_or(TypeInfo::Any);
                    Some(TypeInfo::Map(Box::new(key), Box::new(value)))
                }
                _ => {
                    if generic_args.is_empty() {
                        Some(TypeInfo::Named(name))
                    } else {
                        let rendered = format!(
                            "{}<{}>",
                            name,
                            generic_args
                                .iter()
                                .map(|arg| arg.describe())
                                .collect::<Vec<_>>()
                                .join(", ")
                        );
                        Some(TypeInfo::Named(rendered))
                    }
                }
            }
        }
        ast::TsType::TsTypeLit(type_lit) => {
            let mut props = HashMap::new();
            for member in &type_lit.members {
                if let ast::TsTypeElement::TsPropertySignature(prop) = member {
                    if let Some(name) = ts_property_key_to_string(&prop.key) {
                        let value = prop
                            .type_ann
                            .as_ref()
                            .and_then(|ann| infer_type_from_ts_type(&ann.type_ann))
                            .unwrap_or(TypeInfo::Any);
                        props.insert(name, value);
                    }
                }
            }
            Some(TypeInfo::Object(props))
        }
        ast::TsType::TsParenthesizedType(paren) => infer_type_from_ts_type(&paren.type_ann),
        ast::TsType::TsLitType(lit) => match &lit.lit {
            ast::TsLit::Str(_) => Some(TypeInfo::String),
            ast::TsLit::Bool(_) => Some(TypeInfo::Boolean),
            ast::TsLit::Number(_) => Some(TypeInfo::Number),
            ast::TsLit::BigInt(_) => Some(TypeInfo::Number),
            ast::TsLit::Tpl(_) => Some(TypeInfo::String),
        },
        ast::TsType::TsOptionalType(optional) => {
            let base = infer_type_from_ts_type(&optional.type_ann)?;
            Some(TypeInfo::from_union(vec![
                base,
                TypeInfo::Named("undefined".to_string()),
            ]))
        }
        _ => Some(TypeInfo::Any),
    }
}

fn ts_entity_name_to_string(name: &ast::TsEntityName) -> String {
    match name {
        ast::TsEntityName::Ident(ident) => ident.sym.to_string(),
        ast::TsEntityName::TsQualifiedName(qualified) => {
            format!(
                "{}.{}",
                ts_entity_name_to_string(&qualified.left),
                qualified.right.sym
            )
        }
    }
}

fn ts_property_key_to_string(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(ident) => Some(ident.sym.to_string()),
        Expr::Lit(Lit::Str(str_lit)) => Some(str_lit.value.to_string()),
        Expr::Lit(Lit::Num(num_lit)) => Some(num_lit.value.to_string()),
        _ => None,
    }
}
