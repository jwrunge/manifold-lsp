use rust_lapper::{Interval, Lapper};
use std::fmt::Display;

use crate::{
    manifold_lang::{
        Ast, DirectiveType, ManifoldAttribute, ManifoldElement, ManifoldInterpolation,
        ManifoldValue,
    },
    span::Span,
    symbol_table::{ReferenceId, SymbolId, SymbolTable},
};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, SemanticError>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IdentType {
    Binding(SymbolId),
    Reference(ReferenceId),
}
type IdentRangeLapper = Lapper<usize, IdentType>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    Number,
    Boolean,
    Null,
    Array(Box<Type>),
    Object(std::collections::HashMap<String, Type>),
    Function,
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Null => write!(f, "null"),
            Type::Array(ty) => write!(f, "array<{}>", ty),
            Type::Object(_) => write!(f, "object"),
            Type::Function => write!(f, "function"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Undefined variable {name} in Manifold expression")]
    UndefinedVariable { name: String, span: Span },
    #[error("Invalid directive usage: {directive}")]
    InvalidDirective { directive: String, span: Span },
    #[error("Manifold registration required for element with directives")]
    ManifoldRegistrationRequired { span: Span },
    #[error("Element is ignored by data-mf-ignore")]
    ElementIgnored { span: Span },
    #[error("Invalid interpolation syntax")]
    InvalidInterpolation { span: Span },
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch {
        expected: String,
        actual: String,
        span: Span,
    },
}

impl SemanticError {
    pub fn span(&self) -> Span {
        match self {
            SemanticError::UndefinedVariable { span, .. } => span.clone(),
            SemanticError::InvalidDirective { span, .. } => span.clone(),
            SemanticError::ManifoldRegistrationRequired { span } => span.clone(),
            SemanticError::ElementIgnored { span } => span.clone(),
            SemanticError::InvalidInterpolation { span } => span.clone(),
            SemanticError::TypeMismatch { span, .. } => span.clone(),
        }
    }
}

#[derive(Debug)]
pub struct Semantic {
    pub table: SymbolTable,
    pub ident_range: IdentRangeLapper,
}

#[derive(Debug)]
pub struct ManifoldCtx {
    // Track Manifold registrations by name
    registered_contexts: std::collections::HashMap<Option<String>, ManifoldScope>,
    table: SymbolTable,
    current_scope: Option<String>,
    current_element_is_registered: bool,
    current_element_is_ignored: bool,
}

#[derive(Debug, Clone)]
pub struct ManifoldScope {
    pub name: Option<String>,
    pub variables: std::collections::HashMap<String, (Type, Span)>,
    pub is_ignored: bool,
}

impl ManifoldCtx {
    fn new() -> Self {
        let mut ctx = ManifoldCtx {
            registered_contexts: std::collections::HashMap::new(),
            table: SymbolTable::default(),
            current_scope: None,
            current_element_is_registered: false,
            current_element_is_ignored: false,
        };

        // Add default unnamed scope with common Manifold variables
        let mut default_scope = ManifoldScope {
            name: None,
            variables: std::collections::HashMap::new(),
            is_ignored: false,
        };

        // Add some default Manifold global variables/functions
        default_scope.variables.insert(
            "$".to_string(),
            (
                Type::Object(std::collections::HashMap::new()),
                Span { start: 0, end: 0 },
            ),
        );

        ctx.registered_contexts.insert(None, default_scope);
        ctx
    }

    fn enter_element(&mut self, element: &ManifoldElement) {
        match element {
            ManifoldElement::HtmlElement {
                is_manifold_registered,
                registration_name,
                attributes,
                ..
            } => {
                // Check if this element has data-mf-ignore
                let has_ignore = attributes
                    .iter()
                    .any(|attr| matches!(attr, ManifoldAttribute::ManifoldIgnore { .. }));

                if has_ignore {
                    self.current_element_is_ignored = true;
                    return;
                }

                // Inherit parent's ignore state if not explicitly registered
                if !is_manifold_registered && self.current_element_is_ignored {
                    return;
                }

                if *is_manifold_registered {
                    self.current_scope = registration_name.clone();
                    self.current_element_is_registered = true;
                    self.current_element_is_ignored = false;

                    // Create scope if it doesn't exist
                    if !self.registered_contexts.contains_key(registration_name) {
                        let scope = ManifoldScope {
                            name: registration_name.clone(),
                            variables: std::collections::HashMap::new(),
                            is_ignored: false,
                        };
                        self.registered_contexts
                            .insert(registration_name.clone(), scope);
                    }
                }
            }
            _ => {}
        }
    }

    fn find_variable(&self, name: &str) -> Option<(Type, Span)> {
        if let Some(scope) = self.registered_contexts.get(&self.current_scope) {
            if let Some(var) = scope.variables.get(name) {
                return Some(var.clone());
            }
        }

        // Fall back to default scope
        if let Some(default_scope) = self.registered_contexts.get(&None) {
            default_scope.variables.get(name).cloned()
        } else {
            None
        }
    }
}

pub fn analyze_program(ast: &Ast) -> Result<Semantic> {
    let mut ctx = ManifoldCtx::new();

    for (element, _span) in ast.iter() {
        analyze_element(element, &mut ctx)?;
    }

    let mut ident_range = IdentRangeLapper::new(vec![]);
    for (symbol_id, range) in ctx.table.symbol_id_to_span.iter_enumerated() {
        ident_range.insert(Interval {
            start: range.start,
            stop: range.end,
            val: IdentType::Binding(symbol_id),
        });
    }

    for (reference_id, reference) in ctx.table.reference_id_to_reference.iter_enumerated() {
        let range = &reference.span;
        ident_range.insert(Interval {
            start: range.start,
            stop: range.end,
            val: IdentType::Reference(reference_id),
        });
    }

    Ok(Semantic {
        table: ctx.table,
        ident_range,
    })
}

fn analyze_element(element: &ManifoldElement, ctx: &mut ManifoldCtx) -> Result<()> {
    ctx.enter_element(element);

    match element {
        ManifoldElement::HtmlElement {
            attributes,
            children,
            span,
            ..
        } => {
            // Skip analysis if element is ignored
            if ctx.current_element_is_ignored {
                return Ok(());
            }

            // Analyze attributes for Manifold directives
            for attr in attributes {
                analyze_attribute(attr, ctx)?;
            }

            // Analyze children
            for child in children {
                analyze_element(child, ctx)?;
            }
        }

        ManifoldElement::TextContent {
            interpolations,
            span,
            ..
        } => {
            // Skip if in ignored context
            if ctx.current_element_is_ignored {
                return Ok(());
            }

            // Analyze interpolations
            for interpolation in interpolations {
                analyze_interpolation(interpolation, ctx)?;
            }
        }

        ManifoldElement::ManifoldDirective {
            directive_type,
            expression,
            span,
        } => {
            if ctx.current_element_is_ignored {
                return Ok(());
            }

            if !ctx.current_element_is_registered {
                return Err(SemanticError::ManifoldRegistrationRequired { span: span.clone() });
            }

            analyze_directive(directive_type, expression, span, ctx)?;
        }
    }

    Ok(())
}

fn analyze_attribute(attr: &ManifoldAttribute, ctx: &mut ManifoldCtx) -> Result<()> {
    match attr {
        ManifoldAttribute::ManifoldDirective {
            directive_type,
            expression,
            span,
        } => {
            if !ctx.current_element_is_registered {
                return Err(SemanticError::ManifoldRegistrationRequired { span: span.clone() });
            }

            analyze_directive(directive_type, expression, span, ctx)?;
        }

        ManifoldAttribute::ManifoldRegister { name, span } => {
            // Register this as a symbol
            ctx.table.add_symbol(span.clone());
        }

        _ => {} // Regular attributes and data-mf-ignore don't need semantic analysis
    }

    Ok(())
}

fn analyze_interpolation(
    interpolation: &ManifoldInterpolation,
    ctx: &mut ManifoldCtx,
) -> Result<()> {
    if !ctx.current_element_is_registered {
        return Err(SemanticError::ManifoldRegistrationRequired {
            span: interpolation.span.clone(),
        });
    }

    // Simple variable reference analysis for ${variable} syntax
    let expr = interpolation.expression.trim();

    // Basic identifier check (this would be more sophisticated in a real implementation)
    if expr
        .chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '$')
    {
        if let Some((_var_type, var_span)) = ctx.find_variable(expr) {
            let symbol_id = if let Some(&existing_id) = ctx.table.span_to_symbol_id.get(&var_span) {
                existing_id
            } else {
                ctx.table.add_symbol(var_span.clone())
            };
            ctx.table
                .add_reference(interpolation.span.clone(), Some(symbol_id));
        } else {
            return Err(SemanticError::UndefinedVariable {
                name: expr.to_string(),
                span: interpolation.span.clone(),
            });
        }
    }

    Ok(())
}

fn analyze_directive(
    directive_type: &DirectiveType,
    expression: &str,
    span: &Span,
    ctx: &mut ManifoldCtx,
) -> Result<()> {
    match directive_type {
        DirectiveType::If | DirectiveType::ElseIf => {
            // Analyze boolean expression
            analyze_expression(expression, span, ctx, &Type::Boolean)?;
        }

        DirectiveType::Each => {
            // Analyze array expression and binding
            // For "items as item" syntax, we'd need more sophisticated parsing
            analyze_expression(expression, span, ctx, &Type::Array(Box::new(Type::Unknown)))?;
        }

        DirectiveType::Event(_event_name) => {
            // Analyze event handler expression (usually a function call or assignment)
            analyze_expression(expression, span, ctx, &Type::Function)?;
        }

        DirectiveType::Bind(_) | DirectiveType::Sync(_) => {
            // Analyze binding expression
            analyze_expression(expression, span, ctx, &Type::Unknown)?;
        }

        DirectiveType::Class(_) | DirectiveType::Style(_) => {
            // Analyze conditional class/style expressions
            analyze_expression(expression, span, ctx, &Type::Boolean)?;
        }

        _ => {
            // Other directive types
            analyze_expression(expression, span, ctx, &Type::Unknown)?;
        }
    }

    Ok(())
}

fn analyze_expression(
    expression: &str,
    span: &Span,
    ctx: &mut ManifoldCtx,
    _expected_type: &Type,
) -> Result<()> {
    // Very basic expression analysis - in a real implementation this would parse JS expressions
    let expr = expression.trim();

    // Look for simple variable references
    if expr
        .chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '$')
    {
        if let Some((_var_type, var_span)) = ctx.find_variable(expr) {
            let symbol_id = if let Some(&existing_id) = ctx.table.span_to_symbol_id.get(&var_span) {
                existing_id
            } else {
                ctx.table.add_symbol(var_span.clone())
            };
            ctx.table.add_reference(span.clone(), Some(symbol_id));
        } else {
            return Err(SemanticError::UndefinedVariable {
                name: expr.to_string(),
                span: span.clone(),
            });
        }
    }

    Ok(())
}
