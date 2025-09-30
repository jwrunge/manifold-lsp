use chumsky::Parser;
use chumsky::{prelude::*, stream::Stream};
use core::fmt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tower_lsp::lsp_types::SemanticTokenType;

use crate::semantic_token::LEGEND_TYPE;
use crate::span::Span;

/// Parser for Manifold language features in HTML files
/// Handles data-mf-register/data-mf-ignore attributes, :directives, and ${} interpolations

#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    // HTML tokens
    TagOpen,
    TagClose,
    TagSelfClose,
    TagName(String),
    AttrName(String),
    AttrValue(String),
    Text(String),

    // Manifold-specific tokens
    ManifoldRegister(Option<String>), // data-mf-register="name" or data-mf-register
    ManifoldIgnore,                   // data-mf-ignore
    ManifoldDirective(String),        // :if, :each, :onclick, etc.
    Interpolation(String),            // ${expression}

    // JavaScript expression tokens (for interpolations and directives)
    Identifier(String),
    String(String),
    Number(String),
    Boolean(bool),
    Null,
    Operator(String),

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    Dot,

    // Keywords for directive expressions
    If,
    Else,
    Each,
    As,
    Await,
    Then,
    Catch,
}

pub type Ast = Vec<Spanned<ManifoldElement>>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::TagOpen => write!(f, "<"),
            Token::TagClose => write!(f, ">"),
            Token::TagSelfClose => write!(f, "/>"),
            Token::TagName(name) => write!(f, "{}", name),
            Token::AttrName(name) => write!(f, "{}", name),
            Token::AttrValue(val) => write!(f, "\"{}\"", val),
            Token::Text(text) => write!(f, "{}", text),
            Token::ManifoldRegister(name) => match name {
                Some(n) => write!(f, "data-mf-register=\"{}\"", n),
                None => write!(f, "data-mf-register"),
            },
            Token::ManifoldIgnore => write!(f, "data-mf-ignore"),
            Token::ManifoldDirective(dir) => write!(f, ":{}", dir),
            Token::Interpolation(expr) => write!(f, "${{{}}}", expr),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::Null => write!(f, "null"),
            Token::Operator(op) => write!(f, "{}", op),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Each => write!(f, "each"),
            Token::As => write!(f, "as"),
            Token::Await => write!(f, "await"),
            Token::Then => write!(f, "then"),
            Token::Catch => write!(f, "catch"),
        }
    }
}

pub type Spanned<T> = (T, Span);

// Manifold AST structures
#[derive(Debug, Clone)]
pub enum ManifoldElement {
    HtmlElement {
        tag_name: String,
        attributes: Vec<ManifoldAttribute>,
        children: Vec<ManifoldElement>,
        is_manifold_registered: bool,
        registration_name: Option<String>,
        span: Span,
    },
    TextContent {
        text: String,
        interpolations: Vec<ManifoldInterpolation>,
        span: Span,
    },
    ManifoldDirective {
        directive_type: DirectiveType,
        expression: String,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum ManifoldAttribute {
    Regular {
        name: String,
        value: Option<String>,
        span: Span,
    },
    ManifoldRegister {
        name: Option<String>,
        span: Span,
    },
    ManifoldIgnore {
        span: Span,
    },
    ManifoldDirective {
        directive_type: DirectiveType,
        expression: String,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum DirectiveType {
    // Conditional directives
    If,
    ElseIf,
    Else,

    // Loop directives
    Each,

    // Async directives
    Await,
    Then,
    Catch,

    // Event directives
    Event(String), // onclick, oninput, etc.

    // Binding directives
    Bind(String), // :value, :checked, etc.
    Sync(String), // :sync:value, :sync:checked, etc.

    // Style/Class directives
    Class(String), // :class:active, etc.
    Style(String), // :style:color, etc.

    // Transition directives
    Transition,
}

#[derive(Debug, Clone)]
pub struct ManifoldInterpolation {
    pub expression: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ManifoldState {
    pub name: Option<String>,
    pub properties: HashMap<String, ManifoldValue>,
}

#[derive(Debug, Clone)]
pub enum ManifoldValue {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
    Array(Vec<ManifoldValue>),
    Object(HashMap<String, ManifoldValue>),
    Function(String), // Function body as string
}

#[derive(Debug)]
pub struct ParserResult {
    pub ast: Option<Vec<Spanned<ManifoldElement>>>,
    pub parse_errors: Vec<Simple<String>>,
    pub semantic_tokens: Vec<ImCompleteSemanticToken>,
}

// Basic HTML/XML lexer for Manifold
fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let whitespace = one_of(" \t\r\n").repeated();

    // HTML tag name
    let tag_name = text::ident().map(Token::TagName);

    // Attribute name (including Manifold directives)
    let attr_name = text::ident()
        .or(just(':')
            .then(text::ident())
            .map(|(_, name)| format!(":{}", name)))
        .map(|name| {
            if name.starts_with(':') {
                Token::ManifoldDirective(name[1..].to_string())
            } else if name == "data-mf-register" {
                Token::ManifoldRegister(None)
            } else if name == "data-mf-ignore" {
                Token::ManifoldIgnore
            } else {
                Token::AttrName(name)
            }
        });

    // Attribute value
    let attr_value = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::AttrValue);

    // Text interpolation ${expression}
    let interpolation = just("${")
        .ignore_then(filter(|c| *c != '}').repeated())
        .then_ignore(just('}'))
        .collect::<String>()
        .map(Token::Interpolation);

    // Regular text content
    let text = filter(|c| *c != '<' && *c != '$')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Text);

    // HTML structure tokens
    let tag_open = just('<').to(Token::TagOpen);
    let tag_close = just('>').to(Token::TagClose);
    let tag_self_close = just("/>").to(Token::TagSelfClose);

    let token = tag_open
        .or(tag_close)
        .or(tag_self_close)
        .or(tag_name)
        .or(attr_name)
        .or(attr_value)
        .or(interpolation)
        .or(text)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(whitespace)
        .repeated()
}

// Simplified parser for MVP - just identifies Manifold regions and interpolations
pub fn parse(src: &str) -> ParserResult {
    let (tokens, errs) = lexer().parse_recovery(src);

    let (ast, tokenize_errors, semantic_tokens) = if let Some(tokens) = tokens {
        let semantic_tokens = tokens
            .iter()
            .filter_map(|(token, span)| match token {
                Token::TagName(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::TYPE)
                        .unwrap(),
                }),
                Token::ManifoldDirective(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                }),
                Token::Interpolation(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::STRING)
                        .unwrap(),
                }),
                Token::AttrName(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::PROPERTY)
                        .unwrap(),
                }),
                _ => None,
            })
            .collect();

        // For now, create a simple AST structure
        let elements = parse_elements(&tokens);

        (Some(elements), Vec::new(), semantic_tokens)
    } else {
        (None, Vec::new(), Vec::new())
    };

    let parse_errors = errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(tokenize_errors.into_iter())
        .collect();

    ParserResult {
        ast,
        parse_errors,
        semantic_tokens,
    }
}

// Helper function to parse elements from tokens (simplified for MVP)
fn parse_elements(tokens: &[(Token, Span)]) -> Vec<Spanned<ManifoldElement>> {
    let mut elements = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let (token, span) = &tokens[i];

        match token {
            Token::Text(text) => {
                // Check for interpolations in text
                let interpolations = extract_interpolations(text, span.start);
                elements.push((
                    ManifoldElement::TextContent {
                        text: text.clone(),
                        interpolations,
                        span: span.clone(),
                    },
                    span.clone(),
                ));
            }
            Token::TagOpen => {
                // Basic HTML element parsing would go here
                // For MVP, just create a placeholder
                if let Some((element, consumed)) = parse_html_element(&tokens[i..]) {
                    elements.push(element);
                    i += consumed - 1; // -1 because we'll increment at the end
                }
            }
            _ => {}
        }
        i += 1;
    }

    elements
}

// Helper to extract interpolations from text
fn extract_interpolations(text: &str, start_offset: usize) -> Vec<ManifoldInterpolation> {
    let mut interpolations = Vec::new();
    let mut chars = text.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        if c == '$' {
            if let Some((_, '{')) = chars.peek() {
                chars.next(); // consume '{'
                let expr_start = i + 2;
                let mut expr_end = expr_start;
                let mut depth = 1;

                while let Some((j, c)) = chars.next() {
                    if c == '{' {
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            expr_end = j;
                            break;
                        }
                    }
                }

                let expression = text[expr_start..expr_end].to_string();
                interpolations.push(ManifoldInterpolation {
                    expression,
                    span: Span {
                        start: start_offset + i,
                        end: start_offset + expr_end + 1,
                    },
                });
            }
        }
    }

    interpolations
}

// Helper to parse a single HTML element (simplified)
fn parse_html_element(tokens: &[(Token, Span)]) -> Option<(Spanned<ManifoldElement>, usize)> {
    if tokens.is_empty() {
        return None;
    }

    // For MVP, create a basic element
    let span = tokens[0].1.clone();
    let element = ManifoldElement::HtmlElement {
        tag_name: "div".to_string(),
        attributes: Vec::new(),
        children: Vec::new(),
        is_manifold_registered: false,
        registration_name: None,
        span: span.clone(),
    };

    Some(((element, span), 1))
}

// Placeholder for type inference
pub fn type_inference(
    _expr: &Spanned<ManifoldElement>,
    _symbol_type_table: &mut HashMap<Span, ManifoldValue>,
) {
    // Implementation for type inference would go here
}
