use std::io::sink;

use swc_common::{
    errors::{EmitterWriter, Handler, HANDLER},
    sync::Lrc,
    FileName, SourceMap, GLOBALS,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{
    lexer::Lexer,
    token::{Token, Word},
    Parser, StringInput, Syntax, TsConfig,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionTokenKind {
    Keyword,
    Identifier,
    Number,
    String,
    Operator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpressionToken {
    pub start: usize,
    pub end: usize,
    pub kind: ExpressionTokenKind,
}

pub fn parse_expression(expr: &str) -> Result<(), String> {
    let trimmed = expr.trim();
    if trimmed.is_empty() {
        return Err("Empty expression".into());
    }

    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_emitter(
        false,
        false,
        Box::new(EmitterWriter::new(Box::new(sink()), None, false, false)),
    );

    GLOBALS.set(&Default::default(), || {
        HANDLER.set(&handler, || {
            let fm = cm.new_source_file(FileName::Anon, trimmed.to_string());
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
            match parser.parse_expr() {
                Ok(_) => {
                    for err in parser.take_errors() {
                        err.into_diagnostic(&handler).emit();
                        return Err("Failed to parse expression".into());
                    }
                    Ok(())
                }
                Err(err) => {
                    err.into_diagnostic(&handler).emit();
                    Err("Failed to parse expression".into())
                }
            }
        })
    })
}

pub fn tokenize_expression(expr: &str) -> Vec<ExpressionToken> {
    let trimmed = expr.trim();
    if trimmed.is_empty() {
        return Vec::new();
    }

    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_emitter(
        false,
        false,
        Box::new(EmitterWriter::new(Box::new(sink()), None, false, false)),
    );

    GLOBALS.set(&Default::default(), || {
        HANDLER.set(&handler, || {
            let fm = cm.new_source_file(FileName::Anon, trimmed.to_string());
            let mut lexer = Lexer::new(
                Syntax::Typescript(TsConfig {
                    tsx: false,
                    dts: false,
                    ..Default::default()
                }),
                EsVersion::Es2022,
                StringInput::from(&*fm),
                None,
            );

            let mut tokens = Vec::new();
            while let Some(token_and_span) = lexer.next() {
                if let Some(kind) = classify_token(&token_and_span.token) {
                    let start = token_and_span.span.lo.0 as usize;
                    let end = token_and_span.span.hi.0 as usize;
                    if end > start {
                        tokens.push(ExpressionToken { start, end, kind });
                    }
                }
            }

            tokens
        })
    })
}

fn classify_token(token: &Token) -> Option<ExpressionTokenKind> {
    use ExpressionTokenKind as Kind;

    match token {
        Token::Word(word) => Some(match word {
            Word::Keyword(_) | Word::Null | Word::True | Word::False => Kind::Keyword,
            Word::Ident(_) => Kind::Identifier,
        }),
        Token::Str { .. } | Token::Template { .. } => Some(Kind::String),
        Token::Regex(_, _) => Some(Kind::String),
        Token::Num { .. } | Token::BigInt { .. } => Some(Kind::Number),
        Token::BinOp(_) | Token::AssignOp(_) => Some(Kind::Operator),
        Token::Arrow
        | Token::Hash
        | Token::At
        | Token::Dot
        | Token::DotDotDot
        | Token::Bang
        | Token::LParen
        | Token::RParen
        | Token::LBracket
        | Token::RBracket
        | Token::LBrace
        | Token::RBrace
        | Token::Semi
        | Token::Comma
        | Token::Colon
        | Token::DollarLBrace
        | Token::QuestionMark
        | Token::PlusPlus
        | Token::MinusMinus
        | Token::Tilde
        | Token::BackQuote
        | Token::JSXTagStart
        | Token::JSXTagEnd
        | Token::Shebang(_) => Some(Kind::Operator),
        Token::JSXName { .. } => Some(Kind::Identifier),
        Token::JSXText { .. } => Some(Kind::String),
        Token::Error(_) => None,
    }
}
