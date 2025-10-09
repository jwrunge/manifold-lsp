use std::io::sink;

use swc_common::{
    errors::{EmitterWriter, Handler, HANDLER},
    sync::Lrc,
    FileName, SourceMap, GLOBALS,
};
use swc_ecma_ast::{self as ast, EsVersion, Expr};
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
    parse_expression_ast(expr).map(|_| ())
}

pub fn parse_expression_ast(expr: &str) -> Result<Box<Expr>, String> {
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
                Ok(ast) => {
                    for err in parser.take_errors() {
                        err.into_diagnostic(&handler).emit();
                        return Err("Failed to parse expression".into());
                    }
                    Ok(ast)
                }
                Err(err) => {
                    err.into_diagnostic(&handler).emit();
                    Err("Failed to parse expression".into())
                }
            }
        })
    })
}

pub fn validate_expression(
    expr: &str,
    allow_assignments: bool,
    allow_inline_functions: bool,
) -> Result<(), String> {
    let ast = parse_expression_ast(expr)?;
    let restrictions = Restrictions {
        allow_assignments,
        allow_inline_functions,
    };
    check_expr(&ast, &restrictions)
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
                    let start = token_and_span.span.lo.0.saturating_sub(1) as usize;
                    let end = token_and_span.span.hi.0.saturating_sub(1) as usize;
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

struct Restrictions {
    allow_assignments: bool,
    allow_inline_functions: bool,
}

fn check_expr(expr: &Expr, restrictions: &Restrictions) -> Result<(), String> {
    use ast::Expr::*;

    match expr {
        Assign(assign) => {
            if !restrictions.allow_assignments {
                return Err("Assignments are not allowed in this Manifold expression.".into());
            }
            check_expr(&assign.right, restrictions)
        }
        Update(update) => {
            if !restrictions.allow_assignments {
                return Err("Assignments are not allowed in this Manifold expression.".into());
            }
            check_expr(&update.arg, restrictions)
        }
        Arrow(arrow) => {
            if !restrictions.allow_inline_functions {
                return Err(
                    "Inline functions are not supported in this Manifold expression.".into(),
                );
            }
            match &*arrow.body {
                ast::BlockStmtOrExpr::BlockStmt(block) => {
                    for stmt in &block.stmts {
                        check_stmt(stmt, restrictions)?;
                    }
                }
                ast::BlockStmtOrExpr::Expr(body_expr) => {
                    check_expr(body_expr, restrictions)?;
                }
            }
            Ok(())
        }
        Fn(fn_expr) => {
            if !restrictions.allow_inline_functions {
                return Err(
                    "Inline functions are not supported in this Manifold expression.".into(),
                );
            }
            if let Some(body) = &fn_expr.function.body {
                for stmt in &body.stmts {
                    check_stmt(stmt, restrictions)?;
                }
            }
            Ok(())
        }
        Array(array) => {
            for elem in &array.elems {
                if let Some(expr_or_spread) = elem {
                    check_expr(&expr_or_spread.expr, restrictions)?;
                }
            }
            Ok(())
        }
        Object(object) => {
            for prop in &object.props {
                check_prop_or_spread(prop, restrictions)?;
            }
            Ok(())
        }
        Call(call) => {
            match &call.callee {
                ast::Callee::Expr(expr) => check_expr(expr, restrictions)?,
                ast::Callee::Super(_) | ast::Callee::Import(_) => {}
            }
            for arg in &call.args {
                check_expr(&arg.expr, restrictions)?;
            }
            Ok(())
        }
        New(new_expr) => {
            check_expr(&new_expr.callee, restrictions)?;
            if let Some(args) = &new_expr.args {
                for arg in args {
                    check_expr(&arg.expr, restrictions)?;
                }
            }
            Ok(())
        }
        Unary(unary) => check_expr(&unary.arg, restrictions),
        Bin(bin) => {
            check_expr(&bin.left, restrictions)?;
            check_expr(&bin.right, restrictions)
        }
        Cond(cond) => {
            check_expr(&cond.test, restrictions)?;
            check_expr(&cond.cons, restrictions)?;
            check_expr(&cond.alt, restrictions)
        }
        Seq(seq) => {
            for expr in &seq.exprs {
                check_expr(expr, restrictions)?;
            }
            Ok(())
        }
        Paren(paren) => check_expr(&paren.expr, restrictions),
        Tpl(tpl) => {
            for expr in &tpl.exprs {
                check_expr(expr, restrictions)?;
            }
            Ok(())
        }
        TaggedTpl(tagged) => {
            check_expr(&tagged.tag, restrictions)?;
            for expr in &tagged.tpl.exprs {
                check_expr(expr, restrictions)?;
            }
            Ok(())
        }
        Member(member) => {
            check_expr(&member.obj, restrictions)?;
            match &member.prop {
                ast::MemberProp::Computed(comp) => check_expr(&comp.expr, restrictions)?,
                _ => {}
            }
            Ok(())
        }
        OptChain(opt) => check_opt_chain(opt, restrictions),
        Await(await_expr) => check_expr(&await_expr.arg, restrictions),
        Yield(yield_expr) => {
            if let Some(arg) = &yield_expr.arg {
                check_expr(arg, restrictions)?;
            }
            Ok(())
        }
        TsAs(ts) => check_expr(&ts.expr, restrictions),
        TsConstAssertion(ts) => check_expr(&ts.expr, restrictions),
        TsNonNull(ts) => check_expr(&ts.expr, restrictions),
        TsTypeAssertion(ts) => check_expr(&ts.expr, restrictions),
        TsInstantiation(ts) => check_expr(&ts.expr, restrictions),
        TsSatisfies(ts) => check_expr(&ts.expr, restrictions),
        _ => Ok(()),
    }
}

fn check_stmt(stmt: &ast::Stmt, restrictions: &Restrictions) -> Result<(), String> {
    match stmt {
        ast::Stmt::Expr(expr_stmt) => check_expr(&expr_stmt.expr, restrictions),
        ast::Stmt::Return(ret) => {
            if let Some(arg) = &ret.arg {
                check_expr(arg, restrictions)?;
            }
            Ok(())
        }
        ast::Stmt::If(if_stmt) => {
            check_expr(&if_stmt.test, restrictions)?;
            check_stmt(&if_stmt.cons, restrictions)?;
            if let Some(alt) = &if_stmt.alt {
                check_stmt(alt, restrictions)?;
            }
            Ok(())
        }
        ast::Stmt::Block(block) => {
            for stmt in &block.stmts {
                check_stmt(stmt, restrictions)?;
            }
            Ok(())
        }
        ast::Stmt::While(while_stmt) => {
            check_expr(&while_stmt.test, restrictions)?;
            check_stmt(&while_stmt.body, restrictions)
        }
        ast::Stmt::DoWhile(do_while) => {
            check_stmt(&do_while.body, restrictions)?;
            check_expr(&do_while.test, restrictions)
        }
        ast::Stmt::For(for_stmt) => {
            if let Some(init) = &for_stmt.init {
                match init {
                    ast::VarDeclOrExpr::VarDecl(var_decl) => {
                        for decl in &var_decl.decls {
                            if let Some(init_expr) = &decl.init {
                                check_expr(init_expr, restrictions)?;
                            }
                        }
                    }
                    ast::VarDeclOrExpr::Expr(expr) => {
                        check_expr(expr, restrictions)?;
                    }
                }
            }
            if let Some(test) = &for_stmt.test {
                check_expr(test, restrictions)?;
            }
            if let Some(update) = &for_stmt.update {
                check_expr(update, restrictions)?;
            }
            check_stmt(&for_stmt.body, restrictions)
        }
        ast::Stmt::ForOf(for_of) => {
            check_expr(&for_of.right, restrictions)?;
            check_stmt(&for_of.body, restrictions)
        }
        ast::Stmt::ForIn(for_in) => {
            check_expr(&for_in.right, restrictions)?;
            check_stmt(&for_in.body, restrictions)
        }
        ast::Stmt::Switch(switch_stmt) => {
            check_expr(&switch_stmt.discriminant, restrictions)?;
            for case in &switch_stmt.cases {
                if let Some(test) = &case.test {
                    check_expr(test, restrictions)?;
                }
                for stmt in &case.cons {
                    check_stmt(stmt, restrictions)?;
                }
            }
            Ok(())
        }
        ast::Stmt::Try(try_stmt) => {
            check_block_stmt_or_stmts(&try_stmt.block, restrictions)?;
            if let Some(catch) = &try_stmt.handler {
                check_block_stmt_or_stmts(&catch.body, restrictions)?;
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                check_block_stmt_or_stmts(finalizer, restrictions)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn check_block_stmt_or_stmts(
    block: &ast::BlockStmt,
    restrictions: &Restrictions,
) -> Result<(), String> {
    for stmt in &block.stmts {
        check_stmt(stmt, restrictions)?;
    }
    Ok(())
}

fn check_prop_or_spread(
    prop: &ast::PropOrSpread,
    restrictions: &Restrictions,
) -> Result<(), String> {
    match prop {
        ast::PropOrSpread::Prop(prop) => match &**prop {
            ast::Prop::KeyValue(kv) => check_expr(&kv.value, restrictions),
            ast::Prop::Method(method) => {
                if !restrictions.allow_inline_functions {
                    return Err(
                        "Inline functions are not supported in this Manifold expression.".into(),
                    );
                }
                if let Some(body) = &method.function.body {
                    for stmt in &body.stmts {
                        check_stmt(stmt, restrictions)?;
                    }
                }
                Ok(())
            }
            ast::Prop::Getter(getter) => {
                if !restrictions.allow_inline_functions {
                    return Err(
                        "Inline functions are not supported in this Manifold expression.".into(),
                    );
                }
                if let Some(body) = &getter.body {
                    check_block_stmt_or_stmts(body, restrictions)?;
                }
                Ok(())
            }
            ast::Prop::Setter(setter) => {
                if !restrictions.allow_inline_functions {
                    return Err(
                        "Inline functions are not supported in this Manifold expression.".into(),
                    );
                }
                if let Some(body) = &setter.body {
                    check_block_stmt_or_stmts(body, restrictions)?;
                }
                Ok(())
            }
            ast::Prop::Assign(assign) => check_expr(&assign.value, restrictions),
            ast::Prop::Shorthand(_) => Ok(()),
        },
        ast::PropOrSpread::Spread(spread) => check_expr(&spread.expr, restrictions),
    }
}

fn check_opt_chain(opt: &ast::OptChainExpr, restrictions: &Restrictions) -> Result<(), String> {
    use ast::OptChainBase;

    match &*opt.base {
        OptChainBase::Member(member) => {
            check_expr(&member.obj, restrictions)?;
            if let ast::MemberProp::Computed(comp) = &member.prop {
                check_expr(&comp.expr, restrictions)?;
            }
            Ok(())
        }
        OptChainBase::Call(call) => {
            check_expr(&call.callee, restrictions)?;
            for arg in &call.args {
                check_expr(&arg.expr, restrictions)?;
            }
            Ok(())
        }
    }
}
