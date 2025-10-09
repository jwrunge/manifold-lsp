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
        return Err("Empty expression. Provide a valid Manifold expression such as a variable, property access, or simple operation.".into());
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
                        return Err("Invalid expression syntax. Manifold expressions support literals, variables, property access, arithmetic/logical operations, ternary operators, function calls, and array/object literals. For complex logic, define functions in your Manifold state.".into());
                    }
                    Ok(ast)
                }
                Err(err) => {
                    err.into_diagnostic(&handler).emit();
                    Err("Invalid expression syntax. Manifold expressions support literals, variables, property access, arithmetic/logical operations, ternary operators, function calls, and array/object literals. For complex logic, define functions in your Manifold state.".into())
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
                return Err("Assignment expressions (=) are only allowed in event handlers like :onclick. For complex logic, define functions in your Manifold state and call them instead.".into());
            }
            check_expr(&assign.right, restrictions)
        }
        Update(update) => {
            if !restrictions.allow_assignments {
                return Err("Increment/decrement operators (++, --) are only allowed in event handlers like :onclick. For complex logic, define functions in your Manifold state and call them instead.".into());
            }
            check_expr(&update.arg, restrictions)
        }
        Arrow(arrow) => {
            if !restrictions.allow_inline_functions {
                return Err(
                    "Arrow functions are only supported in event handlers. For complex logic, define functions in your Manifold state and call them instead.".into(),
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
                    "Function expressions are only supported in event handlers. For complex logic, define functions in your Manifold state and call them instead.".into(),
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
                    if expr_or_spread.spread.is_some() {
                        return Err("The spread operator (...) is not supported in Manifold expressions. Define array composition logic in your Manifold state functions instead.".into());
                    }
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
        Unary(unary) => {
            use swc_ecma_ast::UnaryOp;
            match unary.op {
                UnaryOp::TypeOf => {
                    return Err("The 'typeof' operator is not supported in Manifold expressions. Handle type checking in your Manifold state functions instead.".into());
                }
                UnaryOp::Delete => {
                    return Err("The 'delete' operator is not supported in Manifold expressions. Handle object modifications in your Manifold state functions instead.".into());
                }
                UnaryOp::Void => {
                    return Err(
                        "The 'void' operator is not supported in Manifold expressions.".into(),
                    );
                }
                _ => {}
            }
            check_expr(&unary.arg, restrictions)
        }
        Bin(bin) => {
            use swc_ecma_ast::BinaryOp;
            match bin.op {
                BinaryOp::InstanceOf => {
                    return Err("The 'instanceof' operator is not supported in Manifold expressions. Handle type checking in your Manifold state functions instead.".into());
                }
                BinaryOp::In => {
                    return Err("The 'in' operator is not supported in Manifold expressions. Use property access with optional chaining (obj?.prop) or define checking logic in your Manifold state instead.".into());
                }
                _ => {}
            }
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
            if !tpl.exprs.is_empty() {
                return Err("Template literals with interpolation (`hello ${name}`) are not supported in Manifold expressions. Use string concatenation like 'Hello ' + name instead.".into());
            }
            Ok(())
        }
        TaggedTpl(_) => {
            return Err("Tagged template literals are not supported in Manifold expressions. Use string concatenation or define a function in your Manifold state instead.".into());
        }
        Member(member) => {
            check_expr(&member.obj, restrictions)?;
            match &member.prop {
                ast::MemberProp::Computed(comp) => check_expr(&comp.expr, restrictions)?,
                _ => {}
            }
            // Check for global access patterns
            if let Some(global_error) = check_for_global_access(member) {
                return Err(global_error);
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
        // Check for unsupported JavaScript features
        PrivateName(_) => {
            return Err(
                "Private properties (#property) are not supported in Manifold expressions.".into(),
            );
        }
        Class(_) => {
            return Err("Class expressions are not supported in Manifold expressions. Define classes in your Manifold state instead.".into());
        }
        MetaProp(meta) => {
            return Err(format!(
                "Meta properties ({}) are not supported in Manifold expressions.",
                if meta.kind == ast::MetaPropKind::NewTarget {
                    "new.target"
                } else {
                    "import.meta"
                }
            ));
        }
        SuperProp(_) => {
            return Err("The 'super' keyword is not supported in Manifold expressions.".into());
        }
        This(_) => {
            return Err("The 'this' keyword is not supported in Manifold expressions. Use variables from your Manifold state instead.".into());
        }
        Ident(ident) => {
            // Check for global identifiers
            if let Some(global_error) = check_for_global_identifier(ident) {
                return Err(global_error);
            }
            Ok(())
        }
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
        ast::Stmt::If(_) => {
            return Err("If statements are not supported in Manifold expressions. Use ternary operators (condition ? value1 : value2) instead, or define logic in your Manifold state.".into());
        }
        ast::Stmt::Block(block) => {
            for stmt in &block.stmts {
                check_stmt(stmt, restrictions)?;
            }
            Ok(())
        }
        ast::Stmt::While(_) => {
            return Err("While loops are not supported in Manifold expressions. Define loop logic in your Manifold state functions instead.".into());
        }
        ast::Stmt::DoWhile(_) => {
            return Err("Do-while loops are not supported in Manifold expressions. Define loop logic in your Manifold state functions instead.".into());
        }
        ast::Stmt::For(_) => {
            return Err("For loops are not supported in Manifold expressions. Use the :each directive for iteration, or define loop logic in your Manifold state functions.".into());
        }
        ast::Stmt::ForOf(_) => {
            return Err("For-of loops are not supported in Manifold expressions. Use the :each directive for iteration, or define loop logic in your Manifold state functions.".into());
        }
        ast::Stmt::ForIn(_) => {
            return Err("For-in loops are not supported in Manifold expressions. Use the :each directive for iteration, or define loop logic in your Manifold state functions.".into());
        }
        ast::Stmt::Switch(_) => {
            return Err("Switch statements are not supported in Manifold expressions. Use ternary operators or define the logic in your Manifold state functions instead.".into());
        }
        ast::Stmt::Try(_) => {
            return Err("Try-catch statements are not supported in Manifold expressions. Handle errors in your Manifold state functions instead.".into());
        }
        ast::Stmt::Throw(_) => {
            return Err("Throw statements are not supported in Manifold expressions. Handle errors in your Manifold state functions instead.".into());
        }
        ast::Stmt::Decl(_) => {
            return Err("Declarations are not supported in Manifold expressions. Define functions and variables in your Manifold state instead.".into());
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
                        "Object methods are only supported in event handlers. For complex logic, define functions in your Manifold state and call them instead.".into(),
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
                        "Object getters are only supported in event handlers. For complex logic, define functions in your Manifold state and call them instead.".into(),
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
                        "Object setters are only supported in event handlers. For complex logic, define functions in your Manifold state and call them instead.".into(),
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
        ast::PropOrSpread::Spread(_) => {
            return Err("The spread operator (...) is not supported in Manifold expressions. Define object composition logic in your Manifold state functions instead.".into());
        }
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

fn check_for_global_access(member: &ast::MemberExpr) -> Option<String> {
    // Check if this is accessing a known global object
    if let ast::Expr::Ident(ident) = &*member.obj {
        let name = ident.sym.as_str();
        match name {
            "Math" => {
                return Some("Math functions are not available in Manifold expressions. Define mathematical operations in your Manifold state functions instead.".into());
            }
            "Date" => {
                return Some("Date functions are not available in Manifold expressions. Define date operations in your Manifold state functions instead.".into());
            }
            "window" => {
                return Some("Window object is not available in Manifold expressions. Define browser interactions in your Manifold state functions instead.".into());
            }
            "document" => {
                return Some("Document object is not available in Manifold expressions. Use Manifold directives for DOM manipulation instead.".into());
            }
            "console" => {
                return Some("Console functions are not available in Manifold expressions. Use console methods in your Manifold state functions for debugging instead.".into());
            }
            "JSON" => {
                return Some("JSON functions are not available in Manifold expressions. Define JSON operations in your Manifold state functions instead.".into());
            }
            "localStorage" | "sessionStorage" => {
                return Some("Storage APIs are not available in Manifold expressions. Define storage operations in your Manifold state functions instead.".into());
            }
            "fetch" => {
                return Some("Fetch API is not available in Manifold expressions. Define API calls in your Manifold state functions instead.".into());
            }
            "setTimeout" | "setInterval" | "clearTimeout" | "clearInterval" => {
                return Some("Timer functions are not available in Manifold expressions. Define timer logic in your Manifold state functions instead.".into());
            }
            "alert" | "confirm" | "prompt" => {
                return Some("Browser dialog functions are not available in Manifold expressions. Define user interactions in your Manifold state functions instead.".into());
            }
            _ => {}
        }
    }
    None
}

fn check_for_global_identifier(ident: &ast::Ident) -> Option<String> {
    let name = ident.sym.as_str();
    match name {
        "Math" | "Date" | "window" | "document" | "console" | "JSON" | 
        "localStorage" | "sessionStorage" | "fetch" | "setTimeout" | 
        "setInterval" | "clearTimeout" | "clearInterval" | "alert" | 
        "confirm" | "prompt" | "Array" | "Object" | "String" | "Number" | 
        "Boolean" | "RegExp" | "Error" | "Promise" => {
            Some(format!("Global '{}' is not available in Manifold expressions. Define this functionality in your Manifold state functions instead.", name))
        }
        _ => None
    }
}
