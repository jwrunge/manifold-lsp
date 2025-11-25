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

use crate::state::{ManifoldStates, StateVariable};

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

#[derive(Debug, Clone)]
pub struct ValidationContext<'a> {
    pub state_name: Option<&'a str>,
    pub states: &'a ManifoldStates,
    pub locals: &'a [StateVariable],
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
                    let errors = parser.take_errors();
                    if !errors.is_empty() {
                        // Provide specific error messages based on common syntax errors
                        return Err(analyze_syntax_errors(&errors, trimmed));
                    }
                    Ok(ast)
                }
                Err(err) => {
                    // Analyze the error to provide better feedback
                    Err(analyze_parse_error(&err, trimmed))
                }
            }
        })
    })
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn validate_expression(
    expr: &str,
    allow_assignments: bool,
    allow_inline_functions: bool,
) -> Result<(), String> {
    validate_expression_with_context(expr, allow_assignments, allow_inline_functions, None)
}

pub fn validate_expression_with_context(
    expr: &str,
    allow_assignments: bool,
    allow_inline_functions: bool,
    context: Option<&ValidationContext>,
) -> Result<(), String> {
    let ast = parse_expression_ast(expr)?;
    let restrictions = Restrictions {
        allow_assignments,
        allow_inline_functions,
    };

    // First check structural validation (existing rules)
    check_expr(&ast, &restrictions)?;

    // Then check variable references if context is provided
    if let Some(ctx) = context {
        check_variable_references(&ast, ctx)?;
    }

    Ok(())
}

fn analyze_syntax_errors(errors: &[swc_ecma_parser::error::Error], expr: &str) -> String {
    // Analyze common syntax errors and provide helpful feedback
    for error in errors {
        let error_msg = format!("{error:?}");

        if error_msg.contains("UnterminatedStr") {
            return "Syntax error: Unterminated string literal. Make sure to close your quotes."
                .into();
        }

        if error_msg.contains("Expected") || error_msg.contains("expected") {
            if error_msg.contains("';'") {
                return "Syntax error: Unexpected semicolon. Manifold expressions should not end with semicolons.".into();
            }
            if error_msg.contains("')'") {
                return "Syntax error: Missing closing parenthesis ')'.".into();
            }
            if error_msg.contains("']'") {
                return "Syntax error: Missing closing bracket ']'.".into();
            }
            if error_msg.contains("'}'") {
                return "Syntax error: Missing closing brace '}'.".into();
            }
        }

        if error_msg.contains("Unexpected") {
            if expr.contains("=")
                && !expr.contains("==")
                && !expr.contains("===")
                && !expr.contains("!=")
                && !expr.contains("!==")
                && !expr.contains(">=")
                && !expr.contains("<=")
            {
                return "Syntax error: Assignment operators are only allowed in event handlers like :onclick.".into();
            }
            return "Syntax error: Unexpected token. Check your expression syntax.".into();
        }
    }

    "Invalid expression syntax. Check for common issues like unmatched brackets, unterminated strings, or unsupported syntax.".into()
}

fn analyze_parse_error(error: &swc_ecma_parser::error::Error, expr: &str) -> String {
    let error_msg = format!("{error:?}");

    // Check for common patterns in the expression that might help identify the issue
    if expr.contains("'") && (expr.matches("'").count() % 2 != 0) {
        return "Syntax error: Unterminated string literal with single quotes. Make sure to close your quotes.".into();
    }

    if expr.contains("\"") && (expr.matches("\"").count() % 2 != 0) {
        return "Syntax error: Unterminated string literal with double quotes. Make sure to close your quotes.".into();
    }

    if expr.contains("(") && expr.matches("(").count() != expr.matches(")").count() {
        return "Syntax error: Unmatched parentheses. Check that every '(' has a corresponding ')'.".into();
    }

    if expr.contains("[") && expr.matches("[").count() != expr.matches("]").count() {
        return "Syntax error: Unmatched brackets. Check that every '[' has a corresponding ']'."
            .into();
    }

    if expr.contains("{") && expr.matches("{").count() != expr.matches("}").count() {
        return "Syntax error: Unmatched braces. Check that every '{' has a corresponding '}'."
            .into();
    }

    if error_msg.contains("Expected") {
        return "Syntax error: Invalid expression structure. Manifold expressions should be valid JavaScript expressions.".into();
    }

    "Invalid expression syntax. Manifold expressions support literals, variables, property access, arithmetic/logical operations, ternary operators, function calls, and array/object literals.".into()
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

            let mut tokens = Vec::new();
            for token_and_span in lexer {
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
            for expr_or_spread in array.elems.iter().flatten() {
                if expr_or_spread.spread.is_some() {
                    return Err("The spread operator (...) is not supported in Manifold expressions. Define array composition logic in your Manifold state functions instead.".into());
                }
                check_expr(&expr_or_spread.expr, restrictions)?;
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
            if let ast::Callee::Expr(expr) = &call.callee {
                check_expr(expr, restrictions)?;
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
                // Explicitly disallow unsupported unary operators
                UnaryOp::Plus => {
                    return Err("The unary '+' operator is not supported in Manifold expressions. Use explicit values or state variables instead.".into());
                }
                UnaryOp::Tilde => {
                    return Err(
                        "Bitwise operators (like '~') are not supported in Manifold expressions."
                            .into(),
                    );
                }
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
                // Disallow operators outside Manifold's supported set
                BinaryOp::EqEq | BinaryOp::NotEq => {
                    return Err(
                        "Use strict equality operators (===, !==) in Manifold expressions.".into(),
                    );
                }
                BinaryOp::BitXor | BinaryOp::BitAnd | BinaryOp::BitOr => {
                    return Err(
                        "Bitwise operators (&, |, ^) are not supported in Manifold expressions."
                            .into(),
                    );
                }
                BinaryOp::LShift | BinaryOp::RShift | BinaryOp::ZeroFillRShift => {
                    return Err("Bitwise shift operators (<<, >>, >>>) are not supported in Manifold expressions.".into());
                }
                BinaryOp::Exp => {
                    return Err("The exponentiation operator (**) is not supported in Manifold expressions.".into());
                }
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
        TaggedTpl(_) => Err("Tagged template literals are not supported in Manifold expressions. Use string concatenation or define a function in your Manifold state instead.".into()),
        Member(member) => {
            check_expr(&member.obj, restrictions)?;
            if let ast::MemberProp::Computed(comp) = &member.prop {
                check_expr(&comp.expr, restrictions)?;
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
        PrivateName(_) => Err(
            "Private properties (#property) are not supported in Manifold expressions.".into(),
        ),
        Class(_) => Err("Class expressions are not supported in Manifold expressions. Define classes in your Manifold state instead.".into()),
        MetaProp(meta) => Err(format!(
            "Meta properties ({}) are not supported in Manifold expressions.",
            if meta.kind == ast::MetaPropKind::NewTarget {
                "new.target"
            } else {
                "import.meta"
            }
        )),
        SuperProp(_) => Err("The 'super' keyword is not supported in Manifold expressions.".into()),
        This(_) => Err("The 'this' keyword is not supported in Manifold expressions. Use variables from your Manifold state instead.".into()),
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
        ast::Stmt::If(_) => Err("If statements are not supported in Manifold expressions. Use ternary operators (condition ? value1 : value2) instead, or define logic in your Manifold state.".into()),
        ast::Stmt::Block(block) => {
            for stmt in &block.stmts {
                check_stmt(stmt, restrictions)?;
            }
            Ok(())
        }
        ast::Stmt::While(_) => Err("While loops are not supported in Manifold expressions. Define loop logic in your Manifold state functions instead.".into()),
        ast::Stmt::DoWhile(_) => Err("Do-while loops are not supported in Manifold expressions. Define loop logic in your Manifold state functions instead.".into()),
        ast::Stmt::For(_) => Err("For loops are not supported in Manifold expressions. Use the :each directive for iteration, or define loop logic in your Manifold state functions.".into()),
        ast::Stmt::ForOf(_) => Err("For-of loops are not supported in Manifold expressions. Use the :each directive for iteration, or define loop logic in your Manifold state functions.".into()),
        ast::Stmt::ForIn(_) => Err("For-in loops are not supported in Manifold expressions. Use the :each directive for iteration, or define loop logic in your Manifold state functions.".into()),
        ast::Stmt::Switch(_) => Err("Switch statements are not supported in Manifold expressions. Use ternary operators or define the logic in your Manifold state functions instead.".into()),
        ast::Stmt::Try(_) => Err("Try-catch statements are not supported in Manifold expressions. Handle errors in your Manifold state functions instead.".into()),
        ast::Stmt::Throw(_) => Err("Throw statements are not supported in Manifold expressions. Handle errors in your Manifold state functions instead.".into()),
        ast::Stmt::Decl(_) => Err("Declarations are not supported in Manifold expressions. Define functions and variables in your Manifold state instead.".into()),
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
        ast::PropOrSpread::Spread(_) => Err("The spread operator (...) is not supported in Manifold expressions. Define object composition logic in your Manifold state functions instead.".into()),
    }
}

fn check_variable_references(expr: &Expr, context: &ValidationContext) -> Result<(), String> {
    visit_expr_for_identifiers(expr, context)
}

fn visit_expr_for_identifiers(expr: &Expr, context: &ValidationContext) -> Result<(), String> {
    use ast::Expr::*;

    match expr {
        Ident(ident) => {
            validate_identifier_reference(ident, context)?;
            Ok(())
        }
        Member(member) => {
            visit_expr_for_identifiers(&member.obj, context)?;
            if let ast::MemberProp::Computed(comp) = &member.prop {
                visit_expr_for_identifiers(&comp.expr, context)?;
            }
            Ok(())
        }
        Call(call) => {
            if let ast::Callee::Expr(expr) = &call.callee {
                visit_expr_for_identifiers(expr, context)?;
            }
            for arg in &call.args {
                visit_expr_for_identifiers(&arg.expr, context)?;
            }
            Ok(())
        }
        Bin(bin) => {
            visit_expr_for_identifiers(&bin.left, context)?;
            visit_expr_for_identifiers(&bin.right, context)?;
            Ok(())
        }
        Unary(unary) => {
            visit_expr_for_identifiers(&unary.arg, context)?;
            Ok(())
        }
        Assign(assign) => {
            // For assignments, we only check the right side for variable references
            // The left side is the target being assigned to
            visit_expr_for_identifiers(&assign.right, context)?;
            Ok(())
        }
        Update(update) => {
            visit_expr_for_identifiers(&update.arg, context)?;
            Ok(())
        }
        Cond(cond) => {
            visit_expr_for_identifiers(&cond.test, context)?;
            visit_expr_for_identifiers(&cond.cons, context)?;
            visit_expr_for_identifiers(&cond.alt, context)?;
            Ok(())
        }
        Array(array) => {
            for expr_or_spread in array.elems.iter().flatten() {
                visit_expr_for_identifiers(&expr_or_spread.expr, context)?;
            }
            Ok(())
        }
        Object(object) => {
            for prop in &object.props {
                visit_prop_for_identifiers(prop, context)?;
            }
            Ok(())
        }
        Paren(paren) => visit_expr_for_identifiers(&paren.expr, context),
        Seq(seq) => {
            for expr in &seq.exprs {
                visit_expr_for_identifiers(expr, context)?;
            }
            Ok(())
        }
        OptChain(opt) => visit_opt_chain_for_identifiers(opt, context),
        // Literals and other expressions that don't contain identifiers
        Lit(_) | This(_) | Arrow(_) | Fn(_) | Class(_) | Tpl(_) | TaggedTpl(_) | Yield(_)
        | Await(_) | New(_) | MetaProp(_) | SuperProp(_) | PrivateName(_) | TsAs(_)
        | TsConstAssertion(_) | TsNonNull(_) | TsTypeAssertion(_) | TsInstantiation(_)
        | TsSatisfies(_) | Invalid(_) | JSXMember(_) | JSXNamespacedName(_) | JSXEmpty(_)
        | JSXElement(_) | JSXFragment(_) => Ok(()),
    }
}

fn visit_prop_for_identifiers(
    prop: &ast::PropOrSpread,
    context: &ValidationContext,
) -> Result<(), String> {
    match prop {
        ast::PropOrSpread::Prop(prop) => match &**prop {
            ast::Prop::KeyValue(kv) => visit_expr_for_identifiers(&kv.value, context),
            ast::Prop::Shorthand(ident) => validate_identifier_reference(ident, context),
            ast::Prop::Method(method) => {
                if let Some(body) = &method.function.body {
                    for stmt in &body.stmts {
                        visit_stmt_for_identifiers(stmt, context)?;
                    }
                }
                Ok(())
            }
            ast::Prop::Getter(getter) => {
                if let Some(body) = &getter.body {
                    for stmt in &body.stmts {
                        visit_stmt_for_identifiers(stmt, context)?;
                    }
                }
                Ok(())
            }
            ast::Prop::Setter(setter) => {
                if let Some(body) = &setter.body {
                    for stmt in &body.stmts {
                        visit_stmt_for_identifiers(stmt, context)?;
                    }
                }
                Ok(())
            }
            ast::Prop::Assign(assign) => visit_expr_for_identifiers(&assign.value, context),
        },
        ast::PropOrSpread::Spread(spread) => visit_expr_for_identifiers(&spread.expr, context),
    }
}

fn visit_stmt_for_identifiers(stmt: &ast::Stmt, context: &ValidationContext) -> Result<(), String> {
    match stmt {
        ast::Stmt::Expr(expr_stmt) => visit_expr_for_identifiers(&expr_stmt.expr, context),
        ast::Stmt::Return(ret) => {
            if let Some(arg) = &ret.arg {
                visit_expr_for_identifiers(arg, context)?;
            }
            Ok(())
        }
        ast::Stmt::Block(block) => {
            for stmt in &block.stmts {
                visit_stmt_for_identifiers(stmt, context)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn visit_opt_chain_for_identifiers(
    opt: &ast::OptChainExpr,
    context: &ValidationContext,
) -> Result<(), String> {
    use ast::OptChainBase;

    match &*opt.base {
        OptChainBase::Member(member) => {
            visit_expr_for_identifiers(&member.obj, context)?;
            if let ast::MemberProp::Computed(comp) = &member.prop {
                visit_expr_for_identifiers(&comp.expr, context)?;
            }
            Ok(())
        }
        OptChainBase::Call(call) => {
            visit_expr_for_identifiers(&call.callee, context)?;
            for arg in &call.args {
                visit_expr_for_identifiers(&arg.expr, context)?;
            }
            Ok(())
        }
    }
}

fn validate_identifier_reference(
    ident: &ast::Ident,
    context: &ValidationContext,
) -> Result<(), String> {
    let name = ident.sym.as_str();

    // Skip validation for known globals that we already handle elsewhere
    if is_known_global(name) {
        return Ok(());
    }

    // Check if it's in local variables (from :each bindings)
    if context.locals.iter().any(|var| var.name == name) {
        return Ok(());
    }

    // Check if it's in the current state
    if let Some(state_name) = context.state_name {
        if let Some(state) = context.states.get(state_name) {
            if state.property_type(name).is_some() {
                return Ok(());
            }
        }
    }

    // Check if it's in the default state
    // Only fall back to the default state when no specific state is active
    if context.state_name.is_none() {
        if let Some(default_state) = context.states.get("default") {
            if default_state.property_type(name).is_some() {
                return Ok(());
            }
        }
    }

    // If we get here, the variable is not found
    Err(match context.state_name {
        Some(state_name) => format!(
            "Unknown variable '{name}'. Variables must be defined in your Manifold state ('{state_name}') or provided from the current scope (e.g., :each locals).",
        ),
        None => format!(
            "Unknown variable '{name}'. Variables must be defined in your Manifold state or provided from the current scope (e.g., :each locals).",
        ),
    })
}

fn is_safe_global(name: &str) -> bool {
    matches!(
        name,
        "Array"
            | "Boolean"
            | "console"
            | "Date"
            | "JSON"
            | "Map"
            | "Math"
            | "Number"
            | "Object"
            | "Promise"
            | "Reflect"
            | "Set"
            | "String"
            | "Symbol"
            | "WeakMap"
            | "WeakSet"
    )
}

fn is_known_global(name: &str) -> bool {
    matches!(
        name,
        "window"
            | "document"
            | "JSON"
            | "localStorage"
            | "sessionStorage"
            | "fetch"
            | "setTimeout"
            | "setInterval"
            | "clearTimeout"
            | "clearInterval"
            | "alert"
            | "confirm"
            | "prompt"
            | "RegExp"
            | "Error"
            | "undefined"
            | "null"
            | "true"
            | "false"
    ) || is_safe_global(name)
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
        if is_safe_global(name) {
            return None;
        }
        match name {
            "window" => {
                return Some("Window object is not available in Manifold expressions. Define browser interactions in your Manifold state functions instead.".into());
            }
            "document" => {
                return Some("Document object is not available in Manifold expressions. Use Manifold directives for DOM manipulation instead.".into());
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
    if is_safe_global(name) {
        return None;
    }
    match name {
        "window" | "document" | "JSON" |
        "localStorage" | "sessionStorage" | "fetch" | "setTimeout" | 
        "setInterval" | "clearTimeout" | "clearInterval" | "alert" | 
        "confirm" | "prompt" | "RegExp" | "Error" => {
            Some(format!("Global '{name}' is not available in Manifold expressions. Define this functionality in your Manifold state functions instead."))
        }
        _ => None
    }
}
