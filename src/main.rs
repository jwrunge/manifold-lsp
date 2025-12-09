use tokio::{io, net::TcpListener};
use tower_lsp::{LspService, Server};

mod attribute;
mod backend;
mod document;
mod expression;
mod lineindex;
mod notification;
mod state;

use backend::Backend;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .init();

    let (service, socket) = LspService::new(Backend::new);

    if let Ok(addr) = std::env::var("MANIFOLD_LSP_TCP") {
        let listener = TcpListener::bind(addr).await.unwrap();
        let (stream, _) = listener.accept().await.unwrap();
        let (read, write) = tokio::io::split(stream);
        Server::new(read, write, socket).serve(service).await;
    } else {
        let stdin = io::stdin();
        let stdout = io::stdout();
        Server::new(stdin, stdout, socket).serve(service).await;
    }
}

#[cfg(test)]
mod tests {
    use super::attribute::ManifoldAttributeKind;
    use super::document::ManifoldDocument;
    use super::expression::ExpressionTokenKind;
    use super::state::TypeInfo;

    fn attribute_names(source: &str) -> Vec<String> {
        ManifoldDocument::parse(source)
            .attributes
            .into_iter()
            .filter(|attr| attr.kind == ManifoldAttributeKind::Attribute)
            .map(|attr| attr.name)
            .collect()
    }

    fn text_expressions(source: &str) -> Vec<String> {
        ManifoldDocument::parse(source)
            .attributes
            .into_iter()
            .filter(|attr| attr.kind == ManifoldAttributeKind::TextExpression)
            .map(|attr| attr.name)
            .collect()
    }

    fn collected_expressions(source: &str) -> Vec<Option<String>> {
        ManifoldDocument::parse(source)
            .attributes
            .into_iter()
            .map(|attr| attr.expression)
            .collect()
    }

    #[test]
    fn produces_semantic_tokens_for_attribute_expressions() {
        let html = r#"
            <div data-mf-register>
                <button :onclick="count++"></button>
            </div>
        "#;

        let document = ManifoldDocument::parse(html);
        let tokens = document.semantic_tokens();

        assert!(tokens
            .iter()
            .any(|token| token.kind == ExpressionTokenKind::Identifier));
    }

    #[test]
    fn produces_semantic_tokens_for_text_expressions() {
        let html = r#"
            <section data-mf-register>
                ${user.name}
            </section>
        "#;

        let document = ManifoldDocument::parse(html);
        let tokens = document.semantic_tokens();

        assert!(tokens
            .iter()
            .any(|token| token.kind == ExpressionTokenKind::Identifier));
    }

    #[test]
    fn infers_type_information_from_state_scripts() {
        let html = r#"
            <div data-mf-register>
                <p>${count}</p>
            </div>
            <script type="module">
                const state = Manifold.create()
                    .add("count", 0)
                    .build();
            </script>
        "#;

        let document = ManifoldDocument::parse(html);
        let attribute = document
            .attributes
            .iter()
            .find(|attr| attr.name == "${count}")
            .expect("count expression recorded");

        let ty = document.expression_type(attribute);
        assert!(matches!(ty, Some(TypeInfo::Number)));
    }

    #[test]
    fn each_binding_introduces_local_types() {
        let html = r#"
            <div data-mf-register>
                <ul>
                    <li :each="items as item, index">${item}</li>
                </ul>
            </div>
            <script type="module">
                const state = Manifold.create()
                    .add("items", ["Apple", "Banana"])
                    .build();
            </script>
        "#;

        let document = ManifoldDocument::parse(html);
        let attribute = document
            .attributes
            .iter()
            .find(|attr| attr.name == "${item}")
            .expect("item text expression tracked");

        let ty = document.expression_type(attribute);
        assert_eq!(ty.unwrap().describe(), "string");

        assert!(attribute
            .locals
            .iter()
            .any(|var| var.name == "item" && var.ty.describe() == "string"));
        assert!(attribute
            .locals
            .iter()
            .any(|var| var.name == "index" && var.ty.describe() == "number"));
    }

    #[test]
    fn skips_elements_without_register() {
        let html = r#"
            <div>
                <button :onclick="count++">Click</button>
                <input data-mf-sync="value" />
            </div>
        "#;

        assert!(attribute_names(html).is_empty());
    }

    #[test]
    fn captures_registered_prefixed_attributes_and_descendants() {
        let html = r#"
            <section data-mf-register>
                <button :onclick="count++">Click</button>
                <input data-mf-sync="value" />
                <div data-mf-register="nested" class="plain" :if="show"></div>
                <span class="plain" data-something="else"></span>
            </section>
        "#;

        let names = attribute_names(html);
        assert_eq!(names, vec![":onclick", "data-mf-sync", ":if"]);
    }

    #[test]
    fn ignores_subtrees_until_reregistered() {
        let html = r#"
            <div data-mf-register>
                <section data-mf-ignore>
                    <button :onclick="ignored"></button>
                    <div data-mf-register>
                        <span :if="visible"></span>
                    </div>
                </section>
                <p :text="still_there"></p>
            </div>
        "#;

        let names = attribute_names(html);
        assert_eq!(names, vec![":if", ":text"]);
    }

    #[test]
    fn text_nodes_are_highlighted_when_registered() {
        let html = r#"
            <div data-mf-register>
                <p>Hello, ${name}!</p>
                <section data-mf-ignore>
                    ${ignored}
                    <span data-mf-register>
                        ${nested}
                    </span>
                </section>
                ${outside}
            </div>
            ${global}
        "#;

        let expressions = text_expressions(html);
        assert_eq!(expressions, vec!["${name}", "${nested}", "${outside}"]);
    }

    #[test]
    fn expression_text_is_captured() {
        let html = r#"
            <div data-mf-register>
                <button :onclick=" count++ "></button>
                ${ user.name }
            </div>
        "#;

        let expressions = collected_expressions(html)
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        assert!(expressions.contains(&"count++".to_string()));
        assert!(expressions.contains(&"user.name".to_string()));
    }

    #[test]
    fn provides_helpful_error_messages_for_invalid_expressions() {
        use super::expression::validate_expression;

        // Test assignment outside event handler
        let err = validate_expression("count = 5", false, false).unwrap_err();
        assert!(err.contains("only allowed in event handlers"));
        assert!(err.contains("define functions in your Manifold state"));

        // Test template literal
        let err = validate_expression("`hello ${name}`", false, false).unwrap_err();
        assert!(err.contains("Template literals"));
        assert!(err.contains("Use string concatenation"));

        // Test safe global access
        assert!(validate_expression("Math.max(1, 2)", false, false).is_ok());

        // Test restricted global access
        let err = validate_expression("window.location.href", false, false).unwrap_err();
        assert!(err.contains("Global 'window' is not available"));

        // Test typeof operator
        let err = validate_expression("typeof value", false, false).unwrap_err();
        assert!(err.contains("typeof"));
        assert!(err.contains("not supported"));

        // Test arrow function outside event handler
        let err = validate_expression("x => x + 1", false, false).unwrap_err();
        assert!(err.contains("Arrow functions"));
        assert!(err.contains("only supported in event handlers"));

        // Test spread operator
        let err = validate_expression("[...array]", false, false).unwrap_err();
        assert!(err.contains("spread operator"));
        assert!(err.contains("not supported"));

        // Test unsupported binary operators
        let err = validate_expression("count >> 3", false, false).unwrap_err();
        assert!(
            err.contains("shift operators") || err.contains(">>") || err.contains("not supported")
        );

        let err = validate_expression("count >>> 1", false, false).unwrap_err();
        assert!(
            err.contains("shift operators") || err.contains(">>>") || err.contains("not supported")
        );

        let err = validate_expression("count << 2", false, false).unwrap_err();
        assert!(
            err.contains("shift operators") || err.contains("<<") || err.contains("not supported")
        );

        let err = validate_expression("a & b", false, false).unwrap_err();
        assert!(err.contains("Bitwise") || err.contains("not supported"));

        let err = validate_expression("a | b", false, false).unwrap_err();
        assert!(err.contains("Bitwise") || err.contains("not supported"));

        let err = validate_expression("a ^ b", false, false).unwrap_err();
        assert!(err.contains("Bitwise") || err.contains("not supported"));

        let err = validate_expression("a ** b", false, false).unwrap_err();
        assert!(
            err.contains("exponentiation") || err.contains("**") || err.contains("not supported")
        );

        let err = validate_expression("a == b", false, false).unwrap_err();
        assert!(err.contains("strict equality") || err.contains("===") || err.contains("=="));

        let err = validate_expression("a != b", false, false).unwrap_err();
        assert!(err.contains("strict equality") || err.contains("!==") || err.contains("!="));
    }

    #[test]
    fn allows_valid_manifold_expressions() {
        use super::expression::validate_expression;

        // These should all pass
        assert!(validate_expression("count", false, false).is_ok());
        assert!(validate_expression("user.name", false, false).is_ok());
        assert!(validate_expression("items[0]", false, false).is_ok());
        assert!(validate_expression("a + b", false, false).is_ok());
        assert!(validate_expression("console.log('x')", false, false).is_ok());
        assert!(validate_expression("condition ? value1 : value2", false, false).is_ok());
        assert!(validate_expression("{ key: value }", false, false).is_ok());
        assert!(validate_expression("[1, 2, 3]", false, false).is_ok());
        assert!(validate_expression("func(arg1, arg2)", false, false).is_ok());
        assert!(validate_expression("a && b || c", false, false).is_ok());
        assert!(validate_expression("obj?.prop?.nested", false, false).is_ok());
        assert!(validate_expression("a ?? b", false, false).is_ok());
        assert!(validate_expression("!flag", false, false).is_ok());
        assert!(validate_expression("-value", false, false).is_ok());

        // These should pass in event handlers
        assert!(validate_expression("count++", true, true).is_ok());
        assert!(validate_expression("count = value", true, true).is_ok());
        assert!(validate_expression("(e) => handleClick(e)", true, true).is_ok());
    }

    #[test]
    fn event_handlers_allow_assignments() {
        let html = r#"
            <div data-mf-register>
                <button :onclick="count++">Increment</button>
                <button :onclick="count--">Decrement</button>
                <button :onclick="count = 0">Reset</button>
                <button :onchange="value = 'new'">Change</button>
                <button data-mf-onclick="items.push(newItem)">Add Item</button>
            </div>
            <script type="module">
                const state = Manifold.create()
                    .add("count", 0)
                    .add("value", "old")
                    .add("items", [])
                    .add("newItem", "item")
                    .build();
            </script>
        "#;

        let document = ManifoldDocument::parse(html);
        let diagnostics = document.diagnostics();

        // Should have no errors for valid event handler assignments
        assert!(
            diagnostics.is_empty(),
            "Event handlers should allow assignments but got errors: {diagnostics:?}"
        );
    }

    #[test]
    fn non_event_attributes_reject_assignments() {
        let html = r#"
            <div data-mf-register>
                <div :class="count = 5">Should error</div>
                <div :text="value++">Should error</div>
                <div :style="prop = 'test'">Should error</div>
            </div>
        "#;

        let document = ManifoldDocument::parse(html);
        let diagnostics = document.diagnostics();

        // Should have 3 errors for assignments in non-event attributes
        assert_eq!(
            diagnostics.len(),
            3,
            "Non-event attributes should reject assignments"
        );

        for diagnostic in &diagnostics {
            assert!(diagnostic
                .message
                .contains("only allowed in event handlers"));
        }
    }

    #[test]
    fn detects_unknown_variable_references() {
        let html = r#"
            <div data-mf-register>
                <div :text="unknownVar">Should error</div>
                <div :if="anotherUnknown > 5">Should error</div>
                <div>${missingVariable}</div>
            </div>
            <script type="module">
                const state = Manifold.create()
                    .add("knownVar", "value")
                    .build();
            </script>
        "#;

        let document = ManifoldDocument::parse(html);
        let diagnostics = document.diagnostics();

        // Should have 3 errors for unknown variables
        assert_eq!(
            diagnostics.len(),
            3,
            "Should detect unknown variable references"
        );

        for diagnostic in &diagnostics {
            assert!(diagnostic.message.contains("Unknown variable"));
            assert!(diagnostic
                .message
                .contains("must be defined in your Manifold state"));
        }
    }

    #[test]
    fn allows_known_variables_and_locals() {
        let html = r#"
            <div data-mf-register>
                <div :text="knownVar">Valid reference</div>
                <div :if="count > 0">Valid reference</div>
                <ul>
                    <li :each="items as item, index">
                        <span :text="item">Valid local</span>
                        <span :text="index">Valid index</span>
                    </li>
                </ul>
            </div>
            <script type="module">
                const state = Manifold.create()
                    .add("knownVar", "value")
                    .add("count", 0)
                    .add("items", ["a", "b"])
                    .build();
            </script>
        "#;

        let document = ManifoldDocument::parse(html);
        let diagnostics = document.diagnostics();

        // Should have no errors for valid variable references
        assert!(
            diagnostics.is_empty(),
            "Known variables should not cause errors: {diagnostics:?}"
        );
    }

    #[test]
    fn provides_specific_syntax_error_messages() {
        let html = r#"
            <div data-mf-register>
                <div :text="'unterminated string">Should error</div>
                <div :text="missing )">Should error</div>
                <div :text="[unclosed bracket">Should error</div>
                <div :text="{unclosed: brace">Should error</div>
                <div :text="invalid === = syntax">Should error</div>
            </div>
        "#;

        let document = ManifoldDocument::parse(html);
        let diagnostics = document.diagnostics();

        // Should have syntax errors
        assert!(!diagnostics.is_empty(), "Should detect syntax errors");

        // Check that we get specific error messages
        let messages: Vec<&str> = diagnostics.iter().map(|d| d.message.as_str()).collect();

        // Look for specific syntax error indicators
        let has_string_error = messages
            .iter()
            .any(|msg| msg.contains("string") || msg.contains("quote"));
        let has_bracket_error = messages.iter().any(|msg| {
            msg.contains("bracket") || msg.contains("parenthes") || msg.contains("brace")
        });

        assert!(
            has_string_error || has_bracket_error,
            "Should provide specific syntax error messages: {messages:?}"
        );
    }

    #[test]
    fn handles_mixed_state_contexts() {
        let html = r#"
            <div data-mf-register>
                <div :text="globalVar">Should be valid</div>
            </div>
            <div data-mf-register="namedState">
                <div :text="localVar">Should be valid</div>
                <div :text="globalVar">Should be invalid (not visible in named state)</div>
                <div :text="unknownVar">Should error</div>
            </div>
            <script type="module">
                const globalState = Manifold.create()
                    .add("globalVar", "global")
                    .build();
                    
                const namedState = Manifold.create("namedState")
                    .add("localVar", "local")
                    .build();
            </script>
        "#;

        let document = ManifoldDocument::parse(html);
        let diagnostics = document.diagnostics();

        // Should have 2 errors: one for unknownVar and one for globalVar not visible in namedState
        assert_eq!(
            diagnostics.len(),
            2,
            "Should detect unknown variables with proper scoping: {diagnostics:?}"
        );

        let messages: Vec<String> = diagnostics.iter().map(|d| d.message.clone()).collect();
        assert!(messages
            .iter()
            .any(|m| m.contains("Unknown variable") && m.contains("unknownVar")));
        assert!(messages
            .iter()
            .any(|m| m.contains("Unknown variable") && m.contains("globalVar")));
    }

    #[test]
    fn rejects_forbidden_property_access() {
        use super::expression::validate_expression;

        // Test forbidden properties
        let forbidden_props = vec![
            "prototype",
            "constructor",
            "__proto__",
            "apply",
            "bind",
            "call",
            "__defineGetter__",
            "__defineSetter__",
            "__lookupGetter__",
            "__lookupSetter__",
        ];

        for prop in forbidden_props {
            let expr = format!("obj.{}", prop);
            let err = validate_expression(&expr, false, false).unwrap_err();
            assert!(
                err.contains(&format!("Property '{}'", prop)) && err.contains("not accessible"),
                "Expected error for {}, got: {}",
                prop,
                err
            );

            // Also test computed property access
            let expr = format!("obj['{}']", prop);
            let err = validate_expression(&expr, false, false).unwrap_err();
            assert!(
                err.contains(&format!("Property '{}'", prop)) && err.contains("not accessible"),
                "Expected error for computed access of {}, got: {}",
                prop,
                err
            );
        }
    }

    #[test]
    fn rejects_removed_safe_globals() {
        use super::expression::validate_expression;

        // These globals were removed from the safe list
        let removed_globals = vec!["Array", "Reflect", "Symbol", "WeakMap", "WeakSet"];

        for global in removed_globals {
            let err = validate_expression(global, false, false).unwrap_err();
            assert!(
                err.contains(&format!("Global '{}'", global))
                    && err.contains("no longer available"),
                "Expected error for removed global {}, got: {}",
                global,
                err
            );
        }
    }

    #[test]
    fn allows_new_fetch_helpers() {
        use super::expression::validate_expression;

        // These new fetch helpers should be recognized as valid globals
        let fetch_helpers = vec![
            "mfGet",
            "mfPost",
            "mfPut",
            "mfDelete",
            "mfPatch",
            "mfHead",
            "mfOptions",
        ];

        for helper in fetch_helpers {
            let expr = format!("{}('/api/data')", helper);
            assert!(
                validate_expression(&expr, false, false).is_ok(),
                "Fetch helper {} should be recognized as a valid global",
                helper
            );
        }

        // Test chaining methods on fetch helpers
        assert!(validate_expression(
            "mfGet('/api/data').replace('#content', { from: '#payload' })",
            false,
            false
        )
        .is_ok());

        assert!(validate_expression(
            "mfPost('/api/save', { body: JSON.stringify(data) }).append('#results')",
            false,
            false
        )
        .is_ok());
    }

    #[test]
    fn allows_remaining_safe_globals() {
        use super::expression::validate_expression;

        // These globals should still be safe
        let safe_globals = vec![
            "Boolean", "console", "Date", "JSON", "Map", "Math", "Number", "Object", "Promise",
            "Set", "String",
        ];

        for global in safe_globals {
            assert!(
                validate_expression(global, false, false).is_ok(),
                "Safe global {} should be allowed",
                global
            );
        }

        // Test using them in expressions
        assert!(validate_expression("Math.max(1, 2)", false, false).is_ok());
        assert!(validate_expression("JSON.stringify(obj)", false, false).is_ok());
        assert!(validate_expression("Date.now()", false, false).is_ok());
        assert!(validate_expression("console.log('test')", false, false).is_ok());
        assert!(validate_expression("new Map()", false, false).is_ok());
        assert!(validate_expression("new Set()", false, false).is_ok());
        assert!(validate_expression("Promise.resolve(value)", false, false).is_ok());
    }
}
