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

    let (service, socket) = LspService::new(|client| Backend::new(client));

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
}
