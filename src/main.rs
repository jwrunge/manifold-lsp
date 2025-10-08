use tokio::{io, net::TcpListener};
use tower_lsp::{LspService, Server};

mod attribute;
mod backend;
mod document;
mod lineindex;
mod notification;

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
    use super::document::ManifoldDocument;

    fn attribute_names(source: &str) -> Vec<String> {
        ManifoldDocument::parse(source)
            .attributes
            .into_iter()
            .map(|attr| attr.name)
            .collect()
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
    fn captures_registered_prefixed_attributes_only() {
        let html = r#"
            <section data-mf-register>
                <button :onclick="count++">Click</button>
                <input data-mf-sync="value" />
                <div data-mf-register="nested" class="plain" :if="show"></div>
                <span class="plain" data-something="else"></span>
            </section>
        "#;

        let names = attribute_names(html);
        assert_eq!(names, vec![":if"]);
    }
}
