use serde::{Deserialize, Serialize};
use tower_lsp::{
    async_trait,
    lsp_types::{
        notification::Notification, ExecuteCommandOptions, InitializeParams, InitializeResult,
        ServerCapabilities,
    },
    Client, LanguageServer,
};

#[derive(Debug, Serialize, Deserialize)]
struct NotificationParams {
    title: String,
    message: String,
    description: String,
}

enum CustomNotification {}

impl Notification for CustomNotification {
    type Params = NotificationParams;

    const METHOD: &'static str = "custom/notification";
}

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        OK(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![String::from("custom/notification")],
                    work_done_progress_options: None,
                }),
            },
        })
    }
}
