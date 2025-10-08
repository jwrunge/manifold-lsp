use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::notification::Notification;

#[derive(Debug, Serialize, Deserialize)]
pub struct NotificationParams {
    pub title: String,
    pub message: String,
    pub description: String,
}

pub enum ManifoldNotification {}

impl Notification for ManifoldNotification {
    type Params = NotificationParams;
    const METHOD: &'static str = "custom/notification";
}
