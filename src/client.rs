use serde_json::json;
use std::error::Error;
use tokio::io::{AsyncReadExt, AsyncWriteExt};

// Very minimal, naive LSP client for manual testing.
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let mut stream = tokio::net::TcpStream::connect("127.0.0.1:8081").await?;

    // 1. Send initialize request
    let initialize_request = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": null,
            "capabilities": {},
            "trace": "off"
        }
    });

    let init_str = initialize_request.to_string();
    let init_wire = format!("Content-Length: {}\r\n\r\n{}", init_str.len(), init_str);
    stream.write_all(init_wire.as_bytes()).await?;

    let mut buf = vec![0u8; 4096];
    let n = stream.read(&mut buf).await?;
    println!(
        "Initialize response (raw): {}",
        String::from_utf8_lossy(&buf[..n])
    );

    // 2. Send executeCommand request to trigger custom notification
    let execute_command_request = json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "workspace/executeCommand",
        "params": {
            "command": "custom.notification",
            "arguments": [
                {
                    "title": "Hello!",
                    "message": "This is a custom notification.",
                    "description": "Sent from the Rust LSP client."
                }
            ]
        }
    });

    let exec_str = execute_command_request.to_string();
    let exec_wire = format!("Content-Length: {}\r\n\r\n{}", exec_str.len(), exec_str);
    stream.write_all(exec_wire.as_bytes()).await?;

    let n2 = stream.read(&mut buf).await?;
    println!(
        "ExecuteCommand response (raw): {}",
        String::from_utf8_lossy(&buf[..n2])
    );

    Ok(())
}
