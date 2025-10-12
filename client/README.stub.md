## VS Code Extension Details

Level up your Manifold HTML authoring with completions, diagnostics, inline type hints, and more—directly inside VS Code.

### Features

-   **Context-aware completions** for Manifold directives, loop locals, and state properties
-   **Inline type annotations** to understand what each expression returns
-   **Diagnostics** that highlight unsupported syntax or missing variables as you type
-   **Go to Definition** and **Find References** across Manifold states and templates
-   **Custom commands** for toggling auto-triggered completions and type hints

### Getting Started

1. Install the Manifold Language Client from the VS Code Marketplace.
2. Open a project containing Manifold HTML templates.
3. The extension activates automatically for HTML/JavaScript/TypeScript files. Toggle auto-completions or inline hints via the command palette (`⌘⇧P` / `Ctrl+Shift+P`).

### Commands

| Command                             | Description                                                                |
| ----------------------------------- | -------------------------------------------------------------------------- |
| `Manifold: Toggle Auto Suggestions` | Enable or disable automatic completions when typing attribute expressions. |
| `Manifold: Toggle Type Annotations` | Show or hide inline type hints for expressions.                            |
| `Manifold: Restart Language Server` | Manually restart the bundled Manifold language server process.             |

### Configuration

`Settings → Manifold Language Server`

| Setting                                          | Default                            | Notes                                                                    |
| ------------------------------------------------ | ---------------------------------- | ------------------------------------------------------------------------ |
| `manifoldLanguageServer.completions.autoTrigger` | `true`                             | Automatically request completions after triggering characters.           |
| `manifoldLanguageServer.typeHints.enabled`       | `true`                             | Show inline type annotations when available.                             |
| `manifoldLanguageServer.trace.server`            | `off`                              | Enables verbose logging between VS Code and the LSP for troubleshooting. |
| `manifoldLanguageServer.documentSelectors`       | `html`, `javascript`, `typescript` | Customize which language IDs should activate the server.                 |
