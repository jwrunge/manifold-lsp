## JetBrains Plugin Details

The IntelliJ Platform plugin bridges JetBrains IDEs to the Manifold language server through JetBrains' built-in LSP client APIs.

### Features

-   Starts the `manifold-language-server` executable when HTML documents are opened.
-   Resolves the server binary from a bundled copy, the `MANIFOLD_LSP_BIN` environment variable, or the current `PATH`.
-   Displays an IDE notification if the language server cannot be located.
