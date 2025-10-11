# Manifold Language Server

A Language Server Protocol (LSP) implementation for the Manifold HTML framework, providing intelligent code completion, syntax highlighting, and error checking for Manifold's reactive HTML syntax.

## Features

-   **Syntax Highlighting**: Proper color coding for Manifold directives and interpolations
-   **Code Completion**: Intelligent completions for Manifold directives, HTML elements, and attributes
-   **Scoped Suggestions**: Automatic completions for `${}` interpolations, Manifold attributes, and property access with a toggleable auto-trigger command (`Manifold: Toggle Auto Suggestions`)
-   **Type Hints**: Type information for variables and expressions
-   **Error Detection**: Real-time error reporting for invalid Manifold syntax
-   **Go-to Definition**: Navigate to variable and function definitions
-   **Hover Information**: Contextual information for Manifold elements

## JetBrains IDE support

A Gradle-based IntelliJ Platform plugin lives under `jetbrains/`. Use `./gradlew runIde` from that directory to launch a sandbox IDE, or `./gradlew buildPlugin` to produce an installable ZIP. The plugin looks for the `manifold-language-server` binary in `jetbrains/bin/<platform>/`, the Cargo `target` folders, `MANIFOLD_LSP_BIN`, or your `PATH`.

## Manifold Syntax Support

This language server supports the following Manifold features:

### Directives

-   `:if`, `:else`, `:elif` - Conditional rendering
-   `:each` - List iteration
-   `:value`, `:checked` - One-way data binding
-   `:sync:value`, `:sync:checked` - Two-way data binding
-   `:onclick`, `:onchange`, etc. - Event handlers
-   `:class`, `:style` - Dynamic styling

### Data Attributes

-   `data-mf-register` - Register a Manifold context
-   `data-mf-ignore` - Exclude elements from Manifold processing

### Text Interpolation

-   `${}` expressions for dynamic text content
-   Variable references and expressions within interpolations

## Example Manifold HTML

```html
<!DOCTYPE html>
<html lang="en">
	<body data-mf-register>
		<h1>Todo App</h1>

		<div>
			<input :sync:value="newTodo" placeholder="Add a todo..." />
			<button :onclick="addTodo()">Add</button>
		</div>

		<ul>
			<li :each="todos as todo, index">
				<span :class="{ completed: todo.done }">${todo.text}</span>
				<button :onclick="toggleTodo(index)">Toggle</button>
				<button :onclick="deleteTodo(index)">Delete</button>
			</li>
		</ul>

		<p :if="todos.length === 0">No todos yet!</p>
		<p :else>Total: ${todos.length} todos</p>
	</body>
</html>
```

## Other Editors

The language server speaks standard LSP, so you can wire it into editors beyond VS Code and JetBrains.

### Vim / Neovim

1.  Install the language server binary (`cargo install --path .` or copy `target/release/manifold-language-server` onto your `PATH`).
2.  Add an LSP client configuration. For Neovim with `nvim-lspconfig`:

        ```lua
        require('lspconfig').manifold_html.setup {
        	cmd = { 'manifold-language-server' },
        	filetypes = { 'html', 'javascript', 'typescript' },
        	root_dir = require('lspconfig.util').root_pattern('manifold.config.ts', '.git'),
        }
        ```

        For Vim with [`coc.nvim`](https://github.com/neoclide/coc.nvim), add to `coc-settings.json`:

        ```json
        {
        	"languageserver": {
        		"manifold": {
        			"command": "manifold-language-server",
        			"filetypes": ["html", "javascript", "typescript"],
        			"rootPatterns": ["manifold.config.ts", ".git"]
        		}
        	}
        }
        ```

3.  Restart the editor. Opening a Manifold HTML file should now trigger completions, diagnostics, and hovers.

### Helix

1.  Ensure `manifold-language-server` is on your `PATH` (or provide an absolute path in the command below).
2.  Add the following block to your `~/.config/helix/languages.toml`:

        ```toml
        [[language]]
        name = "html"
        scope = "text.html.basic"
        file-types = ["html", "htm"]

        [language.language-server]
        command = "manifold-language-server"
        args = []

        [[language.auto-pairs]]
        open = "${"
        close = "}"
        ```

3.  Restart Helix. The server will attach to HTML buffers and provide Manifold-specific assistance.
