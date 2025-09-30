# Manifold Language Server

A Language Server Protocol (LSP) implementation for the Manifold HTML framework, providing intelligent code completion, syntax highlighting, and error checking for Manifold's reactive HTML syntax.

## Features

-   **Syntax Highlighting**: Proper color coding for Manifold directives and interpolations
-   **Code Completion**: Intelligent completions for Manifold directives, HTML elements, and attributes
-   **Type Hints**: Type information for variables and expressions
-   **Error Detection**: Real-time error reporting for invalid Manifold syntax
-   **Go-to Definition**: Navigate to variable and function definitions
-   **Hover Information**: Contextual information for Manifold elements

## Manifold Syntax Support

This language server supports the following Manifold features:

### Directives

-   `:if`, `:else`, `:elif` - Conditional rendering
-   `:each` - List iteration
-   `:sync:value`, `:sync:checked` - Two-way data binding
-   `:onclick`, `:onchange`, etc. - Event handlers
-   `:class`, `:style` - Dynamic styling
-   `:show`, `:hide` - Visibility control

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

    print("Hello, world!");
    print("The meaning of life is...");

    if meaning_of_life == 42 {
        print(meaning_of_life);
    } else {
        print("...something we cannot know");

        print("However, I can tell you that the factorial of 10 is...");
        // Function calling
        print(factorial(10));
    }

}

````
## Introduction
This repo is a template for `tower-lsp`, a useful github project template which makes writing new language servers easier.
## Development using VSCode
1. `pnpm i`
2. `cargo build`
3. Open the project in VSCode: `code .`
4. In VSCode, press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>.
5. In the newly launched VSCode instance, open the file `examples/test.nrs` from this project.
6. If the LSP is working correctly you should see syntax highlighting and the features described below should work.

> [!note]
> If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
> please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details

### Preview and test extension locally with `VsCode`
1. Make sure all dependency are installed.
2. Make sure the `nrs-language-server` is under your `PATH`
3. `pnpm run package`
4. `code --install-extension nrs-language-server-${version}.vsix`, the `version` you could inspect in file system.
5. Restart the `VsCode`, and write a minimal `nano rust` file, then inspect the effect.

For other editor, please refer the related manual, you could skip steps above.


## Features
This repo use a language `nano rust` which first introduced by [ chumsky ](https://github.com/zesterer/chumsky/blob/master/examples/nano_rust.rs). Most common language feature has been implemented, you could preview via the video below.

- [x] InlayHint for LiteralType
![inlay hint](https://user-images.githubusercontent.com/17974631/156926412-c3823dac-664e-430e-96c1-c003a86eabb2.gif)

- [x] semantic token
make sure your semantic token is enabled, you could enable your `semantic token` by
adding this line  to your `settings.json`
```json
{
 "editor.semanticHighlighting.enabled": true,
}
````

-   [x] syntactic error diagnostic

https://user-images.githubusercontent.com/17974631/156926382-a1c4c911-7ea1-4d3a-8e08-3cf7271da170.mp4

-   [x] code completion

https://user-images.githubusercontent.com/17974631/156926355-010ef2cd-1d04-435b-bd1e-8b0dab9f44f1.mp4

-   [x] go to definition

https://user-images.githubusercontent.com/17974631/156926103-94d90bd3-f31c-44e7-a2ce-4ddfde89bc33.mp4

-   [x] find reference

https://user-images.githubusercontent.com/17974631/157367235-7091a36c-631a-4347-9c1e-a3b78db81714.mp4

-   [x] rename

https://user-images.githubusercontent.com/17974631/157367229-99903896-5583-4f67-a6da-1ae1cf206876.mp4
