# Manifold LSP - Selective Parsing Update

## Problem Solved

The language server was generating numerous unnecessary HTML parsing errors because it was trying to parse entire HTML files as Manifold syntax. This caused a poor user experience with lots of false-positive error messages.

## Solution: Selective Manifold Parsing

Instead of parsing the entire HTML file, the language server now uses a **selective approach** that:

1. **Extracts only Manifold-specific content** using regex patterns
2. **Leaves HTML parsing to the default HTML language server**
3. **Provides intelligent features only for Manifold syntax**

## Key Changes Made

### 1. New Module: `manifold_extract.rs`

-   Added regex-based extraction for Manifold-specific content
-   Supports three types of extracts:
    -   **Interpolations**: `${expression}` patterns
    -   **Directives**: `:attribute="value"` patterns
    -   **Data Attributes**: `data-mf-*` patterns

### 2. Updated `main.rs` Architecture

-   **Removed**: Full HTML document parsing via `parse()` function
-   **Added**: Selective extraction using `extract_manifold_content()`
-   **Replaced**: AST-based storage with extract-based storage
-   **Updated**: Error reporting to only show Manifold-specific issues

### 3. Improved Diagnostics

-   **Before**: Hundreds of HTML parsing errors
-   **After**: Only meaningful Manifold syntax errors
-   **Validates**:
    -   Empty interpolation expressions
    -   Missing directive values
    -   Invalid `:each` syntax (missing "as")
    -   Invalid context names in `data-mf-register`

### 4. Enhanced Completions

-   **Context-aware**: Provides different completions based on location
-   **Inside interpolations**: JavaScript expressions, console.log, etc.
-   **Inside directives**: Boolean values, common patterns
-   **In HTML tags**: Manifold directive suggestions
-   **Snippet support**: Templates with placeholders

### 5. Better Type Hints

-   **Interpolations**: Shows `(expr)` type hints
-   **Boolean directives**: Shows `(boolean)` for `:if`, `:show`, etc.
-   **Non-intrusive**: Only adds hints for Manifold content

## Example: What Changed

### Before (Full Parsing)

```
❌ Unexpected token '<' at position 0
❌ Unexpected token 'DOCTYPE' at position 1
❌ Unexpected token 'html' at position 9
❌ Expected expression, found '>'
... (hundreds more HTML parsing errors)
```

### After (Selective Parsing)

```
✅ No HTML errors (handled by default HTML LSP)
⚠️  Only meaningful Manifold errors:
    - Empty interpolation expression in ${}
    - Each directive requires 'items as item' syntax
    - Context name should contain only alphanumeric characters
```

## Benefits

1. **Cleaner Error Experience**: No more HTML parsing noise
2. **Better Performance**: Only processes Manifold-specific content
3. **Coexistence**: Works alongside default HTML language server
4. **Focused Features**: Intelligent completions only where relevant
5. **Maintainable**: Easier to extend with new Manifold features

## Validation

-   ✅ **Compiles successfully** with only warnings
-   ✅ **Language server runs** without errors
-   ✅ **Extracts Manifold content** correctly
-   ✅ **Provides context-specific completions**
-   ✅ **Shows relevant type hints**
-   ✅ **Reports only Manifold errors**

## Testing

The solution has been tested with the `examples/manifold-example.html` file which contains:

-   Multiple interpolation expressions: `${count}`, `${username}`
-   Various directives: `:onclick`, `:if`, `:each`, `:sync:value`
-   Data attributes: `data-mf-register`, `data-mf-ignore`
-   Mixed HTML/Manifold content

The language server now correctly identifies and processes only the Manifold-specific parts while leaving HTML validation to the default language server.

## Next Steps

1. **Test in VS Code**: Verify the improved error experience
2. **Enhance Regex Patterns**: Handle more complex Manifold syntax
3. **Add More Completions**: Expand context-specific suggestions
4. **Improve Type Inference**: Better analysis of JavaScript expressions
5. **Add Configuration**: Allow users to customize validation rules
