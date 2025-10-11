## JetBrains Plugin Details

The IntelliJ Platform plugin bridges JetBrains IDEs to the Manifold language server through JetBrains' built-in LSP client APIs.

### Features

-   Starts the `manifold-language-server` executable when HTML documents are opened.
-   Resolves the server binary from a bundled copy, the `MANIFOLD_LSP_BIN` environment variable, or the current `PATH`.
-   Displays an IDE notification if the language server cannot be located.

### Project Structure

-   `build.gradle.kts` – IntelliJ Platform Gradle configuration (`org.jetbrains.intellij`).
-   `gradle/` – Gradle wrapper configuration (downloads Gradle 8.x on demand).
-   `src/main/kotlin` – Kotlin sources for the LSP bridge.
-   `src/main/resources/META-INF/plugin.xml` – Plugin metadata and extension point registrations.

### Prerequisites

-   JDK 17 or newer.
-   The Manifold language server binary built via Cargo (`cargo build --release` produces `target/release/manifold-language-server`).

### Working with the Plugin

```bash
./gradlew buildPlugin       # Build the distributable ZIP
./gradlew runIde            # Launch a sandbox IDE with the plugin installed
```

The first invocation downloads the Gradle wrapper JAR automatically. Subsequent runs reuse the cached artifact.

### Supplying the Language Server Binary

1. **Bundled binary (recommended)** – Copy platform-specific builds into:

    - `jetbrains/bin/macos/manifold-language-server`
    - `jetbrains/bin/linux/manifold-language-server`
    - `jetbrains/bin/windows/manifold-language-server.exe`

    The Gradle build copies these binaries into the generated plugin so they ship with the distribution.

2. **Cargo build outputs** – Running `cargo build --release` (or `--debug`) from the repository root lets `./gradlew prepareServerBinary` detect and package `target/{release,debug}/manifold-language-server` automatically.

3. **Environment override** – Set `MANIFOLD_LSP_BIN=/absolute/path/to/manifold-language-server`.

4. **PATH resolution** – Ensure `manifold-language-server` is discoverable on `PATH`.

If no executable is found, the plugin posts a notification prompting you to install or configure the server.

### Installing into JetBrains IDEs

1. Run `./gradlew buildPlugin`.
2. Locate the ZIP artifact under `build/distributions`.
3. In your IDE, open **Settings → Plugins → ⚙ → Install Plugin from Disk…** and select the ZIP.
4. Restart the IDE.

Open an HTML document with Manifold annotations (`data-mf-register`, `${...}` expressions). The IDE starts the language server using STDIO pipes and wires IntelliSense features through JetBrains' LSP client.

### Next Steps

-   Automate packaging of release binaries for each platform and place them in `jetbrains/bin/<platform>/`.
-   Add functional tests using the IntelliJ test framework to verify the LSP handshake.
