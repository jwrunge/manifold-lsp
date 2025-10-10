package com.manifold.jetbrains

import com.intellij.execution.configurations.GeneralCommandLine
import java.nio.file.Path

internal object ManifoldProcessFactory {
    fun create(executable: Path): GeneralCommandLine {
        return GeneralCommandLine(executable.toAbsolutePath().toString())
            .withEnvironment(mapOf("RUST_LOG" to "info"))
    }
}
