@file:Suppress("unused")

package com.intellij.platform.lsp.api

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project

/**
 * Minimal stub of the IntelliJ Platform LSP server descriptor so the project can compile
 * on environments where the official `com.intellij.platform.lsp` plugin is unavailable.
 *
 * This stub is excluded from the published plugin artifact and only used at compile time.
 */
abstract class LspServerDescriptor(
    val project: Project,
    val presentableName: String
) {
    open val startInBackground: Boolean = true

    abstract fun createCommandLine(): GeneralCommandLine
}
