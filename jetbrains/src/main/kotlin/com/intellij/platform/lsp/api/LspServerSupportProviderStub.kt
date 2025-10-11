@file:Suppress("unused")

package com.intellij.platform.lsp.api

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
 * Minimal stub of the IntelliJ Platform LSP server support API so the project can compile
 * without the external `com.intellij.platform.lsp` plugin being present on the classpath.
 *
 * This stub is excluded from the published plugin artifact and only used at compile time.
 */
interface LspServerSupportProvider {
    fun fileOpened(project: Project, file: VirtualFile, serverStarter: LspServerStarter)

    interface LspServerStarter {
        fun startServer(descriptor: LspServerDescriptor)
    }
}
