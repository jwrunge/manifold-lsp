package com.manifold.jetbrains

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.LspServerDescriptor
import com.intellij.platform.lsp.api.LspServerSupportProvider

/**
 * Bridges JetBrains IDEs to the Manifold language server via the IntelliJ Platform LSP API.
 */
class ManifoldLspServerSupportProvider : LspServerSupportProvider {
    override fun fileOpened(
        project: Project,
        file: VirtualFile,
        serverStarter: LspServerSupportProvider.LspServerStarter
    ) {
        if (!ManifoldProjectScope.accepts(file)) {
            return
        }

    val executable = ManifoldExecutableLocator.locateExecutable()
        if (executable == null) {
            ManifoldNotifications.showMissingServer(project)
            return
        }

        val descriptor = object : LspServerDescriptor(project, "Manifold Language Server") {
            override fun createCommandLine() = ManifoldProcessFactory.create(executable)
        }

        serverStarter.startServer(descriptor)
    }
}
