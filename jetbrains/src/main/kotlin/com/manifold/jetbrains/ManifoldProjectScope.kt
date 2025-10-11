package com.manifold.jetbrains

import com.intellij.openapi.vfs.VirtualFile

internal object ManifoldProjectScope {
    private val SUPPORTED_EXTENSIONS = setOf("html", "htm")

    fun accepts(file: VirtualFile): Boolean {
        val ext = file.extension?.lowercase() ?: return false
        return ext in SUPPORTED_EXTENSIONS
    }
}
