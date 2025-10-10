package com.manifold.jetbrains

import com.intellij.openapi.application.PathManager
import java.nio.file.Files
import java.nio.file.Path

internal object ManifoldPluginPaths {
    fun pluginHome(): Path? {
        val jarPath = PathManager.getJarPathForClass(ManifoldPluginPaths::class.java) ?: return null
        val path = Path.of(jarPath)
        return if (Files.isDirectory(path)) path else path.parent
    }
}
