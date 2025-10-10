package com.manifold.jetbrains

import com.intellij.execution.configurations.PathEnvironmentVariableUtil
import com.intellij.openapi.util.SystemInfoRt
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.exists
import kotlin.io.path.isExecutable

object ManifoldExecutableLocator {
    private const val ENV_OVERRIDE = "MANIFOLD_LSP_BIN"

    fun locateExecutable(): Path? {
        locateFromEnvironment()?.let { return it }
        locateBundledBinary()?.let { return it }
        locateOnPath()?.let { return it }
        return null
    }

    private fun locateFromEnvironment(): Path? {
        val override = System.getenv(ENV_OVERRIDE).takeUnless { it.isNullOrBlank() } ?: return null
        val path = Path.of(override)
        return if (path.exists() && path.isExecutable()) path else null
    }

    private fun locateBundledBinary(): Path? {
        val pluginHome = ManifoldPluginPaths.pluginHome() ?: return null
        val osDirectory = when {
            SystemInfoRt.isWindows -> "windows"
            SystemInfoRt.isMac -> "macos"
            else -> "linux"
        }

        val binaryName = if (SystemInfoRt.isWindows) "manifold-language-server.exe" else "manifold-language-server"
        val candidate = pluginHome.resolve("bin/$osDirectory/$binaryName")
        return if (Files.isExecutable(candidate)) candidate else null
    }

    private fun locateOnPath(): Path? {
        val binaryName = if (SystemInfoRt.isWindows) "manifold-language-server.exe" else "manifold-language-server"
        return PathEnvironmentVariableUtil.findExecutableInPath(binaryName)?.toPath()
    }
}
