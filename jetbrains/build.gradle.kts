import org.gradle.internal.os.OperatingSystem

plugins {
    kotlin("jvm") version "1.9.24"
    id("org.jetbrains.intellij") version "1.17.3"
}

group = "com.manifold"
version = "0.1.0"

repositories {
    mavenCentral()
}

intellij {
    version.set("2023.3")
    type.set("IC")
    plugins.set(listOf("com.intellij.platform.lsp"))
}

kotlin {
    jvmToolchain(17)
}

tasks {
    patchPluginXml {
        sinceBuild.set("233")
        untilBuild.set("")
    }

    buildSearchableOptions {
        enabled = false
    }

    val prepareServerBinary by registering<Copy>("prepareServerBinary") {
        description = "Copy the Manifold language server binary into the plugin distribution if present."
        group = "build"

        val os = OperatingSystem.current()
        val binaryName = if (os.isWindows) "manifold-language-server.exe" else "manifold-language-server"
        val projectRoot = projectDir.resolve("..")
        val osDirectory = when {
            os.isWindows -> "windows"
            os.isMacOsX -> "macos"
            else -> "linux"
        }

        val bundledBinary = projectDir.resolve("bin/$osDirectory/$binaryName").takeIf { it.exists() }
        val releaseBinary = projectRoot.resolve("target/release/$binaryName").takeIf { it.exists() }
        val debugBinary = projectRoot.resolve("target/debug/$binaryName").takeIf { it.exists() }

        val binaryToCopy = bundledBinary ?: releaseBinary ?: debugBinary

        onlyIf { binaryToCopy != null }

        if (binaryToCopy != null) {
            from(binaryToCopy)
            into(layout.buildDirectory.dir("generated/server/${os.name}").get().asFile)
        }
    }

    prepareSandbox {
        dependsOn(prepareServerBinary)
        from(layout.buildDirectory.dir("generated/server")) {
            into("bin")
        }
    }

    publishPlugin {
        val tokenProvider = providers.environmentVariable("JETBRAINS_MARKETPLACE_TOKEN")
            .orElse(providers.environmentVariable("JETBRAINS_TOKEN"))
        token.set(tokenProvider)

        val channelEnv = providers.environmentVariable("JETBRAINS_MARKETPLACE_CHANNEL")
            .orElse(providers.environmentVariable("JETBRAINS_CHANNEL"))
        val channelsValue = channelEnv.orNull
        if (!channelsValue.isNullOrBlank()) {
            val parsedChannels = channelsValue.split(',')
                .map { it.trim() }
                .filter { it.isNotEmpty() }
            channels.set(parsedChannels)
        }
    }
}
