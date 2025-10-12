import org.gradle.api.tasks.compile.JavaCompile
import org.gradle.internal.os.OperatingSystem
import org.gradle.jvm.tasks.Jar
import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
    kotlin("jvm") version "1.9.24"
    id("org.jetbrains.intellij") version "1.17.3"
}

group = "com.manifold"
version = "1.0.13"

repositories {
    mavenCentral()
}

intellij {
    version.set("2024.2")
    type.set("IC")
}

kotlin {
    compilerOptions {
        jvmTarget.set(JvmTarget.JVM_17)
    }
}

tasks.withType<JavaCompile>().configureEach {
    options.release.set(17)
}

tasks.withType<Jar>().configureEach {
    exclude("com/intellij/platform/lsp/api/**")
}

tasks {
    patchPluginXml {
        sinceBuild.set("242")
        untilBuild.set("")
    }

    buildSearchableOptions {
        enabled = false
    }

    val prepareServerBinary = register<Copy>("prepareServerBinary") {
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

        into(layout.buildDirectory.dir("generated/server/${os.name}"))
        binaryToCopy?.let { from(it) }
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
