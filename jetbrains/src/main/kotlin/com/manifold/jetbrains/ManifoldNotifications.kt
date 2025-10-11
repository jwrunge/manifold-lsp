package com.manifold.jetbrains

import com.intellij.notification.NotificationGroupManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.project.Project

internal object ManifoldNotifications {
    private const val GROUP_ID = "Manifold LSP"

    fun showMissingServer(project: Project) {
        NotificationGroupManager.getInstance()
            .getNotificationGroup(GROUP_ID)
            .createNotification(
                "Manifold language server not found",
                "Install the manifold-language-server binary and make it available via MANIFOLD_LSP_BIN or your PATH.",
                NotificationType.WARNING
            )
            .notify(project)
    }
}
