/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
	workspace,
	EventEmitter,
	ExtensionContext,
	window,
	languages,
	InlayHint as VSInlayHint,
	InlayHintKind as VSInlayHintKind,
	CancellationToken,
	commands,
	ConfigurationTarget,
	DocumentFilter,
	DocumentSelector,
	TextDocument,
	Range,
	CompletionTriggerKind,
	StatusBarAlignment,
	StatusBarItem,
	Uri,
	env,
	OutputChannel,
} from "vscode";

import {
	Disposable,
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	ExecuteCommandRequest,
	State,
} from "vscode-languageclient/node";
import { accessSync, constants } from "fs";
import * as path from "path";
import which = require("which");

let client: LanguageClient;
let statusItem: StatusBarItem | undefined;
let traceOutputChannel: OutputChannel | undefined;

const MISSING_SERVER_DOCS = Uri.parse(
	"https://github.com/jwrunge/manifold-lsp#installation"
);

function resolveServerCommand(
	context: ExtensionContext,
	outputChannel: OutputChannel
): string | undefined {
	const override = process.env.SERVER_PATH;
	if (override) {
		if (isExecutable(override)) {
			outputChannel.appendLine(
				`Using language server from SERVER_PATH: ${override}`
			);
			return override;
		}
		outputChannel.appendLine(
			`SERVER_PATH is set to "${override}" but the file is not executable or does not exist.`
		);
	}

	const workspaceBinary = getWorkspaceBinary(context, outputChannel);
	if (workspaceBinary) {
		return workspaceBinary;
	}

	const bundled = getBundledBinary(context, outputChannel);
	if (bundled) {
		outputChannel.appendLine(
			`Using bundled language server binary at ${bundled}`
		);
		return bundled;
	}

	try {
		const resolved = which.sync("manifold-language-server");
		outputChannel.appendLine(
			`Using language server found on PATH: ${resolved}`
		);
		return resolved;
	} catch (error) {
		outputChannel.appendLine(
			"manifold-language-server was not found on PATH."
		);
		if (error instanceof Error) {
			outputChannel.appendLine(error.message);
		}
		return undefined;
	}
}

function handleMissingServerBinary(context: ExtensionContext): void {
	const message =
		"Manifold language server executable not found. Install the CLI or set SERVER_PATH.";
	const learnMore = "Open setup guide";
	void window.showErrorMessage(message, learnMore).then((selection) => {
		if (selection === learnMore) {
			void env.openExternal(MISSING_SERVER_DOCS);
		}
	});
	registerMissingServerCommands(context, message);
}

function registerMissingServerCommands(
	ctx: ExtensionContext,
	message: string
): void {
	const commandIds = [
		"manifoldLanguageServer.toggleTypeHints",
		"manifoldLanguageServer.toggleCompletionAutoTrigger",
		"manifoldLanguageServer.restart",
	];

	for (const commandId of commandIds) {
		ctx.subscriptions.push(
			commands.registerCommand(commandId, () => {
				const learnMore = "Open setup guide";
				void window
					.showErrorMessage(message, learnMore)
					.then((selection) => {
						if (selection === learnMore) {
							void env.openExternal(MISSING_SERVER_DOCS);
						}
					});
			})
		);
	}
}

function getBundledBinary(
	context: ExtensionContext,
	outputChannel: OutputChannel
): string | undefined {
	const platform = process.platform;
	const binaryName =
		platform === "win32"
			? "manifold-language-server.exe"
			: "manifold-language-server";
	const candidates = [
		context.asAbsolutePath(path.join("bin", platform, binaryName)),
		context.asAbsolutePath(path.join("bin", binaryName)),
	];

	for (const candidate of candidates) {
		if (isExecutable(candidate)) {
			return candidate;
		}
	}

	if (candidates.length > 0) {
		outputChannel.appendLine(
			"No bundled Manifold language server binary was found in the extension package."
		);
	}

	return undefined;
}

function getWorkspaceBinary(
	context: ExtensionContext,
	outputChannel: OutputChannel
): string | undefined {
	const platform = process.platform;
	const binaryName =
		platform === "win32"
			? "manifold-language-server.exe"
			: "manifold-language-server";
	const repoRoot = path.resolve(context.extensionPath, "..");
	const candidates = [
		path.join(repoRoot, "target", "debug", binaryName),
		path.join(repoRoot, "target", "release", binaryName),
	];

	for (const candidate of candidates) {
		if (isExecutable(candidate)) {
			outputChannel.appendLine(
				`Using workspace language server binary at ${candidate}`
			);
			return candidate;
		}
	}

	return undefined;
}

function isExecutable(filePath: string): boolean {
	try {
		accessSync(filePath, constants.X_OK);
		return true;
	} catch {
		try {
			accessSync(filePath, constants.F_OK);
			return true;
		} catch {
			return false;
		}
	}
}

export async function activate(context: ExtensionContext): Promise<void> {
	console.log("[Manifold] activate() called");
	traceOutputChannel = window.createOutputChannel(
		"Manifold Language Server trace"
	);
	const channel = traceOutputChannel;
	channel.appendLine("Activating Manifold language client...");
	const command = resolveServerCommand(context, channel);
	if (!command) {
		console.warn("[Manifold] No server command resolved; prompting user");
		channel.show(true);
		handleMissingServerBinary(context);
		return;
	}
	channel.appendLine(`Resolved language server command: ${command}`);
	const configuration = workspace.getConfiguration("manifoldLanguageServer");
	const traceSetting = configuration.get<string>("trace.server", "off");
	const rustLogLevel = mapTraceToRustLog(traceSetting);
	const run: Executable = {
		command,
		options: {
			env: {
				...process.env,
				// eslint-disable-next-line @typescript-eslint/naming-convention
				RUST_LOG: rustLogLevel,
			},
		},
	};
	const serverOptions: ServerOptions = {
		run,
		debug: run,
	};

	const selectors = getDocumentSelectors();
	const clientOptions: LanguageClientOptions = {
		documentSelector:
			selectors as LanguageClientOptions["documentSelector"],
		traceOutputChannel: channel,
	};

	client = new LanguageClient(
		"manifold-language-server",
		"Manifold Language Server",
		serverOptions,
		clientOptions
	);
	registerRestartCommand(context);
	try {
		await client.start();
		channel.appendLine("Manifold language server started successfully.");
	} catch (error) {
		console.error("[Manifold] Failed to start language server", error);
		channel.appendLine(
			`Failed to start Manifold language server with command: ${command}`
		);
		channel.appendLine(String(error));
		void window.showErrorMessage(
			"Manifold language server failed to launch. Check the output channel for details."
		);
		channel.show(true);
		return;
	}
	context.subscriptions.push({
		dispose: () => {
			void client.stop();
		},
	});
	activateInlayHints(context, client, selectors);
	activateCompletions(context, client, selectors);
}

function registerRestartCommand(context: ExtensionContext): void {
	const commandId = "manifoldLanguageServer.restart";
	context.subscriptions.push(
		commands.registerCommand(commandId, async () => {
			if (!client) {
				void window.showWarningMessage(
					"Manifold language server is not currently running."
				);
				return;
			}
			try {
				await client.restart();
				traceOutputChannel?.appendLine(
					"Language server restarted via command palette."
				);
				void window.showInformationMessage(
					"Manifold language server restarted."
				);
			} catch (error) {
				traceOutputChannel?.appendLine(
					`Failed to restart Manifold language server: ${String(
						error
					)}`
				);
				void window.showErrorMessage(
					"Failed to restart the Manifold language server. Check the output channel for details."
				);
			}
		})
	);
}

type SelectorConfig = {
	language?: string;
	scheme?: string;
};

function getDocumentSelectors(): DocumentFilter[] {
	const configuration = workspace.getConfiguration("manifoldLanguageServer");
	const configured = configuration.get<SelectorConfig[]>(
		"documentSelectors",
		[
			{
				language: "html",
				scheme: "file",
			},
			{
				language: "manifold",
				scheme: "file",
			},
			{
				language: "manifold-html",
				scheme: "file",
			},
			{
				language: "html",
				scheme: "untitled",
			},
		]
	);

	const normalized = configured
		.map((selector) => {
			if (!selector || typeof selector !== "object") {
				return null;
			}
			const language = selector.language ?? "";
			const scheme = selector.scheme ?? "file";
			if (!language) {
				return null;
			}
			const documentFilter: DocumentFilter = {
				language,
				scheme,
			};
			return documentFilter;
		})
		.filter((selector): selector is DocumentFilter => selector !== null);

	if (normalized.length === 0) {
		return [
			{
				language: "html",
				scheme: "file",
			},
			{
				language: "manifold",
				scheme: "file",
			},
		];
	}

	return normalized;
}

function mapTraceToRustLog(value: string | undefined): string {
	switch (value) {
		case "verbose":
			return "debug";
		case "messages":
			return "info";
		default:
			return "error";
	}
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

export function activateInlayHints(
	ctx: ExtensionContext,
	languageClient: LanguageClient,
	selectors: DocumentSelector
): void {
	const changeEmitter = new EventEmitter<void>();
	ctx.subscriptions.push(changeEmitter);

	const provider = {
		onDidChangeInlayHints: changeEmitter.event,
		async provideInlayHints(
			document: TextDocument,
			range: Range,
			token: CancellationToken
		): Promise<VSInlayHint[]> {
			if (!isTypeHintsEnabled()) {
				return [];
			}

			if (token.isCancellationRequested) {
				return [];
			}

			const params = {
				textDocument:
					languageClient.code2ProtocolConverter.asTextDocumentIdentifier(
						document
					),
				range: languageClient.code2ProtocolConverter.asRange(range),
			};

			try {
				const response = await languageClient.sendRequest(
					"textDocument/inlayHint",
					params
				);
				if (!Array.isArray(response)) {
					return [];
				}

				return response
					.map((hint) => convertInlayHint(languageClient, hint))
					.filter((hint): hint is VSInlayHint => hint !== null);
			} catch (error) {
				console.error("Failed to fetch Manifold inlay hints", error);
				return [];
			}
		},
	};

	ctx.subscriptions.push(
		languages.registerInlayHintsProvider(selectors, provider)
	);

	ctx.subscriptions.push(
		workspace.onDidChangeConfiguration((event) => {
			if (
				event.affectsConfiguration(
					"manifoldLanguageServer.typeHints.enabled"
				)
			) {
				changeEmitter.fire();
			}
		})
	);

	ctx.subscriptions.push(
		commands.registerCommand(
			"manifoldLanguageServer.toggleTypeHints",
			async () => {
				const config = workspace.getConfiguration(
					"manifoldLanguageServer"
				);
				const enabled = config.get<boolean>("typeHints.enabled", true);
				await config.update(
					"typeHints.enabled",
					!enabled,
					ConfigurationTarget.Global
				);
				changeEmitter.fire();
			}
		)
	);
}

function isTypeHintsEnabled(): boolean {
	return workspace
		.getConfiguration("manifoldLanguageServer")
		.get<boolean>("typeHints.enabled", true);
}

export function activateCompletions(
	ctx: ExtensionContext,
	languageClient: LanguageClient,
	selectors: DocumentSelector
): void {
	const triggerCharacters = [".", "{", '"'];
	const statusBar = window.createStatusBarItem(
		"manifoldAutoSuggestions",
		StatusBarAlignment.Right,
		100
	);
	statusBar.command = "manifoldLanguageServer.toggleCompletionAutoTrigger";
	ctx.subscriptions.push(statusBar);

	const updateStatusBar = () => {
		if (isCompletionAutoTriggerEnabled()) {
			statusBar.text = "Manifold Auto Suggs: On";
			statusBar.tooltip =
				"Click to disable auto-triggered Manifold completions (still available via Ctrl+Space).";
		} else {
			statusBar.text = "Manifold Auto Suggs: Off";
			statusBar.tooltip =
				"Click to re-enable auto-triggered Manifold completions.";
		}
		statusBar.show();
	};

	updateStatusBar();

	ctx.subscriptions.push(
		workspace.onDidChangeConfiguration((event) => {
			if (
				event.affectsConfiguration(
					"manifoldLanguageServer.completions.autoTrigger"
				)
			) {
				updateStatusBar();
			}
		})
	);

	ctx.subscriptions.push(
		languages.registerCompletionItemProvider(
			selectors,
			{
				async provideCompletionItems(
					document,
					position,
					token,
					context
				) {
					if (
						context.triggerKind ===
							CompletionTriggerKind.TriggerCharacter &&
						!isCompletionAutoTriggerEnabled()
					) {
						return undefined;
					}

					if (
						context.triggerKind ===
							CompletionTriggerKind.TriggerCharacter &&
						context.triggerCharacter === "{" &&
						position.character >= 1
					) {
						const preceding = document.getText(
							new Range(position.translate(0, -1), position)
						);
						if (preceding !== "$") {
							return undefined;
						}
					}

					if (
						context.triggerKind ===
							CompletionTriggerKind.TriggerCharacter &&
						context.triggerCharacter === '"'
					) {
						const linePrefix = document.getText(
							new Range(position.with(undefined, 0), position)
						);
						const attributeMatch = linePrefix.match(
							/([\w:.$-]+)\s*=\s*(?:["'][^"']*)?$/
						);
						const attributeName = attributeMatch?.[1] ?? "";
						if (
							attributeName.length > 0 &&
							!attributeName.startsWith(":") &&
							!attributeName.startsWith("data-mf")
						) {
							return undefined;
						}
					}

					try {
						const params = {
							textDocument:
								languageClient.code2ProtocolConverter.asTextDocumentIdentifier(
									document
								),
							position:
								languageClient.code2ProtocolConverter.asPosition(
									position
								),
						};

						const response = (await languageClient.sendRequest(
							"textDocument/completion",
							params,
							token
						)) as unknown;
						if (!response) {
							return undefined;
						}

						return languageClient.protocol2CodeConverter.asCompletionResult(
							response as any
						);
					} catch (error) {
						console.error(
							"Failed to fetch Manifold completions",
							error
						);
						return undefined;
					}
				},
			},
			...triggerCharacters
		)
	);

	ctx.subscriptions.push(
		commands.registerCommand(
			"manifoldLanguageServer.toggleCompletionAutoTrigger",
			async () => {
				const config = workspace.getConfiguration(
					"manifoldLanguageServer"
				);
				const enabled = config.get<boolean>(
					"completions.autoTrigger",
					true
				);
				const next = !enabled;
				await config.update(
					"completions.autoTrigger",
					next,
					ConfigurationTarget.Global
				);
				updateStatusBar();
				void window.showInformationMessage(
					next
						? "Manifold auto suggestions enabled."
						: "Manifold auto suggestions disabled; use Ctrl+Space to request completions."
				);
			}
		)
	);
}

function isCompletionAutoTriggerEnabled(): boolean {
	return workspace
		.getConfiguration("manifoldLanguageServer")
		.get<boolean>("completions.autoTrigger", true);
}

function convertInlayHint(
	languageClient: LanguageClient,
	hint: unknown
): VSInlayHint | null {
	if (!hint || typeof hint !== "object") {
		return null;
	}

	const rawHint = hint as {
		position?: unknown;
		label?: unknown;
		kind?: number;
		paddingLeft?: boolean;
		paddingRight?: boolean;
	};

	if (!rawHint.position) {
		return null;
	}

	const position = languageClient.protocol2CodeConverter.asPosition(
		rawHint.position as any
	);

	let labelText = "";
	if (typeof rawHint.label === "string") {
		labelText = rawHint.label;
	} else if (Array.isArray(rawHint.label)) {
		labelText = rawHint.label
			.map((part: any) =>
				typeof part?.value === "string" ? part.value : ""
			)
			.join("");
	} else {
		return null;
	}

	const vsHint = new VSInlayHint(position, labelText);
	if (rawHint.kind === 1) {
		vsHint.kind = VSInlayHintKind.Type;
	} else if (rawHint.kind === 2) {
		vsHint.kind = VSInlayHintKind.Parameter;
	}
	if (typeof rawHint.paddingLeft === "boolean") {
		vsHint.paddingLeft = rawHint.paddingLeft;
	}
	if (typeof rawHint.paddingRight === "boolean") {
		vsHint.paddingRight = rawHint.paddingRight;
	}

	return vsHint;
}
