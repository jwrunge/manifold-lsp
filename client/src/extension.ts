/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
	workspace,
	EventEmitter,
	ExtensionContext,
	window,
	TextDocumentChangeEvent,
	DocumentFilter,
	commands,
	StatusBarAlignment,
	StatusBarItem,
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

let client: LanguageClient;
let statusItem: StatusBarItem | undefined;

export async function activate(ctx: ExtensionContext) {
	const traceOutputChannel = window.createOutputChannel(
		"Manifold Language Server trace"
	);
	const command = process.env.SERVER_PATH || "nrs-language-server";
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
		traceOutputChannel,
	};

	client = new LanguageClient(
		"manifold-language-server",
		"Manifold Language Server",
		serverOptions,
		clientOptions
	);

	statusItem = window.createStatusBarItem(StatusBarAlignment.Left, 1);
	statusItem.name = "Manifold Language Server";
	statusItem.text = "$(sync~spin) Manifold LSP";
	statusItem.tooltip = "Manifold language server status";
	statusItem.command = "manifoldLanguageServer.showStateTree";
	statusItem.show();

	const restartCommand = commands.registerCommand(
		"manifoldLanguageServer.restart",
		async () => {
			if (!client) {
				return;
			}
			statusItem!.text = "$(sync~spin) Manifold LSP";
			await client.stop();
			await client.start();
		}
	);

	const showStateTreeCommand = commands.registerCommand(
		"manifoldLanguageServer.showStateTree",
		async () => {
			if (!client) {
				return;
			}
			const editor = window.activeTextEditor;
			if (!editor) {
				window.showInformationMessage(
					"Open a Manifold template to inspect its state tree."
				);
				return;
			}

			try {
				const response = await client.sendRequest(
					ExecuteCommandRequest.type,
					{
						command: "manifold.showState",
						arguments: [editor.document.uri.toString()],
					}
				);

				if (!response || !response.states) {
					window.showWarningMessage(
						"No Manifold state information is available for this document."
					);
					return;
				}

				traceOutputChannel.clear();
				traceOutputChannel.appendLine("Manifold state tree:\n");
				const states = response.states as Record<
					string,
					Record<string, string>
				>;
				for (const [stateName, props] of Object.entries(states)) {
					traceOutputChannel.appendLine(`• ${stateName}`);
					for (const [prop, ty] of Object.entries(props ?? {})) {
						traceOutputChannel.appendLine(`   ↳ ${prop}: ${ty}`);
					}
					traceOutputChannel.appendLine("");
				}
				traceOutputChannel.show(true);
			} catch (error) {
				window.showErrorMessage(
					`Unable to fetch Manifold state tree: ${String(error)}`
				);
			}
		}
	);

	client.onDidChangeState((event) => {
		switch (event.newState) {
			case State.Starting:
				statusItem!.text = "$(sync~spin) Manifold LSP";
				break;
			case State.Running:
				statusItem!.text = "$(zap) Manifold LSP";
				break;
			case State.Stopped:
				statusItem!.text = "$(error) Manifold LSP";
				break;
		}
	});

	await client.start();
	statusItem!.text = "$(zap) Manifold LSP";

	ctx.subscriptions.push(restartCommand);
	ctx.subscriptions.push(showStateTreeCommand);
	ctx.subscriptions.push(statusItem!);
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

export function activateInlayHints(ctx: ExtensionContext) {
	const maybeUpdater = {
		hintsProvider: null as Disposable | null,
		updateHintsEventEmitter: new EventEmitter<void>(),

		async onConfigChange() {
			this.dispose();
		},

		onDidChangeTextDocument({
			contentChanges,
			document,
		}: TextDocumentChangeEvent) {
			// debugger
			// this.updateHintsEventEmitter.fire();
		},

		dispose() {
			this.hintsProvider?.dispose();
			this.hintsProvider = null;
			this.updateHintsEventEmitter.dispose();
		},
	};

	workspace.onDidChangeConfiguration(
		maybeUpdater.onConfigChange,
		maybeUpdater,
		ctx.subscriptions
	);
	workspace.onDidChangeTextDocument(
		maybeUpdater.onDidChangeTextDocument,
		maybeUpdater,
		ctx.subscriptions
	);

	maybeUpdater.onConfigChange().catch(console.error);
}
