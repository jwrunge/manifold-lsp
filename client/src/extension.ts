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
} from "vscode";

import {
	Disposable,
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate() {
	const traceOutputChannel = window.createOutputChannel(
		"Manifold Language Server trace"
	);
	const command = process.env.SERVER_PATH || "nrs-language-server";
	const run: Executable = {
		command,
		options: {
			env: {
				...process.env,
				// eslint-disable-next-line @typescript-eslint/naming-convention
				RUST_LOG: "debug",
			},
		},
	};
	const serverOptions: ServerOptions = {
		run,
		debug: run,
	};

	const selectors = getDocumentSelectors();
	let clientOptions: LanguageClientOptions = {
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
	client.start();
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
		];
	}

	return normalized;
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
