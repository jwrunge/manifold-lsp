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
} from "vscode";

import {
	Disposable,
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate(context: ExtensionContext): Promise<void> {
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
	await client.start();
	context.subscriptions.push({
		dispose: () => {
			void client.stop();
		},
	});
	activateInlayHints(context, client, selectors);
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
