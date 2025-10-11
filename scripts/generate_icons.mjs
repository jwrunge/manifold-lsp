#!/usr/bin/env node
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import sharp from "sharp";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const repoRoot = resolve(__dirname, "..");
const sourceSvg = resolve(repoRoot, "assets", "manifold_noglow.svg");
const vscodeIcon = resolve(repoRoot, "client", "media", "icon.png");
const jetbrainsIcon = resolve(
	repoRoot,
	"jetbrains",
	"src",
	"main",
	"resources",
	"META-INF",
	"pluginIcon.svg"
);
const jetbrainsDarkIcon = resolve(
	repoRoot,
	"jetbrains",
	"src",
	"main",
	"resources",
	"META-INF",
	"pluginIcon_dark.svg"
);

/**
 * @param {string} pathname
 */
async function ensureDir(pathname) {
	await mkdir(dirname(pathname), { recursive: true });
}

async function generateVsCodeIcon() {
	await ensureDir(vscodeIcon);
	const buffer = await sharp(sourceSvg)
		.resize(256, 256, {
			fit: "contain",
			background: { r: 0, g: 0, b: 0, alpha: 0 },
		})
		.png()
		.toBuffer();
	await writeFile(vscodeIcon, buffer);
}

async function updateJetBrainsIcon() {
	await ensureDir(jetbrainsIcon);
	const svg = await readFile(sourceSvg, "utf-8");
	await writeFile(jetbrainsIcon, svg, "utf-8");
	await writeFile(jetbrainsDarkIcon, svg, "utf-8");
}

async function main() {
	await Promise.all([generateVsCodeIcon(), updateJetBrainsIcon()]);
	console.log("Icons generated from assets/manifold_noglow.svg");
}

main().catch((err) => {
	console.error(err);
	process.exitCode = 1;
});
