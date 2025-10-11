#!/usr/bin/env python3
"""Synchronize extension versions with the Cargo package version.

Reads the version from `Cargo.toml` and applies it to the VS Code extension's
`package.json` and the JetBrains plugin's `gradle.properties` so that all
artifacts share a single version number.
"""
from __future__ import annotations

import json
import os
from pathlib import Path
import sys

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:  # pragma: no cover - fallback for older interpreters
    import tomli as tomllib  # type: ignore


def read_cargo_version(root: Path) -> str:
    cargo_toml = root / "Cargo.toml"
    with cargo_toml.open("rb") as fh:
        data = tomllib.load(fh)
    try:
        return data["package"]["version"]
    except KeyError as exc:  # pragma: no cover - defensive
        raise KeyError("Unable to locate package.version in Cargo.toml") from exc


def update_client_package(root: Path, version: str) -> bool:
    package_json = root / "client" / "package.json"
    with package_json.open("r", encoding="utf-8") as fh:
        package_data = json.load(fh)

    if package_data.get("version") == version:
        return False

    package_data["version"] = version
    with package_json.open("w", encoding="utf-8") as fh:
        json.dump(package_data, fh, indent=4)
        fh.write("\n")
    return True


def update_jetbrains_properties(root: Path, version: str) -> bool:
    properties_path = root / "jetbrains" / "gradle.properties"
    if not properties_path.exists():
        return False

    changed = False
    lines: list[str] = []
    with properties_path.open("r", encoding="utf-8") as fh:
        for line in fh:
            line = line.rstrip("\n")
            if line.startswith("pluginVersion="):
                if line != f"pluginVersion={version}":
                    line = f"pluginVersion={version}"
                    changed = True
            lines.append(line)

    if changed:
        with properties_path.open("w", encoding="utf-8") as fh:
            fh.write("\n".join(lines) + "\n")
    return changed


def main() -> int:
    root = Path(__file__).resolve().parents[1]
    version = read_cargo_version(root)

    changed_any = False
    changed_any |= update_client_package(root, version)
    changed_any |= update_jetbrains_properties(root, version)

    if changed_any:
        print(f"Synchronized version strings to {version}.")
    else:
        print(f"Version strings already up-to-date ({version}).")

    return 0


if __name__ == "__main__":
    sys.exit(main())
