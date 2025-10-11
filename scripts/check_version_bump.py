#!/usr/bin/env python3
"""Detect whether the Cargo package version increased compared to a previous ref."""
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:  # pragma: no cover
    import tomli as tomllib  # type: ignore


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--before",
        metavar="SHA",
        default="",
        help="Git commit to compare against (typically github.event.before).",
    )
    parser.add_argument(
        "--output",
        metavar="PATH",
        default="",
        help="Optional file path (usually $GITHUB_OUTPUT) to receive bumped=true/false.",
    )
    return parser.parse_args()


def read_version_from_bytes(data: bytes) -> tuple[int, ...]:
    toml = tomllib.loads(data.decode("utf-8"))
    version_str = toml["package"]["version"]
    parts = version_str.split(".")
    numbers: list[int] = []
    for part in parts:
        try:
            numbers.append(int(part))
        except ValueError:
            # Ignore any pre-release / build metadata by stopping parsing here.
            break
    return tuple(numbers)


def read_current_version() -> tuple[int, ...]:
    cargo_path = Path("Cargo.toml")
    data = cargo_path.read_bytes()
    return read_version_from_bytes(data)


def read_previous_version(before: str) -> tuple[int, ...] | None:
    if not before or set(before) == {"0"}:  # GitHub sets "000..0" on branch creation
        return None

    try:
        completed = subprocess.run(
            ["git", "show", f"{before}:Cargo.toml"],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError:
        return None

    return read_version_from_bytes(completed.stdout)


def versions_bumped(current: tuple[int, ...], previous: tuple[int, ...] | None) -> bool:
    if previous is None:
        return True

    # Pad tuples for safe lexicographic comparison
    length = max(len(current), len(previous))
    padded_current = current + (0,) * (length - len(current))
    padded_previous = previous + (0,) * (length - len(previous))
    return padded_current > padded_previous


def write_output(path: str, bumped: bool) -> None:
    if not path:
        return
    out_path = Path(path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("a", encoding="utf-8") as fh:
        fh.write(f"bumped={'true' if bumped else 'false'}\n")


def main() -> int:
    args = parse_args()
    current = read_current_version()
    previous = read_previous_version(args.before)
    bumped = versions_bumped(current, previous)

    write_output(args.output, bumped)

    msg = "Version bump detected." if bumped else "No version bump detected."
    print(msg)
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
