# Spec 94 — Emacs Coverage Auto Version Detection

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md)

## Overview

`tart emacs-coverage` already detects the Emacs version from PATH but requires `--emacs-version` to select typings. Use the detected version automatically.

## Requirements

### R1: Auto-select typings from detected version

When `tart emacs-coverage` is run without `--emacs-version`, select typings using the existing fallback chain for the detected version (e.g., `31.0.50 → 31.0 → 31 → latest`).

### R2: Explicit version overrides

`--emacs-version V2` selects typings for `V2` regardless of the detected version.

### R3: Version logged in verbose mode

When `--verbose` is active, stderr shows `Detected Emacs version: V` and the fallback chain used.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/coverage/emacs_source.ml` | Version detection from Emacs binary |
| `bin/main.ml` | CLI dispatch and typings version selection |
