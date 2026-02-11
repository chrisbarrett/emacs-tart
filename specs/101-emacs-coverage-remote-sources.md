# Spec 101 — Emacs Coverage Remote Sources

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md), [Spec 94 — Emacs Coverage Auto Version Detection](./94-emacs-coverage-auto-version.md)

## Overview

Wire version resolution ([Spec 99](./99-emacs-version-resolution.md)) and source acquisition ([Spec 100](./100-emacs-source-acquisition.md)) into `tart emacs-coverage`. When `--emacs-version` differs from the local Emacs (or none is installed), sources are fetched automatically.

## Requirements

### R1: Automatic source fetching

When `--emacs-version V` differs from the locally-detected version (or no Emacs is on PATH), resolve and acquire sources per specs 99–100, then use the cached tree for coverage analysis.

### R2: `--emacs-source` overrides fetching

`--emacs-source PATH` takes precedence — no remote fetch occurs, even if `--emacs-version` is also provided.

### R3: Local version match skips fetch

When `--emacs-version V` matches the locally-detected version (after the fallback chain from [Spec 94](./94-emacs-coverage-auto-version.md)), use the local source directory.

### R4: No Emacs and no `--emacs-version`

Exit with code 1 and an error explaining that either Emacs must be on PATH or `--emacs-version` must be provided.

### R5: Verbose output

With `--verbose`, stderr shows: resolved version, cache status (cached vs fresh download), and source tree path.

### R6: Typings version selection

The resolved version selects both the source tree and the typings directory (`typings/emacs/{version}/`), applying the fallback chain from [Spec 94](./94-emacs-coverage-auto-version.md).

## Key Files

| File | Role |
|:-----|:-----|
| `bin/main.ml` | CLI dispatch, version/source orchestration |
| `lib/coverage/emacs_source.ml` | Source discovery and version detection |
| `lib/coverage/emacs_source.mli` | Emacs source interface |
| `lib/coverage/emacs_coverage.ml` | Coverage calculation using resolved source tree |
