# Spec 100 — Emacs Source Acquisition

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md), [Spec 72 — CLI](./72-cli.md)

## Overview

After version resolution ([Spec 99](./99-emacs-version-resolution.md)), `tart` needs the corresponding Emacs source tree (`src/*.c`, `lisp/*.el`) for coverage analysis. Sources are cached under `$XDG_CACHE_HOME/tart/emacs-sources/` per [Spec 72](./72-cli.md).

## Requirements

### R1: Tarball for releases

Download the release tarball from GNU mirrors for resolved release tags. Fall back to a shallow git clone if no tarball exists for that version.

### R2: Shallow clone for development versions

`dev`/`devel`/`git` performs a shallow clone (depth 1) of the Emacs git repository.

### R3: Git fetch for arbitrary SHAs

Fetch the specific commit from the Emacs git repository.

### R4: Cache reuse

Release versions are immutable once cached — no re-download on subsequent runs. Development versions run `git fetch` every invocation (no-op if HEAD unchanged).

### R5: Cache layout

Source trees live at `$XDG_CACHE_HOME/tart/emacs-sources/{version-or-sha}/` (e.g., `29.4/`, `dev/`, `abc1234/`).

### R6: Atomic writes

Acquire to a temporary directory, then rename to the final path. Interrupted acquisitions leave no partial state.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/coverage/emacs_source.ml` | Source acquisition and cache management |
| `lib/coverage/emacs_source.mli` | Source acquisition interface |
| `lib/cache/content_cache.ml` | XDG cache directory utilities |
