# Spec 98 — Package Coverage Output Format

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md), [Spec 96 — Coverage Table Format](./96-coverage-table-format.md)

## Overview

`tart coverage` always operates on a narrow project scope, so it uses drill-down format by default: a per-file coverage table followed by untyped identifier locations.

## Requirements

### R1: Table + drill-down output

`tart coverage` outputs:

1. Per-file table (same columns as `tart emacs-coverage`: FILENAME, PRIVATE, PUBLIC, COVERAGE)
2. Blank line
3. Each untyped public identifier as `file:line:col: identifier`

### R2: Shared formatting flags

`--color`, `--format`, `--sort`, `--reverse`, `--min-percentage`, `--max-percentage` behave identically to `tart emacs-coverage` (see [Spec 96](./96-coverage-table-format.md) and [Spec 97](./97-coverage-listing-options.md)).

### R3: Existing flags preserved

`--fail-under`, `--exclude`, and positional `PATH` arguments continue to work as specified in [Spec 79](./79-coverage-reporting.md).

## Key Files

| File | Role |
|:-----|:-----|
| `lib/cli/coverage.ml` | Package coverage command |
| `bin/main.ml` | CLI dispatch and shared output rendering |
