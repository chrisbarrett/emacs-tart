# Spec 96 — Coverage Table Format

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md)

## Overview

Replace the flat summary from `tart emacs-coverage` with a per-file table and color coding.

## Requirements

### R1: Per-file table columns

Default output is a table with columns:

| Column | Content |
|:-------|:--------|
| FILENAME | Source file basename |
| PRIVATE | Count of `--` identifiers |
| PUBLIC | `covered/total` (e.g., `30/45`) |
| COVERAGE | Percentage (e.g., `66.7%`) |

Table is the default format (no `--format` flag needed).

### R2: Default sort order

`.c` files first, then `.el` files; alphabetical within each group.

### R3: Color thresholds

PUBLIC and COVERAGE values are colored on TTY (or `--color=always`):

- **Green**: ≥ 95%
- **Yellow**: ≥ 50% and < 95%
- **Red**: < 50%

### R4: `--color` flag

- `auto` (default): colors if stdout is a TTY
- `always`: colors always
- `off`: no colors

### R5: JSON output

`--format=json` produces:

```json
{
  "emacs_version": "31.0",
  "files": [
    {
      "filename": "alloc.c",
      "private": 12,
      "public_covered": 30,
      "public_total": 45,
      "coverage_pct": 66.7,
      "uncovered": ["identifier1", "identifier2"]
    }
  ],
  "totals": {
    "private": 100,
    "public_covered": 500,
    "public_total": 800,
    "coverage_pct": 62.5
  }
}
```

Colors are never emitted in JSON mode.

## Key Files

| File | Role |
|:-----|:-----|
| `bin/main.ml` | CLI flags and output rendering |
| `lib/coverage/emacs_coverage.ml` | Coverage data structures |
