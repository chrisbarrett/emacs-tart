# Spec 97 — Coverage Listing Options and Drill-Down

> Extends: [Spec 96 — Coverage Table Format](./96-coverage-table-format.md)

## Overview

Sorting, filtering, and drill-down for the per-file coverage table to help prioritize typings work.

## Requirements

### R1: `--sort` flag

`--sort=VALUE` sorts by the specified column:

| Value | Behaviour |
|:------|:----------|
| `default` | `.c` first, then `.el`; alphabetical within groups |
| `name` | Strictly alphabetical by filename |
| `coverage` | By coverage percentage |
| `public` | By public covered count |
| `private` | By private identifier count |

Bare `--sort` (no value) implies `--sort=coverage`.

### R2: `--reverse` flag

Reverses the sort order.

### R3: Percentage range filtering

`--min-percentage=N` and `--max-percentage=M` filter to files with coverage `>= N` and `<= M`.

### R4: Positional argument interpretation

Each positional argument is interpreted as:

- **Glob** if it contains `*` or `?` — e.g., `buf*` matches `buffer.c`, `buffer.el`
- **Filename** if it contains `.el` or `.c` — e.g., `alloc.c` matches exactly
- **Feature name** otherwise — matches any source file with that basename (e.g., `buffer` → `buffer.c` and `buffer.el`)

Multiple positional arguments combine as AND filters.

### R5: Drill-down output

When positional arguments are provided, output is:

1. Filtered coverage table ([Spec 96](./96-coverage-table-format.md) format)
2. Blank line
3. One line per untyped public identifier: `file:line:col: identifier`

Sorted by file, then line number.

### R6: Drill-down in JSON mode

With positional arguments and `--format=json`, JSON includes location details:

```json
{
  "files": [...],
  "uncovered_details": [
    {"file": "buffer.c", "line": 123, "col": 0, "identifier": "foo-bar"}
  ]
}
```

## Key Files

| File | Role |
|:-----|:-----|
| `bin/main.ml` | CLI argument parsing and output |
| `lib/coverage/emacs_coverage.ml` | Coverage calculation |
| `lib/coverage/c_scanner.ml` | C source locations |
| `lib/coverage/definition_extractor.ml` | Elisp source locations |
