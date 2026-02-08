# Spec 72 — CLI

> Consolidates specs: [09](./.archive/09-cli-interface.md), [36](./.archive/36-cmdliner-cli.md), [40](./.archive/40-content-cache.md), [44](./.archive/44-timing-stats.md), [53](./.archive/53-structured-logging.md)

## Overview

The tart CLI uses Cmdliner for declarative argument parsing with subcommands for type-checking, evaluation, macro expansion, REPL, LSP, and coverage. It features content-addressable caching, timing/memory instrumentation, and structured logging. Cmdliner provides auto-generated help, typo suggestions for options and subcommands, and argument validation with structured error messages.

## Subcommands

### check (default)

`tart [check] [--emacs-version VER] [--format=human|compact|json] [--no-cache] FILE...`

Type-checks one or more `.el` or `.tart` files. Files are processed in order; definitions from earlier files are visible to later files. `.tart` signature files provide types without requiring implementations. Diagnostics print to stderr in compiler-style format (`file:line:col: error: message`).

`check` is the default subcommand: `tart file.el` is equivalent to `tart check file.el`, implemented via Cmdliner's `Cmd.group ~default`.

Clean files (no errors) are cached; files with errors are always re-checked (see [Content Cache](#content-cache)). The `--no-cache` flag disables caching entirely.

### eval

`tart eval EXPR`

Parses, evaluates, and type-infers the given expression. Output format: `VALUE :: TYPE`. Errors print to stderr with exit code 1.

### expand

`tart expand [--load FILE]... FILE`

Macro-expands the given file and pretty-prints the result to stdout as readable Elisp. `--load` is repeatable and makes macros from the specified files available during expansion.

### repl

`tart repl`

Starts an interactive REPL with prompt `tart> `. Each input is parsed, evaluated, and the result printed with its type. Definitions persist across inputs within the session. Special commands:

| Command | Action |
|---------|--------|
| `,quit` / `,q` | Exit REPL |
| `,type EXPR` | Show type without evaluating |
| `,expand EXPR` | Show macro expansion |
| `,env` | List current bindings |
| `,help` | Show available commands |

Ctrl-D also exits cleanly.

### lsp

`tart lsp [--port PORT] [--log-level LEVEL]`

Starts an LSP server on stdio (default) or TCP when `--port` is given. Handles `initialize`, `textDocument/didOpen`, and other LSP protocol messages. The `--port` value is validated as an integer in range 1--65535.

### coverage

`tart coverage [--format=human|json] [--verbose] [--fail-under=N] [--exclude=PATTERN] [PATH...]`

Measures type signature coverage. `--fail-under` accepts values 0--100; out-of-range values produce an error listing the valid range. `--format` accepts `human` or `json`; invalid values produce an error listing valid choices.

### emacs-coverage

`tart emacs-coverage [--emacs-source PATH] [--emacs-version VER] [--verbose]`

Measures C layer type coverage for a given Emacs version.

## Argument Parsing

Cmdliner provides the declarative argument framework. Each subcommand is a `Cmd.t` with typed argument specs, grouped under the main command:

```ocaml
Cmd.group ~default:check_term info
  [check_cmd; eval_cmd; expand_cmd; repl_cmd;
   lsp_cmd; coverage_cmd; emacs_coverage_cmd]
```

### Global flags

The following flags are available on every subcommand:

| Flag | Effect |
|------|--------|
| `--version` | Print version string and exit |
| `--help` | Auto-generated help (per-subcommand) |
| `-v` / `--verbose` | Shorthand for `--log-level=verbose`; on `check`/`eval`/`expand` also enables timing output |
| `--memory` | Show GC stats (check, eval, expand) |
| `--log-level=LEVEL` | Set log verbosity: `quiet`, `normal` (default), `verbose`, `debug` |
| `--log-format=FORMAT` | Log output format: `text` (default) or `json` |
| `--no-cache` | Disable content cache |

When both `-v` and `--log-level` are given, `--log-level` takes precedence.

### Exit codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Input error (type error, parse error, eval error) |
| 2 | Usage error (bad arguments, invalid options) |

### Error quality

Cmdliner automatically provides:

- Typo suggestions for misspelled options (e.g., `--prot` suggests `--port`)
- Typo suggestions for misspelled subcommands (e.g., `chekc` suggests `check`)
- Missing-argument messages with usage hints
- Enum validation listing valid values on mismatch
- Range validation with clear error messages

Validation errors use the structured error system from [Spec 73](./73-diagnostics.md).

## Content Cache

### Location

XDG-compliant: `$XDG_CACHE_HOME/tart/` with fallback to `~/.cache/tart/`. Directories are created on demand when storing.

### Key computation

Cache key = `sha256(tart_binary_content + input_file_content + transitive_dependency_contents)`. Transitive dependencies include `.tart` signature files (c-core, lisp-core, prelude, sibling files) resolved via the dependency graph. Changing any `.tart` typing file invalidates cached results for all `.el` files that depend on it.

### Storage

Entries are stored under `v1/XX/XXXX...XX.json` where `XX` is a two-character prefix directory to prevent too many files per directory. The JSON format is human-readable:

```json
{
  "version": 1,
  "created_at": "2024-01-15T10:30:00Z",
  "data": "..."
}
```

Writes are atomic (temp file + rename) to handle concurrent access.

### Retrieval

`retrieve` returns `Some data` on hit, `None` on miss. Missing entries, corrupted files, and I/O errors all produce `None`---never an error.

### Pass-only caching

Only clean results (no errors) are cached. Files with type errors are always re-checked, ensuring that fixes are picked up immediately.

### Eviction

Age-based eviction with a 30-day default threshold. Runs at most once per session, gated by a marker file (`$CACHE_DIR/.last-eviction`) with mtime check (skipped if eviction ran within 1 hour). Eviction is best-effort: permission and I/O errors are logged as warnings without failing the operation.

### Cache versioning

The `v1/` subdirectory scopes the format version. Future format changes use `v2/` etc., coexisting with old entries.

## Timing & Memory Statistics

### Timing (`-v` / `--verbose`)

When enabled on `check`, `eval`, or `expand`, per-phase timing prints to stderr:

- **check**: Parsing (per file), Macro expansion, Type inference, Total
- **eval**: Parsing, Evaluation, Type inference, Total
- **expand**: Parsing (per file), Expansion, Total

With multiple files, per-file timing is shown. Units are human-readable (`ms` or `s`).

### Memory (`--memory`)

When enabled, GC statistics print to stderr:

- **Per-phase**: allocation delta and GC count
- **Summary**: heap size, minor/major GC counts, total allocations

Units are human-readable (`KB` or `MB`).

### Combined output (`-v --memory`)

Produces coherent combined output with time and memory per phase:

```
[verbose] Parsing large.el... 12ms (1.2MB alloc, 0 GC)
[verbose] Macro expansion... 5ms (0.3MB alloc, 0 GC)
[verbose] Type inference... 234ms (45MB alloc, 3 minor GC)
[verbose] Total: 251ms

[memory] Heap: 12MB, Minor GC: 3, Major GC: 0, Total alloc: 46.5MB
```

Stats always go to stderr; normal command output goes to stdout.

## Logging

### Module

A single `Log` module replaces the former `Verbose_log` module and LSP server ad-hoc logging. Mutable global state is set once at CLI startup before any work begins.

### Levels

| Level | Includes | Use case |
|-------|----------|----------|
| `quiet` | Nothing | Suppress all output |
| `normal` | Info, errors | Default; operational messages |
| `verbose` | + detailed operations | Signature loading, path resolution, timing |
| `debug` | + trace-level internals | Unification steps, env lookups |

Higher levels include all output from lower levels.

### Zero-cost gating

Disabled log calls do not allocate or format strings (`Printf.ifprintf` or equivalent). This is critical for `debug`-level calls in hot paths.

### Output formats

**Plain text** (default): one message per line, prefixed with level in brackets.

```
[info] Loading typings from /path/to/typings/emacs/31.0
[verbose] c-core/data.tart: 80 signatures
```

**JSON lines** (`--log-format=json`): one JSON object per line with optional `ctx` field.

```json
{"level":"info","msg":"Loading typings from /path/to/typings/emacs/31.0"}
{"level":"debug","msg":"resolve: car -> (cons a b) -> a","ctx":{"env_depth":3}}
```

### Stderr/stdout separation

All log output goes to stderr. Command results (diagnostics, types, reports, expanded code) go to stdout. `--log-level=quiet` suppresses all stderr output.

## Key Files

| File | Role |
|------|------|
| `bin/main.ml` | Cmdliner subcommand dispatch, flag wiring, instrumentation |
| `bin/main.mli` | CLI interface |
| `lib/cache/content_cache.ml` | Content-addressable cache implementation |
| `lib/cache/content_cache.mli` | Cache interface |
| `lib/timing/timing.ml` | Timing measurement |
| `lib/timing/timing.mli` | Timing interface |
| `lib/memory_stats/memory_stats.ml` | GC stats tracking |
| `lib/memory_stats/memory_stats.mli` | Memory stats interface |
| `lib/log/log.ml` | Structured logging |
| `lib/log/log.mli` | Log interface |

## Deferred

- **Structured logging R9** (debug-level unification tracing): infrastructure is ready, but call sites in the unification/constraint-solving hot loop are not yet instrumented. These are added incrementally as specific debugging needs arise.
