# CLI Reference

## Synopsis

```
tart [COMMAND] [OPTION]... [ARG]...
```

## Description

Tart is a type checker for Emacs Lisp. It reads `.el` source files and `.tart`
signature files, infers types, and reports diagnostics to stderr. The default
subcommand is `check`, so `tart file.el` is equivalent to `tart check file.el`.

## Commands

### check (default)

Type-check one or more Elisp files.

```
tart [check] [OPTION]... [FILE]...
```

Files are processed in order; definitions from earlier files are visible to
later files. `.tart` signature files may also be passed directly. Diagnostics
print to stderr in compiler-style format (`file:line:col: level: message`).

Clean results (no errors) are cached. Files with errors are always re-checked.
See [Content Cache](#content-cache) for details.

**Options:**

| Option              | Description                                   |
| ------------------- | --------------------------------------------- |
| `--emacs-version`   | Use typings for the given Emacs version       |
| `--format`          | Output format: `human` (default) or `json`    |
| `--ignore-hints`    | Suppress hint-level diagnostics               |
| `--ignore-warnings` | Suppress warning-level diagnostics            |
| `--no-cache`        | Skip cache; always run full type-checking     |
| `--warn-as-error`   | Treat warnings as errors (exit 1 on warnings) |

### eval

Parse, evaluate, and type-infer a single expression.

```
tart eval [OPTION]... EXPR
```

`EXPR` is a required positional argument containing an s-expression. Output is
printed to stdout in the format `VALUE :: TYPE`. Errors print to stderr with
exit code 1.

### expand

Macro-expand a file and pretty-print the result.

```
tart expand [OPTION]... FILE
```

`FILE` is required. The expanded output prints to stdout as readable Elisp.

**Options:**

| Option   | Description                                         |
| -------- | --------------------------------------------------- |
| `--load` | Load macros from FILE before expanding (repeatable) |

### repl

Start an interactive REPL.

```
tart repl [OPTION]...
```

Each input is parsed, evaluated, and printed with its inferred type. Definitions
persist across inputs within a session.

**REPL commands:**

| Command        | Action                           |
| -------------- | -------------------------------- |
| `,quit` / `,q` | Exit the REPL                    |
| `,type EXPR`   | Show the type without evaluating |
| `,expand EXPR` | Show macro expansion             |
| `,env`         | List current bindings            |
| `,help`        | Show available commands          |

Ctrl-D also exits.

### lsp

Start an LSP server.

```
tart lsp [OPTION]...
```

By default the server communicates over stdio. Use `--port` to listen on TCP
instead.

**Options:**

| Option   | Description                                    |
| -------- | ---------------------------------------------- |
| `--port` | Listen on TCP port (1--65535) instead of stdio |

### coverage

Measure type signature coverage for a set of Elisp files.

```
tart coverage [OPTION]... [PATH]...
```

`PATH` arguments are directories or files to scan for `.el` files. Defaults to
the current directory when omitted.

**Options:**

| Option         | Description                                    |
| -------------- | ---------------------------------------------- |
| `--exclude`    | Exclude files matching PATTERN (repeatable)    |
| `--fail-under` | Exit 1 if coverage is below N percent (0--100) |
| `--format`     | Output format: `human` (default) or `json`     |

### emacs-coverage

Measure type coverage for the Emacs C layer.

```
tart emacs-coverage [OPTION]...
```

Reports how many C-defined primitives (`DEFUN`/`DEFVAR`) have corresponding
`.tart` signatures.

**Options:**

| Option            | Description                             |
| ----------------- | --------------------------------------- |
| `--emacs-source`  | Path to an Emacs source directory       |
| `--emacs-version` | Use typings for the given Emacs version |

### Internal Commands

The `corpus` and `roundtrip` subcommands are developer tools used for managing
the Emacs source corpus and testing parse-print round-trip consistency. They are
not intended for general use.

## Global Options

The following options are available on all subcommands:

| Option          | Description                                                    |
| --------------- | -------------------------------------------------------------- |
| `--help`        | Show help for the command                                      |
| `--version`     | Print version and exit                                         |
| `--log-format`  | Log output format: `text` (default) or `json`                  |
| `--log-level`   | Log verbosity: `quiet`, `normal` (default), `verbose`, `debug` |
| `--memory`      | Print GC and memory statistics to stderr                       |
| `-v, --verbose` | Shorthand for `--log-level=verbose`                            |

When both `-v` and `--log-level` are given, `--log-level` takes precedence.

All log output goes to stderr. Command results go to stdout.

## Exit Codes

| Code | Meaning                                           |
| ---- | ------------------------------------------------- |
| 0    | Success                                           |
| 1    | Input error (type error, parse error, eval error) |
| 2    | Usage error (bad arguments, invalid options)      |

## Search Path

Tart resolves `.tart` signature files using a fixed search order. The first
match wins, allowing project-local overrides.

1. **Sibling file** -- A `module.tart` file next to the `module.el` being
   checked.
2. **Search path directories** -- Directories configured via the
   `tart-type-path` Emacs variable.
3. **Bundled typings** -- Signatures shipped with tart under the `typings/`
   directory.

Sibling files take highest priority, so you can override any bundled signature
by placing a `.tart` file alongside your `.el` source.

## Emacs Version Resolution

The `--emacs-version` flag selects which set of bundled typings to load from
`typings/emacs/{version}/`. Each version directory contains `c-core/` and
`lisp-core/` subdirectories with `.tart` files for Emacs primitives and core
Lisp libraries.

When `--emacs-version` is not given, tart runs `emacs --version` and parses the
output. If Emacs is not on `PATH`, tart falls back to the latest bundled version
with a warning.

The resolved version feeds a fallback chain:

```
31.0.50 -> 31.0 -> 31 -> latest
```

The first directory that exists wins. For example, if you specify
`--emacs-version=31.0.50` and no `31.0.50/` directory exists, tart uses `31.0/`
instead.

## Content Cache

Type-checking results are cached at `$XDG_CACHE_HOME/tart/` (falling back to
`~/.cache/tart/`). Only clean results (no errors) are cached; files with errors
are always re-checked.

The cache key is a SHA-256 hash of the tart binary, the input file, and all
transitive `.tart` dependencies. Changing any signature file invalidates cached
results for files that depend on it.

Cache entries expire after 30 days. Use `--no-cache` on the `check` subcommand
to bypass the cache entirely.

## Examples

Type-check a single file:

```sh
tart example.el
```

Type-check with a specific Emacs version:

```sh
tart check --emacs-version 31.0 example.el
```

Get JSON-formatted diagnostics:

```sh
tart check --format=json src/*.el
```

Treat warnings as errors in CI:

```sh
tart check --warn-as-error src/*.el
```

Evaluate an expression:

```sh
tart eval '(+ 1 2)'
```

Macro-expand a file with loaded macros:

```sh
tart expand --load macros.el target.el
```

Start the REPL:

```sh
tart repl
```

Start the LSP server on TCP port 9257:

```sh
tart lsp --port 9257
```

Measure coverage, failing if below 50%:

```sh
tart coverage --fail-under=50 src/
```

Measure C-layer coverage for Emacs 31.0:

```sh
tart emacs-coverage --emacs-version=31.0
```

## See Also

- [Getting Started][getting-started] -- Tutorial for new users
- [Writing Typings][writing-typings] -- Writing `.tart` files and inline
  annotations
- [Tooling Setup][tooling-setup] -- Editor configuration
- [.tart Format Reference][tart-format] -- Grammar and syntax reference
- [Type System Reference][type-system] -- Full type system documentation

[getting-started]: ./getting-started.md
[writing-typings]: ./reference/writing-typings.md
[tooling-setup]: ./tooling-setup.md
[tart-format]: ./reference/tart-format.md
[type-system]: ./reference/type-system.md
