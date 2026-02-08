# Spec 73 — Diagnostics

> Consolidates specs: [13](./.archive/13-error-reporting.md), [35](./.archive/35-structured-errors.md), [37](./.archive/37-file-io-errors.md), [45](./.archive/45-source-excerpts.md), [47](./.archive/47-error-codes.md), [51](./.archive/51-diagnostic-severity.md)

## Overview

The diagnostic system produces clear, actionable error messages in Elm/Rust
style. Each diagnostic carries a structured error type, severity level, and
error code, and renders with source excerpts, underline markers, and
conversational explanations. Three output formats serve different consumers:
`human` for interactive terminal use with ANSI colors and syntax highlighting,
`compact` for grep-friendly single-line output, and `json` for machine
consumption. The unified `Error.t` wrapper composes errors from all subsystems
(type, parse, eval, I/O, CLI) under a single reporting pipeline with summary
counts and configurable severity filtering.

## Message Format

Every diagnostic follows this structure:

```
error[CODE]: one-line summary
  --> file.el:LINE:COL
   |
NN |   (expression here)
   |    ^^^^^^^^^^^^^^^^
   |    expected: EXPECTED_TYPE
   |    found: ACTUAL_TYPE
   |
note: explanation of why
  --> file.el:RELATED_LINE:COL
   |
NN |   (related code)
   |    ^^^^^ this is TYPE because...

help: suggested fix
   |
NN |   (corrected code)
```

### Diagnostic kinds

| Kind | Example |
|------|---------|
| Type mismatch | Expected `String`, found `Int`; suggests conversion |
| Branch mismatch | Branch type incompatible with return type; points to declaration |
| Possible nil | `(Option String)` where `String` expected; suggests `when-let*` or `or` |
| Undefined variable | Levenshtein-based typo suggestions |
| Arity mismatch | Expected vs actual argument count with signature display |
| Signature mismatch | Shows both `.tart` and `.el` locations side by side |

### LSP mapping

Primary span maps to `Diagnostic` with message. Secondary spans map to
`DiagnosticRelatedInformation`. Help text appends to the message or becomes a
`CodeAction`.

## Source Excerpts

Source excerpts render only in `human` format. Compact and JSON formats are
unchanged.

### Headers

Elm-style headers span from the error category to the file location:

```
-- TYPE MISMATCH ---------------------------------------- init.el:42:10
```

### Line rendering

Source lines display in a numbered gutter with caret underlines marking the
span. Gutter width adapts to the largest line number shown. Multi-line spans
underline each line independently.

```
 9 |   (let ((x 1))
10 |     (+ x "foo"))
   |          ^^^^^
```

### Conversational explanations

The prose varies by constraint context:

| Context | Opening |
|---------|---------|
| `FunctionArg` | "The function `f` expects argument N to be:" |
| `IfBranch` | "The branches of this `if` have different types:" |
| `DeclaredReturn` | "The return type doesn't match the declaration:" |

### Provenance

When a type mismatch involves a function from a `.tart` signature file, the
excerpt shows the signature location and the relevant declaration line.

### Related locations

Diagnostics with related spans (e.g. branch mismatches) render excerpts for
each span that has a valid source location. Generated spans without source files
are omitted.

### Graceful degradation

When a source file is unreadable, the excerpt falls back to a location-only
display with `(source not available)` and the expected/found types. No
exceptions propagate from source reading.

### ANSI colors

Colors are TTY-aware: enabled when stdout is a terminal, plain text when piped.

| Element | Color |
|---------|-------|
| `error` | Red bold |
| `warning` | Yellow bold |
| `hint` / help text | Cyan |
| Error code `[E0001]` | Red |
| `-->` location | Blue |
| Line numbers | Blue dim |
| Underline carets | Red |
| Type names | Green |

### Syntax highlighting

Minimal Lisp syntax highlighting in source excerpts:

| Element | Color |
|---------|-------|
| Keywords (`defun`, `let`, `if`, ...) | Magenta |
| Strings | Green |
| Comments | Gray/dim |
| Numbers | Cyan |
| Quoted symbols | Yellow |
| Parentheses | Default |

## Error Codes

Error codes are sequential from E0001, grouped by category. Once assigned, a
code is never reassigned. Every diagnostic includes its code in the output:
`error[E0001]: ...`.

### Type Errors (E0001--E0099)

| Code | Name | Description |
|------|------|-------------|
| E0001 | TypeMismatch | Expected one type, found another |
| E0002 | BranchMismatch | If/cond branches have incompatible types |
| E0003 | InfiniteType | Occurs check failed (recursive type) |
| E0004 | SignatureMismatch | Implementation does not match declared signature |
| E0005 | AnnotationMismatch | Expression does not match tart annotation |
| E0006 | ReturnMismatch | Function body does not match declared return |
| E0007 | UnificationFailed | Types cannot be unified |
| E0008 | DisjointEquality | eq/eql args are provably disjoint |

### Name Errors (E0100--E0199)

| Code | Name | Description |
|------|------|-------------|
| E0100 | UndefinedVariable | Variable not in scope |
| E0101 | UndefinedFunction | Function not in scope |
| E0102 | UndefinedType | Type not in scope |
| E0103 | UndefinedField | Field not present in record type |
| E0104 | MissingSignature | Function defined but not in .tart file |
| E0105 | AmbiguousName | Name resolves to multiple definitions |

### Arity Errors (E0200--E0299)

| Code | Name | Description |
|------|------|-------------|
| E0200 | WrongArity | Wrong number of arguments to function |
| E0201 | WrongTypeArity | Wrong number of type arguments |
| E0202 | MissingRequired | Required argument not provided |
| E0203 | UnknownKeyword | Unknown keyword argument |

### Kind Errors (E0300--E0399)

| Code | Name | Description |
|------|------|-------------|
| E0300 | KindMismatch | Expected one kind, found another |
| E0301 | InfiniteKind | Occurs check failed at kind level |
| E0302 | TypeArityMismatch | Type constructor applied to wrong number of args |

### Pattern Errors (E0400--E0499)

| Code | Name | Description |
|------|------|-------------|
| E0400 | NonExhaustive | Pattern match does not cover all cases |
| E0401 | RedundantPattern | Pattern can never match (already covered) |
| E0402 | InvalidPattern | Pattern syntax not supported |

### Row/Record Errors (E0500--E0599)

| Code | Name | Description |
|------|------|-------------|
| E0500 | MissingField | Required field not present |
| E0501 | DuplicateField | Field specified multiple times |
| E0502 | RowMismatch | Row types cannot be unified |
| E0503 | ClosedRowExtra | Extra field in closed row type |

### Union Errors (E0600--E0699)

| Code | Name | Description |
|------|------|-------------|
| E0600 | UnionMismatch | Value does not match any union variant |
| E0601 | EmptyUnion | Type subtraction produced empty type |
| E0602 | AmbiguousVariant | Cannot determine which union variant |

### Module Errors (E0700--E0799)

| Code | Name | Description |
|------|------|-------------|
| E0700 | MissingModule | Required module not found |
| E0701 | CircularDependency | Modules have circular require |
| E0702 | SignatureNotFound | No .tart signature file found |

### File Errors (E0800--E0899)

| Code | Name | Description |
|------|------|-------------|
| E0800 | FileNotFound | Source file does not exist |
| E0801 | FileUnreadable | Source file cannot be read |
| E0802 | ParseError | Syntax error in source file |

### Version Errors (E0900--E0999)

| Code | Name | Description |
|------|------|-------------|
| E0900 | VersionTooLow | Feature requires newer Emacs version |
| E0901 | VersionTooHigh | Feature removed in declared Emacs version |
| E0902 | VersionParseFailed | Package-Requires parse error |
| E0903 | RedundantGuard | Feature guard redundant given min version |

## Severity

Three active severity levels control output and exit behavior. Info is reserved
for future use.

| Level | LSP value | Exit code | Meaning |
|-------|-----------|-----------|---------|
| Error | 1 | 1 | Must fix |
| Warning | 2 | 0 | Should review |
| Hint | 4 | 0 | Optional improvement |

Default severity derives from the error code prefix: `E` codes are errors, `W`
codes are warnings, `H` codes are hints. A diagnostic's severity can be
overridden at construction time.

### CLI flags

| Flag | Effect |
|------|--------|
| `--warn-as-error` | Promotes warnings to errors for exit code |
| `--ignore-warnings` | Suppresses warning output |
| `--ignore-hints` | Suppresses hint output |

Flags combine: `--warn-as-error --ignore-hints` shows errors and warnings,
exits 1 on warnings.

### Summary

After all diagnostics, a summary line counts by severity:

```
Found 2 errors, 1 warning, 3 hints
```

## Structured Errors

The unified `Error.t` variant type wraps errors from all subsystems:

```ocaml
type t =
  | Type of Diagnostic.t
  | Parse of { message: string; span: Location.span }
  | Eval of { message: string; span: Location.span }
  | Io of { path: string; message: string }
  | Cli of { message: string; hint: string option }
```

### Recoverability

`is_fatal` classifies whether processing should stop:

- Type and parse errors are recoverable (collect all before reporting)
- I/O errors are fatal (cannot proceed without the file)
- CLI errors are fatal (invalid invocation)

### Source location

`location` returns the primary span when available. Type, parse, and eval
errors carry spans; I/O and CLI errors do not.

### Error accumulator

`Error.Acc` collects errors during processing and reports them in order with a
summary count at the end.

### Serialization

`to_string` produces the human-readable compiler-style output. `to_json`
produces structured JSON with kind, code, severity, message, location,
expected/actual types, related locations, and help suggestions.

## File I/O Errors

File errors use a structured `file_error` type:

| Type | Code | Description |
|------|------|-------------|
| `File_not_found` | E0001 | File does not exist; suggests similar filenames (Levenshtein distance <= 2) and missing `.el` extensions |
| `Permission_denied` | E0002 | File cannot be read due to permissions |
| `Is_directory` | E0003 | Path is a directory, not a file |
| `Signature_not_found` | E0004 | No `.tart` signature file found; lists searched paths |
| `Read_error` | E0005 | I/O error during file reading |

Filename suggestions use Levenshtein matching against sibling files in the same
directory. When a path has no extension and the same name with `.el` exists,
that is suggested.

## Output Formats

| Format | Flag | Use case | Description |
|--------|------|----------|-------------|
| `human` | `--format=human` (default) | Interactive terminal | Elm-style headers, source excerpts, colors, syntax highlighting |
| `compact` | `--format=compact` | IDE integration, grep | Single-line: `file:line:col: severity[CODE]: message [details]` |
| `json` | `--format=json` | Machine consumption | One JSON object per diagnostic with all structured fields |

## Key Files

| File | Role |
|------|------|
| `lib/core/error.ml` | Unified `Error.t` type, accumulator, reporting |
| `lib/core/error.mli` | Error interface |
| `lib/typing/diagnostic.ml` | Type error diagnostics with severity and JSON |
| `lib/typing/source_excerpt.ml` | Source line extraction and underline rendering |
| `lib/typing/source_excerpt.mli` | Source excerpt interface |
| `lib/util/ansi.ml` | ANSI color codes and TTY detection |
| `lib/util/ansi.mli` | ANSI interface |
| `lib/errors/file_error.ml` | Structured file error types and formatting |
| `lib/errors/file_error.mli` | File error interface |

## Deferred

- **Error codes**: all codes listed above are defined in the `diagnostic.ml`
  enum. Codes for implemented features (E0001--E0008, E0100--E0104, E0200--E0201,
  E0300--E0302, E0400, E0800--E0802, E0900--E0903) are actively emitted.
  Remaining codes are assigned incrementally as the features that emit them are
  implemented.
- **Info severity**: defined in the type and mapped to LSP Information severity.
  No features currently emit Info diagnostics.
