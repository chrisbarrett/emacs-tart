# Spec 35: Structured Errors

Unified error type system across the codebase with rich context and machine-readable output.

**Dependencies:** Spec 13 (error-reporting)

## Goal

When users or agents make mistakes with the tart CLI or encounter file errors, they immediately understand what went wrong and how to fix it - with structured errors that can also be serialized for automation.

## Constraints

| Constraint | Detail |
|------------|--------|
| Rich context | Errors carry source locations, related values, suggestions |
| Composable | Errors from different subsystems combine uniformly |
| Recoverable vs fatal | Clear distinction between "collect more" vs "stop now" |
| Machine-readable | JSON serialization for tooling integration |

## Background

### Current State

The codebase has multiple error representations:
- `Diagnostic.t` - rich type errors with spans, expected/actual, related locations
- `Unify.error` - type unification failures
- `Read.parse_error` - parser errors with span
- `Eval.eval_error` - runtime errors with span
- Ad-hoc `prerr_endline` in CLI code

### Unified Wrapper Approach

Rather than replacing `Diagnostic.t`, we create a unified `Error.t` wrapper that:
1. Wraps existing diagnostic types
2. Adds CLI/IO error variants
3. Provides uniform serialization
4. Tracks recoverability

## Output

```
lib/
├── core/
│   ├── error.ml          ; Unified error type
│   └── error.mli
└── typing/
    └── diagnostic.ml     ; (extended with to_json)
bin/
└── main.ml               ; (uses Error.t for CLI errors)
```

## Requirements

### R1: Unified error type

**Given** errors from different subsystems
**When** collected for reporting
**Then** all wrap into a single `Error.t` variant type

```ocaml
type t =
  | Type of Diagnostic.t
  | Parse of { message: string; span: Location.span }
  | Eval of { message: string; span: Location.span }
  | Io of { path: string; message: string }
  | Cli of { message: string; hint: string option }
```

**Verify:** `dune build`; each error kind representable as `Error.t`

### R2: Recoverability classification

**Given** an error
**When** deciding whether to continue processing
**Then** `is_fatal` indicates if processing should stop

```ocaml
val is_fatal : t -> bool
(* Io errors are fatal; Type/Parse errors are recoverable *)
```

- Type errors: recoverable (collect all before reporting)
- Parse errors: recoverable (continue parsing next form)
- IO errors (file not found): fatal (cannot proceed)
- CLI errors (bad arguments): fatal (invalid invocation)

**Verify:** `dune test`; type errors allow collecting multiple; IO errors stop immediately

### R3: Source location access

**Given** any error
**When** needing the location for display
**Then** `location` returns the primary span if available

```ocaml
val location : t -> Location.span option
(* Type/Parse/Eval have locations; Io/Cli do not *)
```

**Verify:** `dune test`; type errors return span; CLI errors return None

### R4: Human-readable formatting

**Given** an error
**When** formatting for terminal output
**Then** `to_string` produces compiler-style output

```
error[E0308]: type mismatch
  --> init.el:42:10
   |
   = expected: String
   = found: Int
   |
help: convert the integer to a string: (number-to-string ...)
```

For CLI errors:
```
error: no input files
hint: use --help for usage
```

**Verify:** `dune test`; output matches format per error kind

### R5: JSON serialization

**Given** an error
**When** `--format=json` is requested
**Then** `to_json` produces structured JSON

```json
{
  "kind": "type",
  "code": "E0308",
  "severity": "error",
  "message": "type mismatch",
  "location": {
    "file": "init.el",
    "line": 42,
    "column": 10
  },
  "expected": "String",
  "actual": "Int",
  "related": [
    {
      "location": { "file": "init.el", "line": 5, "column": 1 },
      "message": "expected type from function signature"
    }
  ],
  "help": ["convert the integer to a string: (number-to-string ...)"]
}
```

**Verify:** `./tart check --format=json bad.el | jq .`; valid JSON with all fields

### R6: Error list with summary

**Given** multiple errors collected during type-checking
**When** reporting results
**Then** `Error.report` prints all errors plus summary

```
error[E0308]: type mismatch
  --> init.el:42:10
  ...

error[E0425]: variable `strng` is not defined
  --> init.el:50:5
  ...

Found 2 errors
```

**Verify:** `./tart check file-with-errors.el`; shows count at end

### R7: Diagnostic.t JSON extension

**Given** existing `Diagnostic.t` type
**When** adding JSON support
**Then** extend with `to_json : t -> Yojson.Safe.t`

```ocaml
val to_json : t -> Yojson.Safe.t
```

**Verify:** `dune test`; diagnostic JSON round-trips correctly

### R8: CLI error handling

**Given** a CLI usage error (e.g., missing file)
**When** the error occurs
**Then** create `Cli` variant with helpful hint

```elisp
;; Current: prerr_endline "tart: no input files. Use --help for usage."
;; After: Error.cli ~message:"no input files" ~hint:"use --help for usage" ()
```

**Verify:** `./tart 2>&1`; shows error with hint; `./tart --format=json 2>&1`; produces JSON

### R9: IO error wrapping

**Given** a file read failure (e.g., file not found)
**When** the error occurs
**Then** wrap system error with path context

```ocaml
val io_error : path:string -> exn:exn -> t
(* io_error ~path:"foo.el" ~exn:(Unix.Unix_error (ENOENT, _, _))
   => Io { path = "foo.el"; message = "No such file or directory" } *)
```

**Verify:** `./tart check nonexistent.el`; shows path and system error

### R10: Error accumulator

**Given** type-checking that can produce multiple errors
**When** errors are collected
**Then** `Error.Acc` module provides ergonomic accumulation

```ocaml
module Acc : sig
  type 'a t
  val empty : 'a t
  val add : Error.t -> 'a t -> 'a t
  val add_list : Error.t list -> 'a t -> 'a t
  val to_list : 'a t -> Error.t list
  val has_errors : 'a t -> bool
end
```

**Verify:** `dune test`; accumulator collects errors in order

### R11: Integration with existing Diagnostic flow

**Given** current code using `Diagnostic.of_unify_errors`
**When** migrating to unified errors
**Then** provide conversion function

```ocaml
val of_diagnostics : Diagnostic.t list -> t list
(* Wrap each diagnostic in Type constructor *)
```

Existing code:
```ocaml
let diagnostics = Diagnostic.of_unify_errors errors in
List.iter (fun d -> prerr_endline (Diagnostic.to_string d)) diagnostics
```

Migrated code:
```ocaml
let errors = Error.of_diagnostics (Diagnostic.of_unify_errors errors) in
Error.report errors
```

**Verify:** `dune test`; existing diagnostic flow produces same output

## Non-Requirements

- Localization/i18n of error messages
- Error recovery suggestions beyond existing help text
- Structured logging (separate concern)
- Error codes for non-type errors (future enhancement)

## Tasks

- [ ] [R1] Define `Error.t` variant type in `lib/core/error.ml`
- [ ] [R2] Implement `is_fatal` classification
- [ ] [R3] Implement `location` accessor
- [ ] [R4] Implement `to_string` for all variants
- [ ] [R7] Add `to_json` to `Diagnostic.t`
- [ ] [R5] Implement `to_json` for `Error.t`
- [ ] [R10] Implement `Error.Acc` accumulator module
- [ ] [R11] Add `of_diagnostics` conversion
- [ ] [R6] Implement `Error.report` with summary
- [ ] [R9] Add `io_error` wrapper function
- [ ] [R8] Migrate CLI errors in `bin/main.ml` to use `Error.t`
- [ ] Add `--format=json` flag to `tart check` command
- [ ] Update tests to verify JSON output
