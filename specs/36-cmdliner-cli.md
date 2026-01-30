# Spec 36: Cmdliner CLI

Migrate manual argument parsing to Cmdliner for declarative CLI definition.

**Dependencies:** Spec 35 (structured-errors).

## Goal

Users and agents immediately understand CLI mistakes and how to fix them, via
auto-generated help, argument suggestions, and structured error messages.

## Constraints

| Constraint | Detail |
|------------|--------|
| Drop-in | Existing commands and options work identically |
| Helpful | Typos suggest correct options; missing args explain what's needed |
| Documented | `--help` and man pages auto-generated from specs |
| Structured | Validation errors use structured error system (spec 35) |

## Output

```
bin/
├── main.ml           ; Rewritten with Cmdliner
└── main.mli
dune                  ; Add cmdliner dependency
```

## Background

### Current Pain Points

The existing ~1100-line `main.ml` uses manual pattern matching:

```ocaml
let rec parse_lsp_args = function
  | [] -> ()
  | "--port" :: p :: rest ->
      port := Some (int_of_string p);  (* Raises on "abc" *)
      parse_lsp_args rest
```

Problems:
1. Raw exceptions bubble up (e.g., `int_of_string` failure)
2. Error messages are ad-hoc strings like `"tart eval: missing expression"`
3. No suggestions for typos (e.g., `--prot` doesn't suggest `--port`)
4. Help text is manually duplicated

### Cmdliner Benefits

Cmdliner provides:
- Declarative argument specifications with types
- Auto-generated `--help` and man pages
- Argument validation with clear error messages
- Typo suggestions via Levenshtein distance
- Subcommand grouping and documentation

## Requirements

### R1: Cmdliner dependency

**Given** the project dune file
**When** building
**Then** cmdliner is listed as a dependency

**Verify:** `grep cmdliner bin/dune` finds the dependency

### R2: Check subcommand (default)

**Given** `tart check FILE...` or `tart FILE...`
**When** invoked
**Then** type-checks files as before
**And** `--emacs-version VER` option works

**Verify:** `./tart check test.el` behaves identically to current

### R3: Eval subcommand

**Given** `tart eval EXPR`
**When** invoked
**Then** evaluates expression and prints `VALUE :: TYPE`

**Verify:** `./tart eval '(+ 1 2)'` outputs `3 :: Int`

### R4: Expand subcommand

**Given** `tart expand [--load FILE]... FILE`
**When** invoked
**Then** macro-expands and prints result
**And** `--load` is repeatable

**Verify:** `./tart expand --load macros.el main.el` works

### R5: Repl subcommand

**Given** `tart repl`
**When** invoked
**Then** starts interactive REPL

**Verify:** `echo ',quit' | ./tart repl` exits cleanly

### R6: Lsp subcommand

**Given** `tart lsp [--port PORT] [--log-level LEVEL]`
**When** invoked
**Then** starts LSP server on stdio or TCP
**And** log-level accepts: debug, normal, quiet

**Verify:** `./tart lsp --help` shows port and log-level options

### R7: Coverage subcommand

**Given** `tart coverage [OPTIONS] [PATH...]`
**When** invoked
**Then** measures type signature coverage
**And** options work: `--format`, `--verbose`, `--fail-under`, `--exclude`

**Verify:** `./tart coverage --format=json .` outputs valid JSON

### R8: Emacs-coverage subcommand

**Given** `tart emacs-coverage [OPTIONS]`
**When** invoked
**Then** measures C layer type coverage
**And** options work: `--emacs-source`, `--emacs-version`, `--verbose`

**Verify:** `./tart emacs-coverage --help` shows all options

### R9: Invalid port error

**Given** `tart lsp --port abc`
**When** invoked
**Then** error message explains the issue clearly
**And** uses structured error type (spec 35)
**And** exit code is 2

**Verify:** `./tart lsp --port abc 2>&1` contains "invalid" and "integer"

### R10: Missing required argument

**Given** `tart eval` (no expression)
**When** invoked
**Then** error explains what argument is required
**And** shows usage hint
**And** exit code is 2

**Verify:** `./tart eval 2>&1` mentions "EXPR" or "expression"

### R11: Unknown option suggestion

**Given** `tart lsp --prot 8080` (typo)
**When** invoked
**Then** error suggests `--port`
**And** exit code is 2

**Verify:** `./tart lsp --prot 8080 2>&1` suggests "--port"

### R12: Unknown subcommand suggestion

**Given** `tart chekc test.el` (typo)
**When** invoked
**Then** error suggests `check`
**And** exit code is 2

**Verify:** `./tart chekc 2>&1` suggests "check"

### R13: Auto-generated help

**Given** `tart --help` or `tart SUBCOMMAND --help`
**When** invoked
**Then** help is auto-generated from Cmdliner specs
**And** includes all options with descriptions
**And** exit code is 0

**Verify:** `./tart coverage --help` documents all coverage options

### R14: Version flag

**Given** `tart --version`
**When** invoked
**Then** prints version string
**And** exit code is 0

**Verify:** `./tart --version` outputs version

### R15: Exit codes preserved

**Given** any tart command
**When** it completes
**Then** exit codes match current behavior:
- 0: success
- 1: input error (type/parse/eval error)
- 2: usage error (bad arguments)

**Verify:** Exit codes unchanged from current implementation

### R16: Structured validation errors

**Given** argument validation fails
**When** error is reported
**Then** error uses structured type from spec 35
**And** includes error kind, message, and remediation hint

**Verify:** Validation errors are structured, not raw exceptions

### R17: Enum validation

**Given** `tart coverage --format=xml` (invalid enum)
**When** invoked
**Then** error lists valid values: human, json
**And** exit code is 2

**Verify:** `./tart coverage --format=xml 2>&1` lists "human" and "json"

### R18: Range validation

**Given** `tart coverage --fail-under=150` (out of range)
**When** invoked
**Then** error explains valid range: 0-100
**And** exit code is 2

**Verify:** `./tart coverage --fail-under=150 2>&1` mentions range

## Implementation Notes

### Cmdliner Structure

Each subcommand is defined as a `Cmd.t`:

```ocaml
let check_cmd =
  let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE") in
  let emacs_version = (* optional arg *) in
  let doc = "Type-check Elisp files" in
  Cmd.v (Cmd.info "check" ~doc)
    Term.(const cmd_check $ files $ emacs_version)
```

Group into main command:

```ocaml
let main_cmd =
  let doc = "A type checker for Emacs Lisp" in
  let info = Cmd.info "tart" ~version:Tart.version ~doc in
  Cmd.group ~default:check_term info
    [check_cmd; eval_cmd; expand_cmd; repl_cmd; lsp_cmd; coverage_cmd; emacs_coverage_cmd]
```

### Validation with Structured Errors

Wrap Cmdliner's `conv` to produce structured errors:

```ocaml
let port_conv =
  let parse s =
    match int_of_string_opt s with
    | Some n when n > 0 && n < 65536 -> Ok n
    | Some _ -> Error (`Msg "port must be 1-65535")
    | None -> Error (`Msg "port must be an integer")
  in
  Arg.conv (parse, Format.pp_print_int)
```

### Default Subcommand

Cmdliner's `Cmd.group ~default` makes `check` the default when no subcommand is
given, so `tart file.el` works as `tart check file.el`.

## Tasks

- [ ] [R1] Add cmdliner dependency to bin/dune
- [ ] [R2] Implement check subcommand with Cmdliner
- [ ] [R3] Implement eval subcommand
- [ ] [R4] Implement expand subcommand
- [ ] [R5] Implement repl subcommand
- [ ] [R6] Implement lsp subcommand
- [ ] [R7] Implement coverage subcommand
- [ ] [R8] Implement emacs-coverage subcommand
- [ ] [R9-R12, R16-R18] Implement validation with structured errors
- [ ] [R13-R14] Verify auto-generated help and version
- [ ] [R15] Verify exit codes match current behavior
- [ ] Remove manual argument parsing code
- [ ] Update main.mli
