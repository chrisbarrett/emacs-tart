# Spec 09: CLI Interface

Subcommands for type-checking, evaluation, macro expansion, and interactive use.

**Dependencies:** Spec 05 interpreter complete; Spec 06 for type display.

## Goal

Provide a usable CLI that exposes tart's capabilities: type-checking files,
evaluating expressions, expanding macros, running a REPL, and serving LSP.

## Constraints

- **Ergonomic**: Sensible defaults; minimal flags for common cases
- **Composable**: Output suitable for scripting and editor integration
- **Incremental**: Subcommands work independently as capabilities are added

## Output

```
tart/
├── bin/
│   └── main.ml           ; Subcommand dispatch
├── lib/
│   └── cli/
│       ├── dune
│       ├── cli.ml        ; Shared CLI utilities
│       ├── check.ml      ; Type-check command
│       ├── eval_cmd.ml   ; Eval command
│       ├── expand.ml     ; Expand command
│       ├── repl.ml       ; REPL command
│       └── lsp.ml        ; LSP server command
└── test/
    └── cli/
        └── cli_test.ml   ; CLI integration tests
```

## Usage

```
tart [OPTIONS] FILE [FILES...]   Type-check files (default command)
tart eval <EXPR>                 Evaluate expression, print value and type
tart expand FILE                 Print fully macro-expanded source
tart repl                        Interactive REPL
tart lsp [OPTIONS]               Start LSP server
```

## Requirements

### R1: Default command (type-check)

**Given** one or more `.el` or `.tart` files as arguments
**When** `tart file.el` is invoked (no subcommand)
**Then** each file is parsed, macro-expanded, and type-checked
**And** diagnostics are printed to stderr in compiler-style format
**And** exit code is 0 if no errors, 1 if errors

Diagnostic format: `file:line:col: error: message`

**Verify:** `dune exec tart -- test/fixtures/error.el` exits 1 with diagnostic;
`dune exec tart -- test/fixtures/valid.el` exits 0 silently

### R2: Multiple file handling

**Given** multiple files: `tart a.el b.el c.tart`
**When** executed
**Then** files are processed in order
**And** definitions from earlier files are visible to later files
**And** `.tart` signature files provide types without requiring implementations

**Verify:** `tart prelude.tart main.el` uses types from prelude.tart

### R3: Eval subcommand

**Given** `tart eval <expr>`
**When** executed
**Then** the expression is parsed, evaluated, and the result printed
**And** the inferred type is printed after `::` (when type inference available)

Output format:
```
<value> :: <type>
```

If type inference is not yet available, print only the value.

**Verify:** `tart eval '(+ 1 2)'` prints `3 :: Int`;
`tart eval '(cons 1 nil)'` prints `(1) :: (List Int)`

### R4: Eval error handling

**Given** `tart eval <expr>` with a malformed or erroring expression
**When** executed
**Then** the error is printed to stderr
**And** exit code is 1

**Verify:** `tart eval '(+ 1 "x")'` prints type error, exits 1;
`tart eval '(car)'` prints arity error, exits 1

### R5: Expand subcommand

**Given** `tart expand file.el`
**When** executed
**Then** the file is parsed and all macros fully expanded
**And** the result is pretty-printed to stdout as readable Elisp

**Verify:** File containing `(when test body)` expands to `(if test (progn body))`

### R6: Expand with macro loading

**Given** `tart expand file.el` where file.el uses macros from other files
**When** `--load prelude.el` is provided
**Then** macros from prelude.el are available during expansion

**Verify:** `tart expand --load macros.el main.el` expands macros defined in
macros.el

### R7: REPL subcommand

**Given** `tart repl`
**When** executed
**Then** an interactive REPL starts with prompt `tart> `
**And** each input is parsed, evaluated, and result printed with type
**And** definitions persist across inputs within the session
**And** `,quit` or Ctrl-D exits cleanly

**Verify:** Manual testing; REPL handles multi-line input, shows types

### R8: REPL commands

**Given** a running REPL
**When** special commands are entered
**Then** they are handled:
- `,quit` or `,q` — exit REPL
- `,type <expr>` — show type without evaluating
- `,expand <expr>` — show macro expansion
- `,env` — list current bindings
- `,help` — show available commands

**Verify:** Each REPL command produces expected output

### R9: LSP subcommand

**Given** `tart lsp`
**When** executed
**Then** an LSP server starts on stdio (default)
**And** the server handles initialize, textDocument/didOpen, etc.

**Verify:** LSP client (Emacs, VS Code) connects and receives diagnostics

### R10: LSP port option

**Given** `tart lsp --port 9000`
**When** executed
**Then** the LSP server listens on TCP port 9000 instead of stdio

**Verify:** `nc localhost 9000` can send LSP messages

### R11: Help and version

**Given** `tart --help` or `tart <subcommand> --help`
**When** executed
**Then** usage information is printed

**Given** `tart --version`
**When** executed
**Then** version string is printed

**Verify:** Help output includes all subcommands and their options

### R12: Exit codes

**Given** any tart invocation
**When** it completes
**Then** exit codes follow convention:
- 0: success
- 1: error in input (type error, parse error, eval error)
- 2: usage error (bad arguments)

**Verify:** Test exit codes for each error class

## Tasks

- [x] [R11] Refactor main.ml to use subcommand dispatch (cmdliner or manual)
- [x] [R1] Implement default type-check command
- [x] [R2] Implement multi-file processing with definition accumulation
- [x] [R3,R4] Implement `eval` subcommand
- [x] [R5,R6] Implement `expand` subcommand
- [x] [R7,R8] Implement `repl` subcommand with commands
- [x] [R9,R10] Implement `lsp` subcommand (stdio and TCP port modes)
- [x] [R12] Standardize exit codes across all commands
- [x] Add test fixtures for CLI integration tests

Run review agent after `tart eval` and `tart expand` work correctly before
implementing REPL.
