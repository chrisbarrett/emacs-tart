# Spec 39: Emacs Reader Oracle

Invoke Emacs as gold-standard oracle to verify tart parses correctly.

**Deps:** [Spec 35][], [Spec 37][]

## Links

### Deps
[Spec 35]: ./35-structured-errors.md
[Spec 37]: ./37-file-io-errors.md

## Constraints

| Constraint | Detail |
|------------|--------|
| Gold-standard | Emacs reader is authoritative |
| PATH-based | Uses `emacs` from PATH |
| Graceful | Reader errors captured, not fatal |
| Fast | `--batch --quick` for minimal startup |

## Output

```
lib/oracle/
├── emacs_reader.ml
├── emacs_reader.mli
├── oracle.ml
└── oracle.mli
test/oracle/
└── oracle_test.ml
```

## Requirements

### R1: Read string via Emacs

**Given** Elisp string **When** passed to oracle **Then** Emacs reads and prints via `prin1-to-string`

```ocaml
val read_string : string -> (string, emacs_error) result
(* read_string "(+ 1 2)" => Ok "(+ 1 2)" *)
```

**Verify:** `dune test`; `read_string "(+ 1 2)"` returns `Ok "(+ 1 2)"`

### R2: Read file via Emacs

**Given** `.el` file path **When** passed to oracle **Then** returns list of printed forms

```ocaml
val read_file : string -> (string list, emacs_error) result
```

**Verify:** `dune test`; `read_file "test.el"` returns forms matching content

### R3: Handle Emacs reader errors

**Given** malformed input **When** Emacs fails to read **Then** error captured with message

```ocaml
type emacs_error =
  | Read_error of { input: string; message: string }
  | Emacs_not_found
  | Emacs_failed of { exit_code: int; stderr: string }
```

**Verify:** `dune test`; `read_string "(foo"` returns `Error (Read_error ...)`

### R4: PATH-based Emacs

**Given** system PATH **When** invoking oracle **Then** runs `emacs` from PATH

**Verify:** `which emacs` matches oracle's Emacs

### R5: Batch mode invocation

**Given** any oracle call **When** Emacs invoked **Then** uses `--batch --quick`

```
emacs --batch --quick --eval '(princ (prin1-to-string (read "(+ 1 2)")))'
```

**Verify:** `dune test`; oracle uses `--batch --quick`

### R6: Compare tart vs Emacs

**Given** Elisp string/file **When** comparing **Then** both printed to canonical form

```ocaml
type comparison_result =
  | Match
  | Mismatch of { tart_output: string; emacs_output: string }
  | Tart_error of Read.parse_error
  | Emacs_error of emacs_error

val compare_string : string -> comparison_result
val compare_file : string -> comparison_result list
```

**Verify:** `dune test`; `compare_string "(+ 1 2)"` returns `Match`

### R7: Canonical printing

**Given** tart AST **When** printing for comparison **Then** matches `prin1-to-string` conventions

| Type | Format |
|------|--------|
| Lists | `(a b c)` |
| Dotted pairs | `(a . b)` |
| Strings | `"foo"` with escapes |
| Characters | `?a`, `?\n` |
| Symbols | `foo` |
| Numbers | `42`, `3.14` |
| Vectors | `[a b c]` |

**Verify:** `dune test`; tart print matches Emacs print

### R8: Special read syntax

**Given** special syntax **When** compared **Then** tart handles correctly

- Quote: `'foo` => `(quote foo)`
- Backquote: `` `(a ,b ,@c) ``
- Function: `#'foo` => `(function foo)`
- Reader macros: `#("str" 0 3 (face bold))`

**Verify:** `dune test`; special syntax round-trips

### R9: Emacs not found error

**Given** Emacs not on PATH **When** oracle invoked **Then** `Error Emacs_not_found`

**Verify:** `dune test`; mock missing Emacs returns structured error

### R10: Timeout handling

**Given** oracle invocation **When** Emacs hangs **Then** times out

```ocaml
val read_string : ?timeout_ms:int -> string -> (string, emacs_error) result
(* Default: 5000ms *)
```

**Verify:** `dune test`; infinite loop input times out

### R11: Capture stderr separately

**Given** Emacs produces stderr warnings **When** reading result **Then** stderr separate from stdout

**Verify:** `dune test`; stderr warnings don't corrupt output

### R12: Multi-expression files

**Given** file with multiple top-level forms **When** read via Emacs **Then** each form separate

```ocaml
read_file "test.el"
(* => Ok ["(defun foo nil 1)"; "(defun bar nil 2)"] *)
```

**Verify:** `dune test`; multi-form files return list

### R13: Comments stripped

**Given** Elisp with comments **When** compared **Then** comments stripped by both readers

**Verify:** `dune test`; commented input matches uncommented

## Non-Requirements

- Non-PATH Emacs installations
- Caching Emacs process
- Comparing evaluation results
- Emacs versions < 27.1
- Byte-compilation comparison

## Tasks

- [ ] [R1, R5] `read_string` with batch mode
- [ ] [R2, R12] `read_file` for multi-form files
- [ ] [R3, R9, R11] `emacs_error` type and handling
- [ ] [R4] PATH-based Emacs lookup
- [ ] [R6] `compare_string`/`compare_file`
- [ ] [R7] Canonical printer matching `prin1-to-string`
- [ ] [R8] Test special read syntax
- [ ] [R10] Timeout handling
- [ ] [R13] Test comment stripping
- [ ] Create test fixtures
- [ ] Wire into CI
