# Implementation Plan: Emacs Reader Oracle (Spec 39)

Invoke Emacs as gold-standard oracle to verify tart parses correctly.

## Analysis

### Current Architecture

**Parser** exists in `lib/syntax/read.ml`: reads Elisp source into
`Sexp.t` AST. Well-tested with 88+ tests.

**Printer** exists in `lib/syntax/print.ml`: prints `Sexp.t` to valid
Elisp string. 65 unit tests including round-trip tests.

**Emacs detection** exists in `lib/sig/emacs_version.ml`: has
`detect()` which runs `emacs --version`. Uses `Sys.command` for
process invocation.

**File errors** exist in `lib/typing/file_error.ml`: structured I/O
error handling (Spec 37).

### Key Design Decisions

1. **Separate `emacs_reader.ml` for process invocation** — isolates
   Emacs subprocess management (PATH lookup, batch mode, timeout,
   stdout/stderr capture) from comparison logic.

2. **`oracle.ml` for comparison** — takes tart parse output and Emacs
   oracle output, normalises both, produces structured diff.

3. **Reuse `Print.to_string`** for tart's canonical form — already
   matches Elisp `prin1-to-string` conventions closely. Any gaps
   found by oracle testing feed back as printer fixes.

4. **Process timeout via Unix alarm** — `Unix.alarm` + `SIGALRM`
   handler gives clean timeout without threads.

### Scope Deferral

- CI integration (Task 11) — deferred until Spec 43 (CI matrix)
- Caching of Emacs process — not required per spec

---

## Iteration 1: Emacs process invocation

Core subprocess management: find Emacs, run batch commands, capture
output.

### Task 1.1: Create emacs_reader module

- [ ] Create `lib/oracle/dune` (library `tart_oracle`, deps
      `tart_syntax`, `unix`)
- [ ] Create `lib/oracle/emacs_reader.ml` and `emacs_reader.mli`
- [ ] `emacs_error` type: `ReadError`, `EmacsNotFound`,
      `EmacsFailed`, `Timeout`
- [ ] `find_emacs : unit -> string option` — search PATH for `emacs`
- [ ] `run_batch : ?timeout_ms:int -> string -> (string * string, emacs_error) result`
      — runs `emacs --batch --quick --eval <expr>`, captures
      stdout + stderr, handles exit codes
- [ ] Default timeout: 5000ms
- [ ] Build + test

### Task 1.2: read_string and read_file

- [ ] `read_string : ?timeout_ms:int -> string -> (string, emacs_error) result`
      — wraps input in `(prin1-to-string (read ...))` Elisp, calls
      `run_batch`
- [ ] `read_file : ?timeout_ms:int -> string -> (string list, emacs_error) result`
      — reads file with multi-form loop, `prin1-to-string` each form
- [ ] Re-export in `tart.ml/tart.mli`
- [ ] Build + test

---

## Iteration 2: Oracle comparison

Compare tart output against Emacs oracle.

### Task 2.1: Create oracle module

- [ ] Create `lib/oracle/oracle.ml` and `oracle.mli`
- [ ] `comparison_result` type: `Match`, `Mismatch`, `TartError`,
      `EmacsError`
- [ ] `compare_string : ?timeout_ms:int -> string -> comparison_result`
      — parse with tart, read with Emacs, compare canonical forms
- [ ] `compare_file : ?timeout_ms:int -> string -> comparison_result list`
      — per-form comparison for multi-form files
- [ ] Build + test

### Task 2.2: Canonical form normalisation

- [ ] Verify `Print.to_string` matches Emacs `prin1-to-string` for:
      integers, floats, strings, symbols, keywords, lists, vectors,
      cons pairs, quote/backquote/function reader macros
- [ ] Fix any discrepancies found (feed back into print.ml)
- [ ] Build + test

---

## Iteration 3: Error handling + timeout

### Task 3.1: Robust error handling

- [ ] `Emacs_not_found` when PATH has no emacs
- [ ] `Emacs_failed` with exit code + stderr for non-read failures
- [ ] `Read_error` with input + message for malformed Elisp
- [ ] stderr capture separate from stdout
- [ ] Build + test

### Task 3.2: Timeout

- [ ] Implement timeout via `Unix.alarm` + `SIGALRM` or
      `Unix.select` on process pipes
- [ ] Test with intentional hang: `(while t)` input
- [ ] Build + test

---

## Iteration 4: Unit tests + spec completion

### Task 4.1: Oracle unit tests

- [ ] Create `test/oracle/dune` and `test/oracle/oracle_test.ml`
- [ ] `read_string` basic forms: int, string, symbol, list, vector
- [ ] `read_string` special syntax: quote, backquote, function,
      character literals
- [ ] `read_file` multi-form: defun + defvar file
- [ ] `compare_string` match and mismatch cases
- [ ] Error cases: malformed input, Emacs not found
- [ ] Comment stripping: comments in input don't affect comparison
- [ ] Build + test

### Task 4.2: Spec completion

- [ ] Check all task boxes in specs/39-emacs-reader-oracle.md
- [ ] Add Status section
- [ ] Build + test
