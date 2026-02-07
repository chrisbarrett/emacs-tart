# Spec 42: Round-Trip Test Harness

Parse-print-parse testing for 100% parsing accuracy.

**Deps:** [Spec 31](./31-fast-feedback.md)

## Goal

Verify tart parser/printer consistency and match Emacs reader:
1. Parsed file prints back to Elisp that parses identically
2. Emacs's native reader accepts printed output as equivalent

## Constraints

| Constraint | Detail |
|------------|--------|
| Exhaustive | All `.el` in emacs-corpus |
| Fast | Cache hits skip work |
| CI-friendly | `dune test` or standalone script |
| Debuggable | Failure shows path, error, diff |

## Output

```
lib/roundtrip/
├── roundtrip.ml
├── roundtrip.mli
├── cache.ml
└── cache.mli
test/roundtrip/
└── roundtrip_test.ml
scripts/
└── run-roundtrip.sh
```

## Requirements

### R1: Structural equality

**Given** `.el` that parses **When** AST printed + re-parsed **Then** ASTs equal

**Verify:** `dune test` passes

### R2: Emacs oracle verification

**Given** printed Elisp **When** fed to `emacs --batch --eval "(read ...)"` **Then** matches original

**Verify:** `./scripts/run-roundtrip.sh --with-emacs` passes

### R3: Failure shows path + error

**Given** parse failure **When** reported **Then** includes absolute path + error with location

**Verify:** Broken file produces path + error

### R4: Failure shows diff

**Given** AST mismatch **When** reported **Then** unified diff of expected vs actual

**Verify:** Output contains diff format

### R5: Cache hit skips

**Given** file previously succeeded **When** content hash matches cache **Then** skip, report

**Verify:** Second run faster than first

### R6: Cache miss triggers check

**Given** file changed **When** hash differs **Then** full test, update cache on success

**Verify:** Modifying file triggers re-verification

### R7: Corpus discovery

**Given** emacs-corpus dir **When** `./scripts/run-roundtrip.sh` **Then** all `.el` discovered recursively

**Verify:** All corpus files processed

### R8: Exit failure on parse error

**Given** parse failures **When** complete **Then** exit non-zero

**Verify:** Broken file exits 1

### R9: Exit failure on mismatch

**Given** round-trip mismatch **When** complete **Then** exit non-zero

**Verify:** Mismatched AST exits 1

### R10: dune test integration

**Given** tests in `test/roundtrip/` **When** `dune test` **Then** roundtrip runs, failures fail suite

**Verify:** `dune test` includes roundtrip output

### R11: Summary statistics

**Given** all files processed **When** complete **Then** summary: total, passed, failed, cached

**Verify:** Output ends with summary

### R12: Parallel execution

**Given** multiple files **When** running **Then** test in parallel

**Verify:** Faster with parallelism

## API

```ocaml
module Roundtrip : sig
  type result =
    | Pass
    | Parse_error of { path: string; error: string }
    | Mismatch of { path: string; expected: Sexp.t; actual: Sexp.t; diff: string }
    | Emacs_mismatch of { path: string; tart_output: string; emacs_output: string }

  val check_file : path:string -> result
  val check_file_with_emacs : path:string -> result
  val run_corpus : corpus_dir:string -> cache_dir:string -> parallel:bool -> Summary.t
end

module Cache : sig
  val hit : cache_dir:string -> path:string -> bool
  val record : cache_dir:string -> path:string -> unit
  val clear : cache_dir:string -> unit
end

module Summary : sig
  type t = { total: int; passed: int; failed: int; cached: int; failures: Roundtrip.result list }
  val to_string : t -> string
end
```

## Implementation Notes

- Cache key: SHA256 of content, stored at `.tart-cache/roundtrip/<hash>` (empty file = success)
- Emacs oracle: `emacs --batch --eval '(prin1 (read ...))'`
- Diff: `Sexp.to_string_hum` + unified diff

## Tasks

- [x] Create `lib/roundtrip/` structure
- [x] [R1] `check_file`
- [x] [R2] `check_file_with_emacs`
- [x] [R3] Failure output with path/error
- [x] [R4] Diff output
- [x] [R5-6] Content-addressable cache
- [x] [R7] Corpus discovery
- [x] [R8-9] Exit codes
- [x] [R10] `dune test` integration
- [x] [R11] Summary stats
- [ ] [R12] Parallel execution
- [x] Create `run-roundtrip.sh`

## Status

Complete (except R12 parallel execution — OCaml 4.x lacks domains;
sequential is fine given caching)
