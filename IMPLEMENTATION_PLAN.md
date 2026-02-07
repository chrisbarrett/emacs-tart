# Implementation Plan: Round-Trip Test Harness (Spec 42)

Parse-print-parse testing for parsing accuracy verification against
Emacs corpus.

## Analysis

### Current Architecture

**Oracle module** (`lib/oracle/`): `Compare.compare_file` already does
per-form tart-vs-Emacs comparison. `Emacs_reader.read_file` reads all
forms via Emacs subprocess. `Compare.normalise` handles insignificant
differences.

**Corpus module** (`lib/corpus/`): `Emacs_corpus.list_el_files()`
recursively discovers `.el` files. `corpus_dir()` gives the path.

**Content cache** (`lib/cache/`): `Content_cache.compute_key` hashes
binary+input. `store`/`retrieve` handle JSON envelope. Already used
for type-checking cache.

**Parser/printer** (`lib/syntax/`): `Read.parse_file` returns
`parse_result` with `sexps` and `errors`. `Print.to_string` prints
individual sexps. `Sexp.equal` does structural comparison.

### Key Design Decisions

1. **Reuse `Content_cache`** for roundtrip caching — same XDG path,
   same binary+file keying, same eviction. No separate cache module
   needed. Store `"roundtrip:pass"` as data.

2. **Reuse `Oracle_compare.compare_file`** for Emacs oracle mode —
   already implements per-form comparison with normalisation.

3. **Separate structural round-trip from oracle** — `check_file`
   does parse→print→reparse→compare (fast, no subprocess);
   `check_file_with_emacs` delegates to `Oracle_compare`.

4. **Simple text diff** — OCaml stdlib has no diff library; implement
   minimal line-by-line diff showing expected vs actual (not full
   unified diff). Good enough for debugging.

5. **Sequential by default** — R12 says parallel, but OCaml's
   `Domain` is Multicore-only (5.0+). Use sequential with future
   parallel note. The cache makes repeated runs fast anyway.

6. **CLI subcommand** — `tart roundtrip` rather than a shell script.
   More consistent with existing `tart corpus` pattern.

---

## Iteration 1: Core roundtrip library

Create `lib/roundtrip/` with result types, check_file, summary.

### Task 1.1: Create lib/roundtrip sub-library

- [ ] Create `lib/roundtrip/dune` (library `roundtrip`, public
      `tart.roundtrip`, deps `tart.syntax tart.oracle tart.cache
      tart.corpus tart.log unix`)
- [ ] Create `lib/roundtrip/roundtrip.mli` with full interface
- [ ] Create `lib/roundtrip/roundtrip.ml` implementation
- [ ] Add `tart.roundtrip` to `lib/dune` libraries
- [ ] Re-export as `Tart.Roundtrip` in `tart.ml`/`tart.mli`
- [ ] Build

### Task 1.2: Implement core functions

- [ ] `result` type: `Pass`, `Cached`, `Parse_error of {path; error}`,
      `Mismatch of {path; expected; actual; diff}`,
      `Emacs_mismatch of {path; tart_output; emacs_output}`
- [ ] `summary` type: `{total; passed; failed; cached; failures}`
- [ ] `summary_to_string`: format as "N total, N passed, N failed,
      N cached" (R11)
- [ ] `check_file`: parse file → print each sexp → reparse printed →
      compare with `Sexp.equal` (R1); on mismatch return
      `Mismatch` with textual diff (R3, R4)
- [ ] `check_file_with_emacs`: delegates to
      `Oracle_compare.compare_file` (R2)
- [ ] `make_diff`: show expected vs actual line-by-line (R4)
- [ ] `cache_key`: `Content_cache.compute_key` with binary_path +
      file path (R5)
- [ ] `check_file_cached`: if cache hit return `Cached`; else run
      `check_file`, record on pass (R5, R6)
- [ ] `run_corpus`: iterate `list_el_files`, call
      `check_file_cached` on each, collect summary (R7, R11)
- [ ] Build + test

---

## Iteration 2: CLI subcommand + script

Wire roundtrip into `tart roundtrip` CLI and create shell script.

### Task 2.1: Add CLI subcommand

- [ ] `roundtrip_cmd`: `tart roundtrip [--with-emacs] [--no-cache]
      [FILES...]`
- [ ] Default (no files): use `Emacs_corpus.list_el_files()` (R7)
- [ ] Explicit files: check just those files
- [ ] `--with-emacs`: also run oracle check (R2)
- [ ] `--no-cache`: skip cache lookup
- [ ] Exit 0 on all pass, exit 1 on any failure (R8, R9)
- [ ] Print summary at end (R11)
- [ ] Wire into `main_cmd` group
- [ ] Build + test

### Task 2.2: Create run-roundtrip.sh

- [ ] Create `scripts/run-roundtrip.sh`: thin wrapper calling
      `tart roundtrip` with appropriate flags
- [ ] `--with-emacs` flag forwarded
- [ ] Make executable
- [ ] Build + test

---

## Iteration 3: Unit tests + spec completion

### Task 3.1: Create test suite

- [ ] Create `test/roundtrip/dune` and
      `test/roundtrip/roundtrip_test.ml`
- [ ] `check_file`: valid .el passes, broken .el returns Parse_error,
      round-trip mismatch returns Mismatch with diff
- [ ] `check_file_with_emacs`: match and mismatch cases
- [ ] `check_file_cached`: cache hit returns Cached, cache miss runs
      check
- [ ] `run_corpus`: summary counts correct
- [ ] `make_diff`: shows expected vs actual
- [ ] `summary_to_string`: correct format
- [ ] Build + test

### Task 3.2: Spec completion

- [ ] Check all task boxes in specs/42-roundtrip-harness.md
- [ ] Add Status section
- [ ] Build + test
