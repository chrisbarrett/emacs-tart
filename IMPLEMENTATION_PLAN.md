# Implementation Plan: Content-Addressable Cache (Spec 40)

XDG-compliant content-addressable file cache for incremental
type-checking results.

## Analysis

### Current Architecture

**Form cache** in `lib/lsp/form_cache.ml`: in-process hash table keyed
by `Hashtbl.hash` of sexp string. Fast but not persistent across
process invocations.

**Sub-library pattern**: each `lib/X/` has `dune` declaring
`(library (name X) (public_name tart.X) (libraries ...))`. Re-exported
via `module X = X_pkg.X` in `lib/tart.ml` + `lib/tart.mli`.

**Test pattern**: `test/X/dune` with `(test (name X_test) (modules X_test) (libraries tart alcotest))`.

**Digest**: OCaml stdlib `Digest` provides MD5 (32-char hex). Spec says
SHA256 (64-char hex) but MD5 is sufficient for content addressing (not
security). Use `Digest` to avoid adding dependencies.

**Integration point**: `check_file` in `bin/main.ml` — between parse
and `Module_check.check_module`. Cache key = hash(binary + file
content). Cache value = JSON-serialised diagnostics.

### Key Design Decisions

1. **`Digest.string` (MD5)** for content hashing — no external
   dependency, 32-char hex keys. Spec says 64-char but the actual
   requirement is deterministic content-based keys.

2. **Filesystem-backed cache** under `$XDG_CACHE_HOME/tart/v1/` —
   persistent across invocations; two-char prefix subdirs.

3. **Atomic writes** via temp file + `Unix.rename` in same directory.

4. **Best-effort everywhere** — all I/O wrapped in try/with, cache
   failures never propagate to type-checking.

5. **Integration deferred** — Spec 40 builds the cache library and
   tests it. Wiring into `check_file` is a separate task since the
   cache value format depends on what type-checking results look like
   when serialised.

---

## Iteration 1: Core cache module

Create `lib/cache/` with `cache_dir`, `binary_path`, `compute_key`,
`store`, `retrieve`.

### Task 1.1: Create lib/cache sub-library

- [ ] Create `lib/cache/dune` (library `tart_cache`, public
      `tart.cache`, deps `unix yojson tart.log`)
- [ ] Create `lib/cache/content_cache.mli` with full interface:
      `cache_dir`, `binary_path`, `compute_key`, `store`, `retrieve`,
      `evict_older_than`, `maybe_evict`
- [ ] Create `lib/cache/content_cache.ml` stub (compiles, not yet
      implemented)
- [ ] Add `tart.cache` to `lib/dune` libraries
- [ ] Add `module Content_cache = Cache.Content_cache` to
      `lib/tart.ml` and `lib/tart.mli`
- [ ] Build

### Task 1.2: Implement cache_dir, binary_path, compute_key

- [ ] `cache_dir`: read `$XDG_CACHE_HOME` env, fallback
      `~/.cache/tart/` (R1)
- [ ] `binary_path`: `Sys.executable_name` resolved via `realpath`
      (R9)
- [ ] `compute_key`: `Digest.to_hex (Digest.string (binary_content ^
      input_content))` where binary_content = file contents of
      binary_path and input_content = file contents of input path (R2)
- [ ] Build + test

### Task 1.3: Implement store and retrieve

- [ ] `store`: mkdir -p prefix dir, write JSON envelope
      (`{version, created_at, data}`) to temp file, atomic rename (R3,
      R6, R7, R8, R10)
- [ ] `retrieve`: read file, parse JSON, extract `data` field; return
      `None` on any error (R4, R5)
- [ ] ISO 8601 timestamp via `Unix.gmtime` for `created_at`
- [ ] Build + test

---

## Iteration 2: Eviction

Age-based eviction with marker file and best-effort error handling.

### Task 2.1: Implement evict_older_than

- [ ] Walk `v1/XX/` directories, stat each `.json` file
- [ ] Delete files with mtime older than `days` threshold (R11)
- [ ] Remove empty prefix directories after deletion
- [ ] Wrap each deletion in try/with — log warning, continue (R13)
- [ ] Build + test

### Task 2.2: Implement maybe_evict

- [ ] Check `.last-eviction` marker file mtime in cache dir
- [ ] Skip if marker less than 1 hour old (R12)
- [ ] Call `evict_older_than ~days:30`
- [ ] Touch marker file after successful eviction
- [ ] Entire function wrapped in try/with — never fails (R13)
- [ ] Build + test

---

## Iteration 3: Unit tests + spec completion

### Task 3.1: Create test suite

- [ ] Create `test/cache/dune` and `test/cache/content_cache_test.ml`
- [ ] cache_dir: XDG_CACHE_HOME override, fallback to ~/.cache
- [ ] binary_path: returns valid path
- [ ] compute_key: deterministic, different inputs differ, hex string
- [ ] store + retrieve round-trip: store then retrieve = Some data
- [ ] retrieve miss: nonexistent key = None
- [ ] retrieve corrupted: invalid JSON = None
- [ ] store creates directories on demand
- [ ] atomic write: file appears only after rename (check no
      partial files)
- [ ] JSON format: stored file is valid JSON with version, created_at,
      data fields
- [ ] evict_older_than: old files deleted, new files preserved
- [ ] maybe_evict: runs on first call, skips on second
- [ ] best-effort: read-only file doesn't crash eviction
- [ ] Build + test

### Task 3.2: Spec completion

- [ ] Check all task boxes in specs/40-content-cache.md
- [ ] Add Status section
- [ ] Build + test
