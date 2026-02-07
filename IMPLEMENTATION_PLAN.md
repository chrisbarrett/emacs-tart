# Implementation Plan: Emacs Source Corpus (Spec 41)

Lazy-cloned Emacs source in XDG cache for testing against real
Elisp files.

## Analysis

### Current Architecture

**XDG path**: `Content_cache.cache_dir()` returns
`$XDG_CACHE_HOME/tart/` or `~/.cache/tart/`. Corpus goes under
`emacs-src/` subdirectory.

**Version detection**: `Emacs_version` module in `lib/sig/` provides
`detect`, `parse_version`, `version_to_string`. Already used by
`bin/main.ml` for typings version selection.

**Sub-library pattern**: `lib/X/dune` →
`(library (name X) (public_name tart.X))`, re-exported in
`lib/tart.ml`/`lib/tart.mli`.

**CLI pattern**: `bin/main.ml` uses Cmdliner. Subcommands defined
as `Cmd.v info Term.(...)`, grouped in `Cmd.group` at bottom.

**Process invocation**: `Emacs_reader.run_batch` shells out to
emacs. For git operations, use `Unix.open_process_full` similarly.

### Key Design Decisions

1. **Reuse `Content_cache.cache_dir`** for XDG path — already handles
   `$XDG_CACHE_HOME` and fallback. Corpus path =
   `cache_dir() ^ "/emacs-src"`.

2. **Shell out to git** via `Unix.open_process_full` — simpler than
   FFI, git is universally available.

3. **Shallow clone + shallow fetch** — `--depth 1` for clone,
   `--depth 1 origin tag <tag>` for fetch. Minimises disk/network.

4. **Version auto-detection reuses `Emacs_version.detect`** — already
   parses `emacs --version` output.

5. **`corpus` subcommand group** — `tart corpus checkout`,
   `tart corpus list`, `tart corpus path`, `tart corpus clean`.

---

## Iteration 1: Core corpus library

Create `lib/corpus/` with path resolution, clone, checkout, list.

### Task 1.1: Create lib/corpus sub-library

- [x] Create `lib/corpus/dune` (library `corpus`, public
      `tart.corpus`, deps `unix tart.cache tart.sig tart.log`)
- [x] Create `lib/corpus/emacs_corpus.mli` with full interface
- [x] Create `lib/corpus/emacs_corpus.ml` implementation
- [x] Add `tart.corpus` to `lib/dune` libraries
- [x] Re-export as `Tart.Emacs_corpus` in `tart.ml`/`tart.mli`
- [x] Build

### Task 1.2: Implement core functions

- [x] `corpus_error` type: `Clone_failed`, `Fetch_failed`,
      `Checkout_failed`, `No_emacs`, `Invalid_ref`
- [x] `corpus_dir`: `Content_cache.cache_dir() ^ "/emacs-src"` (R3, R4)
- [x] `run_git`: helper to shell out to git, capture stdout/stderr,
      return result
- [x] `ensure_clone`: if dir missing, `git clone --depth 1 --branch
      <tag> <url> <path>` (R1); if exists, no-op (R2)
- [x] `checkout`: `git fetch --depth 1 origin tag <tag> --no-tags`
      then `git checkout <tag>` (R5); for SHA, `git fetch --depth 1
      origin <sha>` then checkout (R6)
- [x] `detect_tag`: uses `Emacs_version.detect` → `emacs-M.N` tag
      (R7, R8); returns error if no Emacs found
- [x] `list_el_files`: recursive `.el` discovery in corpus dir,
      returns absolute paths (R10, R11)
- [x] `clean`: `rm -rf` corpus dir, return bytes freed (R15)
- [x] Build + test

---

## Iteration 2: CLI subcommands

Wire corpus operations into `tart corpus` command group.

### Task 2.1: Add corpus subcommand group

- [ ] `corpus_checkout_cmd`: `tart corpus checkout [REF]`
      with optional `--emacs-version` override (R9);
      auto-detects if no ref given (R7)
- [ ] `corpus_list_cmd`: `tart corpus list` prints `.el` paths
- [ ] `corpus_path_cmd`: `tart corpus path` prints corpus dir (R14)
- [ ] `corpus_clean_cmd`: `tart corpus clean` removes corpus (R15)
- [ ] `corpus_cmd`: `Cmd.group` combining subcommands
- [ ] Wire `corpus_cmd` into `main_cmd` group
- [ ] Error handling: network failures → exit 1 with message (R12,
      R13); invalid version → exit 2; no Emacs → exit 3
- [ ] Build + test

---

## Iteration 3: Unit tests + spec completion

### Task 3.1: Create test suite

- [ ] Create `test/corpus/dune` and
      `test/corpus/emacs_corpus_test.ml`
- [ ] `corpus_dir`: XDG override, fallback
- [ ] `run_git`: captures stdout/stderr
- [ ] `detect_tag`: parses version to tag string
- [ ] `list_el_files`: discovers .el in temp dir
- [ ] `clean`: removes directory
- [ ] Error types: all constructors tested
- [ ] Build + test

### Task 3.2: Spec completion

- [ ] Check all task boxes in specs/41-emacs-corpus.md
- [ ] Add Status section
- [ ] Build + test
