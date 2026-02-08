# Spec 76 — Typings Distribution: Precision & Coverage

Drive typings toward accurate, precise signatures that leverage the full type
system: multi-clause dispatch, polymorphism, union types, type predicates, and
clause diagnostics.

**Current state:** 54 c-core files, 35 lisp-core files, 29.4% C-layer symbol
coverage (1414/4804). Many signatures use `any` where precise types are
expressible.

**Goal:** High-precision typings for the most-used Emacs functions, validated
against real Emacs Lisp code.

---

## Iteration 1: Precision audit — c-core foundational files

Improve the 4 most impactful c-core files (`data.tart`, `fns.tart`,
`eval.tart`, `alloc.tart`) by replacing `any` with precise types where the
type system supports it.

### data.tart

- `car-safe`: convert to multi-clause — `((cons a b) -> a) ((_) -> nil)`.
  Remove from BUGS.md (multi-clause dispatch resolves the "overloaded return
  types" gap).
- `cdr-safe`: same — `((cons a b) -> b) ((_) -> nil)`.
- `natnump`: convert to multi-clause predicate — `((int) -> bool) ((_) -> nil)`.
- `arrayp`: `(((vector any)) -> t) ((string) -> t) ((bool-vector) -> t)
  ((char-table) -> t) ((_) -> nil)`.
- `nlistp`: multi-clause inverse of `listp`.
- `char-or-string-p`: `((int) -> t) ((string) -> t) ((_) -> nil)`.
- `integer-or-marker-p`: `((int) -> t) ((marker) -> t) ((_) -> nil)`.
- `interactive-form`: tighten `any` arg → `(symbol | (any -> any))`.
- `variable-binding-locus`: already precise, verify.

### fns.tart

- `concat`: `(&rest (string | symbol | (list int) | (vector int))) -> string`
  (Emacs's actual input types).
- `vconcat`: `(&rest ((list any) | (vector any) | string | bool-vector)) ->
  (vector any)`.
- `sort` on vectors: add vector clause — currently only handles lists.
  Multi-clause: `(((list a) ((a a) -> any)) -> (list a))
  (((vector a) ((a a) -> any)) -> (vector a))`.
- `reverse`/`nreverse`: add vector and string clauses.
- `copy-sequence`: add multi-clause for list/vector/string preservation.
- `plist-put`: tighten to use plist type — `[k v] ((plist k v) k v &optional
  any) -> (plist k v)`.

### eval.tart

- `signal`: return type is `never` (always throws).
- `error`/`user-error`: return type is `never`.
- `throw`: return type is `never`.
- `commandp`: multi-clause predicate.
- `autoload`: tighten parameter types from `any`.

### alloc.tart

- `cons`: verify polymorphic — `[a b] (a b) -> (cons a b)`.
- `list`: verify polymorphic — `[a] (&rest a) -> (list a)`.
- `vector`: verify polymorphic — `[a] (&rest a) -> (vector a)`.
- `make-marker`: verify returns `marker`.
- `make-finalizer`: verify returns `finalizer`.

**Files:** `typings/emacs/31.0/c-core/{data,fns,eval,alloc}.tart`,
`typings/emacs/BUGS.md`

**Verify:**
```
Bash(command="nix develop --command dune test --force 2>&1")
```

---

## Iteration 2: Precision audit — c-core I/O and editing files

### editfns.tart

- `char-after`/`char-before`: `(&optional (int | marker | nil)) -> (int |
  nil)` (already close, verify).
- `buffer-substring`: returns `string` (verify).
- `insert`: `(&rest (string | int)) -> nil` (tighten from `any`).
- `format`: first arg is string, rest are `any` — already correct but verify.
- `encode-time`/`decode-time`: tighten from `any` where time-value structure
  is known.

### fileio.tart

- `expand-file-name`: `(string &optional (string | nil)) -> string`.
- `file-name-directory`/`file-name-nondirectory`: `(string) -> (string | nil)`
  / `(string) -> string`.
- File predicates (`file-exists-p`, `file-readable-p`, etc.): all `(string) ->
  bool`.
- `insert-file-contents`: returns `(cons string int)` (file + bytes read).
- `write-region`: tighten arg types.

### buffer.tart

- `get-buffer`: multi-clause — `((buffer) -> buffer) ((string) -> (buffer |
  nil))` (returns arg if already a buffer).
- `get-buffer-create`: `((buffer | string) &optional bool) -> buffer` (never
  nil).
- Buffer predicates: verify multi-clause narrowing.

### search.tart

- Search functions: verify `(int | nil)` return for position-or-nil pattern.
- `match-string`: `(int &optional (string | nil)) -> (string | nil)`.

**Files:** `typings/emacs/31.0/c-core/{editfns,fileio,buffer,search}.tart`

**Verify:**
```
Bash(command="nix develop --command dune test --force 2>&1")
```

---

## Iteration 3: Precision audit — lisp-core foundational files

### subr.tart (highest priority — used by all Elisp code)

- `when`/`unless`: special forms, keep `any` returns (body type is dynamic).
  But tighten where possible — `when` returns nil when condition false.
- `if-let*`/`when-let*`: spec bindings as `(list any)` is imprecise. Verify
  these are handled by the type checker's special-form logic rather than
  signature types.
- `with-current-buffer`: `((buffer | string) &rest any) -> any` — already
  good.
- `with-temp-buffer`: `(&rest any) -> any`.
- `save-excursion`, `save-restriction`: `(&rest any) -> any` — these are
  special forms, keep as-is.
- `error`: if re-exported from eval.c, should be `never`.
- `string-prefix-p`/`string-suffix-p`: `(string string &optional bool) ->
  bool`.
- `string-trim`/`string-trim-left`/`string-trim-right`: `(string &optional
  string) -> string`.
- `assoc-string`: `[v] (string (list (cons string v)) &optional bool) ->
  ((cons string v) | nil)`.
- `number-sequence`: `(int &optional (int | nil) (int | nil)) -> (list int)`.
- `plist-get`/`lax-plist-get`: delegate to fns.tart or provide precise sig.

### simple.tart

- Already has good types for mark/region, buffer display, movement.
- Audit for remaining `any` in return positions.
- `read-string`: `(string &optional string any any string) -> string`.
- `shell-command-to-string`: `(string) -> string`.
- `blink-matching-open`: `() -> nil`.

### seq.tart

- Replace `((list any) | (vector any) | string)` unions with a local type
  alias if `.tart` `let` bindings are available, or keep as-is.
- `seq-map`: tighten — `[a b] (((a) -> b) (list a)) -> (list b)` (currently
  uses `any`).
- `seq-filter`: tighten predicate type.
- `seq-reduce`: `[a b] (((b a) -> b) ((list a) | (vector a) | string) b) ->
  b`.
- `seq-find`: `[a] (((a) -> any) (list a) &optional a) -> (a | nil)`.

### cl-lib.tart

- `cl-remove-if`/`cl-remove-if-not`: polymorphic.
- `cl-reduce`: polymorphic accumulator.
- `cl-mapcar`: polymorphic.
- `cl-position`/`cl-find`: polymorphic with optional test.
- `cl-loop`: returns `any` (macro, dynamic).

**Files:** `typings/emacs/31.0/lisp-core/{subr,simple,seq,cl-lib}.tart`

**Verify:**
```
Bash(command="nix develop --command dune test --force 2>&1")
```

---

## Iteration 4: Validation against Emacs lisp/

Run `./tart check` against Emacs's own lisp/ directory to surface incorrect
signatures. This is the feedback loop that catches real-world type errors.

### 4a: Pick 10 high-value files from `lisp/`

Start with files that exercise the most signatures:
- `lisp/simple.el`
- `lisp/files.el`
- `lisp/startup.el`
- `lisp/subr.el`
- `lisp/minibuffer.el`
- `lisp/window.el`
- `lisp/emacs-lisp/bytecomp.el`
- `lisp/emacs-lisp/cl-lib.el`
- `lisp/emacs-lisp/seq.el`
- `lisp/progmodes/elisp-mode.el`

### 4b: Fix and iterate

For each file:
1. Run `./tart check --emacs-version 31.0 <file>`.
2. Categorise each error:
   - **Signature bug:** fix the `.tart` file.
   - **Type system gap:** document in `BUGS.md` with category.
   - **Parser issue:** log as a regression fixture.
3. Re-run until errors are documented or fixed.

### 4c: Add regression fixtures

Create `test/fixtures/typing/regression/` entries for any bugs discovered
during validation, per the template:

```elisp
;; Regression: [brief description]
;; Fixed: [date or commit]
```

**Verify:**
```
Bash(command="./tart check --emacs-version 31.0 <file> 2>&1")
Bash(command="nix develop --command dune test --force 2>&1")
```

---

## Iteration 5: Remaining c-core coverage gaps

After precision work, address the ~70% of uncovered C-layer symbols. Many are
DEFSYMs (keywords/symbols) and internal functions. Focus on DEFUN/DEFVAR
entries that users actually call.

### 5a: Triage uncovered symbols

Run `./tart emacs-coverage -v --emacs-version 31.0` and categorise:

- **DEFSYM (keywords):** Most `:foo` symbols are keyword constants, not
  callable. These can be covered via `(defvar :foo keyword)` declarations in
  bulk, or accepted as a known gap.
- **Internal (`--` prefix):** Already excluded as private. Verify filter works.
- **GUI-only (ns.c, w32.c, etc.):** Defer — platform-specific.
- **Compilation (comp.c):** Low priority — native compilation internals.
- **Worth typing:** Remaining DEFUN entries in covered source files.

### 5b: Fill gaps in existing files

For each existing c-core `.tart` file, compare its `defun` count against the
source file's DEFUN count. Add missing entries, using multi-clause dispatch
where applicable.

### 5c: Add remaining high-value c-core files

Files not yet covered that have user-facing functions:
- `emacs.c` — `kill-emacs`, `invocation-name`, etc.
- `xselect.c` — clipboard/selection (X11, but widely used concepts)
- Verify all 54 existing files are comprehensive.

**Verify:**
```
Bash(command="./tart emacs-coverage --emacs-version 31.0 2>&1 | head -10")
Bash(command="nix develop --command dune test --force 2>&1")
```

---

## Iteration 6: Lisp-core expansion

### 6a: High-value lisp-core files to add or expand

Check existing coverage and add missing signatures:

| file | source | priority |
|:-----|:-------|:---------|
| `subr.tart` | `subr.el` | critical — audit completeness |
| `simple.tart` | `simple.el` | critical — audit completeness |
| `files.tart` | `files.el` | high — file operations |
| `faces.tart` | `faces.el` | high — face/font-lock |
| `custom.tart` | `custom.el` | medium — defcustom ecosystem |
| `minibuffer.tart` | `minibuffer.el` | medium — completion |
| `compile.tart` | `compile.el` | medium — compilation mode |
| `comint.tart` | `comint.el` | medium — process interaction |

### 6b: Third-party / ELPA packages

Lower priority, but useful for dogfooding:

| file | source | notes |
|:-----|:-------|:------|
| `map.tart` | `map.el` | already exists, audit precision |
| `seq.tart` | `seq.el` | already exists, audit precision |
| `eglot.tart` | `eglot.el` | already exists, needed for dogfooding |
| `flymake.tart` | `flymake.el` | already exists, audit |

**Verify:**
```
Bash(command="nix develop --command dune test --force 2>&1")
```

---

## Iteration 7: Version backfill

### 7a: Audit 30.1 vs 31.0 differences

- Diff `typings/emacs/31.0/c-core/` against Emacs 30.1 C source.
- Identify functions added in 31.0 (mark with `(available-since "31.0")`).
- Identify functions removed or changed.
- Update 30.1/ typings to be accurate copies minus 31.0-only additions.

### 7b: Audit 29.1 vs 30.1 differences

Same process for 29.1.

### 7c: Use `(include)` for shared signatures

Factor common signatures into shared files via `(include)` directive to avoid
duplication across versions.

**Verify:**
```
Bash(command="./tart check --emacs-version 30.1 test/fixtures/typing/core/arithmetic.el 2>&1")
Bash(command="./tart check --emacs-version 29.1 test/fixtures/typing/core/arithmetic.el 2>&1")
```

---

## Iteration 8: Sync and document

- Copy final 31.0 typings to 30.1/ and 29.1/ (minus version-specific diffs).
- Update `typings/emacs/BUGS.md` with all newly discovered gaps.
- Update `typings/emacs/31.0/BUGS.md` with version-specific items.
- Update Spec 76 deferred section with final status.
- Run full test suite.

**Verify:**
```
Bash(command="nix develop --command dune test --force 2>&1")
Bash(command="./tart emacs-coverage --emacs-version 31.0 2>&1 | head -5")
```
