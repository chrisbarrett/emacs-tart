# Spec 76 — Typings Precision: c-core I/O & Editing Files

Improve the 4 next-priority c-core files by replacing `any` with precise types
where the type system supports it. Validate changes against the test suite.

Derived from [Spec 76](./specs/76-typings-distribution.md) deferred items and
[.ralph/IMPLEMENTATION_PLAN.md](./.ralph/IMPLEMENTATION_PLAN.md) Iteration 2.

---

## Task 1 — Precision audit: editfns.tart

Tighten signatures in `typings/emacs/31.0/c-core/editfns.tart`:

- `compare-buffer-substrings`: BUFFER1/BUFFER2 from `any` → `(buffer | string | nil)`
  (Emacs accepts buffer objects, names, or nil for current buffer)
- `replace-region-contents`: SOURCE from `any` → `((any -> any))` (it's a
  function that inserts replacement text)
- `translate-region-internal`: TABLE from `(string | any)` → `(string | char-table)`
- `verify-visited-file-modtime` (in editfns scope): skip — lives in fileio.tart

Note: most remaining `any` in editfns.tart are genuinely dynamic (`format` rest
args, special-form bodies, boolean flags using non-nil convention). Leave those.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 2 — Precision audit: fileio.tart

Tighten signatures in `typings/emacs/31.0/c-core/fileio.tart`:

- `expand-file-name`: DEFAULT-DIRECTORY from `string` → `(string | nil)` (nil
  means use `default-directory`)
- `write-region`: START/END from `any` → `((int | string | nil))` — Emacs
  accepts positions, nil (meaning point-min/point-max), or a string to write
  directly. VISIT from `any` → `(bool | string | nil)`. APPEND from `any` →
  `(bool | int | nil)` (non-nil or byte offset)
- `insert-file-contents`: VISIT from `any` → `bool`, REPLACE from `any` →
  `bool`
- `verify-visited-file-modtime`: BUF from `any` → `(buffer | nil)`
- `set-binary-mode`: MODE from `any` → `bool`, return from `any` → `bool`
- `make-temp-file-internal`: DIR-FLAG from `any` → `bool`
- `car-less-than-car`: params from `any` → `((cons num any) (cons num any))`

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 3 — Precision audit: buffer.tart

Tighten signatures in `typings/emacs/31.0/c-core/buffer.tart`:

- `buffer-live-p`: multi-clause predicate `((buffer) -> bool) ((_) -> nil)`
- `get-buffer`: multi-clause `((buffer) -> buffer) ((string) -> (buffer | nil))`
- `get-buffer-create`: INHIBIT-BUFFER-HOOKS from `any` → `bool`
- `set-buffer-modified-p`: FLAG from `any` → `bool`
- `restore-buffer-modified-p`: FLAG from `any` → `bool`
- `set-buffer-multibyte`: FLAG from `any` → `bool`, return from `any` → `bool`

Note: overlay property values (`overlay-get`/`overlay-put`), `buffer-local-value`
return, and `buffer-local-variables` return are genuinely dynamic. Leave as `any`.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 4 — Precision audit: search.tart

Tighten signatures in `typings/emacs/31.0/c-core/search.tart`:

- `replace-match`: FIXEDCASE from `any` → `bool`, LITERAL from `any` → `bool`
- `newline-cache-check`: BUFFER from `any` → `(buffer | nil)`, return from
  `(any | nil)` → `((list any) | nil)`

Note: INHIBIT-MODIFY and NOERROR params are boolean flags used with non-nil
convention in Emacs — `any` is the correct representation since callers pass
arbitrary non-nil values (not just `t`). Leave these as `any`.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 5 — Verify and document

- Run full test suite
- Update `typings/emacs/BUGS.md` if any new untypeable cases are found
- No 29.1/30.1 version dirs exist; sync not needed

**Verify:** `nix develop --command dune test --force 2>&1`
