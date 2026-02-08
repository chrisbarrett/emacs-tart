# Spec 76 — Validation Against Emacs lisp/

Validate the tightened signatures from Iterations 1–3 by running `./tart check`
against real Emacs Lisp source files. Fix signature bugs, document type system
gaps, and add regression fixtures for any parser issues discovered.

Derived from [Spec 76](./specs/76-typings-distribution.md) deferred items and
[.ralph/IMPLEMENTATION_PLAN.md](./.ralph/IMPLEMENTATION_PLAN.md) Iteration 4.

Source files are obtained by decompressing Emacs 30.2 `.el.gz` from the Nix
store to `/tmp/tart-validation/`. The `--emacs-version 31.0` flag is used
since typings target 31.0 (signatures are forward-compatible with 30.x for this
validation pass).

---

## Task 1 — Validate seq.el and cl-lib.el

These two files exercise the polymorphic signatures added in Iteration 3.

Decompress from Nix store and run:
```
./tart check --emacs-version 31.0 /tmp/tart-validation/seq.el
./tart check --emacs-version 31.0 /tmp/tart-validation/cl-lib.el
```

For each error:
- **Signature bug** → fix the `.tart` file
- **Type system gap** → document in `typings/emacs/BUGS.md`
- **Parser issue** → add regression fixture under `test/fixtures/typing/`

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 2 — Validate subr.el and simple.el

These two files exercise the core lisp-core signatures tightened in Iteration 3.

```
./tart check --emacs-version 31.0 /tmp/tart-validation/subr.el
./tart check --emacs-version 31.0 /tmp/tart-validation/simple.el
```

Same triage process: fix signature bugs, document gaps, add fixtures.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 3 — Validate files.el and startup.el

These files heavily exercise fileio.tart, buffer.tart, and editfns.tart
signatures from Iteration 2.

```
./tart check --emacs-version 31.0 /tmp/tart-validation/files.el
./tart check --emacs-version 31.0 /tmp/tart-validation/startup.el
```

Same triage process.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 4 — Validate minibuffer.el and window.el

```
./tart check --emacs-version 31.0 /tmp/tart-validation/minibuffer.el
./tart check --emacs-version 31.0 /tmp/tart-validation/window.el
```

Same triage process.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 5 — Update BUGS.md and verify

- Review all newly discovered gaps from Tasks 1–4
- Consolidate entries in `typings/emacs/BUGS.md`
- Run full test suite to confirm no regressions
- Count remaining errors per file and document status

**Verify:** `nix develop --command dune test --force 2>&1`
