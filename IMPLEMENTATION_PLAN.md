# Implementation Plan: Download Security ([Spec 65][s65])

Harden binary installation against `.dir-locals.el` attacks by
marking risky variables and tightening the version safe predicate.

All work targets `lisp/tart-mode.el`, `lisp/tart-mode-tests.el`,
and `specs/23-binary-installation.md`.

---

## Dependency Graph

```
Task 1 (risky-local-variable) ─┐
Task 2 (safe-version predicate) ├── Task 3 (spec cross-ref)
                                │
```

Tasks 1 and 2 are independent. Task 3 depends on both.

---

## Task 1 — Mark risky variables ([Spec 65, R1][s65])

**Problem:** `tart-executable` and `tart-install-directory` control
which binary runs and where binaries are written. A malicious
`.dir-locals.el` can override these without warning.

**Approach:** Add `risky-local-variable` property to both symbols
immediately after their `defcustom` forms:

```elisp
(put 'tart-executable 'risky-local-variable t)
(put 'tart-install-directory 'risky-local-variable t)
```

Add ERT tests verifying the property is set:

```elisp
(ert-deftest tart-mode-executable-is-risky-local-variable ()
  (should (get 'tart-executable 'risky-local-variable)))

(ert-deftest tart-mode-install-directory-is-risky-local-variable ()
  (should (get 'tart-install-directory 'risky-local-variable)))
```

**Files:** `lisp/tart-mode.el`, `lisp/tart-mode-tests.el`

---

## Task 2 — Tighten safe-version predicate ([Spec 65, R2][s65])

**Problem:** The current `:safe` predicate on `tart-version` accepts
any string (`(stringp v)`), allowing path-traversal payloads like
`"../../evil"` to reach the GitHub API URL.

**Approach:**

1. Extract a named predicate `tart--safe-version-p` that accepts
   `'latest` or strict semver (`X.Y.Z` with optional pre-release
   suffix matching `[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9][A-Za-z0-9.-]*)?`).

2. Replace the inline `:safe` lambda in `tart-version`'s `defcustom`
   with `#'tart--safe-version-p`.

3. Add ERT tests from the spec's verify section:
   - `(tart--safe-version-p "0.2.0")` → `t`
   - `(tart--safe-version-p "1.0.0-rc1")` → `t`
   - `(tart--safe-version-p "2.3.1-20250601")` → `t`
   - `(tart--safe-version-p 'latest)` → `t`
   - `(tart--safe-version-p "../../foo")` → `nil`
   - `(tart--safe-version-p "0.1")` → `nil`
   - `(tart--safe-version-p "")` → `nil`
   - `(tart--safe-version-p 42)` → `nil`

**Files:** `lisp/tart-mode.el`, `lisp/tart-mode-tests.el`

---

## Task 3 — Cross-reference specs ([Spec 65, task 3][s65])

**Problem:** [Spec 23][s23] R1 defined the original `:safe`
predicate. It should reference Spec 65 for the tightened version.

**Approach:** Add a note to the R1 section in Spec 23 referencing
Spec 65 for the hardened safe predicate.

**Files:** `specs/23-binary-installation.md`

---

## Risks

- **No risk of breakage:** Both changes are additive — existing
  valid values (`'latest`, semver strings) continue to work. Only
  malicious/malformed strings are newly rejected.
- **No test infrastructure needed:** Uses existing ERT test file.

[s65]: ./specs/65-download-security.md
[s23]: ./specs/23-binary-installation.md
