# Known Issues and Gaps

This document tracks known limitations and bugs in the tart type checker.

## Type System Gaps

### BUG-001: No union subtyping

**Status:** Open
**Severity:** High
**Impact:** Many Emacs C-core signatures expect `(T | nil)` but callers often
pass just `T`. This causes false positive type errors.

**Example:**
```elisp
(message "Hello")  ; Error: expected (string | nil), found string
```

**Root cause:** The type system uses strict equality for unification. When
comparing `string` with `(string | nil)`, unification fails because they are
structurally different types.

**Expected behavior:** `T` should be a subtype of `(T | nil)`. The call above
should succeed.

**Workaround:** None currently. Signatures can be loosened (e.g., `string`
instead of `(string | nil)`), but this loses precision.

**Fix required:**
1. Add subtype checking during unification
2. `T <: (T | U)` for any T, U
3. Update constraint solving to use subtype constraints where appropriate

---

## C-Core Typing Gaps

### GAP-001: Functions not in c-core yet

The following commonly used functions are defined in Emacs C source files not
yet typed:

- `start-process`, `call-process` — in callproc.c (not process.c)
- `sit-for` — in dispnew.c (not keyboard.c)
- `yes-or-no-p`, `y-or-n-p` — in fns.c (not minibuf.c)
- `propertize` — in fns.c (not textprop.c)
- `message`, `format` — in editfns.c (covered, but may need attention)

---

## Parser Gaps

(None documented yet)

---

## LSP Gaps

(None documented yet)
