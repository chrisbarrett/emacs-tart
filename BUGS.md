# Known Issues and Gaps

This document tracks known limitations and bugs in the tart type checker.

## Type System Gaps

### BUG-001: No union subtyping

**Status:** FIXED
**Severity:** High
**Impact:** Many Emacs C-core signatures expect `(T | nil)` but callers often
pass just `T`. This caused false positive type errors.

**Example:**
```elisp
(message "Hello")  ; Previously: Error: expected (string | nil), found string
                   ; Now: OK (string matches (string | nil))
```

**Fix implemented:** Added asymmetric union handling in `lib/typing/unify.ml`:
1. `TUnion ts, t` (union expected, non-union provided): succeeds if t matches
   any member of the union. This is the safe direction.
2. `t, TUnion ts` (non-union expected, union provided): fails, preserving
   soundness (nil might be passed at runtime).

Added `is_union : typ -> bool` helper to `lib/core/types.ml`

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
