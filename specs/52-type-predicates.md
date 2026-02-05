# Spec 52: Type Predicates

Declare functions as type predicates that narrow types in conditional branches.

**Deps:** Spec 34, 46, 11

## Goal

Enable `stringp`, `listp`, and user-defined predicates to narrow types in conditionals via signature declarations (not hardcoded).

## Syntax

```lisp
(defun stringp (x) -> (x is string))
(defun listp (x) -> (x is (list any)))
```

Parameter name before `is` must match signature. Type after `is` is narrowed type when truthy.

## Constraints

| Constraint    | Detail                                    |
| ------------- | ----------------------------------------- |
| Declarative   | Declared in signatures, not hardcoded     |
| Composable    | Works with unions, negation               |
| Extensible    | User predicates supported                 |
| Compatible    | Non-predicate functions unchanged         |

## Requirements

### R1-R2: Basic narrowing

```elisp
(if (stringp x)
    (upcase x)    ; x : string
  (other x))      ; x : (original - string)
```

Then-branch: type intersection. Else-branch: type subtraction.

### R3: Cumulative narrowing in cond

```elisp
(cond
  ((stringp x) ...)   ; x : string
  ((listp x) ...)     ; x : list, already ¬string
  ((symbolp x) ...))  ; x : symbol, already ¬string ¬list
```

### R4: Predicates in `and`

```elisp
(when (and (listp x) (not (null x)))
  (car x))   ; x : nonempty list
```

All narrowings combine.

### R5: Predicates in `or` early-exit

```elisp
(or (stringp x) (error "Expected string"))
(upcase x)   ; x : string
```

### R6-R7: User predicates, specific parameter

```lisp
(defun my-thing-p (x) -> (x is my-thing))
(defun string-or-null-p (x default) -> (x is (string | nil)))
```

Only named parameter narrowed.

### R8: Union narrowing

```lisp
(defun number-or-marker-p (x) -> (x is (num | marker)))
```

Input `(string | int | float | marker)` → narrowed `(int | float | marker)`.

### R9: Nested checks accumulate

### R10: `(x is T)` is subtype of `bool`

### R11: Invalid parameter error

```lisp
(defun bad (x) -> (y is string))  ; Error: 'y' not in params
```

### R12: Inline-only narrowing

```elisp
(let ((result (stringp x)))
  (when result ...))   ; x NOT narrowed
```

Stored results don't narrow (same as Spec 49 R17).

## Standard Library Predicates

Declare in `typings/emacs/*/c-core/data.tart`:

```lisp
;; Type predicates
(defun stringp (x) -> (x is string))
(defun symbolp (x) -> (x is symbol))
(defun integerp (x) -> (x is int))
(defun floatp (x) -> (x is float))
(defun numberp (x) -> (x is num))
(defun consp (x) -> (x is (cons any any)))
(defun listp (x) -> (x is (list any)))
(defun vectorp (x) -> (x is (vector any)))
(defun sequencep (x) -> (x is (list any | vector any | string)))
(defun functionp (x) -> (x is (-> (&rest any) any)))
(defun keywordp (x) -> (x is keyword))
(defun hash-table-p (x) -> (x is (hash-table any any)))
(defun bufferp (x) -> (x is buffer))
(defun windowp (x) -> (x is window))
(defun framep (x) -> (x is frame))
(defun processp (x) -> (x is process))
(defun markerp (x) -> (x is marker))
(defun overlayp (x) -> (x is overlay))

;; Nil/null
(defun null (x) -> (x is nil))
(defun booleanp (x) -> (x is bool))

;; Compound
(defun atom (x) -> (x is (any - (cons any any))))
(defun number-or-marker-p (x) -> (x is (num | marker)))
```

## Non-Requirements

- Multi-parameter narrowing
- Conditional narrowing (A if arg1, B if arg2)
- Automatic inference from body
- Composition syntax

## Design Notes

### Implementation

1. Parse `(x is T)` in `sig_parser.ml`
2. Add `TPredicate of string * typ` to return type AST
3. Track predicate info in function schemes
4. In `infer.ml` for `if`/`cond`/`when`/`unless`:
   - Detect predicate calls in condition
   - Apply intersection in then-branch
   - Apply subtraction in else-branch
5. Narrowing is lexical scope

### Type Operations

```
narrow(T, P→S) = T ∩ S
negate(T, P→S) = T - S
```

### Integration with Feature Guards (Spec 49)

Same narrowing infrastructure, different tracking:
- Feature guards: `feature_env` → available symbols
- Type predicates: `type_env` → narrowed types per variable

## Tasks

- [x] Parse `(x is T)` return syntax
- [x] Named parameter syntax for signatures `((name type))`
- [x] Register predicate info in sig_loader
- [ ] Predicate narrowing in if/when/unless
- [ ] Type subtraction for else branches
- [ ] Cumulative narrowing in cond
- [ ] Predicates in and/or expressions
- [ ] User-defined predicates
- [ ] Union intersection for narrowing
- [ ] Validate predicate parameter names
- [ ] Inline-only restriction
- [ ] Standard library declarations

**Status:** Parsing infrastructure complete. `STPredicate` AST node added, named parameter syntax `((x type))` parsed, predicate info extracted and registered during signature loading. Remaining: apply narrowing in conditional branches.
