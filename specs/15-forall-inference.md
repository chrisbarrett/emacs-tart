# Spec 15: Forall Quantifier Inference

Infer universal quantifiers from lowercase type variables in signatures.

**Dependencies:** Spec 07

## Goal

Allow type variables in signatures to be implicitly quantified, reducing
boilerplate while maintaining explicit quantification as an option.

## Constraints

- Quantifier order = left-to-right first occurrence
- Explicit `[vars]` disables inference (exclusive modes)

## Output

No new files; extends `lib/sig/sig_parser.ml` and related modules.

## Requirements

### R1: Implicit quantification

**Given** a signature with lowercase type variables and no explicit quantifier
**When** parsed
**Then** type variables are collected and quantified in left-to-right order

```elisp
;; Inferred as [a b]
(defun seq-map (((a -> b)) (seq a)) -> (list b))

;; Equivalent explicit form
(defun seq-map [a b] (((a -> b)) (seq a)) -> (list b))
```

**Verify:** `dune test`; signature parses with inferred `[a b]` quantifier

### R2: Explicit quantification disables inference

**Given** a signature with an explicit `[vars]` quantifier
**When** an unbound type variable appears in the type
**Then** a parse/load error is emitted

```elisp
(defun foo [a] ((a -> b)) -> a)  ; Error: unbound type variable 'b'
```

**Verify:** `dune test`; unbound variable in explicit mode produces error

### R3: Phantom types require explicit quantification

**Given** a type variable that appears only in the return type (phantom)
**When** no explicit quantifier is provided
**Then** inference still works (phantom is valid)

```elisp
(defun make-tagged int -> (tagged tag int))  ; Inferred as [tag]
```

**Verify:** `dune test`; phantom type variable is quantified

### R4: Literals vs type variables

**Given** a type expression mixing lowercase names and quoted symbols
**When** parsed
**Then** lowercase names are type variables; quoted symbols are literal types

```elisp
(type status ('pending | 'complete | a))  ; a is quantified, 'pending is literal
```

**Verify:** `dune test`; quoted symbols parsed as literals, not quantified

### R5: Nested arrows

**Given** a signature with nested arrow types
**When** type variables are collected
**Then** they are ordered by first occurrence across all nesting levels

```elisp
;; Inferred as [b c a] (first occurrence order)
(defun compose (((b -> c)) ((a -> b))) -> ((a -> c)))
```

**Verify:** `dune test`; quantifier order matches first occurrence

### R6: Deduplication across type applications

**Given** a type variable appearing multiple times
**When** collected for quantification
**Then** it appears exactly once in the quantifier list

```elisp
;; Single [a] despite multiple occurrences
(defun seq-find (((a -> bool)) (seq a)) -> (option a))
```

**Verify:** `dune test`; no duplicate type variables in quantifier

## Non-Requirements

- Kind inference (future HKT spec)
- Scoped type variables across signatures
- Explicit type instantiation

## Tasks

- [ ] [R1] Collect type vars; order by first occurrence
- [ ] [R2] Detect explicit quantifiers; error on unbound vars
- [ ] [R3] Handle phantom type variables
- [ ] [R4] Distinguish literals from type variables
- [ ] [R5] Traverse nested arrow types
- [ ] [R6] Deduplicate quantifier list

Run review agent after R1 and R2 work before proceeding to other specs.
