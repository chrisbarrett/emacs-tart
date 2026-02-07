# Spec 15: Explicit Forall Quantifiers

Require explicit universal quantifiers in polymorphic signatures.

**Dependencies:** [Spec 07](./07-signature-files.md)

## Goal

Since type constructors can be lowercase, implicit quantifier inference is
ambiguous. All polymorphic signatures must use explicit `[vars]` syntax.

## Constraints

- No implicit quantification
- Unbound type variables are errors

## Output

No new files; extends `lib/sig/sig_parser.ml` and related modules.

## Requirements

### R1: Explicit quantification required

**Given** a signature with type variables
**When** no `[vars]` quantifier is present
**Then** lowercase names are resolved as type constructors, not variables

```elisp
;; 'a' and 'b' are type constructor references, not variables
(defun seq-map (((a -> b)) (seq a)) -> (list b))

;; Polymorphic version requires explicit quantifier
(defun seq-map [a b] (((a -> b)) (seq a)) -> (list b))
```

**Verify:** `dune test`; unquantified lowercase names resolve as type refs

### R2: Unbound type variables error

**Given** a signature with an explicit `[vars]` quantifier
**When** an unbound type variable appears in the type
**Then** a parse/load error is emitted

```elisp
(defun foo [a] ((a -> b)) -> a)  ; Error: unbound type variable 'b'
```

**Verify:** `dune test`; unbound variable produces error

### R3: Literals vs type references

**Given** a type expression mixing lowercase names and quoted symbols
**When** parsed
**Then** lowercase names are type references; quoted symbols are literal types

```elisp
(type status ('pending | 'complete | a))  ; 'pending is literal, a is type ref
```

**Verify:** `dune test`; quoted symbols parsed as literals

## Non-Requirements

- Implicit quantifier inference (removed due to lowercase type constructor ambiguity)
- Kind inference (future HKT spec)
- Scoped type variables across signatures

## Tasks

- [x] [R1] Parse lowercase names as type references without quantifier
- [x] [R2] Error on unbound vars in explicit quantifier mode
- [x] [R3] Distinguish literals from type references
