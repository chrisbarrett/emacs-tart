# Spec 15: Forall Quantifier Inference

Infer universal quantifiers from lowercase type variables in signatures.

**Dependencies:** Spec 07

## Constraints

- Quantifier order = left-to-right first occurrence
- Explicit `[vars]` disables inference (exclusive modes)

## Requirements

### R1: Implicit quantification

```elisp
;; Inferred as [a b]
(defun seq-map ((a -> b) (Seq a)) -> (List b))

;; Equivalent explicit form
(defun seq-map [a b] ((a -> b) (Seq a)) -> (List b))
```

### R2: Explicit quantification disables inference

```elisp
(defun foo [a] (a -> b) -> a)  ; Error: unbound type variable 'b'
```

### R3: Phantom types require explicit quantification

```elisp
(defun make-tagged [tag] Int -> (Tagged tag Int))
```

### R4: Literals vs type variables

Existing syntax distinguishes: `a` = type variable, `'foo` = literal symbol.

```elisp
(type Status (Or 'pending 'complete a))  ; a is quantified
```

### R5: Nested arrows

```elisp
;; Inferred as [b c a] (first occurrence order)
(defun compose ((b -> c) (a -> b)) -> (a -> c))
```

### R6: Deduplication across type applications

```elisp
;; Single [a] despite multiple occurrences
(defun seq-find ((a -> Bool) (Seq a)) -> (Option a))
```

## Non-Requirements

- Kind inference (future HKT spec)
- Scoped type variables across signatures
- Explicit type instantiation

## Tasks

- [ ] [R1] Collect type vars; order by first occurrence
- [ ] [R2] Detect explicit quantifiers; error on unbound vars
- [ ] [R5] Traverse nested arrow types
- [ ] [R6] Deduplicate quantifier list
