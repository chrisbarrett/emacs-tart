# Spec 11: Union Types and Row Polymorphism

Type-safe structural typing for Elisp's idiomatic alist/plist patterns.

**Dependencies:** Spec 07 (signature files), Spec 46 (truthiness-aware unions)

## Goal

Provide sum types and record types via unions and row polymorphism, matching
Elisp's native use of alists and plists rather than introducing foreign ADT
representations.

## Rationale

Elisp idiomatically represents structured data as:

- **Alists** for records: `((name . "Alice") (age . 30))`
- **Plists** for keyword-based records: `(:name "Alice" :age 30)`
- **Tagged values** for variants: `(ok . value)` or `(err . message)`

Traditional ADTs would impose foreign runtime representations. Instead, unions
and row polymorphism type-check existing Elisp patterns directly.

## Constraints

- **Zero overhead**: Types describe existing Elisp structures, no wrappers
- **Exhaustiveness**: Pattern matches on unions must cover all cases
- **Row polymorphism**: Functions accept records with "at least these fields"
- **pcase integration**: Work with existing `pcase` destructuring

## Output

```
tart/
├── lib/
│   ├── union.ml           ; Union type representation and subtyping
│   ├── row.ml             ; Row polymorphism for map-like types
│   └── exhaustiveness.ml  ; Pattern match exhaustiveness checking
└── test/
    ├── union_test.ml
    ├── row_test.ml
    └── exhaustiveness_test.ml
```

## Union Types

Tagged unions represent sum types using Elisp's tagged cons idiom:

```lisp
;; Type declaration in .tart
(type result [a e]
  ((ok . a) | (err . e)))

;; Elisp usage—no special constructors needed
(cons 'ok value)   ; Creates (ok . value)
(cons 'err msg)    ; Creates (err . msg)
```

## Row Polymorphism

Row-polymorphic records type alists, plists, and hash-tables with extensibility:

```lisp
;; Type accepts any alist with at least 'name and 'age fields
((alist (name . string) (age . int) & r) -> string)

;; Works with:
((name . "Alice") (age . 30))
((name . "Bob") (age . 25) (email . "bob@example.com"))
```

## Requirements

### R1: Union type declarations

**Given** a union type in `.tart`:
```lisp
(type either [a e] ((ok . a) | (err . e))
```
**When** values are type-checked
**Then** `(cons 'ok x)` unifies to `either a _`

**Verify:** `dune test`; union construction type-checks correctly

### R2: Union narrowing in pcase

**Given** a `pcase` on a union type:
```lisp
(pcase result
  (`(ok . ,value) (use value))
  (`(err . ,msg) (report msg)))
```
**When** type-checked
**Then** `value` has type `a` in the ok branch, `msg` has type `e` in err branch

**Verify:** `dune test`; type narrowing applies in pattern branches

### R3: Exhaustiveness checking for unions

**Given** a non-exhaustive match on a union:
```lisp
(pcase opt
  (`(some . ,x) x))  ; Missing: nil case
```
**When** `opt` has type where any case can be nil
**Then** warning: "Non-exhaustive pattern match. Missing: nil"

**When** `opt` has type where all cases unify with truthy
**Then** no warning—single branch is exhaustive

**Verify:** `dune test`; exhaustiveness respects truthiness refinements

### R4: Row-polymorphic alist types

**Given** a function typed with row polymorphism:
```lisp
(defun get-name (person)
  (declare (tart ((alist (name . string) & r) -> string)))
  (alist-get 'name person))
```
**When** called with an alist having extra fields
**Then** type-checks successfully

**Verify:** `dune test`; extra fields don't cause type errors

### R5: Row-polymorphic plist types

**Given** a function typed for plists:
```lisp
(defun get-title (item)
  (declare (tart ((plist :title string & r) -> string)))
  (plist-get item :title))
```
**When** called with a plist having extra properties
**Then** type-checks successfully

**Verify:** `dune test`; plist row polymorphism works

### R6: Row-polymorphic field access via map patterns

Standard `pcase` is purely structural—use `map` patterns for row-polymorphic access:

```lisp
(require 'map)

(pcase-let (((map :name (:age a)) person))
  ...)
```

**Given** a `map` pattern extracting fields from a row-polymorphic type
**When** the type has more fields than matched
**Then** no warning—extra fields are permitted by row polymorphism

**When** matching on a field not in the type
**Then** type error: field not present

**Verify:** `dune test`; map patterns integrate with row types

### R7: Unions are closed; row polymorphism is for map-like types

Unions are always closed (exhaustive). Row polymorphism (`& r`) applies only to
map-like type constructors (`alist`, `plist`, `hash-table`), not to unions.

```lisp
;; Closed union—only 'a and 'b are valid
(type my-tag ('a | 'b))

;; Row-polymorphic types—extra fields permitted
(type named-alist (alist (name . string) & r))
(type titled-plist (plist :title string & r))
(type keyed-hash (hash-table (id . int) & r))
```

**Given** a value matched against a union type
**Then** exhaustiveness checking requires all variants covered

**Given** a value matched against a row-polymorphic alist/plist/hash-table
**Then** extra fields beyond those specified are permitted

**Verify:** `dune test`; unions are exhaustive, map-like types are extensible

## Tasks

- [ ] [R1] Implement union type representation and subtyping
- [ ] [R2] Implement type narrowing in pcase branches
- [ ] [R3] Implement exhaustiveness checking for unions
- [ ] [R4] Implement row-polymorphic alist types
- [ ] [R5] Implement row-polymorphic plist types
- [ ] [R6] Handle structural pattern exhaustiveness correctly
- [ ] [R7] Distinguish closed and open unions
