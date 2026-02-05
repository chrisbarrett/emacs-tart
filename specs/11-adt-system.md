# Spec 11: Union Types and Row Polymorphism

Type-safe structural typing for Elisp's idiomatic alist/plist patterns.

**Dependencies:** Spec 07 (signature files), Spec 46 (truthiness-aware unions),
Spec 48 (prelude types: `list`, `option`, `is`, `nonempty`)

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

Row types use `{ ... }` syntax. Map-like constructors come in two forms:

```lisp
;; Homogeneous: all keys/values share types
(alist k v)                      ; e.g., (alist symbol int)
(plist k v)                      ; e.g., (plist keyword string)
(hash-table k v)                 ; e.g., (hash-table string int)

;; Record-style: specific fields via row types
(alist {name string age int})    ; closed—mustly these fields
(alist {name string & r})        ; open—at least these fields
(plist {:name string :age int})  ; closed plist
(plist {:name string & r})       ; open plist
(hash-table {id int & r})        ; open hash-table

;; Generic map supertype
(map {name string & r})          ; any map-like with these fields
```

Example function accepting any map with at least a `name` field:

```lisp
(defun get-name (person)
  (declare (tart ((map {name string & r}) -> string)))
  (map-elt person 'name))

;; Works with:
((name . "Alice") (age . 30))                    ; alist
(:name "Alice" :age 30)                          ; plist
#s(hash-table data (name "Alice" age 30))        ; hash-table
```

## Requirements

### R1: Union type declarations

**Given** a union type in `.tart`:

```lisp
(type either [a e] ((ok . a) | (err . e))
```

**When** values are type-checked **Then** `(cons 'ok x)` unifies to `either a _`

**Verify:** `dune test`; union construction type-checks correctly

### R2: Union narrowing in pcase

**Given** a `pcase` on a union type:

```lisp
(pcase result
  (`(ok . ,value) (use value))
  (`(err . ,msg) (report msg)))
```

**When** type-checked **Then** `value` has type `a` in the ok branch, `msg` has
type `e` in err branch

**Verify:** `dune test`; type narrowing applies in pattern branches

### R3: Exhaustiveness checking for unions

**Given** a non-exhaustive match on a union:

```lisp
(pcase opt
  (`(some . ,x) x))  ; Missing: nil case
```

**When** `opt` has type where any case can be nil **Then** warning:
"Non-exhaustive pattern match. Missing: nil"

**When** `opt` has type where all cases unify with truthy **Then** no
warning—single branch is exhaustive

**Verify:** `dune test`; exhaustiveness respects truthiness refinements

### R4: Row-polymorphic alist types

**Given** a function typed with row polymorphism:

```lisp
(defun get-name (person)
  (declare (tart ((alist {name string & r}) -> string)))
  (alist-get 'name person))
```

**When** called with an alist having extra fields **Then** type-checks
successfully

**Verify:** `dune test`; extra fields don't cause type errors

### R5: Row-polymorphic plist types

**Given** a function typed for plists:

```lisp
(defun get-title (item)
  (declare (tart ((plist {:title string & r}) -> string)))
  (plist-get item :title))
```

**When** called with a plist having extra properties **Then** type-checks
successfully

**Verify:** `dune test`; plist row polymorphism works

### R6: Row-polymorphic field access via map patterns

Standard `pcase` is purely structural—use `map` patterns for row-polymorphic
access:

```lisp
(require 'map)

(pcase-let (((map :name (:age a)) person))
  ...)
```

**Given** a `map` pattern extracting fields from a row-polymorphic type **When**
the type has more fields than matched **Then** no warning—extra fields are
permitted by row polymorphism

**When** matching on a field not in the type **Then** type error: field not
present

**Verify:** `dune test`; map patterns integrate with row types

### R7: Unions are closed; row polymorphism is for map-like types

Unions are always closed (exhaustive). Row types (`{ ... & r }`) apply only to
map-like type constructors (`alist`, `plist`, `hash-table`, `map`), not unions.

```lisp
;; Closed union—only 'a and 'b are valid
(type my-tag ('a | 'b))

;; Homogeneous map types
(type symbol-map (alist symbol string))
(type keyword-config (plist keyword int))

;; Row-polymorphic record types—extra fields permitted
(type named [r] (alist {name string & r}))
(type titled [r] (plist {:title string & r}))
(type identifiable [r] (map {id int & r}))
```

**Given** a value matched against a union type **Then** exhaustiveness checking
requires all variants covered

**Given** a value matched against a row-polymorphic map type **Then** extra
fields beyond those specified are permitted

**Verify:** `dune test`; unions are exhaustive, map types are extensible

### R8: Row type inference from field access

**Literal keys → record-style inference:**

```lisp
(defun get-name (x)
  (alist-get 'name x))
```

Infer `x : (alist {name a & r})`, return type `a`.

```lisp
(defun summarize (person)
  (format "%s is %d" (alist-get 'name person) (alist-get 'age person)))
```

Infer `person : (alist {name string age int & r})`.

**Variable keys → homogeneous inference:**

```lisp
(defun lookup (m key)
  (alist-get key m))
```

Infer `m : (alist k v)`, `key : k`, return type `v`.

**Mixed access:**

```lisp
(defun get-or-lookup (m key)
  (or (alist-get 'default m)
      (alist-get key m)))
```

Infer `m : (alist k v)` with constraint `k` includes `'default`.

**Verify:** `dune test`; literal keys infer records, variable keys infer
homogeneous

### R9: Literal types and deferred widening

Literals have literal types that widen via subtyping only at unification:

```lisp
(let ((x 1) (y 1.0))
  (list x y))
```

Within the `let`: `x : 1`, `y : 1.0` (literal types preserved).

At usage site, type depends on context:

| Context expects | Result type                 |
| --------------- | --------------------------- |
| `(list num)`    | `(list num)` via `1 <: num` |
| must structure  | `(cons 1 (cons 1.0 nil))`   |
| `truthy`        | `truthy`                    |
| unconstrained   | principal type preserved    |

**Subtyping lattice (no implicit `any`):**

```
   truthy              nil
    / | \               |
 num  ...  (cons a b)  ()    <- empty list
 / \
int float
 |    |
 1   1.0   (literal types at bottom)
```

`truthy` and `nil` are distinct top-types that do not unify. The prelude (Spec
48) defines `(type any (truthy | nil))`. The type system may not generate
`(truthy | nil)` as a unification—it is always explicitly annotated.

**List/nil resolution via type subtraction** (prelude types from Spec 48):

```lisp
(type list [t] ((cons t (list t)) | nil))
(type is [t] (t - nil))              ; remove nil from union
(type option [(t : truthy)] (t | nil))  ; add nil (inverse of is)

(type nonempty [t] (is (list t)))    ; (cons t (list t))
(type nonempty [t] ((list t) - nil))    ; equivalent
```

Relationships:

- `(nonempty t) <: truthy` — non-nil by construction
- `(nonempty t) <: (list t)` — subtype of list
- `(list t) <: (option (nonempty t))` — list is optionally non-empty

**Verify:** `dune test`; literals widen only when usage demands

### R10: Type subtraction

Type subtraction `(a - b)` removes `b` from union `a`:

```lisp
((int | string) - int)        ; => string
((truthy | nil) - nil)        ; => truthy
((cons t (list t)) | nil) - nil  ; => (cons t (list t))
```

**Given** a type subtraction `(a - b)` **When** `b` is a member of the union `a`
**Then** result is `a` with `b` removed

**When** `b` is not in `a` **Then** result is `a` unchanged (or type error—TBD)

**When** `a - b` would produce an empty type (no inhabitants) **Then** type
error: "empty type"

Standard library types using subtraction:

```lisp
(type is [t] (t - nil))
(type nonempty [t] (is (list t)))
```

**Verify:** `dune test`; subtraction removes union members correctly

### R11: Row unification rules

Row types unify by matching fields and threading row variables:

```
{name string & r1} ~ {name string age int}
  ⟹ r1 = {age int}

{name string & r1} ~ {name string age int & r2}
  ⟹ r1 = {age int & r2}

{name string} ~ {name string age int}
  ⟹ FAIL (closed row rejects extra fields)
```

**Verify:** `dune test`; row unification handles open/closed correctly

### R12: Generic map supertype

**Given** a function typed with `map`:

```lisp
(defun get-id (x)
  (declare (tart ((map {id int & r}) -> int)))
  (map-elt x 'id))
```

**When** called with an alist, plist, or hash-table having an `id` field
**Then** type-checks successfully for all three

**Verify:** `dune test`; `map` accepts alist, plist, and hash-table subtypes

### R13: Map type forms

Three forms for map-like types:

```lisp
(alist)                    ; inferred from field access
(alist k v)                ; homogeneous: all keys type k, all values type v
(alist {name string & r})  ; record: specific fields, optional row variable
```

**Given** a bare `(alist)` in a signature **When** the function body accesses
fields **Then** row type is inferred from access patterns

**Given** a homogeneous `(alist symbol int)` **Then** any alist with symbol keys
and int values is accepted

**Given** a record `(alist {name string age int})` **Then** only alists with
mustly those fields are accepted

**Given** an open record `(alist {name string & r})` **Then** alists with at
least a `name` field are accepted

**Verify:** `dune test`; all three forms work correctly

## Tasks

- [x] [R1] Implement union type representation and subtyping
- [x] [R2] Implement type narrowing in pcase branches
- [x] [R3] Implement exhaustiveness checking for unions
- [ ] [R4] Implement row-polymorphic alist types
- [ ] [R5] Implement row-polymorphic plist types
- [ ] [R6] Handle map pattern exhaustiveness correctly
- [ ] [R7] Distinguish closed unions from open row types
- [ ] [R8] Infer row types from field access (literal vs variable keys)
- [ ] [R9] Implement literal types with deferred widening
- [ ] [R10] Implement type subtraction operator
- [ ] [R11] Implement row unification rules
- [ ] [R12] Implement generic `map` supertype
- [ ] [R13] Implement all map type forms (bare, homogeneous, record)

**Status:** Union types implemented (`TUnion` in `lib/core/types.mli`), pcase narrowing in `lib/typing/infer.ml`, exhaustiveness in `lib/typing/exhaustiveness.ml`. Row polymorphism (R4-R13) not yet implemented.
