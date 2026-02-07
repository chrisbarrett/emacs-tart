# Spec 46: Truthiness-Aware Union Semantics

Union type inference for branching forms with truthiness tracking.

**Dependencies:** [Spec 34][] (R14 union-from-branches), [Spec 13][] (error format),
[Spec 48][] (prelude: `truthy`, `nil`, `t`, `bool`)

## Links

### Deps
[Spec 13]: ./13-error-reporting.md
[Spec 34]: ./34-funcall-apply-typing.md
[Spec 48]: ./48-prelude.md

### Blocks
[Spec 49]: ./49-feature-guards.md
[Spec 52]: ./52-type-predicates.md

## Goal

Infer union types from `and`/`or`/`not` using the type system's truthiness
distinction. Declared return types constrain inferred unions; errors point to
offending branches.

## Constraints

- **Accurate**: Model actual Elisp runtime behavior
- **Tractable**: Return type annotations provide expected-type context
- **Actionable**: Errors identify which branch violates the constraint

## Output

Extends `lib/typing/infer.ml` (union construction), `lib/typing/diagnostic.ml`
(branch errors).

## Requirements

### R1: `or` with truthy argument short-circuits

**Given**:
```elisp
(or a b)  ; a : truthy
```
**When** type-checked
**Then** result type is `A` (b is unreachable)

**Verify:** `dune test`; truthy first arg means no union

### R2: `or` with nullable arguments

**Given**:
```elisp
(or a b)  ; a : (Int | nil), b : String
```
**When** type-checked
**Then** result type is `(Int | String)`

**Verify:** `dune test`; nil stripped from non-final branches

### R3: `or` all nullable

**Given**:
```elisp
(or a b)  ; a : (Int | nil), b : (String | nil)
```
**When** type-checked
**Then** result type is `(Int | String | nil)`

**Verify:** `dune test`; nil preserved when final branch nullable

### R4: `and` with truthy arguments

**Given**:
```elisp
(and a b)  ; a : truthy, b : truthy
```
**When** type-checked
**Then** result type is `B` (last argument)

**Verify:** `dune test`; all truthy means result is last arg type

### R5: `and` with nullable argument

**Given**:
```elisp
(and a b)  ; a : (Int | nil), b : String
```
**When** type-checked
**Then** result type is `(String | nil)`

**Verify:** `dune test`; nullable arg adds nil to result

### R5a: `and` with nil argument short-circuits

**Given**:
```elisp
(and a b)  ; a : nil, b : String
```
**When** type-checked
**Then** result type is `nil` (b is unreachable)

**Verify:** `dune test`; nil arg means result is nil

### R6: `not` inverts truthiness

**Given**:
```elisp
(not x)  ; x : truthy
```
**When** type-checked
**Then** result type is `nil`

**Given**:
```elisp
(not x)  ; x : nil
```
**When** type-checked
**Then** result type is `t`

**Given**:
```elisp
(not x)  ; x : (Int | nil)
```
**When** type-checked
**Then** result type is `bool`

**Verify:** `dune test`; not inverts truthiness correctly

### R7: Union violates declared return type

**Given**:
```elisp
;; (-> (Int) Int)
(defun describe-n (n)
  (if (> n 0)
      n
      "negative"))
```
**When** type-checked
**Then** error:
```
error[E0308]: branch type incompatible with return type
  --> utils.el:5:7
   |
 5 |       "negative"
   |       ^^^^^^^^^^ this branch has type: String
   |
note: function declared to return Int
  --> utils.el:1:1
   |
 1 | ;; (-> (Int) Int)
   |              ^^^ expected return type
```

**Verify:** `dune test`; error points to offending branch and declaration

### R8: `cond` emits union of all branches

**Given**:
```elisp
(cond
  ((< n 0) "negative")
  ((= n 0) 'zero)
  (t n))
```
**When** type-checked
**Then** result type is `(String | Symbol | Int)`

**Verify:** `dune test`; cond unions all branch types

### R9: `cond` without default clause

**Given**:
```elisp
(cond
  ((< n 0) "negative")
  ((= n 0) 'zero))
```
**When** type-checked
**Then** result type is `(String | Symbol | nil)`

**Verify:** `dune test`; implicit nil for non-exhaustive cond

### R10: `if` without else

**Given**:
```elisp
(if (> n 0) n)
```
**When** type-checked
**Then** result type is `(Int | nil)`

**Verify:** `dune test`; missing else adds nil to union

## Checklist

- [x] [R1] or short-circuits on truthy
- [x] [R2] or strips nil from non-final
- [x] [R3] or preserves nil from final
- [x] [R4] and returns last when all truthy
- [x] [R5] and adds nil when any nullable
- [x] [R5a] and short-circuits on nil
- [x] [R6] not inverts truthiness
- [x] [R7] Branch error points to offending + declaration
- [x] [R8] cond unions branches
- [x] [R9] cond implicit nil
- [x] [R10] if without else adds nil

**Status:** Complete
