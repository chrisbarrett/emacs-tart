# Spec 12: Module Boundary Rules

Rules for type checking at module boundaries and handling untyped code.

**Dependencies:** Spec 07 (signature files), Spec 08 (LSP for trigger)

## Goal

Define when type checking is triggered, how typed and untyped code interact,
and what constitutes the public interface of a module.

## Constraints

- **Opt-in**: Only files with `.eli` siblings are type-checked
- **Gradual**: Typed code can call untyped via `require/typed`
- **Sound within typed world**: No `any` escape hatch

## Requirements

### R1: Type checking trigger

**Given** an `.el` file
**When** opened in an LSP-connected editor
**Then** type checking is triggered iff a sibling `.eli` file exists

| File exists?     | Type-checked? |
|------------------|---------------|
| `foo.el` only    | No            |
| `foo.el` + `foo.eli` | Yes       |

**Verify:** No diagnostics for files without `.eli`; diagnostics for files with

### R2: Signature verification

**Given** `foo.eli` declares:
```elisp
(sig foo-add (-> (Int Int) Int))
```
**And** `foo.el` defines:
```elisp
(defun foo-add (a b) (concat a b))  ; Wrong: returns String
```
**When** type-checked
**Then** error: signature mismatch

**Verify:** Mismatched implementations produce errors

### R3: Typed calling untyped

**Given** typed module imports untyped:
```elisp
;; my-app.eli
(require/typed external-lib
  (external-lib-process : (-> (String) String)))
```
**And** `my-app.el` calls `external-lib-process`
**When** type-checked
**Then** the call is checked against the declared signature

**Verify:** Wrong argument types produce errors

### R4: Untyped calling typed

**Given** an untyped module calls a typed module
**When** the typed module is compiled
**Then** no checking occurs at the call site (caller is untyped)

**Verify:** System remains sound within typed world

### R5: Public vs internal

**Given** `foo.eli` lists:
```elisp
(sig foo-public-api (-> (String) String))
;; foo--internal not listed
```
**And** `foo.el` defines both `foo-public-api` and `foo--internal`
**When** type-checked
**Then**:
- `foo-public-api`: checked against signature
- `foo--internal`: inferred, not exported

**Verify:** Internal functions are inferred but not part of module interface

### R6: require/provide interaction

**Given** `my-app.el`:
```elisp
(require 'my-utils)
(my-utils-foo 42)
```
**And** `my-utils.eli` exists with:
```elisp
(sig my-utils-foo (-> (Int) String))
```
**When** type-checked
**Then** `my-utils-foo` is available with type `(-> (Int) String)`

**Verify:** Required module signatures are loaded

### R7: Autoload handling

**Given** an autoloaded function:
```elisp
;; my-package.el
;;;###autoload
(defun my-package-autoload-fn (n) ...)
```
**And** `my-package.eli` declares:
```elisp
(sig my-package-autoload-fn (-> (Int) String))
```
**When** another module calls this function before load
**Then** the signature from `.eli` is used for type checking

**Verify:** Autoloaded functions type-check before actual loading

### R8: Missing signature warning

**Given** `foo.el` defines `foo-public-fn`
**And** `foo.eli` exists but doesn't list `foo-public-fn`
**When** type-checked
**Then** warning: "Function `foo-public-fn` defined but not in signature file"
(only for public-looking names, not `--internal` names)

**Verify:** Drift between implementation and signatures is detected

### R9: Circular dependencies

**Given** modules A and B with:
- A requires B
- B requires A
**When** type-checked
**Then** signatures are loaded lazily; cycles are handled

**Verify:** Mutual recursion between modules works

## Tasks

- [ ] [R1] Implement .eli detection for type-check triggering
- [ ] [R2] Verify implementations match signatures
- [ ] [R3] Load require/typed declarations
- [ ] [R4] Document untyped→typed boundary (no action needed)
- [ ] [R5] Distinguish public vs internal functions
- [ ] [R6] Load signatures for required modules
- [ ] [R7] Handle autoloaded function signatures
- [ ] [R8] Warn on undefined exports
- [ ] [R9] Handle circular module dependencies
