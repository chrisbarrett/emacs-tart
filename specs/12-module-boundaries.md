# Spec 12: Module Boundary Rules

Rules for type checking at module boundaries and handling untyped code.

**Dependencies:** Spec 07 (signature files), Spec 08 (LSP for trigger)

## Goal

Define when type checking is triggered, how typed and untyped code interact,
and what constitutes the public interface of a module.

## Constraints

- **Always available**: Any `.el` file can be type-checked via LSP
- **Gradual**: Types come from search path; untyped code is `Any`
- **Sound within typed world**: No `any` escape hatch

## Requirements

### R1: Type checking availability

**Given** an `.el` file
**When** opened in an LSP-connected editor
**Then** type checking is always available

| Scenario | Behavior |
|----------|----------|
| `foo.el` only | Infer types; require/autoload triggers search for `.tart` files |
| `foo.el` + `foo.tart` | Also verify implementation matches declared signatures |

**Verify:** Any `.el` file gets diagnostics for type errors in code using typed modules

### R2: Signature verification

**Given** `foo.tart` declares:
```elisp
(defun foo-add (Int Int) -> Int)
```
**And** `foo.el` defines:
```elisp
(defun foo-add (a b) (concat a b))  ; Wrong: returns String
```
**When** type-checked
**Then** error: signature mismatch

**Verify:** Mismatched implementations produce errors

### R3: Typed calling untyped

**Given** typed module uses an untyped module
**And** a signature exists in the search path:
```elisp
;; ~/.config/emacs/eli/external-lib.tart
(module external-lib)
(defun external-lib-process String -> String)
```
**And** `my-app.el` calls `(require 'external-lib)` and uses `external-lib-process`
**When** type-checked
**Then** the call is checked against the signature from the search path

**Verify:** Wrong argument types produce errors

### R4: Untyped calling typed

**Given** an untyped module calls a typed module
**When** the typed module is compiled
**Then** no checking occurs at the call site (caller is untyped)

**Verify:** System remains sound within typed world

### R5: Public vs internal

**Given** `foo.tart` lists:
```elisp
(defun foo-public-api String -> String)
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
**And** `my-utils.tart` exists with:
```elisp
(defun my-utils-foo Int -> String)
```
**When** type-checked
**Then** `my-utils-foo` is available as a directly callable function

**Verify:** Required module signatures are loaded

### R7: Autoload handling

**Given** code calls an autoloaded function `my-package-autoload-fn`
**And** `my-package.tart` exists in the search path:
```elisp
(module my-package)
(defun my-package-autoload-fn Int -> String)
```
**When** type-checked
**Then** tart searches for `my-package.tart` based on the function's prefix
**And** the signature is used for type checking

**Verify:** Autoloaded functions type-check using signatures from search path

### R8: Missing signature warning

**Given** `foo.el` defines `foo-public-fn`
**And** `foo.tart` exists but doesn't list `foo-public-fn`
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

- [ ] [R1] Enable type checking for any .el file
- [ ] [R2] Verify implementations match signatures when .tart exists
- [ ] [R3] Load signatures from search path for required modules
- [ ] [R4] Document untyped→typed boundary (no action needed)
- [ ] [R5] Distinguish public vs internal functions
- [ ] [R6] Load signatures for required modules
- [ ] [R7] Handle autoloaded function lookup via search path
- [ ] [R8] Warn on undefined exports
- [ ] [R9] Handle circular module dependencies
