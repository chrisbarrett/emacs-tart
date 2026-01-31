# Spec 12: Module Boundary Rules

Rules for type checking at module boundaries.

**Dependencies:** Spec 07 (signature files), Spec 08 (LSP for trigger)

## Goal

Define when type checking is triggered, how modules interact, and what
constitutes the public interface of a module. All code is fully typed—signatures
exist for Emacs core and are inferred or declared for user code.

## Constraints

- **Fully typed**: No gradual typing; all code has types (inferred or declared)
- **Always available**: Any `.el` file can be type-checked via LSP
- **Sound**: Type errors are always errors, never silently widened

## Output

No new files; extends `lib/typing/check.ml` and `lib/sig/sig_loader.ml`.

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
(defun foo-add (int int) -> int)
```
**And** `foo.el` defines:
```elisp
(defun foo-add (a b) (concat a b))  ; Wrong: returns string
```
**When** type-checked
**Then** error: signature mismatch

**Verify:** `dune test`; mismatched implementations produce errors

### R3: External library signatures

**Given** code uses an external library
**And** a signature exists in the search path:
```elisp
;; ~/.config/emacs/tart/external-lib.tart
(defun external-lib-process (string) -> string)
```
**And** `my-app.el` calls `(require 'external-lib)` and uses `external-lib-process`
**When** type-checked
**Then** the call is checked against the signature from the search path

**Verify:** `dune test`; wrong argument types produce errors

### R4: Missing signature is an error

**Given** code calls a function with no signature (not in search path, not inferred)
**When** type-checked
**Then** error: "No signature found for `unknown-fn`"

**Verify:** `dune test`; missing signatures produce errors, not silent `any`

### R5: Public vs internal

**Given** `foo.tart` lists:
```elisp
(defun foo-public-api (string) -> string)
;; foo--internal not listed
```
**And** `foo.el` defines both `foo-public-api` and `foo--internal`
**When** type-checked
**Then**:
- `foo-public-api`: checked against signature
- `foo--internal`: inferred, not exported

**Verify:** `dune test`; internal functions are inferred but not part of module interface

### R6: require/provide interaction

**Given** `my-app.el`:
```elisp
(require 'my-utils)
(my-utils-foo 42)
```
**And** `my-utils.tart` exists with:
```elisp
(defun my-utils-foo (int) -> string)
```
**When** type-checked
**Then** `my-utils-foo` is available as a directly callable function

**Verify:** `dune test`; required module signatures are loaded

### R7: Autoload handling

**Given** code calls an autoloaded function `my-package-autoload-fn`
**And** `my-package.tart` exists in the search path:
```elisp
(defun my-package-autoload-fn (int) -> string)
```
**When** type-checked
**Then** tart searches for `my-package.tart` based on the function's prefix
**And** the signature is used for type checking

**Verify:** `dune test`; autoloaded functions type-check using signatures from search path

### R8: Missing signature warning

**Given** `foo.el` defines `foo-public-fn`
**And** `foo.tart` exists but doesn't list `foo-public-fn`
**When** type-checked
**Then** warning: "Function `foo-public-fn` defined but not in signature file"
(only for public-looking names, not `--internal` names)

**Verify:** `dune test`; drift between implementation and signatures is detected

### R9: Circular dependencies

**Given** modules A and B with:
- A requires B
- B requires A
**When** type-checked
**Then** signatures are loaded lazily; cycles are handled

**Verify:** `dune test`; mutual recursion between modules works

## Tasks

- [x] [R1] Enable type checking for any .el file
- [x] [R2] Verify implementations match signatures when .tart exists
- [x] [R3] Load signatures from search path for required modules
- [ ] [R4] Error on missing signatures
- [x] [R5] Distinguish public vs internal functions
- [x] [R6] Load signatures for required modules
- [x] [R7] Handle autoloaded function lookup via search path
- [x] [R8] Warn on undefined exports
- [x] [R9] Handle circular module dependencies

Run review agent after R1-R3 work (basic module loading) before implementing R7-R9.
