# Spec 33: Typing Test Fixtures

Expand fixture coverage for type checker acceptance tests.

- **Deps:**
  - [Spec 25](./25-typechecker-test-harness.md) (test harness)

## Goal

Comprehensive fixture suite covering pass/fail cases for all error categories. Fixtures document the type checker's user-facing behavior and integrate with Emacs core typing work.

## Constraints

| Constraint | Detail |
|------------|--------|
| Human-reviewable | Fixtures + expected output readable without tooling |
| Stable snapshots | Output format changes require deliberate review |
| Fast | Individual fixtures check in <1s |
| Categorical | Organized by error type, not source module |

## Directory Structure

```
test/fixtures/typing/
├── core/                  # Existing - passing primitives
├── version/               # Existing - version-specific
├── errors/                # NEW - failure cases by category
│   ├── type-mismatch/
│   ├── arity/
│   ├── unbound/
│   ├── occurs-check/
│   ├── kind/
│   └── exhaustiveness/
└── regression/            # NEW - bug reproductions
```

## Requirements

### R1: Type mismatch fixtures

**Given** fixtures in `errors/type-mismatch/`
**When** `tart check` runs
**Then** each reports type mismatch with expected/actual types

Fixtures:
- `int-for-string.el` - passing int where string expected
- `string-for-int.el` - passing string where int expected
- `list-for-atom.el` - passing list where atom expected
- `function-arity.el` - function type with wrong arity
- `polymorphic.el` - incompatible instantiations of type variable

**Verify:** `dune test` passes, `.expected` files show FAIL with type mismatch diagnostic

### R2: Arity error fixtures

**Given** fixtures in `errors/arity/`
**When** `tart check` runs
**Then** each reports wrong argument count

Fixtures:
- `too-few-args.el` - missing required arguments
- `too-many-args.el` - excess arguments to fixed-arity function
- `optional-required.el` - omitting required arg when optional exists

**Verify:** `dune test` passes, `.expected` files show FAIL with arity diagnostic

### R3: Unbound identifier fixtures

**Given** fixtures in `errors/unbound/`
**When** `tart check` runs
**Then** each reports unbound variable or function

Fixtures:
- `unbound-var.el` - reference to undefined variable
- `unbound-fn.el` - call to undefined function
- `typo.el` - realistic typo in common function name
- `scoping.el` - variable used outside its let scope

**Verify:** `dune test` passes, `.expected` files show FAIL with unbound diagnostic

### R4: Occurs check fixtures

**Given** fixtures in `errors/occurs-check/`
**When** `tart check` runs
**Then** each reports infinite type / occurs check failure

Fixtures:
- `self-reference.el` - `(setq x (cons x nil))`
- `mutual-recursion.el` - mutually recursive definitions creating cycle

**Verify:** `dune test` passes, `.expected` files show FAIL with occurs check diagnostic

### R5: Kind error fixtures

**Given** fixtures in `errors/kind/`
**When** `tart check` runs
**Then** each reports kind mismatch

Fixtures:
- `type-as-value.el` - using type constructor as value
- `value-as-type.el` - using value where type expected (in annotations)
- `wrong-arity-tycon.el` - type constructor with wrong number of args

**Verify:** `dune test` passes, `.expected` files show FAIL with kind diagnostic

### R6: Exhaustiveness fixtures

**Given** fixtures in `errors/exhaustiveness/`
**When** `tart check` runs
**Then** each reports non-exhaustive pattern match

Fixtures:
- `missing-case.el` - pcase missing constructor
- `missing-nil.el` - list match missing nil case
- `missing-default.el` - cond without catch-all

**Verify:** `dune test` passes, `.expected` files show FAIL with exhaustiveness warning

### R7: Regression fixtures

**Given** fixtures in `regression/`
**When** `tart check` runs
**Then** each reproduces a previously-encountered bug

Naming: `{short-description}.el` (e.g., `car-on-string.el`, `mapcar-poly-unify.el`)

Each fixture should include a comment header:
```elisp
;; Regression: [brief description of the bug]
;; Fixed: [date or commit]
```

**Verify:** `dune test` passes, behavior matches `.expected`

### R8: Realistic user scenarios

Each error category should include at least one fixture representing a **realistic mistake** a user might make, not just a minimal reproduction.

Examples:
- `errors/type-mismatch/user-config.el` - wrong type in defcustom
- `errors/arity/hook-function.el` - hook function with wrong signature
- `errors/unbound/require-missing.el` - using function without require

**Verify:** Fixtures read as plausible user code, not synthetic test cases

### R9: Harness discovers new directories

**Given** new `errors/` subdirectories
**When** `fixture_test.ml` runs
**Then** all subdirectories are discovered and tested

**Verify:** Adding new category directory + fixtures causes test count to increase

## Tasks

- [x] Create `errors/` directory structure
- [x] Implement type-mismatch fixtures (R1)
- [x] Implement arity fixtures (R2)
- [x] Implement unbound fixtures (R3)
- [x] Implement occurs-check fixtures (R4)
- [x] Implement kind fixtures (R5)
- [x] Implement exhaustiveness fixtures (R6)
- [x] Create regression directory with initial fixtures (R7)
- [x] Add realistic user scenario fixtures to each category (R8)
- [x] Update `fixture_test.ml` to discover `errors/` subdirectories (R9)
- [x] Generate `.expected` files via `tart check`
- [x] Review all `.expected` output for correctness

**Status: Complete.**
