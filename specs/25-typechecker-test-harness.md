# Spec 25: Type Checker Test Harness

Fixture-based acceptance tests for type checker output.

**Deps:** Spec 09 (CLI), Spec 24 (versioned typings).

## Goal

Automated tests that run `tart check` against `.el` fixture files and verify:
- Expected accept/reject outcomes
- Specific diagnostic messages and locations
- Correct typings loaded for Emacs version

## Constraints

| Constraint | Detail |
|------------|--------|
| Fast | Full suite <30s |
| Declarative | Test expectations in fixture comments or sidecar files |
| Version-aware | Test against multiple Emacs version typings |
| CI-friendly | Run via `dune test` or dedicated script |

## Fixture Format

Sidecar `.expected` files alongside each `.el` fixture:

```
test/fixtures/typing/
├── error-add.el
└── error-add.expected
```

Expected file format:
```
FAIL
6:1: error: type mismatch: expected Int, got String
```

First line is `PASS` or `FAIL`. Remaining lines are expected diagnostic output (exact match or substring match TBD).

To update expectations: `tart check foo.el > foo.expected` (with manual review).

## Directory Structure

```
test/
├── fixtures/
│   └── typing/
│       ├── core/              ; Core Emacs primitives
│       │   ├── arithmetic.el
│       │   ├── lists.el
│       │   ├── strings.el
│       │   └── ...
│       ├── version/           ; Version-specific behavior
│       │   ├── 29-only.el
│       │   └── 31-new-fn.el
│       └── regression/        ; Bug reproductions
└── typing/
    └── acceptance_test.ml     ; Harness implementation
```

## Output

```
lib/test_harness/
├── acceptance.ml       ; Run fixtures, compare output
└── acceptance.mli
test/typing/
└── acceptance_test.ml  ; Alcotest suite
scripts/
└── run-acceptance.sh   ; Standalone runner (optional)
```

## Requirements

### R1: Basic pass/fail

Run `tart check` on fixture. Assert exit code matches expectation.

```
test/fixtures/typing/pass-simple.el      → exit 0
test/fixtures/typing/fail-type-error.el  → exit 1
```

### R2: Diagnostic assertions

Verify specific error messages appear in output.

```elisp
;; test: expect-error "type mismatch"
;; test: expect-error "Int" "String"
```

### R3: Location assertions

Verify error reported at expected line:column.

```elisp
;; test: expect-error-at 6:1
```

### R4: Version-specific tests

Run fixture against specific typings version.

```elisp
;; test: emacs-version 31.0
;; test: expect-pass
(treesit-available-p)  ; Only in 31+
```

### R5: Core typings coverage

Fixtures that exercise each C primitive category:
- `arithmetic.el` - `+ - * / mod`
- `lists.el` - `car cdr cons nth mapcar`
- `strings.el` - `concat substring upcase`
- `predicates.el` - `null listp stringp`
- `control.el` - `funcall apply signal`

Each fixture should have both passing and failing cases.

### R6: Regression fixtures

Named after issue/bug. Documents expected behavior.

```
test/fixtures/typing/regression/
├── issue-42-car-nil.el
└── issue-57-mapcar-poly.el
```

### R7: Harness API

```ocaml
(** Run single fixture, return result *)
val check_fixture : path:string -> Fixture_result.t

(** Discover and run all fixtures in directory *)
val run_all : dir:string -> Summary.t

type Fixture_result.t = {
  path : string;
  expected : expectation;
  actual : tart_output;
  passed : bool;
}
```

### R8: Integration with dune test

Fixtures run as part of `dune test`. Failures show diff between expected and actual.

### R9: Parallel execution

Fixtures run in parallel for speed. No shared state between fixtures.

## Tasks

- [ ] Implement `.expected` file parser
- [ ] Implement acceptance harness
- [ ] Create core typings fixtures (arithmetic, lists, strings, etc.)
- [ ] Add version-specific fixtures
- [ ] Wire into `dune test`
- [ ] Add CI integration
