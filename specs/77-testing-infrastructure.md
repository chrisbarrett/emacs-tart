# Spec 77 — Testing Infrastructure

> Consolidates specs: [21](./.archive/21-e2e-test-harness.md), [25](./.archive/25-typechecker-test-harness.md), [33](./.archive/33-typing-fixtures.md), [31](./.archive/31-fast-feedback.md), [43](./.archive/43-ci-version-matrix.md)

## Overview

Tart's testing infrastructure covers three layers: fixture-based acceptance tests that verify the type checker against `.el`/`.expected` file pairs via Alcotest; end-to-end ERT tests that exercise the full Emacs integration (LSP, REPL, minor mode) against a live tart process; and a CI version matrix that runs both test suites against multiple Emacs versions with blocking and advisory tiers. A `./tart` wrapper script provides fast incremental feedback during development by skipping redundant `nix develop` overhead.

## Fixture-Based Tests

### Fixture format

Each test case is a pair of files under `test/fixtures/typing/`:

- `$name.el` -- Elisp code to type-check.
- `$name.expected` -- Expected outcome and diagnostics.

The `.expected` file format:

```
PASS
```

or

```
FAIL
6:1: error: type mismatch: expected Int, got String
```

Line 1 is `PASS` or `FAIL` (case-insensitive). Remaining non-empty lines are expected diagnostic messages matched as substrings against `tart check` output. To regenerate expectations: `tart check foo.el > foo.expected` followed by manual review.

### Directory structure

```
test/fixtures/typing/
├── core/                  # Passing primitives (arithmetic, lists, strings, predicates)
├── errors/                # Failure cases by category
│   ├── type-mismatch/
│   ├── arity/
│   ├── unbound/
│   ├── occurs-check/
│   ├── kind/
│   └── exhaustiveness/
├── funcall/               # funcall/apply typing
├── guards/                # Feature guard narrowing
├── regression/            # Bug reproductions
├── rows/                  # Row-type fixtures
└── version/               # Version-specific behavior
```

Error subdirectories are discovered dynamically; adding a new directory with fixture pairs automatically includes them in the test run.

### Acceptance harness

The `Acceptance` module (`lib/test_harness/acceptance.mli`) provides the programmatic API:

```ocaml
val check_fixture : tart_bin:string -> path:string -> fixture_result
val run_all : tart_bin:string -> dir:string -> summary
val run_all_parallel : tart_bin:string -> dir:string -> ?jobs:int -> unit -> summary
val discover_fixtures : string -> string list
val parse_expected_file : string -> expected_file option
```

`fixture_test.ml` wires fixture discovery into Alcotest. It locates the tart binary (via `TART_BIN` environment variable or well-known relative paths) and the fixtures directory, then generates one test case per fixture. If the binary or fixtures are missing, the suite reports a warning and exits cleanly rather than failing.

### Fixture directives

Fixtures support directives in Elisp comments:

```elisp
;; test: emacs-version 31.0
```

`emacs-version` overrides which typings are loaded, enabling version-specific fixtures without separate CI jobs.

### Fixture conventions

- **Error fixtures** include at least one realistic user scenario (e.g., wrong type in `defcustom`, hook function with wrong signature) alongside minimal reproductions.
- **Regression fixtures** include a comment header documenting the bug and fix.
- Each fixture checks in under 1 second. The full suite runs in under 30 seconds.

## E2E Tests

End-to-end tests run the tart binary against Emacs via `emacs --batch` and ERT. They verify the full pipeline from Elisp integration through tart process management.

### Test runner

`scripts/run-emacs-tests.sh` discovers `*-tests.el` and `*-e2e-tests.el` files under `lisp/`, sets up the load path, and runs them with `ert-run-tests-batch-and-exit`. It locates the tart binary from `_build/default/bin/main.exe` or the `PATH`.

### Test helpers

`lisp/tart-test-helpers.el` provides:

| Helper | Purpose |
|:-------|:--------|
| `tart-test-with-temp-buffer` | Buffer-local test context with cleanup |
| `tart-test-wait-for` | Poll async predicate with configurable timeout (default 5s) |
| `tart-test-fixture-path` | Resolve fixture file path |
| `tart-test-ensure-tart` | Skip test if tart binary unavailable |

### Coverage areas

- **LSP:** Server connects, diagnostics appear for type errors, hover returns type info.
- **REPL:** Process starts with prompt, evaluates expressions, supports `,type` and `,expand` commands.
- **Integration:** `tart-send-defun` transmits code to REPL, `tart-type-at-point` displays types, keybindings are active in `tart-mode`.
- **Cleanup:** Test teardown kills tart processes and temporary buffers. Async waits fail with a message on timeout rather than hanging.

### CI integration

The `elisp.yml` workflow builds the tart binary, uploads it as an artifact, then runs E2E tests in a separate job that downloads the binary and sets `tart-executable` to its path. This separates OCaml build from Elisp test concerns.

## Development Workflow

### The `./tart` script

A repo-root wrapper script provides fast incremental builds:

```bash
./tart check --emacs-version 31.0 file.el
```

The script detects whether it is running inside `nix develop` (via the `IN_NIX_SHELL` environment variable) and skips the Nix wrapper when already in a dev shell, avoiding 300--500ms of startup overhead. Inside the shell, `dune exec tart --` performs an incremental build (only recompiling changed files) before running tart.

### Running tests

| Command | Effect |
|:--------|:-------|
| `nix develop --command dune test` | Run all OCaml tests (unit + fixture) |
| `nix develop --command dune test --force` | Force re-run even if unchanged |
| `./scripts/run-emacs-tests.sh` | Run Elisp ERT tests locally |
| `dune build --watch` | Continuous rebuild for fast iteration |

Tests are fast enough for interactive development. `dune build --watch` in a separate terminal pre-builds changes, making subsequent `./tart` invocations near-instant.

## CI Matrix

CI runs tests against multiple Emacs versions to catch compatibility issues. Two GitHub Actions workflows share the same matrix strategy.

### Version tiers

| Version | Tier | Behavior |
|:--------|:-----|:---------|
| emacs30 | Blocking | Failure fails the build |
| emacsGit | Advisory | Failure produces a warning but does not fail the build |

Originally the matrix covered emacs28, emacs29, emacs30, and main. It was reduced to two versions after nixpkgs dropped emacs29 (CVEs) and emacs28 (end of life).

### CI workflow structure

**`ci.yml`** -- OCaml build and test:

1. Build with `dune build` under the matrix Emacs dev shell.
2. Run `dune test` (unit tests + fixture acceptance tests).
3. Lint with `dune build @fmt`.

**`elisp.yml`** -- Emacs Lisp:

1. Byte-compile `tart.el` and `tart-mode.el` with `byte-compile-error-on-warn`.
2. Run ERT unit tests across the version matrix.
3. Build the tart binary, upload as artifact, then run E2E tests.
4. Run checkdoc linting.

### Matrix configuration

```yaml
strategy:
  fail-fast: false
  matrix:
    include:
      - emacs: emacs30
        advisory: false
      - emacs: emacsGit
        advisory: true
continue-on-error: ${{ matrix.advisory }}
```

`fail-fast: false` ensures all matrix entries run to completion. Advisory failures emit `::warning::` annotations for visibility without blocking merges. Job names include version and tier for clarity (e.g., `test (emacs30, blocking)`).

### Nix dev shells

Each Emacs version has a corresponding Nix dev shell. Builds use `nix develop .#${{ matrix.emacs }}` to select the correct Emacs. The Nix cache (`magic-nix-cache-action`) and opam cache reduce cold-start times.

### Version-specific test skipping

Tests that require a specific Emacs version skip cleanly on older versions rather than failing:

```elisp
(when (< emacs-major-version 29)
  (ert-skip "Requires Emacs 29+"))
```

## Key Files

| File | Role |
|:-----|:-----|
| `lib/test_harness/acceptance.ml` | Fixture-based acceptance harness implementation |
| `lib/test_harness/acceptance.mli` | Acceptance harness interface |
| `test/test_harness/fixture_test.ml` | Alcotest runner wiring fixtures to dune test |
| `test/test_harness/acceptance_test.ml` | Unit tests for the harness itself |
| `test/fixtures/typing/` | Fixture `.el` and `.expected` file pairs |
| `lisp/tart-test-helpers.el` | ERT test utilities |
| `lisp/tart-e2e-tests.el` | End-to-end ERT tests |
| `scripts/run-emacs-tests.sh` | Standalone ERT test runner |
| `tart` | Fast feedback wrapper script |
| `.github/workflows/ci.yml` | OCaml build/test/lint CI |
| `.github/workflows/elisp.yml` | Elisp compile/test/E2E CI |

## Deferred

No items currently deferred.
