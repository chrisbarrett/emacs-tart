# Implementation Plan: CI Version Matrix (Spec 43)

Multi-version Emacs testing with blocking/advisory tiers.

## Analysis

### Current State

**flake.nix** already has `mkDevShell` helper and two shells:
`emacs30` (stable) and `emacsGit` (development HEAD). Comment notes
emacs29 was removed from nixpkgs due to CVEs. No emacs28 available.

**ci.yml** already has a version matrix with `emacsGit`/`emacs30`,
`fail-fast: false`, `continue-on-error` keyed on `advisory` boolean,
magic-nix-cache, and correct `nix develop .#${{ matrix.emacs }}`
usage. Job name shows version but not tier.

**elisp.yml** has same matrix pattern for Emacs Lisp tests and E2E.

### Gap Analysis

| Requirement | Status | Gap |
|-------------|--------|-----|
| R1 matrix | Partial | Uses `advisory: bool` not `tier: string` |
| R2 blocking fails | Done | `continue-on-error` logic works |
| R3 advisory passes | Done | Same mechanism |
| R4 advisory warning | Missing | No `::warning::` step |
| R5 E2E per version | Done | Both workflows use matrix shells |
| R6 Nix shells | Done | emacs30+emacsGit (28/29 unavailable) |
| R7 parallel | Done | `fail-fast: false` |
| R8 version skip | Missing | No skip helper pattern |
| R9 job naming | Partial | Shows version, not tier |
| R10 build caching | Done | magic-nix-cache present |

### Key Design Decisions

1. **Keep `advisory` boolean** — converting to `tier: string` would
   require changing the `continue-on-error` expression from
   `${{ matrix.advisory }}` to `${{ matrix.tier == 'advisory' }}`.
   The boolean is simpler and already works. Add a comment noting
   the tier semantics instead.

2. **emacs28/29 unavailable** — nixpkgs dropped emacs29 for CVEs,
   emacs28 even older. Spec's 4-version matrix reduces to 2 versions
   (emacs30 blocking, emacsGit advisory). Document this constraint.

3. **Version-skip via Elisp helper** — provide a `tart-test-skip`
   macro that wraps `ert-skip` with version check, usable in tests.

---

## Iteration 1: CI workflow polish + advisory warnings

Small changes to existing workflows for R1, R4, R9.

### Task 1.1: Add tier comments + advisory warning step

- [x] `ci.yml`: add `tier` comment to each matrix entry for clarity
- [x] `ci.yml`: add advisory warning annotation step after Test
      (R4: `echo "::warning::Advisory tier (emacsGit) failed"`)
- [x] `ci.yml`: update job name to include tier info (R9)
- [x] `elisp.yml`: same advisory warning step in test job
- [x] `elisp.yml`: same advisory warning step in e2e job
- [x] `elisp.yml`: update job names to include tier info
- [x] Verify YAML syntax: `python3 -c "import yaml; yaml.safe_load(...)"`

### Task 1.2: Version-skip helper

- [x] Add `tart-test-skip-unless-version` macro to
      `lisp/tart-test-helpers.el` (R8)
- [x] Wraps `(when (< emacs-major-version N) (ert-skip ...))`
- [x] Add unit test in `lisp/tart-tests.el` verifying macro expands

### Task 1.3: Spec completion

- [x] Check all applicable task boxes in specs/43-ci-version-matrix.md
- [x] Add Status section documenting emacs28/29 unavailability
- [x] Build + test
