# Spec 43: CI Version Matrix

Multi-version Emacs testing with blocking/advisory tiers.

**Deps:** [Spec 21][], 22

## Links

### Deps
[Spec 21]: ./21-e2e-test-harness.md

## Constraints

| Constraint | Detail |
|------------|--------|
| Blocking | Last 3 major: 28.x, 29.x, 30.x |
| Advisory | Emacs main/pre-release |
| Nix-based | Reproducible Emacs versions |
| Parallel | Matrix runs concurrently |

## Tiers

| Version | Tier | Notes |
|---------|------|-------|
| 28.x | Blocking | Oldest supported |
| 29.x | Blocking | Tree-sitter |
| 30.x | Blocking | Current stable |
| main | Advisory | Next release preview |

## Output

```
.github/workflows/ci.yml
flake.nix  ; Emacs version inputs
```

## Requirements

### R1: Version matrix

**Given** CI workflow **When** defining matrix **Then** all versions with tier labels

```yaml
strategy:
  fail-fast: false
  matrix:
    include:
      - emacs-version: "28"
        tier: blocking
      - emacs-version: "29"
        tier: blocking
      - emacs-version: "30"
        tier: blocking
      - emacs-version: "main"
        tier: advisory
```

**Verify:** `gh workflow view ci.yml`; 4 versions with tiers

### R2: Blocking failures fail build

**Given** failure in 28/29/30 **When** complete **Then** CI fails

**Verify:** Deliberate failure; CI fails on blocking job

### R3: Advisory failures don't fail build

**Given** failure in main **When** complete **Then** CI passes (if blocking passed)

```yaml
continue-on-error: ${{ matrix.tier == 'advisory' }}
```

**Verify:** Advisory fails + blocking passes = overall pass

### R4: Advisory failure visibility

**Given** advisory failure **When** viewing results **Then** visible warning

```yaml
- name: Report advisory failures
  if: failure() && matrix.tier == 'advisory'
  run: echo "::warning::Advisory tier failed"
```

**Verify:** Warning annotation appears

### R5: Round-trip per version

**Given** each matrix version **When** testing **Then** E2E runs with that Emacs

```yaml
nix develop .#emacs${{ matrix.emacs-version }} --command ./scripts/run-emacs-tests.sh
```

**Verify:** Output shows correct Emacs version

### R6: Nix dev shells

**Given** flake **When** providing versions **Then** each has dev shell

```nix
devShells = {
  default = mkShell { buildInputs = [ emacs30 ]; };
  emacs28 = mkShell { buildInputs = [ emacs28 ]; };
  emacs29 = mkShell { buildInputs = [ emacs29 ]; };
  emacs30 = mkShell { buildInputs = [ emacs30 ]; };
  emacsMain = mkShell { buildInputs = [ emacsPgtk ]; };
};
```

**Verify:** `nix develop .#emacs29 --command emacs --version`; shows 29.x

### R7: Parallel matrix

**Given** version matrix **When** CI runs **Then** all parallel

```yaml
fail-fast: false
```

**Verify:** All jobs start simultaneously

### R8: Version-specific test skip

**Given** test for specific versions **When** unsupported **Then** skip, not fail

```elisp
(when (< emacs-major-version 29)
  (ert-skip "Requires Emacs 29+"))
```

**Verify:** Skipped on older; passes on newer

### R9: Job naming

**Given** CI jobs **When** viewing UI **Then** clear version + tier

```yaml
name: Test (Emacs ${{ matrix.emacs-version }}, ${{ matrix.tier }})
```

**Verify:** Shows "Test (Emacs 29, blocking)"

### R10: Build caching

**Given** each job **When** using Nix **Then** cache shared

```yaml
- uses: DeterminateSystems/magic-nix-cache-action@v8
```

**Verify:** Second run shows cache hits

## Non-Requirements

- Windows/macOS testing
- Every minor version
- Auto Emacs version updates
- Custom configure flags

## Tasks

- [x] [R6] Emacs dev shells in flake.nix
- [x] [R1, R9] Version matrix in ci.yml
- [x] [R2-3] Blocking/advisory logic
- [x] [R4] Advisory warning annotations
- [x] [R5] E2E with correct Emacs
- [x] [R7] Parallel config
- [x] [R8] Version-skip helpers
- [x] [R10] Verify Nix caching

## Status

Complete. Reduced to 2 versions (emacs30 blocking, emacsGit advisory)
because nixpkgs dropped emacs29 due to CVEs and emacs28 is even older.
Uses `advisory` boolean instead of `tier` string for simpler
`continue-on-error` expressions. Job names show version + tier.
