# Spec 16: Migrate to Prek

Replace pre-commit with prek for faster git hook execution.

**Dependencies:** None

## Goal

Switch from pre-commit to prek, a Rust-native drop-in replacement with better
performance. This improves the development loop since hooks run on every commit.

## Constraints

- **Drop-in replacement**: Existing `.pre-commit-config.yaml` works unchanged
- **Nix-managed**: Install prek via flake inputs or devShell

## Output

No new files; updates to `flake.nix` and documentation.

## Requirements

### R1: Install prek in devShell

**Given** a developer enters the nix devShell
**When** they run `prek --version`
**Then** prek is available

**Verify:** `nix develop -c prek --version` succeeds

### R2: Replace pre-commit invocations

**Given** any documentation or scripts referencing `pre-commit`
**When** updated
**Then** they reference `prek` instead

**Verify:** `grep -r pre-commit` finds no remaining references (except config file)

### R3: Reinstall git hooks

**Given** a fresh clone or existing checkout
**When** developer runs `prek install -f`
**Then** git hooks use prek instead of pre-commit

**Verify:** `.git/hooks/pre-commit` invokes prek

### R4: Verify hooks execute correctly

**Given** prek is installed
**When** `prek run --all-files` executes
**Then** all hooks (dune-build, dune-test, dune-fmt) pass

**Verify:** `prek run --all-files` exits 0 on clean repo

## Tasks

- [x] [R1] Add prek to flake.nix devShell packages
- [x] [R2] Update CLAUDE.md or other docs referencing pre-commit
- [x] [R3] Run `prek install -f` to reinstall hooks
- [x] [R4] Verify all hooks pass with prek
