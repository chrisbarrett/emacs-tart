# Spec 31: Fast Feedback

A `./tart` shell script for running tart against current source with minimal wait time.

**Deps:** None.

## Goal

Agents and developers iterating on tart's source code need to run tart against
the current source without waiting for a full `nix develop --command dune build`
cycle. This spec adds a `./tart` wrapper script that provides fast incremental
builds.

## Constraints

| Constraint | Detail |
|------------|--------|
| Incremental | Only rebuilds changed files, not full project |
| Shell-aware | Avoids redundant `nix develop` when already in devShell |
| Transparent | Passes all arguments through to tart |
| Portable | Works from any directory within the repo |

## Background

### dune exec vs dune build

`dune exec tart --` is superior to `dune build && _build/default/bin/main.exe`
because:

1. It performs an incremental build (only recompiles changed files)
2. Automatically runs the binary after building
3. Single command instead of two

### nix develop overhead

`nix develop --command <cmd>` has startup overhead (~300-500ms) even when the
shell is already cached. When already inside `nix develop`, this overhead is
unnecessary. The script should detect this and skip the wrapper.

### Detection mechanism

Inside `nix develop`, the `IN_NIX_SHELL` environment variable is set to
`"impure"` (or `"pure"` for `--pure` shells). The script uses this to detect
whether it's already running in a nix shell.

## Requirements

### R1: Script exists at repo root

**Given** the repository
**When** listing the root directory
**Then** a `./tart` executable script exists

**Verify:** `test -x ./tart`

### R2: Script runs tart with dune exec

**Given** the `./tart` script
**When** invoked with arguments
**Then** it runs `dune exec tart -- <args>`

**Verify:** `./tart --version` outputs tart version

### R3: Script detects nix shell

**Given** the script is run outside `nix develop`
**When** `IN_NIX_SHELL` is unset or empty
**Then** the script wraps execution with `nix develop --command`

**Verify:** `env -u IN_NIX_SHELL ./tart --version` succeeds

### R4: Script skips nix wrapper when inside shell

**Given** the script is run inside `nix develop`
**When** `IN_NIX_SHELL` is set
**Then** the script runs `dune exec` directly without `nix develop` wrapper

**Verify:** `nix develop --command ./tart --version` runs quickly (<2s)

### R5: Script passes all arguments

**Given** the `./tart` script
**When** invoked with multiple arguments including flags
**Then** all arguments are passed through to tart unchanged

**Verify:** `./tart eval '(+ 1 2)'` outputs `3 :: Int`

### R6: Script works from subdirectories

**Given** the script is invoked from a subdirectory
**When** using a relative or absolute path to `./tart`
**Then** the script finds the project root and runs correctly

**Verify:** `cd lib && ../tart --version` succeeds

### R7: Script exits with tart's exit code

**Given** tart exits with a non-zero exit code
**When** the script completes
**Then** the script propagates tart's exit code

**Verify:** `./tart check nonexistent.el; echo $?` outputs `1`

### R8: Script handles no arguments

**Given** the script is invoked without arguments
**When** tart expects input
**Then** the script passes through and tart shows its usage error

**Verify:** `./tart` outputs usage error, exits 2

## Implementation Notes

The script should be a simple shell script:

```bash
#!/usr/bin/env bash
set -euo pipefail

# Find repo root (where dune-project lives)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

if [[ -n "${IN_NIX_SHELL:-}" ]]; then
    # Already in nix shell, run directly
    exec dune exec tart -- "$@"
else
    # Outside nix shell, wrap with nix develop
    exec nix develop --command dune exec tart -- "$@"
fi
```

### dune build --watch (optional enhancement)

For continuous development, users can run `dune build --watch` in a separate
terminal. This keeps dune's file watcher running and pre-builds changes, making
subsequent `./tart` invocations even faster. This is orthogonal to the script
itself.

## Tasks

- [ ] [R1-R8] Create `./tart` script at repo root
- [ ] Make script executable
- [ ] Test from various directories
- [ ] Test inside and outside nix shell
