# Spec 80 — Release Workflow

> Replaces: [22](./.archive/22-ci-releases.md)

## Overview

The release workflow is a manually triggered GitHub Actions pipeline that automates version bumping, tagging, building platform binaries, and publishing GitHub releases. It uses `workflow_dispatch` instead of tag-push triggers to give maintainers control over when releases are cut. The workflow builds binaries via `nix develop --command dune build` (the proven CI build path) rather than `nix build .#default` (which is broken due to missing `packages` output in `flake.nix`). Generated releases include an LLM-powered changelog, four platform binaries, and prerelease detection.

## Trigger

`workflow_dispatch` with a single input:

| Input | Type | Options | Default | Description |
|-------|------|---------|---------|-------------|
| `release_type` | choice | `major`, `minor`, `patch` | `minor` | Semver component to bump |

The workflow is manually triggered from the GitHub Actions UI.

## Jobs

### preflight

Computes the new version string based on the current version and `release_type` input.

**Steps:**

1. Checkout at HEAD
2. Extract current version from `lib/tart.ml` (line 3: `let version = "X.Y.Z"`)
3. Parse as semver and validate format
4. Compute new version:
   - `major`: increment first component, reset others to 0
   - `minor`: increment second component, reset patch to 0
   - `patch`: increment third component
5. Set job outputs: `version` (e.g., `1.2.3`) and `tag` (e.g., `v1.2.3`)

**Runs in parallel with:** `test`

### test

Validates the codebase before tagging. Uses the same build/test pattern as CI.

**Steps:**

1. Checkout
2. Set up Nix (`DeterminateSystems/determinate-nix-action`)
3. Enable Nix cache (`DeterminateSystems/magic-nix-cache-action`)
4. Cache opam directories:
   - Key: `opam-${{ runner.os }}-${{ hashFiles('tart.opam') }}`
   - Paths: `~/.opam`, `_opam`
5. Run `nix develop --command dune build`
6. Run `nix develop --command dune test`

**Runs in parallel with:** `preflight`

### tag

Creates the version bump commit and tag. Runs only after both `preflight` and `test` succeed.

**Needs:** `preflight`, `test`

**Steps:**

1. Checkout with `fetch-depth: 0`
2. Update version in three files using `sed`:
   - `lib/tart.ml`: `let version = "X.Y.Z"`
   - `lisp/tart.el`: `;; Version: X.Y.Z`
   - `lisp/tart-mode.el`: `;; Version: X.Y.Z`
3. Configure git with `github-actions[bot]` identity:
   - User: `github-actions[bot]`
   - Email: `41898282+github-actions[bot]@users.noreply.github.com`
4. Commit changes: `Bump version to ${{ needs.preflight.outputs.version }}`
5. Create annotated tag: `git tag -a ${{ needs.preflight.outputs.tag }} -m "Release ${{ needs.preflight.outputs.version }}"`
6. Push commit and tag to `main`

**Permissions:** `contents: write`

### build

Builds platform binaries using a matrix strategy. Runs after `tag` succeeds.

**Needs:** `tag`

**Matrix:**

| Platform | Runner | Artifact name |
|----------|--------|---------------|
| darwin-arm64 | `macos-latest` | `tart-darwin-arm64` |
| darwin-x86_64 | `macos-13` | `tart-darwin-x86_64` |
| linux-x86_64 | `ubuntu-latest` | `tart-linux-x86_64` |
| linux-arm64 | `ubuntu-24.04-arm` | `tart-linux-arm64` |

**Steps:**

1. Checkout at the tagged ref (`needs.tag.outputs.ref`)
2. Set up Nix (`DeterminateSystems/determinate-nix-action`)
3. Enable Nix cache (`DeterminateSystems/magic-nix-cache-action`)
4. Cache opam directories (same keys as `test` job)
5. Run `nix develop --command dune build`
6. Copy binary: `cp _build/default/bin/main.exe tart-${{ matrix.os }}-${{ matrix.arch }}`
7. Upload artifact using `actions/upload-artifact`

**Permissions:** `contents: read`

### release

Creates the GitHub release with changelog and binaries. Runs after all `build` matrix jobs complete.

**Needs:** `preflight`, `build`

**Environment:** `release` (scoped to `main` branch, provides `ANTHROPIC_API_KEY`)

**Steps:**

1. Checkout with `fetch-depth: 0` at the tag ref
2. Generate changelog:
   - Detect previous tag: `git describe --tags --abbrev=0 HEAD^`
   - Extract commits: `git log --oneline PREV_TAG..HEAD`
   - Call Anthropic API with prompt:
     ```
     Generate a changelog for the following git commits.
     Group changes into: Features, Fixes, Improvements.
     Use markdown formatting with bullet points.
     Be concise but informative.

     Commits:
     [commit list]
     ```
   - On API failure: fall back to raw `git log --oneline` output
   - On success: use LLM response as release body
3. Download all artifacts from `build` jobs into `artifacts/` directory
4. Determine prerelease status:
   - If tag matches `v*-(rc|alpha|beta)`: set `prerelease=true`
   - Otherwise: set `prerelease=false`
5. Create GitHub release using `softprops/action-gh-release`:
   - Tag: `${{ needs.preflight.outputs.tag }}`
   - Name: `Release ${{ needs.preflight.outputs.version }}`
   - Body: Generated changelog
   - Files: `artifacts/*` (all four platform binaries)
   - Prerelease: detected from tag pattern

**Permissions:** `contents: write`

**Environment secrets:**

- `ANTHROPIC_API_KEY`: API key for changelog generation

## Job DAG

```
workflow_dispatch(release_type: major|minor|patch)
  |
  +-- preflight ----+
  |                 |
  +-- test ---------+-- tag -- build (matrix: 4 platforms) -- release
```

## Action Versions

All actions are pinned to commit SHAs for security:

| Action | SHA | Version |
|--------|-----|---------|
| `actions/checkout` | `de0fac2e4500dabe0009e67214ff5f5447ce83dd` | v6.0.2 |
| `DeterminateSystems/determinate-nix-action` | `89ab342bd48ff7318caf8d39d6a330c7b1df8f2f` | v3.15.2 |
| `DeterminateSystems/magic-nix-cache-action` | `565684385bcd71bad329742eefe8d12f2e765b39` | v13 |
| `actions/cache` | `cdf6c1fa76f9f475f3d7449005a359c84ca0f306` | v5.0.3 |
| `actions/upload-artifact` | `b7c566a772e6b6bfb58ed0dc250532a479d7789f` | v6.0.0 |
| `actions/download-artifact` | `37930b1c2abaa49bbe596cd826c3c89aef350131` | v7.0.0 |
| `softprops/action-gh-release` | `a06a81a03ee405af7f2048a818ed3f03bbf83c7b` | v2.5.0 |

## Version String Locations

The workflow updates version strings in three files:

| File | Line | Pattern |
|------|------|---------|
| `lib/tart.ml` | 3 | `let version = "X.Y.Z"` |
| `lisp/tart.el` | 8 | `;; Version: X.Y.Z` |
| `lisp/tart-mode.el` | 8 | `;; Version: X.Y.Z` |

## Binary Distribution

The artifact naming scheme matches what `tart-install-binary` in `tart-mode.el` expects:

- **Tag format:** `vX.Y.Z` (e.g., `v0.2.0`)
- **Asset names:** `tart-{os}-{arch}` (e.g., `tart-darwin-arm64`)
- **Asset count:** 4 (all matrix platform combinations)

The `tart-install-binary` function:

1. Queries GitHub Releases API for the repository
2. Selects release based on `tart-version` customization (default: `latest`)
3. Detects platform using `system-type` and `system-configuration`
4. Finds matching asset by name (e.g., `tart-darwin-arm64`)
5. Downloads to `~/.emacs.d/tart/bin/tart-VERSION`
6. Sets executable permissions

## Prerequisites

### GitHub Environment

Create a GitHub environment called `release` in repository settings:

1. Navigate to Settings → Environments → New environment
2. Name: `release`
3. Deployment branches: restrict to `main` only
4. Add environment secret: `ANTHROPIC_API_KEY`

The `environment: release` declaration in the `release` job ensures the API key is only accessible to release builds on the `main` branch, not to PR-triggered workflows.

### Permissions

The workflow requires `contents: write` for two jobs:

- **tag:** commits version bump and pushes tag
- **release:** creates GitHub release

All other jobs use `contents: read` (the default).

## Verification

After triggering a release workflow:

1. **Version bump:** Verify version strings updated in `lib/tart.ml`, `lisp/tart.el`, `lisp/tart-mode.el`
2. **Tag creation:** Verify tag `vX.Y.Z` exists in repository
3. **Commit push:** Verify version bump commit is on `main`
4. **Build artifacts:** Verify all 4 platform binaries attached to release
5. **Changelog:** Verify release body contains formatted changelog
6. **Prerelease flag:** Verify prerelease detection for rc/alpha/beta tags
7. **Binary compatibility:** Run `M-x tart-install-binary` in Emacs and verify download succeeds

## Error Handling

| Failure Point | Behavior |
|---------------|----------|
| Version parse error | `preflight` job fails, no tag created |
| Test failure | `tag` job does not run, no tag created |
| Build failure (any platform) | `release` job does not run, tag exists but no release |
| Changelog API failure | Falls back to raw git log, release still created |
| Tag already exists | Git push fails, workflow terminates |
