# Spec 22: CI Release Builds

`.github/workflows/release.yml` — build tart binaries on tag push, attach to GitHub Release.

## Constraints

- Trigger: `v*` tags
- Nix flake build
- Artifacts: `tart-{os}-{arch}`

## Matrix

| os     | arch   | runner           |
|--------|--------|------------------|
| darwin | arm64  | macos-latest     |
| darwin | x86_64 | macos-13         |
| linux  | x86_64 | ubuntu-latest    |
| linux  | arm64  | ubuntu-24.04-arm |

## Requirements

### R1: Tag trigger

```yaml
on:
  push:
    tags: ['v*']
```

### R2: Nix build with cache

```yaml
- uses: DeterminateSystems/determinate-nix-action@v3
- uses: DeterminateSystems/magic-nix-cache-action@v8
- run: nix build .#default
```

### R3: Artifact naming

`cp result/bin/tart tart-${{ matrix.os }}-${{ matrix.arch }}`

### R4: Release creation

Use `softprops/action-gh-release`. Tag `v*-rc*`, `v*-alpha*`, `v*-beta*` → `prerelease: true`.

### R5: Assets

All 4 binaries attached: `tart-darwin-arm64`, `tart-darwin-x86_64`, `tart-linux-arm64`, `tart-linux-x86_64`

**Verify:** `gh release view <tag>` lists 4 assets

## Tasks

- [ ] Workflow with trigger + matrix
- [ ] Nix build steps
- [ ] Release job with prerelease logic
