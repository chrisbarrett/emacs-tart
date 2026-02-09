# Spec 80 — Release Workflow

Rewrite `.github/workflows/release.yml` to match [Spec 80](./specs/80-release-workflow.md).
The current workflow uses tag-push triggers and `nix build .#default`; the spec
requires `workflow_dispatch` with automated version bumping, `nix develop`-based
builds, and LLM-powered changelogs.

---

## Task 1 — Replace trigger and add preflight job

Rewrite the top of `release.yml`:

- Change trigger from `push: tags: ['v*']` to `workflow_dispatch` with a
  `release_type` choice input (`major`, `minor`, `patch`; default `minor`).
- Remove the `verify-main` job (no longer needed — `workflow_dispatch` runs on
  the branch it is triggered from).
- Add `preflight` job that:
  1. Checks out at HEAD
  2. Extracts current version from `lib/tart.ml` line 3
     (`let version = "X.Y.Z"`)
  3. Parses as semver, validates format
  4. Computes new version based on `release_type` input
  5. Sets job outputs: `version` and `tag` (`vX.Y.Z`)

`preflight` runs in parallel with `test` (no `needs`).

**Verify:** YAML is valid (`python3 -c "import yaml; yaml.safe_load(open('.github/workflows/release.yml'))"`)

---

## Task 2 — Update test job with opam cache

Update the `test` job to:

- Remove `needs: verify-main` (it now runs at the top level, parallel with
  `preflight`)
- Add opam cache step (matching CI workflow):
  - Key: `opam-${{ runner.os }}-${{ hashFiles('tart.opam') }}`
  - Paths: `~/.opam`, `_opam`
- Keep existing `nix develop --command dune build` and `dune test` steps

**Verify:** YAML is valid

---

## Task 3 — Add tag job

Add a `tag` job with `needs: [preflight, test]` and
`permissions: contents: write`:

1. Checkout with `fetch-depth: 0`
2. Update version in three files using `sed`:
   - `lib/tart.ml`: `let version = "X.Y.Z"`
   - `lisp/tart.el`: `;; Version: X.Y.Z`
   - `lisp/tart-mode.el`: `;; Version: X.Y.Z`
3. Configure git identity (`github-actions[bot]`)
4. Commit: `Bump version to ${{ needs.preflight.outputs.version }}`
5. Create annotated tag:
   `git tag -a ${{ needs.preflight.outputs.tag }} -m "Release ..."`
6. Push commit and tag to `main`
7. Set output `ref` to the new tag for the build job to checkout

**Verify:** YAML is valid

---

## Task 4 — Update build job to use dune

Update the `build` job:

- Change `needs: test` to `needs: tag`
- Checkout at the tagged ref (`needs.tag.outputs.ref`)
- Add opam cache step (same as test job)
- Replace `nix build .#default` with `nix develop --command dune build`
- Replace `cp result/bin/tart ...` with
  `cp _build/default/bin/main.exe tart-${{ matrix.os }}-${{ matrix.arch }}`
- Keep the existing matrix and upload-artifact steps

**Verify:** YAML is valid

---

## Task 5 — Update release job with LLM changelog

Rewrite the `release` job:

- Change `needs: build` to `needs: [preflight, build]`
- Add `environment: release` (provides `ANTHROPIC_API_KEY`)
- Add checkout step with `fetch-depth: 0` at the tag ref
- Add changelog generation step:
  1. Detect previous tag: `git describe --tags --abbrev=0 HEAD^`
  2. Extract commits: `git log --oneline PREV_TAG..HEAD`
  3. Call Anthropic API with curl (prompt from spec)
  4. Fallback to raw git log on API failure
- Update release creation:
  - Set explicit `tag_name`, `name`, and `body` (remove
    `generate_release_notes: true`)
  - Use tag from preflight outputs
  - Use changelog as body
- Keep existing prerelease detection and artifact download

**Verify:** YAML is valid

---

## Task 6 — Final validation

- Full YAML syntax validation
- Verify the job DAG matches spec:
  `preflight ∥ test → tag → build → release`
- Verify all action SHAs match spec's pinned versions table
- Verify permissions: `contents: write` only on `tag` and `release`
- Run `nix develop --command dune test --force` to confirm no regressions
