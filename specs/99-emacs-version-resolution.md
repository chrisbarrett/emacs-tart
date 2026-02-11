# Spec 99 — Emacs Version Resolution

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md), [Spec 94 — Emacs Coverage Auto Version Detection](./94-emacs-coverage-auto-version.md)

## Overview

`tart emacs-coverage` accepts `--emacs-version` but needs to resolve shorthand specifiers to concrete Emacs refs. This enables source acquisition ([Spec 100](./100-emacs-source-acquisition.md)) for any version.

## Requirements

### R1: Semver shorthand

`29` resolves to the latest `29.x.y` release tag. `29.1` resolves to exact tag `emacs-29.1`. Full versions like `29.1.2` resolve exactly if the tag exists; otherwise fail per R5.

### R2: `latest` identifier

`latest` resolves to the most recent stable release tag.

### R3: Development identifiers

`dev`, `devel`, and `git` resolve to HEAD of the Emacs `main` branch — no tag resolution.

### R4: Arbitrary git SHAs

A hex string of 7+ characters is treated as a literal git commit SHA — no tag resolution.

### R5: Resolution failure

When no matching tag or ref exists, print usage plus the 3 most recent tags from the Emacs git repository and exit non-zero (code 2).

### R6: Remote tag listing

Resolution queries tags from the official Emacs git repository (`git.savannah.gnu.org/git/emacs.git`). No local Emacs installation required.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/coverage/emacs_source.ml` | Version detection and resolution |
| `lib/coverage/emacs_source.mli` | Version resolution interface |
| `bin/main.ml` | CLI dispatch and `--emacs-version` parsing |
