# Spec 74 — Emacs Development Mode

> Consolidates specs: [10](./.archive/10-emacs-integration.md), [14](./.archive/14-tart-el.md), [22](./.archive/22-ci-releases.md), [23](./.archive/23-binary-installation.md), [65](./.archive/65-download-security.md), [63](./.archive/63-tart-minor-mode-lifecycle.md), [64](./.archive/64-self-typing.md)

## Overview

The Emacs development mode encompasses the full Emacs integration layer for
tart: the `tart-mode` minor mode for interactive type-checking in
`emacs-lisp-mode` buffers, the `tart.el` runtime macros that provide zero-cost
inline type annotations, binary installation from GitHub releases with platform
detection and download security hardening, minor-mode lifecycle management
ensuring clean setup and teardown, and self-typing of tart's own Elisp
libraries via dogfooded `.tart` signature files.

## tart-mode Minor Mode

### Architecture

Two Elisp files provide a clean separation of concerns:

| File | Role |
|------|------|
| `lisp/tart.el` | Runtime macros only (no dev tooling) |
| `lisp/tart-mode.el` | Development tooling (REPL, eglot, minor mode, binary management) |

Packages depend on `tart.el` via `(require 'tart)` without pulling in
development tooling.

### Minor mode

`tart-mode` is a minor mode for `emacs-lisp-mode` buffers. It provides
keybindings for REPL interaction and type inspection, and displays an indicator
in the mode line. It does not modify `emacs-lisp-mode` globally.

Suggested keymap (under `C-c` prefix):

| Key | Command |
|-----|---------|
| `C-c C-z` | `tart-switch-to-repl` |
| `C-c C-c` | `tart-send-defun` |
| `C-c C-r` | `tart-send-region` |
| `C-c C-e` | `tart-send-last-sexp` |
| `C-c C-b` | `tart-send-buffer` |
| `C-c C-t` | `tart-type-at-point` |
| `C-c C-x` | `tart-expand-at-point` |

### REPL integration

`M-x run-tart` (or `M-x inferior-tart`) opens a comint buffer `*tart*`
running `tart repl`. The buffer uses `inferior-tart-mode` with multi-line
input support, history persistence via `comint-input-ring`, fontified output,
and compilation-mode-style clickable file:line references in error output.

Send-to-REPL commands (`tart-send-region`, `tart-send-defun`,
`tart-send-last-sexp`, `tart-send-buffer`) transmit code from an
`emacs-lisp-mode` buffer to the running REPL. Type inspection commands
(`tart-type-at-point`, `tart-expand-at-point`) send `,type` and `,expand`
meta-commands and display the result in the echo area.

### Eglot integration

`tart-eglot` starts eglot for the current buffer, prompting to install the
binary if it is missing. `tart-eglot-ensure` is suitable for use in
`emacs-lisp-mode-hook`. The eglot server program is registered as
`("tart" "lsp")`.

### Customization

| Variable | Default | Purpose |
|----------|---------|---------|
| `tart-executable` | `'managed` | Path to tart binary, or `'managed` for automatic installation |
| `tart-version` | `'latest` | Release version to install (`'latest` or semver string) |
| `tart-repl-args` | `nil` | Additional arguments for the REPL |
| `tart-lsp-args` | `nil` | Additional arguments for the LSP server |
| `tart-mode-hook` | `nil` | Hook run on `tart-mode` activation |

### Lifecycle management

Enabling `tart-mode` installs buffer-local modifications (advice, syntax
overrides, font-lock additions, hook functions). Disabling `tart-mode` removes
all modifications, restoring the buffer to plain `emacs-lisp-mode` state.

Invariants:

- **No global side effects** --- other `emacs-lisp-mode` buffers are unaffected
  by `tart-mode` in one buffer. No global advice, no shared syntax table
  modifications, no global hook additions.
- **Idempotent toggle** --- toggling `tart-mode` on and off multiple times
  produces no accumulated state; each cycle is clean.

## Runtime Macros (tart.el)

### Design constraints

All macros have zero runtime cost: they expand to their form argument unchanged
(or to `nil`). Type expressions use the same grammar as `.tart` signature
files. Type annotations are checked (assert semantics), not trusted.
Parameterized types (`(list a)`, `(vector a)`, `(hash-table k v)`) are
invariant for soundness.

### Function type signatures

`(declare (tart (PARAMS) -> RETURN))` inside a function definition declares
the function's type signature. The checker verifies the body's inferred type
against the declared return type and binds parameters to their declared types
in the body. Works with `defun`, `cl-defun`, `defmacro`, etc.

```elisp
(defun my-add (x y)
  (declare (tart (int int) -> int))
  (+ x y))

(defun my-identity (x)
  (declare (tart [a] (a) -> a))
  x)
```

### Type assertion macro

`(tart TYPE FORM)` checks that FORM's inferred type is compatible with TYPE.
At runtime, the macro expands to FORM unchanged.

```elisp
(tart string "hello")       ; OK
(tart string (+ 1 2))       ; Error: int is not compatible with string
```

### Variable type declarations

`(defvar NAME (tart TYPE VALUE))` and `(defconst NAME (tart TYPE VALUE))`
give NAME the type TYPE and check VALUE against it. Subsequent `setq`/`setf`
to NAME are checked against the declared type.

`(tart-declare NAME TYPE)` declares a variable's type without providing an
initial value. Reads and writes of NAME are checked against the declared type.

### File-local type aliases

`(tart-type NAME DEFINITION)` defines a type alias scoped to the current file
(not exported). `(tart-type NAME [VARS] DEFINITION)` defines a parameterized
alias. Both expand to `nil` at runtime.

```elisp
(tart-type int-pair (tuple int int))
(tart-type predicate [a] ((a) -> bool))
```

### Interaction with .tart files

`.tart` declarations define the authoritative public interface. Inline
annotations provide internal types and cannot contradict `.tart` declarations.

## Binary Installation

### Version and executable resolution

`tart-version` controls which release to install: `'latest` queries the GitHub
API for the most recent release; a semver string (e.g., `"0.2.0"`) pins to
that version.

`tart-executable` controls which binary is used: `'managed` resolves to the
downloaded binary at `~/.emacs.d/tart/bin/tart-VERSION`; a string value is
used directly as an executable path.

### Install command

`M-x tart-install-binary` queries the GitHub releases API, detects the
platform, downloads the appropriate asset, and makes it executable. Downloads
show progress in the echo area using `url-retrieve` or `make-process` with
curl.

### Platform detection

| `system-type` | arch | Asset |
|---------------|------|-------|
| `darwin` | `aarch64` | `tart-darwin-arm64` |
| `darwin` | `x86_64` | `tart-darwin-x86_64` |
| `gnu/linux` | `aarch64` | `tart-linux-arm64` |
| `gnu/linux` | `x86_64` | `tart-linux-x86_64` |

### Error handling

- No network: clear error message, no hang
- Asset not found for platform: error listing supported platforms
- GitHub rate limit: suggest setting `GITHUB_TOKEN`

## Release Pipeline

`.github/workflows/release.yml` builds tart binaries on tag push and attaches
them to a GitHub Release. This is the supply side of the binary installation
flow above.

### Trigger

The workflow runs on pushes matching `v*` tags:

```yaml
on:
  push:
    tags: ['v*']
```

### Build matrix

Each target platform builds via `nix build .#default` with Nix caching:

| OS | Arch | Runner | Asset name |
|----|------|--------|------------|
| darwin | arm64 | `macos-latest` | `tart-darwin-arm64` |
| darwin | x86_64 | `macos-13` | `tart-darwin-x86_64` |
| linux | x86_64 | `ubuntu-latest` | `tart-linux-x86_64` |
| linux | arm64 | `ubuntu-24.04-arm` | `tart-linux-arm64` |

### Release creation

The release job uses `softprops/action-gh-release` to create the GitHub
Release and attach all 4 binaries. Tags matching `v*-rc*`, `v*-alpha*`, or
`v*-beta*` produce a pre-release.

### Asset naming convention

Asset names follow the pattern `tart-{os}-{arch}`, matching the platform
detection table in [Binary Installation](#binary-installation) above. The
Emacs install command downloads the asset whose name matches the detected
platform.

## Download Security

### Risky local variables

`tart-executable` and `tart-install-directory` are marked with
`risky-local-variable`, causing Emacs to warn before applying values from
`.dir-locals.el`. This prevents a malicious repository from silently
redirecting which binary is executed or where binaries are written.

```elisp
(put 'tart-executable 'risky-local-variable t)
(put 'tart-install-directory 'risky-local-variable t)
```

### Version safe predicate

The `:safe` predicate for `tart-version` rejects path-traversal payloads.
Only `'latest` or strings matching strict semver with optional pre-release
suffix are accepted:

    [0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9][A-Za-z0-9.-]*)?

Values like `"../../evil"` or `"0.1"` are treated as unsafe and trigger
Emacs's risky-variable prompt.

## Self-Typing

### Signature files

| File | Covers |
|------|--------|
| `lisp/tart.tart` | Public macros from `tart.el` (`tart`, `tart-type`, `tart-declare`) |
| `lisp/tart-mode.tart` | Public API from `tart-mode.el` (interactive commands, customization variables) |

Running `tart check` against tart's own `.el` files exercises the type checker
on real-world Emacs Lisp. Only the public API (autoloaded, interactive, and
user-facing functions and variables) is typed; private helpers prefixed
`tart--` may be omitted unless needed for type-checking accuracy.

### Gap documentation

Constructs the type checker cannot handle are documented in `BUGS.md` with the
failing construct, the observed error or unsound behaviour, and a minimal
reproduction where possible.

## Key Files

| File | Role |
|------|------|
| `lisp/tart.el` | Runtime macros (type assertions, aliases, declarations) |
| `lisp/tart-mode.el` | Minor mode, REPL, eglot, binary installation, download security |
| `lisp/tart.tart` | Signatures for `tart.el` public API |
| `lisp/tart-mode.tart` | Signatures for `tart-mode.el` public API |
| `.github/workflows/release.yml` | CI release pipeline (tag-triggered binary builds) |

## Deferred

No items currently deferred.
