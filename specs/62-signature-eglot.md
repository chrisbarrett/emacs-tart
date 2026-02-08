# Spec 62: Signature Eglot

LSP integration for `.tart` signature buffers via eglot.

- **Dependencies:**
  - [Spec 08](./08-lsp-server.md) tart LSP server
  - [Spec 10](./10-emacs-integration.md) eglot registration for elisp
  - [Spec 58](./58-signature-syntax-table.md) syntax table correct

## Goal

Ensure `tart-signature-mode` buffers can connect to the tart LSP server
via eglot, providing diagnostics, hover, and other LSP features when
editing `.tart` files.

## Context

Eglot is currently registered only for `emacs-lisp-mode`.
`tart-signature-mode` derives from `lisp-mode` and has no eglot
configuration, so `M-x eglot` in a `.tart` buffer does not know which
server to use.

## Requirements

### R1: Eglot server registration

**Given** `tart-setup-eglot` is non-nil (the default)
**When** `tart-mode.el` is loaded
**Then** `tart-signature-mode` is registered in `eglot-server-programs`
with the same server command as `emacs-lisp-mode` (`tart lsp`)

**Verify:** `M-x eglot` in a `.tart` buffer connects to the tart LSP
server without manual configuration

### R2: Diagnostics in signature buffers

**Given** eglot is connected in a `.tart` buffer
**When** the file contains a malformed or invalid signature
**Then** diagnostics (errors/warnings) appear in the buffer

**Verify:** Introduce a deliberate error in a `.tart` file; eglot
underlines it

### R3: Shared workspace with elisp buffers

**Given** eglot is running for an `.el` buffer and the user opens the
sibling `.tart` file
**When** both buffers belong to the same project
**Then** they share the same LSP server connection (no duplicate servers)

**Verify:** Open `foo.el` with eglot; open `foo.tart`; `M-x
eglot-show-workspace-configuration` shows one server

## Tasks

- [x] [R1] Add `tart-signature-mode` to `eglot-server-programs`
  alongside the existing `emacs-lisp-mode` entry
- [x] [R3] Verify shared workspace behaviour (may require no code change
  if eglot's project detection handles it)
