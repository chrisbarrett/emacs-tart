# Spec 63: Minor-Mode Lifecycle

Clean setup/teardown of `emacs-lisp-mode` modifications tied to `tart-mode`
activation.

- **Dependencies:**
  - [Spec 10](./10-emacs-integration.md) tart-mode minor mode exists

## Goal

Ensure any advice, syntax-table overrides, or other modifications applied
to `emacs-lisp-mode` buffers by `tart-mode` are properly installed on
enable and removed on disable, so that disabling `tart-mode` fully restores
the buffer to its unmodified state.

## Context

[Spec 10](./10-emacs-integration.md) established that tart should be
"non-intrusive" and not "modify elisp-mode globally."  As tart-mode gains
features (e.g., custom `check-parens` behaviour, additional font-lock
rules), any buffer-local hacks must respect the minor-mode lifecycle.

## Requirements

### R1: Enable installs modifications

**Given** an `emacs-lisp-mode` buffer
**When** `tart-mode` is enabled
**Then** any tart-specific buffer-local modifications (advice, syntax
overrides, font-lock additions, hook functions) are installed

**Verify:** Enable `tart-mode`; relevant modifications are active in
that buffer only

### R2: Disable removes modifications

**Given** an `emacs-lisp-mode` buffer with `tart-mode` enabled
**When** `tart-mode` is disabled
**Then** all tart-specific modifications are removed and the buffer
behaves exactly as a plain `emacs-lisp-mode` buffer

**Verify:** Enable then disable `tart-mode`; `check-parens` and
font-lock behave identically to a buffer that never had `tart-mode`

### R3: No global side effects

**Given** `tart-mode` is enabled in one buffer
**Then** other `emacs-lisp-mode` buffers are unaffected — no global
advice, no modifications to shared syntax tables, no global hook additions

**Verify:** Open two elisp buffers; enable `tart-mode` in one; the other
is unaffected

### R4: Idempotent toggle

**Given** `tart-mode` is toggled on/off multiple times
**Then** no accumulated state — each enable/disable cycle is clean

**Verify:** Toggle `tart-mode` three times; buffer state is identical to
fresh `emacs-lisp-mode`

## Tasks

- [x] [R1–R4] Audit current `tart-mode` body for any modifications that
  need enable/disable handling
- [x] [R1–R2] Implement setup/teardown in the `tart-mode` definition body
  (the `:body` runs on both enable and disable — use the mode variable to
  branch)
- [x] [R3] Ensure all modifications are buffer-local
- [x] [R4] Test idempotent toggling
