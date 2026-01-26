# Spec 10: Emacs Integration

Emacs package providing tart REPL, LSP integration, and interactive commands.

**Dependencies:** Spec 09 CLI complete (especially `tart repl` and `tart lsp`).

## Goal

Provide `lisp/tart.el` with an inferior process mode for the tart REPL, eglot
configuration, and commands for interactive type exploration.

## Constraints

- **Emacs 29+**: Use built-in eglot; no external dependencies beyond comint
- **Standard patterns**: Follow inferior-lisp, comint, and eglot conventions
- **Non-intrusive**: Don't modify elisp-mode globally; use hooks and minor modes

## Output

```
lisp/
└── tart.el              ; All functionality in one file
```

## Requirements

### R1: Inferior tart mode

**Given** `tart repl` is available on PATH or configured via `tart-executable`
**When** user calls `M-x run-tart` or `M-x inferior-tart`
**Then** a comint buffer `*tart*` opens running `tart repl`
**And** the buffer uses `inferior-tart-mode` with appropriate keybindings

**Verify:** `M-x run-tart` opens REPL; typing `(+ 1 2)` shows `3 :: Int`

### R2: REPL input handling

**Given** an `inferior-tart-mode` buffer
**When** user enters an expression and presses RET
**Then** the expression is sent to the tart process
**And** output is displayed with proper fontification
**And** multi-line input is supported (incomplete sexps wait for more input)

**Verify:** Enter `(let ((x 1))` then `x)` on next line; result appears

### R3: Send to REPL commands

**Given** an elisp buffer and a running tart REPL
**When** user invokes send commands
**Then** code is sent to the REPL:
- `tart-send-region` — send active region
- `tart-send-defun` — send defun at point
- `tart-send-last-sexp` — send sexp before point
- `tart-send-buffer` — send entire buffer

**Verify:** `C-c C-r` sends region; `C-c C-c` sends defun; output in `*tart*`

### R4: REPL commands from elisp buffer

**Given** an elisp buffer with `tart-mode` enabled
**When** user invokes type inspection commands
**Then** they interact with the REPL:
- `tart-type-at-point` — send `,type <sexp>` and display result
- `tart-expand-at-point` — send `,expand <sexp>` and display result

**Verify:** `C-c C-t` on `(mapcar #'1+ xs)` shows type in echo area

### R5: Eglot integration

**Given** `tart lsp` is available
**When** user opens an `.el` file with a sibling `.tart` file
**Then** eglot can be started with `M-x eglot`
**And** tart is registered as the LSP server for that buffer

Configuration via:
```elisp
(add-to-list 'eglot-server-programs '(elisp-mode . ("tart" "lsp")))
```

**Verify:** `M-x eglot` in typed elisp file connects; hover shows types

### R7: Minor mode for elisp

**Given** `tart-mode` minor mode
**When** enabled in an elisp buffer
**Then** keybindings are available for REPL interaction
**And** mode-line indicates tart is active

Suggested keymap (under `C-c` prefix):
| Key     | Command                |
|---------|------------------------|
| C-c C-z | `tart-switch-to-repl`  |
| C-c C-c | `tart-send-defun`      |
| C-c C-r | `tart-send-region`     |
| C-c C-e | `tart-send-last-sexp`  |
| C-c C-b | `tart-send-buffer`     |
| C-c C-t | `tart-type-at-point`   |
| C-c C-x | `tart-expand-at-point` |

**Verify:** Enable `tart-mode`; `C-c C-z` switches to REPL

### R8: Customization

**Given** the tart package
**When** user customizes it
**Then** these options are available:
- `tart-executable` — path to tart binary (default: `"tart"`)
- `tart-repl-args` — additional args for REPL (default: `nil`)
- `tart-mode-hook` — hook for tart-mode activation

**Verify:** Setting `tart-executable` to custom path uses that binary

### R9: REPL history

**Given** an `inferior-tart-mode` buffer
**When** user navigates history
**Then** `M-p` / `M-n` cycle through previous inputs
**And** history persists across REPL restarts (via comint-input-ring)

**Verify:** Enter expressions; restart REPL; `M-p` recalls previous input

### R10: Error display

**Given** a type error in the REPL
**When** the error is displayed
**Then** it uses appropriate faces for readability
**And** file:line references are clickable (compilation-mode style)

**Verify:** Type error shows with highlighting; clicking location jumps to file

### R11: README setup instructions

**Given** the README.md in the repository root
**When** a user wants to set up tart.el
**Then** the README contains:
- Installation instructions (load-path, require)
- Eglot configuration snippet
- Basic usage examples (run-tart, tart-mode)
- Keybinding reference table

**Verify:** README contains `(require 'tart)` and eglot setup example

## Tasks

- [x] [R1,R2,R9] Implement `inferior-tart-mode` with comint
- [x] [R3] Implement send-to-REPL commands
- [x] [R4] Implement type/expand inspection commands
- [x] [R5] Add eglot server configuration
- [ ] [R7] Implement `tart-mode` minor mode with keymap
- [x] [R8] Add customization options
- [ ] [R10] Add compilation-mode error parsing
- [x] [R11] Add setup instructions to README

Run review agent after REPL interaction works before implementing eglot integration.
