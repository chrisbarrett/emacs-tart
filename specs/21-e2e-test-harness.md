# Spec 21: E2E Test Harness

ERT tests for Emacs integration against live tart processes.

**Dependencies:** [Spec 10](./10-emacs-integration.md), [Spec 14](./14-tart-el.md).

## Goal

Fast local feedback when developing tart-mode.el. Full coverage: eglot/LSP,
REPL, minor mode.

## Constraints

- `emacs --batch` + ERT
- Real `tart lsp` / `tart repl` processes
- Each test cleans up processes
- <10s full suite

## Output

```
lisp/
├── tart-e2e-tests.el
└── tart-test-helpers.el
scripts/
└── run-emacs-tests.sh
test/fixtures/e2e/
├── valid.el + valid.tart    ; passes type check
└── error.el + error.tart    ; has type errors
```

## Requirements

### R1: Test runner

`./scripts/run-emacs-tests.sh` → `emacs --batch` with `load-path` including
`lisp/`, discovers `*-tests.el`, exits 0/1.

**Verify:** `./scripts/run-emacs-tests.sh`

### R2: Helpers

`tart-test-helpers.el` provides:

```elisp
(tart-test-with-temp-buffer (&rest body))  ; buffer-local context
(tart-test-wait-for (pred &optional timeout))  ; poll async, default 5s
(tart-test-fixture-path (name))  ; → "test/fixtures/e2e/NAME"
(tart-test-ensure-tart)  ; skip if no executable
```

**Verify:** `(macroexpand '(tart-test-with-temp-buffer ...))`

### R3: LSP connects

```elisp
(ert-deftest tart-e2e-lsp-connects ()
  (tart-test-with-fixture "valid.el"
    (eglot-ensure)
    (tart-test-wait-for (lambda () (eglot-current-server)))
    (should (process-live-p (eglot--process (eglot-current-server))))))
```

### R4: LSP diagnostics

Buffer with `(upcase 42)` → `(flymake-diagnostics)` non-empty, message contains
"mismatch".

**Verify:** `ert-deftest tart-e2e-lsp-diagnostics`

### R5: LSP hover

Cursor on function → eldoc returns type info.

**Verify:** `ert-deftest tart-e2e-lsp-hover`

### R6: REPL starts

`(run-tart)` → `*tart*` buffer exists, `inferior-tart-mode`, process live,
prompt within 3s.

**Verify:** `ert-deftest tart-e2e-repl-starts`

### R7: REPL eval

Send `(+ 1 2)` → output contains `3`.

**Verify:** `ert-deftest tart-e2e-repl-eval`

### R8: REPL ,type

Send `,type (mapcar #'1+ '(1 2 3))` → output contains type, no evaluation.

**Verify:** `ert-deftest tart-e2e-repl-type-command`

### R9: send-defun

Point inside defun → `tart-send-defun` → defun appears in REPL.

**Verify:** `ert-deftest tart-e2e-send-defun`

### R10: type-at-point

`tart-type-at-point` on expression → type displayed.

**Verify:** `ert-deftest tart-e2e-type-at-point`

### R11: Keybindings

`tart-mode` active:

| Key       | Command               |
|-----------|-----------------------|
| `C-c C-z` | `tart-switch-to-repl` |
| `C-c C-c` | `tart-send-defun`     |
| `C-c C-t` | `tart-type-at-point`  |

**Verify:** `ert-deftest tart-e2e-keybindings`

### R12: Cleanup

Test teardown kills all tart processes and temp buffers. No orphans.

**Verify:** `(length (process-list))` unchanged after suite

### R13: Timeouts

Async waits fail with message after timeout, never hang.

**Verify:** `(should-error (tart-test-wait-for (lambda () nil) 0.1))`

### R14: Fixtures

`valid.el` + `valid.tart` → `tart check` passes.
`error.el` + `error.tart` → `tart check` fails with diagnostic.

**Verify:** `tart check test/fixtures/e2e/valid.el`

## Tasks

- [x] [R1] `scripts/run-emacs-tests.sh`
- [x] [R2] `lisp/tart-test-helpers.el`
- [x] [R14] Fixture files
- [x] [R3-R5] LSP tests
- [x] [R6-R8] REPL tests
- [x] [R9-R11] Integration tests
- [x] [R12-R13] Cleanup + timeouts
