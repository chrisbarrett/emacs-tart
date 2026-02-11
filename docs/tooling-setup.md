# Editor & Tooling Setup

Tart ships two Emacs Lisp packages for editor integration. Install both to get
type checking, diagnostics, and a REPL inside Emacs.

## Prerequisites

- Emacs 29.1 or later (for eglot)
- The `tart` binary ([Getting Started][getting-started])

## Package Installation

### tart.el -- Runtime Annotation Macros

`tart.el` provides macros that the type checker recognizes but expand to no-ops
at runtime. It has **no dependencies** and supports Emacs 24.1+.

| Macro          | Purpose                             |
| -------------- | ----------------------------------- |
| `tart`         | Assert a type or instantiate a poly |
| `tart-type`    | Define a file-local type alias      |
| `tart-declare` | Declare a variable's type           |

```elisp
(require 'tart)

(tart string "hello")              ; type assertion
(tart [int] identity 42)           ; explicit instantiation
(tart-type predicate [a] ((a) -> bool))
(tart-declare my-buffer buffer)
```

### tart-mode.el -- Development Tools

`tart-mode.el` provides LSP integration (via eglot), a REPL, keybindings, and
binary management. It requires Emacs 29.1+.

```elisp
(require 'tart-mode)
(add-hook 'emacs-lisp-mode-hook #'tart-eglot-ensure)
```

That single hook is enough to enable type checking in every Elisp buffer that
has a sibling `.tart` signature file.

## Managed Binary

By default `tart-executable` is set to `'managed`, which means `tart-mode`
downloads and manages the binary for you.

Run `M-x tart-install-binary` to download the latest release from GitHub. The
binary is stored based on `tart-directory-style`:

| Style    | Location                                              |
| -------- | ----------------------------------------------------- |
| `'xdg`   | `$XDG_DATA_HOME/tart/bin/` (~/.local/share/tart/bin/) |
| `'emacs` | `~/.emacs.d/tart/bin/`                                |

Supported platforms: `darwin-arm64`, `darwin-x86_64`, `linux-arm64`,
`linux-x86_64`.

To use your own binary instead, set `tart-executable` to an absolute path or a
command name on `exec-path`:

```elisp
(setq tart-executable "/usr/local/bin/tart")
```

Pin a project to a specific version in `.dir-locals.el`:

```elisp
((nil . ((tart-version . "0.2.0"))))
```

## Eglot Integration

When `tart-setup-eglot` is non-nil (the default), loading `tart-mode`
automatically registers tart in `eglot-server-programs` for both
`emacs-lisp-mode` and `tart-signature-mode`. Eglot shares a single server
connection for `.el` and `.tart` buffers in the same project.

Two convenience commands control when eglot starts:

| Command             | Behavior                                                                          |
| ------------------- | --------------------------------------------------------------------------------- |
| `tart-eglot`        | Start eglot if a sibling `.tart` file exists; prompt to install binary if missing |
| `tart-eglot-ensure` | Same, but silently skip if no binary (safe for hooks)                             |

LSP features available through eglot:

- Diagnostics (type errors, warnings)
- Hover (display types)
- Go-to-definition
- Find references
- Completion
- Signature help

Pass extra flags to the language server with `tart-lsp-args`:

```elisp
(setq tart-lsp-args '("--verbose"))
```

## tart-signature-mode

`tart-signature-mode` is a major mode for `.tart` signature files, derived from
`lisp-mode`. It activates automatically for any file ending in `.tart`.

Features:

- **Font-lock** for `defun`, `defvar`, `type`, `let-type`, `open`, `include`,
  the `->` arrow operator, and type-variable quantifiers in `[...]`
- **Custom indentation** tuned for signature declarations
- **Imenu** support for `defun`, `defvar`, and `type` declarations

## tart-mode Minor Mode

`tart-mode` is a minor mode for Elisp buffers (lighter: `Tart`). It provides
keybindings for REPL interaction and type inspection.

| Key       | Command                | Description                      |
| --------- | ---------------------- | -------------------------------- |
| `C-c C-z` | `tart-switch-to-repl`  | Switch to REPL (start if needed) |
| `C-c C-c` | `tart-send-defun`      | Send defun at point to REPL      |
| `C-c C-r` | `tart-send-region`     | Send selected region to REPL     |
| `C-c C-e` | `tart-send-last-sexp`  | Send sexp before point to REPL   |
| `C-c C-b` | `tart-send-buffer`     | Send entire buffer to REPL       |
| `C-c C-t` | `tart-type-at-point`   | Show type of sexp at point       |
| `C-c C-x` | `tart-expand-at-point` | Show macro expansion at point    |

Enable it manually or via a hook:

```elisp
(add-hook 'emacs-lisp-mode-hook #'tart-mode)
```

## REPL

Start the REPL with `M-x run-tart` (or its alias `M-x inferior-tart`). The
buffer is named `*tart*` and uses `inferior-tart-mode`, derived from
`comint-mode`.

REPL commands (typed at the `tart>` prompt):

| Command          | Description             |
| ---------------- | ----------------------- |
| `,type <expr>`   | Show type of expression |
| `,expand <expr>` | Macro-expand expression |
| `,env`           | Show the environment    |
| `,help`          | List available commands |
| `,quit` / `,q`   | Exit the REPL           |

Input history is saved between sessions. The history file location depends on
`tart-directory-style`:

| Style    | Location                                                   |
| -------- | ---------------------------------------------------------- |
| `'xdg`   | `$XDG_STATE_HOME/tart/repl-history` (~/.local/state/tart/) |
| `'emacs` | `~/.emacs.d/tart-repl-history`                             |

The REPL also enables `compilation-shell-minor-mode`, so `next-error` and
`previous-error` navigate to locations reported in tart output.

Pass extra flags with `tart-repl-args`:

```elisp
(setq tart-repl-args '("--no-prelude"))
```

## Customization Variables

All variables live in the `tart` customize group (`M-x customize-group RET tart RET`).

| Variable                        | Default    | Description                                                   |
| ------------------------------- | ---------- | ------------------------------------------------------------- |
| `tart-executable`               | `'managed` | `'managed` for auto-download, or path string                  |
| `tart-version`                  | `'latest`  | `'latest` or specific version (e.g. `"0.2.0"`)                |
| `tart-lsp-args`                 | `nil`      | Extra arguments for `tart lsp`                                |
| `tart-repl-args`                | `nil`      | Extra arguments for `tart repl`                               |
| `tart-directory-style`          | `'xdg`     | `'xdg` (XDG dirs) or `'emacs` (~/.emacs.d/)                   |
| `tart-repl-history-file`        | `'default` | History path; based on directory-style, custom path, or `nil` |
| `tart-install-directory`        | `'default` | Binary directory; based on directory-style or custom path     |
| `tart-setup-find-sibling-rules` | `t`        | Add `.tart`/`.el` sibling file navigation                     |
| `tart-setup-eglot`              | `t`        | Register tart as eglot server                                 |

## Troubleshooting

### LSP server does not start

1. Verify the binary is installed: `M-x tart-install-binary`.
2. Check `tart-executable` -- run `M-: (tart--resolve-executable)` to see the
   resolved path.
3. Inspect the `*eglot-events*` buffer for server errors.

### No diagnostics in an Elisp buffer

1. Confirm a sibling `.tart` file exists for the `.el` file.
2. Verify signature syntax by opening the `.tart` file (font-lock highlights
   errors).
3. Save the buffer -- eglot sends diagnostics on save.

### REPL not responding

1. Check the process status with `M-x list-processes`.
2. Kill the `*tart*` buffer and restart with `M-x run-tart`.

## See Also

- [Getting Started][getting-started] -- installation and first steps
- [Signature File Format][tart-format] -- `.tart` file syntax reference
- [CLI Reference][cli-reference] -- `tart` command-line usage

[getting-started]: getting-started.md
[tart-format]: reference/tart-format.md
[cli-reference]: cli-reference.md
