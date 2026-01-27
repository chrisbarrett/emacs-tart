# Spec 23: Binary Installation

Download prebuilt tart binaries from GitHub releases.

**Dependencies:** Spec 22 (CI releases must publish binaries).

## Goal

Users install tart via `M-x tart-install-binary` without building from source.
Hook-friendly `tart-eglot` prompts to install if binary missing.

## Constraints

- Pure Elisp (no external tools beyond `curl`/`wget`)
- GitHub releases API for version discovery
- Platform detection: `system-type` + `(car (split-string system-configuration "-"))`
- Storage: `~/.emacs.d/tart/bin/tart-VERSION`

## Output

Changes to `lisp/tart-mode.el`.

## Requirements

### R1: Version variable

```elisp
(defcustom tart-version 'latest
  "Tart version to install.
When `latest', installs the most recent GitHub release.
When a version string, installs that specific version."
  :type '(choice (const :tag "Latest" latest)
                 (string :tag "Specific version"))
  :group 'tart
  :safe (lambda (v) (or (eq v 'latest) (stringp v))))
```

**Verify:** `.dir-locals.el` with `((emacs-lisp-mode . ((tart-version . "0.2.0"))))` respected

### R2: Install command

`M-x tart-install-binary`:
1. Query GitHub API for release (latest or `tart-version`)
2. Detect platform → asset name `tart-{os}-{arch}`
3. Download to `~/.emacs.d/tart/bin/tart-VERSION`
4. Make executable

**Verify:** `M-x tart-install-binary` → binary in `~/.emacs.d/tart/bin/`, executable

### R3: Platform detection

| `system-type` | arch         | Asset                 |
|---------------|--------------|----------------------|
| `darwin`      | `aarch64`    | `tart-darwin-arm64`  |
| `darwin`      | `x86_64`     | `tart-darwin-x86_64` |
| `gnu/linux`   | `aarch64`    | `tart-linux-arm64`   |
| `gnu/linux`   | `x86_64`     | `tart-linux-x86_64`  |

**Verify:** `(tart--platform-asset)` returns correct asset name

### R4: Hook-friendly eglot starter

```elisp
(defun tart-eglot ()
  "Start eglot for tart, prompting to install if binary missing."
  (interactive)
  (when (tart--has-sibling-tart-file-p)
    (unless (tart--binary-available-p)
      (if (y-or-n-p "Tart binary not found. Install now? ")
          (tart-install-binary)
        (user-error "Tart binary required for type checking")))
    (eglot-ensure)))
```

Add to hook: `(add-hook 'emacs-lisp-mode-hook #'tart-eglot)`

**Verify:** Without binary, `tart-eglot` prompts; after install, eglot connects

### R5: Executable resolution

```elisp
(defcustom tart-executable 'managed
  "Path to tart binary, or `managed' for automatic installation.
When `managed', uses downloaded binary from `tart-install-binary'."
  :type '(choice (const :tag "Managed by tart-mode" managed)
                 (string :tag "Custom path"))
  :group 'tart)
```

`tart--resolve-executable` returns:
- If string: use directly (absolute or exec-path lookup)
- If `managed`: `~/.emacs.d/tart/bin/tart-VERSION` (latest installed, or per `tart-version`)

**Verify:** Default `'managed` uses downloaded binary; string overrides

### R6: Download with progress

Use `url-retrieve` or `make-process` with curl. Show progress in echo area.

**Verify:** Large download shows progress, not frozen Emacs

### R7: Error handling

- No network → clear error message
- Asset not found for platform → error with supported platforms
- GitHub rate limit → suggest setting `GITHUB_TOKEN`

**Verify:** Airplane mode → "Network error" message, not hang

## Tasks

- [x] [R1] Add `tart-version` defcustom
- [x] [R5] Change `tart-executable` default to `'managed`, add `tart--resolve-executable`
- [x] [R3] Implement `tart--platform-asset`
- [x] [R2,R6] Implement `tart-install-binary` with download
- [x] [R4] Implement `tart-eglot` with install prompt
- [x] [R7] Error handling
