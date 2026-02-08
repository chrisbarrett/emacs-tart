# Spec 65: Download Security

Harden binary installation against `.dir-locals.el` attacks.

- **Dependencies:**
  - [Spec 23](./23-binary-installation.md) (binary installation)

## Context

A malicious repository can include a `.dir-locals.el` that overrides
customization variables. If those variables control _what gets downloaded_ or
_which binary gets executed_, an attacker can hijack the tart installation
process just by having a user open a file from the repo.

## Output

Changes to `lisp/tart-mode.el` and `lisp/tart-mode-tests.el`.

## Requirements

### R1: Mark download-controlling variables as risky

`tart-executable` and `tart-install-directory` control which binary runs and
where binaries are written. Mark both with `risky-local-variable` so Emacs
warns before applying them from `.dir-locals.el`.

```elisp
(put 'tart-executable 'risky-local-variable t)
(put 'tart-install-directory 'risky-local-variable t)
```

**Given** a `.dir-locals.el` sets `tart-executable` to a string
**When** the user opens a file in that directory
**Then** Emacs displays a risky-variable warning before applying it

**Verify:** `(get 'tart-executable 'risky-local-variable)` returns `t`;
`(get 'tart-install-directory 'risky-local-variable)` returns `t`

### R2: Tighten tart-version safe predicate

The current `:safe` predicate accepts any string, allowing path-traversal
payloads like `"../../evil"` to manipulate the GitHub API URL. Restrict to
`'latest` or strict semver with optional pre-release suffix.

Valid examples: `"0.2.0"`, `"1.0.0-rc1"`, `"2.3.1-20250601"`

The predicate must match:

    [0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9][A-Za-z0-9.-]*)?

```elisp
:safe (lambda (v)
        (or (eq v 'latest)
            (and (stringp v)
                 (string-match-p
                  "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\(?:-[A-Za-z0-9][A-Za-z0-9.-]*\\)?\\'"
                  v))))
```

**Given** a `.dir-locals.el` sets `tart-version` to `"../../evil"`
**When** the user opens a file in that directory
**Then** Emacs treats the value as unsafe and prompts before applying

**Given** a `.dir-locals.el` sets `tart-version` to `"0.2.0"`
**When** the user opens a file in that directory
**Then** the value is applied without prompting

**Verify:** `(tart--safe-version-p "0.2.0")` → `t`;
`(tart--safe-version-p "1.0.0-rc1")` → `t`;
`(tart--safe-version-p "../../foo")` → `nil`;
`(tart--safe-version-p "0.1")` → `nil`

## Tasks

- [x] [R1] Add `risky-local-variable` property to `tart-executable` and `tart-install-directory`
- [x] [R2] Extract safe-version predicate, tighten to semver regex
- [x] Update [Spec 23](./23-binary-installation.md) R1 to reference this spec
