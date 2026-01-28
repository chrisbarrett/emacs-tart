# Spec 24: Versioned Typings Distribution

Versioned Emacs core typings with auto-detection.

**Deps:** Spec 07 (search path), Spec 09 (CLI). **Testing:** Spec 25. **LSP sync:** Spec 26.

## Goal

Type coverage for Emacs C primitives and core Lisp, versioned by release. Auto-detect Emacs version, load matching typings.

## Constraints

| Constraint | Detail |
|------------|--------|
| Version-specific | Types differ across Emacs 29/30/31 |
| C core first | Prioritize stable C primitives before Lisp |
| Auto-discovery | Detect version from `emacs --version` on PATH |
| Override | `--emacs-version` flag for explicit selection |
| Bundled | Ship in tart repo; separate repo is future work |

## Directory Structure

```
typings/
├── emacs/
│   ├── 29.1/c-core/*.tart
│   ├── 30.1/c-core/*.tart
│   ├── 31.0/c-core/*.tart
│   └── latest -> 31.0
└── (future: dash/, s/, etc.)
```

C-core files map 1:1 to Emacs source:

| File | Source | Functions |
|------|--------|-----------|
| data.tart | data.c | `eq null + - car cdr` predicates, arithmetic |
| fns.tart | fns.c | `length concat mapcar assoc` utilities |
| eval.tart | eval.c | `funcall apply signal catch` control flow |
| alloc.tart | alloc.c | `cons list make-vector` allocation |
| buffer.tart | buffer.c | `current-buffer set-buffer` buffer ops |
| window.tart | window.c | `selected-window window-buffer` window ops |
| frame.tart | frame.c | `selected-frame frame-parameters` frame ops |
| fileio.tart | fileio.c | `find-file-noselect write-region` file I/O |
| editfns.tart | editfns.c | `point goto-char insert` editing |
| search.tart | search.c | `re-search-forward match-string` search |
| process.tart | process.c | `start-process process-send-string` subprocs |
| keyboard.tart | keyboard.c | `read-key-sequence` input |
| keymap.tart | keymap.c | `define-key lookup-key` keymaps |
| minibuf.tart | minibuf.c | `read-string completing-read` minibuffer |
| textprop.tart | textprop.c | `get-text-property put-text-property` props |
| print.tart | print.c | `prin1 princ message` output |

Future: `lisp-core/` for subr.el, simple.el after C coverage complete.

## Optional Features

Ship unconditionally; feature detection is separate spec:

| Feature | Source | Detection |
|---------|--------|-----------|
| JSON | json.c | `(fboundp 'json-parse-string)` |
| Tree-sitter | treesit.c | `(fboundp 'treesit-available-p)` |
| SQLite | sqlite.c | `(fboundp 'sqlite-open)` |
| GnuTLS | gnutls.c | `(fboundp 'gnutls-available-p)` |
| D-Bus | dbusbind.c | `(fboundp 'dbus-call-method)` |

GUI backends (ns/w32/pgtk/x11) deferred.

## Output

```
lib/sig/
├── search_path.ml    ; (modify) version resolution
├── emacs_version.ml  ; (new) detection
└── emacs_version.mli
bin/main.ml           ; (modify) --emacs-version flag
```

## Requirements

### R1: Version detection

Run `emacs --version`, parse major.minor, load `typings/emacs/{version}/`.

```
$ emacs --version → "GNU Emacs 31.0.50"
$ tart check foo.el → uses typings/emacs/31.0/
```

### R2: CLI override

`--emacs-version VERSION` overrides detection. Error if directory missing.

### R3: Fallback chain

Search order: exact → minor → major → latest symlink.
```
31.0.50 → 31.0 → 31 → latest
```

### R4: Missing Emacs

No Emacs on PATH + no flag → use `latest/` with warning.

### R5: C-core typings

Each `c-core/*.tart` covers all DEFUNs from corresponding `.c` file:
- Polymorphic functions use quantifiers
- Error functions return `never`
- All files loaded and merged; conflicts error at load time

### R6: Delete stdlib

Remove `stdlib/` entirely. All typings live under `typings/`.

### R7: LSP

Detect version once at startup; use for entire session. Log detected version.

## Migration

1. Create `typings/emacs/31.0/c-core/`
2. Delete `stdlib/` entirely
3. Add version detection to search_path.ml
4. Add `--emacs-version` CLI flag
5. Backfill 29.1, 30.1 (copy then diff)

## Tasks

- [ ] Emacs version detection module
- [ ] `--emacs-version` CLI flag
- [ ] Version fallback in search_path.ml
- [ ] Handle missing Emacs gracefully
- [ ] Create c-core/*.tart for 31.0
- [ ] Multi-file c-core loading
- [ ] Delete stdlib/
- [ ] Wire LSP to version detection
- [ ] Backfill 29.1, 30.1 typings

## Non-Goals

- Feature availability detection (separate spec)
- Separate typings repository (future)
- GUI backend typings (deferred)
- Third-party package typings (future, under `typings/dash/` etc.)
- Lisp-core typings (after C coverage solid)
