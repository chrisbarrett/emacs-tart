# Implementation Plan: Emacs 31 Complete Typing Coverage

Complete type signature coverage for all Emacs 31 C-layer and lisp-core modules.

**Goal:** 100% coverage with the most accurate, specific types possible.

**Specs:** 11 (Unions/Rows), 24 (Versioned Typings), 32 (Core Typings), 34 (Funcall/Apply), 46 (Truthiness), 49 (Feature Guards), 50 (Version Constraints), 52 (Type Predicates)

## Current State

- **C-layer:** 54/116 files (29.5% symbol coverage: 1418/4804)
- **Lisp-core:** 34 files exist

---

## Phase 0: Type System Features

These features are needed for accurate Elisp typing. Complete before full coverage push.

### 0.1 Row Polymorphism (Spec 11, R4-R13)

Required for alist/plist/hash-table typing - pervasive in Elisp.

- [ ] Implement row type representation (`row.ml`)
- [ ] Parse row syntax: `(alist {name string & r})`
- [ ] Row unification rules (open vs closed)
- [ ] Infer row types from field access (literal vs variable keys)
- [ ] Generic `map` supertype for alist/plist/hash-table
- [ ] Literal types with deferred widening
- [ ] Type subtraction operator `(a - b)`

**Files:** `lib/typing/row.ml`, `lib/sig/sig_parser.ml`, `lib/typing/unify.ml`

### 0.2 Type Predicates & Occurrence Typing (Spec 52, Spec 34 R12-R13)

Required for type predicates (`stringp`, `listp`, etc.) to narrow types.

- [ ] Parse `(x is T)` return type syntax for predicates
- [ ] Implement predicate narrowing in `if`/`cond`/`when`/`unless`
- [ ] Type subtraction for else branches
- [ ] Cumulative narrowing in cond chains
- [ ] Handle predicates in `and`/`or` expressions
- [ ] Restrict narrowing to inline checks only (not stored results)
- [ ] Add standard library predicate declarations to `data.tart`

**Files:** `lib/sig/sig_parser.ml`, `lib/typing/infer.ml`, `lib/typing/narrow.ml`

### 0.3 Feature Guards (Spec 49)

Required for platform-specific and version-conditional code.

- [ ] Guard pattern recognition (`featurep`, `fboundp`, `boundp`)
- [ ] Hard/soft `require` handling
- [ ] Feature environment through control flow
- [ ] Negated guards in else branches
- [ ] Filename-based feature resolution (`json.tart` → feature `json`)

**Files:** `lib/typing/feature_env.ml`, `lib/typing/guards.ml`

### 0.4 Version Constraints (Spec 50, R4-R15)

Required for cross-version typing accuracy.

- [ ] Parse `Package-Requires` from .el headers
- [ ] Constraint propagation (effective min version)
- [ ] Feature guard exemption from version warnings
- [ ] LSP code actions for version bumps

**Files:** `lib/version/package_header.ml`, `lib/version/propagation.ml`

### 0.5 Remaining Funcall/Apply (Spec 34, R9-R11)

- [ ] Tuple <: list subtyping
- [ ] Context-sensitive tuple inference for list literals
- [ ] Union function types for dynamic dispatch

**Files:** `lib/typing/unify.ml`, `lib/typing/infer.ml`

### 0.6 Branch Error Messages (Spec 46, R7)

- [ ] Point to specific offending branch in union errors
- [ ] Include declared return type in error context

**Files:** `lib/typing/diagnostic.ml`

---

## Workflow Per File

1. Run `./tart emacs-coverage -v` to list uncovered DEFUNs/DEFVARs
2. Create/update `.tart` file with precise types (avoid `any` in output positions)
3. Validate: `./tart check` against Emacs lisp/ directory
4. Document untypeable items in `BUGS.md` with category
5. Iterate until 95%+ type-check success

## Phase 1: Complete C-Layer Core (Cross-Platform)

High-value portable modules. These define the primitives used everywhere.

### 1.1 Missing Core Modules

- [ ] `treesit.c` → `treesit.tart` (46 DEFUNs, 3 DEFVARs) — tree-sitter integration
- [ ] `comp.c` → `comp.tart` (15 DEFUNs, 15 DEFVARs) — native compilation
- [ ] `bytecode.c` → `bytecode.tart` (2 DEFUNs, 2 DEFVARs) — byte compiler
- [ ] `emacs.c` → `emacs.tart` (6 DEFUNs, 21 DEFVARs) — core startup/state
- [ ] `fontset.c` → `fontset.tart` (7 DEFUNs, 8 DEFVARs) — font management
- [ ] `ccl.c` → `ccl.tart` (5 DEFUNs, 3 DEFVARs) — code conversion
- [ ] `insdel.c` → `insdel.tart` (1 DEFUN, 2 DEFVARs) — insert/delete
- [ ] `menu.c` → `menu.tart` (3 DEFUNs, 1 DEFVAR) — menu primitives
- [ ] `term.c` → `term.tart` (12 DEFUNs, 5 DEFVARs) — terminal handling
- [ ] `pdumper.c` → `pdumper.tart` (4 DEFUNs, 1 DEFVAR) — portable dumper
- [ ] `sound.c` → `sound.tart` (1 DEFUN) — audio
- [ ] `atimer.c` → `atimer.tart` (1 DEFUN) — async timers
- [ ] `sysdep.c` → `sysdep.tart` (1 DEFUN) — system dependencies
- [ ] `emacs-module.c` → `emacs-module.tart` (1 DEFUN) — dynamic modules
- [ ] `textconv.c` → `textconv.tart` (1 DEFUN, 3 DEFVARs) — text conversion
- [ ] `lcms.c` → `lcms.tart` (8 DEFUNs) — color management

### 1.2 Audit Existing Core Files

Verify existing .tart files have 100% symbol coverage:

- [ ] `data.tart` — verify 120 DEFUNs, 3 DEFVARs covered
- [ ] `fns.tart` — verify 107 DEFUNs, 6 DEFVARs covered
- [ ] `eval.tart` — verify 50 DEFUNs, 19 DEFVARs covered
- [ ] `window.tart` — verify 117 DEFUNs, 22 DEFVARs covered
- [ ] `editfns.tart` — verify 80 DEFUNs, 9 DEFVARs covered
- [ ] `frame.tart` — verify 65 DEFUNs, 25 DEFVARs covered
- [ ] `process.tart` — verify 64 DEFUNs, 10 DEFVARs covered
- [ ] `fileio.tart` — verify 54 DEFUNs, 15 DEFVARs covered
- [ ] `buffer.tart` — verify 53 DEFUNs, 16 DEFVARs covered
- [ ] `keyboard.tart` — verify 33 DEFUNs, 77 DEFVARs covered
- [ ] `coding.tart` — verify 34 DEFUNs, 29 DEFVARs covered
- [ ] `xfaces.tart` — verify 33 DEFUNs, 10 DEFVARs covered
- [ ] `keymap.tart` — verify 29 DEFUNs, 6 DEFVARs covered
- [ ] `floatfns.tart` — verify 25 DEFUNs covered
- [ ] `font.tart` — verify 25 DEFUNs, 5 DEFVARs covered
- [ ] `charset.tart` — verify 23 DEFUNs, 4 DEFVARs covered
- [ ] `xdisp.tart` — verify 23 DEFUNs, 96 DEFVARs covered
- [ ] `minibuf.tart` — verify 23 DEFUNs, 25 DEFVARs covered
- [ ] `syntax.tart` — verify 21 DEFUNs, 9 DEFVARs covered
- [ ] `alloc.tart` — verify 23 DEFUNs, 18 DEFVARs covered
- [ ] `textprop.tart` — verify 20 DEFUNs, 4 DEFVARs covered
- [ ] `lread.tart` — verify 20 DEFUNs, 33 DEFVARs covered
- [ ] `search.tart` — verify 19 DEFUNs, 2 DEFVARs covered
- [ ] `thread.tart` — verify 19 DEFUNs, 1 DEFVAR covered
- [ ] `gnutls.tart` — verify 19 DEFUNs, 2 DEFVARs covered
- [ ] `sqlite.tart` — verify 17 DEFUNs covered
- [ ] `timefns.tart` — verify 14 DEFUNs, 1 DEFVAR covered
- [ ] `chartab.tart` — verify 13 DEFUNs, 1 DEFVAR covered
- [ ] `category.tart` — verify 13 DEFUNs, 2 DEFVARs covered
- [ ] `dispnew.tart` — verify 12 DEFUNs, 12 DEFVARs covered
- [ ] `image.tart` — verify 11 DEFUNs, 7 DEFVARs covered
- [ ] `casefiddle.tart` — verify 11 DEFUNs, 2 DEFVARs covered
- [ ] `print.tart` — verify 11 DEFUNs, 18 DEFVARs covered
- [ ] `character.tart` — verify 10 DEFUNs, 8 DEFVARs covered
- [ ] `profiler.tart` — verify 9 DEFUNs, 2 DEFVARs covered
- [ ] `terminal.tart` — verify 8 DEFUNs, 2 DEFVARs covered
- [ ] `dired.tart` — verify 8 DEFUNs, 1 DEFVAR covered
- [ ] `cmds.tart` — verify 7 DEFUNs, 1 DEFVAR covered
- [ ] `marker.tart` — verify 7 DEFUNs covered
- [ ] `indent.tart` — verify 7 DEFUNs, 1 DEFVAR covered
- [ ] `composite.tart` — verify 6 DEFUNs, 5 DEFVARs covered
- [ ] `doc.tart` — verify 6 DEFUNs, 4 DEFVARs covered
- [ ] `macros.tart` — verify 6 DEFUNs, 3 DEFVARs covered
- [ ] `casetab.tart` — verify 5 DEFUNs covered
- [ ] `filelock.tart` — verify 5 DEFUNs, 2 DEFVARs covered
- [ ] `inotify.tart` — verify 5 DEFUNs covered
- [ ] `fringe.tart` — verify 4 DEFUNs, 2 DEFVARs covered
- [ ] `json.tart` — verify 4 DEFUNs covered
- [ ] `callint.tart` — verify 4 DEFUNs, 6 DEFVARs covered
- [ ] `callproc.tart` — verify 3 DEFUNs, 17 DEFVARs covered
- [ ] `xml.tart` — verify 3 DEFUNs covered
- [ ] `decompress.tart` — verify 2 DEFUNs covered
- [ ] `abbrev.tart` — verify symbols covered
- [ ] `undo.tart` — verify 1 DEFUN, 5 DEFVARs covered

## Phase 2: Platform-Specific Modules

Conditional modules gated by `(featurep 'X)` per Spec 49.

### 2.1 X11/GTK Backend

- [ ] `xfns.c` → `xfns.tart` (54 DEFUNs, 26 DEFVARs)
- [ ] `xselect.c` → `xselect.tart` (9 DEFUNs, 7 DEFVARs)
- [ ] `xwidget.c` → `xwidget.tart` (36 DEFUNs, 3 DEFVARs)
- [ ] `xmenu.c` → `xmenu.tart` (3 DEFUNs)
- [ ] `xsettings.c` → `xsettings.tart` (3 DEFUNs, 2 DEFVARs)
- [ ] `xsmfns.c` → `xsmfns.tart` (1 DEFUN, 2 DEFVARs)
- [ ] `xterm.c` → `xterm.tart` (39 DEFVARs) — variables only
- [ ] `xfont.c` → `xfont.tart` — DEFSYMs only
- [ ] `xftfont.c` → `xftfont.tart` (2 DEFVARs) — variables only

### 2.2 PGTK Backend

- [ ] `pgtkfns.c` → `pgtkfns.tart` (38 DEFUNs, 7 DEFVARs)
- [ ] `pgtkselect.c` → `pgtkselect.tart` (8 DEFUNs, 6 DEFVARs)
- [ ] `pgtkmenu.c` → `pgtkmenu.tart` (2 DEFUNs)
- [ ] `pgtkim.c` → `pgtkim.tart` (1 DEFUN, 1 DEFVAR)
- [ ] `pgtkterm.c` → `pgtkterm.tart` (10 DEFVARs) — variables only

### 2.3 Windows Backend

- [ ] `w32fns.c` → `w32fns.tart` (55 DEFUNs, 38 DEFVARs)
- [ ] `w32proc.c` → `w32proc.tart` (20 DEFUNs, 10 DEFVARs)
- [ ] `w32select.c` → `w32select.tart` (4 DEFUNs, 2 DEFVARs)
- [ ] `w32console.c` → `w32console.tart` (3 DEFUNs, 1 DEFVAR)
- [ ] `w32notify.c` → `w32notify.tart` (3 DEFUNs)
- [ ] `w32menu.c` → `w32menu.tart` (1 DEFUN)
- [ ] `w32image.c` → `w32image.tart` (1 DEFUN)
- [ ] `w32cygwinx.c` → `w32cygwinx.tart` (1 DEFUN)
- [ ] `w32font.c` → `w32font.tart` (1 DEFUN, 1 DEFVAR)
- [ ] `w32term.c` → `w32term.tart` (14 DEFVARs) — variables only
- [ ] `w32.c` → `w32.tart` — DEFSYMs only
- [ ] `w16select.c` → `w16select.tart` (3 DEFUNs, 2 DEFVARs)
- [ ] `cygw32.c` → `cygw32.tart` (2 DEFUNs)

### 2.4 macOS/NS Backend

(No direct NS files in scan - may be in separate location or use different naming)

### 2.5 Haiku Backend

- [ ] `haikufns.c` → `haikufns.tart` (34 DEFUNs, 8 DEFVARs)
- [ ] `haikuselect.c` → `haikuselect.tart` (9 DEFUNs, 4 DEFVARs)
- [ ] `haikufont.c` → `haikufont.tart` (3 DEFUNs, 1 DEFVAR)
- [ ] `haikumenu.c` → `haikumenu.tart` (2 DEFUNs)
- [ ] `haikuterm.c` → `haikuterm.tart` (9 DEFVARs) — variables only

### 2.6 Android Backend

- [ ] `androidfns.c` → `androidfns.tart` (35 DEFUNs, 22 DEFVARs)
- [ ] `androidselect.c` → `androidselect.tart` (8 DEFUNs)
- [ ] `androidmenu.c` → `androidmenu.tart` (1 DEFUN)
- [ ] `androidvfs.c` → `androidvfs.tart` (1 DEFUN)
- [ ] `androidterm.c` → `androidterm.tart` (13 DEFVARs) — variables only
- [ ] `androidfont.c` → `androidfont.tart` — DEFSYMs only
- [ ] `sfntfont-android.c` → `sfntfont-android.tart` (1 DEFUN)
- [ ] `sfntfont.c` → `sfntfont.tart` (3 DEFVARs) — variables only

### 2.7 DOS Backend

- [ ] `dosfns.c` → `dosfns.tart` (10 DEFUNs, 11 DEFVARs)
- [ ] `msdos.c` → `msdos.tart` (5 DEFUNs, 1 DEFVAR)

### 2.8 File Notification Backends

- [ ] `gfilenotify.c` → `gfilenotify.tart` (4 DEFUNs) — GLib
- [ ] `kqueue.c` → `kqueue.tart` (3 DEFUNs) — BSD/macOS

### 2.9 External Integrations

- [ ] `dbusbind.c` → `dbusbind.tart` (3 DEFUNs, 9 DEFVARs) — D-Bus

### 2.10 Font Backends

- [ ] `ftcrfont.c` → `ftcrfont.tart` — DEFSYMs only
- [ ] `ftfont.c` → `ftfont.tart` — DEFSYMs only

## Phase 3: Lisp-Core Validation Loop

Type-check lisp-core files against C-layer types. Errors feed back into C-layer refinements.

### 3.1 Foundation Libraries

- [ ] Type-check `subr.tart` usage in Emacs lisp/subr.el
- [ ] Type-check `simple.tart` usage in Emacs lisp/simple.el
- [ ] Type-check `files.tart` usage in Emacs lisp/files.el

### 3.2 Data Structure Libraries

- [ ] Type-check `cl-lib.tart` against lisp/emacs-lisp/cl-lib.el
- [ ] Type-check `seq.tart` against lisp/emacs-lisp/seq.el
- [ ] Type-check `pcase.tart` against lisp/emacs-lisp/pcase.el

### 3.3 UI Libraries

- [ ] Type-check `faces.tart` against lisp/faces.el
- [ ] Type-check `button.tart` against lisp/button.el
- [ ] Type-check `custom.tart` against lisp/custom.el

### 3.4 Development Tool Libraries

- [ ] Type-check `xref.tart` against lisp/progmodes/xref.el
- [ ] Type-check `project.tart` against lisp/progmodes/project.el
- [ ] Type-check `eglot.tart` against lisp/progmodes/eglot.el
- [ ] Type-check `flymake.tart` against lisp/progmodes/flymake.el
- [ ] Type-check `compile.tart` against lisp/progmodes/compile.el
- [ ] Type-check `eldoc.tart` against lisp/emacs-lisp/eldoc.el

### 3.5 Remaining Lisp-Core

- [ ] Type-check remaining 34 lisp-core modules
- [ ] Document type refinements needed in C-layer

## Phase 4: Documentation and Gaps

- [ ] Ensure `typings/emacs/BUGS.md` documents cross-version issues
- [ ] Ensure `typings/emacs/31.0/BUGS.md` documents version-specific issues
- [ ] Categorize all gaps: `type-system-gap` | `untypeable` | `ergonomic` | `version-specific`
- [ ] Final coverage report: target 95%+ type-check success on Emacs lisp/

## Acceptance Criteria

1. All 116 C source files have corresponding `.tart` files
2. `./tart emacs-coverage` shows 100% symbol coverage
3. `./tart check` against Emacs lisp/ passes with 95%+ success
4. All untypeable items documented in BUGS.md with categories
5. No unjustified `any` in output positions
