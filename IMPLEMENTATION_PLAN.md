# Implementation Plan: Version Constraints (Spec 50)

Track min/max Emacs version from typings and package headers; warn on
violations.

## Analysis

### Current Architecture

**Version detection** exists in `lib/sig/emacs_version.ml`: detects
installed Emacs version via `emacs --version`, provides `parse_version`,
`compare_version`, `version_to_dir`.

**Typings** are stored under `typings/emacs/{version}/c-core/*.tart` and
`typings/emacs/{version}/lisp-core/*.tart`. Directory path implies
minimum version (e.g., `31.0/c-core/json.tart` means `json-*` requires
31.0+). Currently this path info is discarded after loading.

**Module checking** in `module_check.ml` builds a `config` with a
`search_path`, loads c-core/lisp-core/required modules into `type_env`,
then type-checks. No version metadata propagates to diagnostics.

**Type environment** (`type_env.mli`) has `fn_bindings` and `bindings`
lists of `(string * scheme)`. No version annotation per-name.

### Key Design Decisions

1. **Version metadata per-name in type_env** — extend `type_env.t` with
   a `fn_versions` field mapping `string -> version_range`, populated
   during sig loading from the directory path of the source `.tart` file.

2. **Package header parsing in a new `package_header.ml`** — parse
   `Package-Requires` from the first few lines of the entry-point `.el`
   file. Simple regex-style parsing, no full sexp parsing needed.

3. **Constraint propagation via post-check pass** — after type checking
   completes, scan inferred calls against fn_versions and the declared
   min version. Emit warnings for version violations.

4. **Feature guard exemption** — calls inside a feature guard scope
   (already tracked by Spec 49) are exempt from version warnings.

### Scope Deferral

- `@max-version` annotation (R3) — defer initially; no current typings
  use it; can be added later via sig_ast extension
- R13-R15 (LSP code actions) — implement after core diagnostics work

---

## Iteration 1: Version range type + per-name tracking

Add version range metadata to type_env and populate it during sig
loading.

### Task 1.1: Add version_range to type_env

- [ ] Add `version_range` record to `type_env.mli/.ml`:
      `{ min_version : Emacs_version.version option;
         max_version : Emacs_version.version option }`
- [ ] Add `fn_versions : (string * version_range) list` field to
      `type_env.t`
- [ ] Add `var_versions : (string * version_range) list` field
- [ ] Add `extend_fn_version`, `lookup_fn_version`,
      `extend_var_version`, `lookup_var_version` functions
- [ ] Update `empty` and `of_list` constructors
- [ ] Build + test

### Task 1.2: Populate version from typings path

- [ ] In `search_path.ml`: extract version from directory path when
      loading c-core/lisp-core files. The path pattern is
      `typings/emacs/{version}/c-core/` or `.../lisp-core/`
- [ ] Add `?source_version:Emacs_version.version` param to
      `load_c_core_files`; pass through to sig_loader
- [ ] In `sig_loader.ml`: add `?source_version` to `load_signature`
      and `load_signature_with_resolver`; when set, add version_range
      for every loaded defun and defvar name
- [ ] In `load_c_core` and `load_lisp_core`: parse the version from
      the resolved typings directory path and thread it through
- [ ] Build + test

---

## Iteration 2: Package header parsing

Parse `Package-Requires` from `.el` files to determine the package's
declared minimum Emacs version.

### Task 2.1: Create package_header module

- [ ] Create `lib/sig/package_header.ml` and `package_header.mli`
- [ ] `parse_package_requires : string -> Emacs_version.version option`
      — given file content string, scan for
      `Package-Requires: ((emacs "X.Y") ...)` and extract version
- [ ] Only scan first ~50 lines (header section)
- [ ] Handle variations: spaces, multiple deps, emacs-only
- [ ] `find_package_version : string -> Emacs_version.version option`
      — given `.el` file path, read header and parse
- [ ] Add to `lib/sig/dune`
- [ ] Re-export in `tart.ml/tart.mli`
- [ ] Build + test

### Task 2.2: Unit tests

- [ ] Standard: `;; Package-Requires: ((emacs "29.1"))`
- [ ] Multiple deps: `((emacs "28.1") (seq "2.24"))`
- [ ] Missing header → None
- [ ] Malformed → None
- [ ] No emacs dep: `((seq "2.24"))` → None
- [ ] Build + test

---

## Iteration 3: Version constraint diagnostics

Add new error codes and emit version warnings.

### Task 3.1: Add error codes

- [ ] Add `VersionTooLow` (E0900), `VersionTooHigh` (E0901),
      `VersionParseFailed` (E0902) to `error_code` in
      `diagnostic.ml/.mli`
- [ ] Add `error_code_to_string` arms
- [ ] Update `diagnostic_test.ml` error_code_stability test
- [ ] Add constructor:
      `version_too_low : span -> name:string ->
       required:Emacs_version.version -> declared:Emacs_version.version
       -> unit -> t`
- [ ] Include help text: "bump to X.Y, or add feature guard"
- [ ] Build + test

### Task 3.2: Version checking pass in module_check

- [ ] Add `declared_version : Emacs_version.version option` to
      `module_check.config`
- [ ] Add `with_declared_version` setter to config
- [ ] Add `check_version_constraints` function: walks check_result's
      final_env, looks up fn_versions for each called function, compares
      against declared_version
- [ ] Needs call-site spans: collect from the sexp AST (function call
      positions) — reuse `collect_all_call_symbols` approach but return
      spans too
- [ ] Add `version_diagnostics` to check_result
- [ ] Wire into `diagnostics_of_result`
- [ ] Build + test

---

## Iteration 4: CLI + LSP integration

### Task 4.1: CLI integration

- [ ] In `bin/main.ml` `run_check`: call
      `Package_header.find_package_version` on each file
- [ ] Pass declared version into module_check config
- [ ] Fallback: when no Package-Requires, use detected Emacs version
      (warnings only, not errors)
- [ ] Build + test

### Task 4.2: LSP integration

- [ ] In `server.ml`: parse Package-Requires at document open
- [ ] Cache per-workspace
- [ ] Pass declared version into module_check config
- [ ] Build + test

---

## Iteration 5: Feature guard exemption + test fixtures

### Task 5.1: Guard exemption

- [ ] Names loaded via feature guards (Spec 49) are inherently exempt:
      they're only available inside guard scopes, so they won't trigger
      version warnings in unguarded code
- [ ] Names loaded via hard `(require 'X)` DO trigger version warnings
      (the require proves the dependency exists, but if the package
      declares a lower min version, the require might fail)
- [ ] Verify this behavior with tests
- [ ] Build + test

### Task 5.2: Test fixtures

- [ ] `version-constraint.{el,expected}`: function from newer version
      typings produces VERSION TOO LOW warning
- [ ] `version-guarded.{el,expected}`: featurep-guarded call produces
      no version warning
- [ ] `version-no-header.{el,expected}`: no Package-Requires, no
      version warnings (uses detected Emacs as baseline)
- [ ] Build + test

---

## Iteration 6: Spec completion

### Task 6.1: Check boxes and status

- [ ] Check task boxes in specs/50-version-constraints.md
- [ ] Add E0900-E0902 to specs/47-error-codes.md
- [ ] Update status
- [ ] Build + test
