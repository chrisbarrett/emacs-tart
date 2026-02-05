# Spec 50: Version Constraints

Track min/max Emacs version from typings and package headers; warn on violations.

**Dependencies:** Spec 24, 08, 13, 47

## Constraints

| Constraint       | Detail                                         |
| ---------------- | ---------------------------------------------- |
| File granularity | Version bounds from directory, not symbols     |
| Package headers  | Parse `Package-Requires` from entrypoint       |
| Fallback         | Use detected Emacs when no headers             |

## Version Ranges

| Range        | Meaning                           |
| ------------ | --------------------------------- |
| [29.1, nil]  | Added 29.1, still available       |
| [nil, 30.1]  | Always present, removed after 30.1|
| [28.1, 30.1] | Added 28.1, removed after 30.1    |

## Output

```
lib/version/
├── constraint.ml(i)      ; Version range type
├── package_header.ml(i)  ; Package-Requires parsing
└── propagation.ml(i)     ; Constraint collection
lib/lsp/
└── code_action.ml        ; (extend) version actions
```

## Requirements

### R1-R3: Version from typings

```
typings/emacs/29.1/c-core/json.tart  ; json-* requires 29.1+
```

- Directory path implies minimum version
- `(include ...)` preserves source version, not includer
- `@max-version 30.1` annotation sets upper bound

**Verify:** `dune test`

### R4-R7: Package headers

```elisp
;;; my-pkg.el --- -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "29.1"))
```

- Parse from feature-named file (`my-pkg.el`)
- Sibling files inherit; nested directories don't
- No header → detected Emacs version (warnings only)

**Verify:** `dune test`

### R8-R10: Constraint propagation

```elisp
;; Package-Requires: ((emacs "28.1"))
(json-parse-string "{}")  ; requires 29.1+
```

```
warning[E0900]: feature requires newer Emacs version
 --> my-pkg.el:5:3
  | (json-parse-string "{}") requires Emacs 29.1+
note: package declares minimum Emacs 28.1
help: bump to 29.1, or add feature guard
```

```elisp
;; Package-Requires: ((emacs "31.0"))
(old-api)  ; max version 30.1
```

```
warning[E0901]: feature removed in declared Emacs version
 --> my-pkg.el:5:3
  | (old-api) removed after Emacs 30.1
```

Multiple constraints: effective min = max of all requirements.

**Verify:** `dune test`

### R11-R12: Feature guards

```elisp
;; Package-Requires: ((emacs "28.1"))
(when (fboundp 'json-parse-string)
  (json-parse-string "{}"))  ; No warning: guarded
```

Redundant guard lint: see Spec 51.

**Verify:** `dune test`

### R13-R15: LSP code actions

On version warning, offer:
- "Bump minimum Emacs version to X.Y"
- "Wrap in feature guard"

On Package-Requires when version could be lower:
- "Downgrade minimum version to X.Y"

**Verify:** Apply actions

### Error Codes

| Code  | Name             | Description                        |
|-------|------------------|------------------------------------|
| E0900 | VersionTooLow    | Feature requires newer Emacs       |
| E0901 | VersionTooHigh   | Feature removed in declared Emacs  |
| E0902 | VersionParseFailed | Package-Requires parse error     |

## Non-Requirements

- Per-symbol version annotations
- Auto-detection from code patterns
- Non-Emacs package managers
- Version ranges in Package-Requires

## Tasks

- [x] [R1-R3] Version metadata from typings paths
- [ ] [R4-R7] Package header parsing
- [ ] [R8-R10] Constraint propagation
- [ ] [R11] Feature guard exemption
- [ ] [R13-R15] LSP code actions
- [ ] Add E0900-E0902 to Spec 47

**Status:** Version detection implemented in `lib/sig/emacs_version.mli` with `detect()`, `parse_version()`, `version_to_dir()`. Package header parsing and constraint propagation not yet implemented.
