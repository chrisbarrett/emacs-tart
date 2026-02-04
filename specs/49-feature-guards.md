# Spec 49: Feature Guards

Flow-sensitive type narrowing for runtime feature detection.

**Dependencies:** Spec 07, 24, 46, 50

## Constraints

| Constraint        | Detail                                       |
| ----------------- | -------------------------------------------- |
| Flow-sensitive    | Guards affect types within their scope only  |
| Static-only       | No runtime changes                           |
| Macro-transparent | Works on expanded code                       |
| Filename-based    | `json.tart` defines feature `json`           |

## Output

```
lib/typing/
├── feature_env.ml(i)  ; Track features/symbols per scope
├── guards.ml(i)       ; Guard pattern recognition
└── infer.ml           ; (modify) Thread feature env
```

## Guard Patterns

| Guard             | Proves                    | Unlocks                    |
| ----------------- | ------------------------- | -------------------------- |
| `(featurep 'X)`   | feature X loaded          | all types from `X.tart`    |
| `(fboundp 'f)`    | function f bound          | only function f            |
| `(boundp 'v)`     | variable v bound          | only variable v            |
| `(bound-and-true-p v)` | v bound and non-nil  | v with type `t`            |
| `(require 'X)`    | feature X loaded          | all types from `X.tart`    |
| `(require 'X nil t)` | maybe loaded           | nothing (needs guard)      |

## Requirements

### R1-R4: Basic guards

```elisp
(when (featurep 'json) (json-parse-string "{}"))  ; OK
(when (fboundp 'json-parse-string) (json-parse-string "{}"))  ; OK, others unavailable
(when (boundp 'json-null) json-null)  ; OK
(when (bound-and-true-p my-var) my-var)  ; type is t, not just bound
```

**Verify:** `dune test`; unguarded calls error

### R5-R6: Require

```elisp
(require 'json)
(json-parse-string "{}")  ; OK

(require 'json nil t)
(json-parse-string "{}")  ; Error: may not be bound

(when (require 'json nil t)
  (json-parse-string "{}"))  ; OK
```

**Verify:** `dune test`

### R7-R11: Control flow

Guards propagate through truthiness-aware forms:

```elisp
;; if: then-branch only
(if (featurep 'json) (json-parse-string "{}") (read "{}"))

;; cond: each clause independent
(cond ((featurep 'json) (json-parse-string "{}"))
      ((featurep 'libxml) (libxml-parse-xml-region ...)))

;; and: propagates to later operands
(and (featurep 'json) (json-parse-string "{}"))

;; or/unless early-return: proves for rest of scope
(or (featurep 'json) (error "JSON required"))
(json-parse-string "{}")  ; OK

(unless (featurep 'json) (error "required"))
(json-parse-string "{}")  ; OK
```

**Verify:** `dune test`

### R12: Negated guards

```elisp
(if (featurep 'json) (use-json)
  (json-parse-string "{}"))  ; Error: not available in else

(unless (featurep 'json)
  (json-parse-string "{}"))  ; Error: inside negated guard
```

**Verify:** `dune test`

### R13: Filename convention

`(featurep 'json)` loads from `json.tart`. No registry needed.

### R14: Redundant guard warning

```elisp
;; -*- tart-min-emacs-version: "28.1" -*-
(when (featurep 'json) ...)  ; Warning: json built-in since 27.1
```

Types still available; warning only.

**Verify:** `dune test`

### R15: Macro transparency

Macros expand before checking; expanded guards recognized automatically.

### R16: Combined guards

```elisp
(when (and (featurep 'json) (fboundp 'json-parse-buffer))
  (json-parse-buffer))  ; Both proven
```

**Verify:** `dune test`

### R17: Inline guards only

```elisp
(let ((avail (featurep 'json)))
  (when avail (json-parse-string "{}")))  ; Error: not recognized
```

**Verify:** `dune test`

## Non-Requirements

- Runtime enforcement
- Dynamic feature changes
- Autoload detection
- Package.el integration

## Tasks

- [ ] [R1-R4] Guard pattern recognition
- [ ] [R5-R6] Require handling (hard/soft)
- [ ] [R7-R11] Feature env through control flow
- [ ] [R12] Negated guards
- [ ] [R13] Filename resolution
- [ ] [R14] Redundant guard lint
- [ ] [R16] Combined guards
- [ ] [R17] Document inline-only limitation
