# Spec 13: Error Reporting

Diagnostic message format and user-facing error quality.

**Dependencies:** Spec 06 (type representation for printing), Spec 08 (LSP
delivery)

## Goal

Produce clear, actionable error messages following Elm and Rust conventions.

## Constraints

- **Actionable**: Every error should suggest a fix when possible
- **Contextual**: Show related code that explains "why"
- **Consistent**: Follow a standard structure

## Output

Extends `lib/typing/diagnostic.ml` and `lib/lsp/handlers.ml`.

## Message Structure

```
error[CODE]: one-line summary
  --> file.el:LINE:COL
   |
NN |   (expression here)
   |    ^^^^^^^^^^^^^^^^
   |    expected: EXPECTED_TYPE
   |    found: ACTUAL_TYPE
   |
note: explanation of why
  --> file.el:RELATED_LINE:COL
   |
NN |   (related code)
   |    ^^^^^ this is TYPE because...

help: suggested fix
   |
NN |   (corrected code)
```

## Requirements

### R1: Type mismatch errors

**Given**:
```elisp
(upcase count)  ; count is Int
```
**When** type-checked
**Then** error:
```
error[E0308]: type mismatch
  --> init.el:42:10
   |
42 |   (upcase count)
   |           ^^^^^
   |           expected: String
   |           found: Int
   |
note: `upcase` expects a String argument
      (upcase STRING) -> String

help: convert the integer to a string first:
   |
42 |   (upcase (number-to-string count))
```

**Verify:** `dune test`; error pinpoints argument; suggests conversion

### R2: Branch type mismatch

**Given**:
```elisp
(if (> n 0)
    n
    "negative")
```
**When** type-checked
**Then** error:
```
error[E0317]: if branches have incompatible types
  --> utils.el:28:3
   |
28 |   (if (> n 0)
29 |       n
   |       ^ this branch has type: Int
30 |       "negative")
   |       ^^^^^^^^^^ this branch has type: String
   |
note: both branches of `if` must have the same type

help: return a string in both cases:
   |
29 |       (number-to-string n)
```

**Verify:** `dune test`; both branches shown; help suggests fix

### R3: Possible nil errors

**Given**:
```elisp
(upcase (get-name user))  ; get-name returns (Option String)
```
**When** type-checked
**Then** error:
```
error[E0308]: possible nil value
  --> format.el:18:13
   |
18 |   (upcase (get-name user))
   |           ^^^^^^^^^^^^^^^^
   |           expected: String
   |           found: (Option String)
   |
note: `get-name` may return nil

help: check for nil first:
   |
18 |   (when-let ((name (get-name user)))
19 |     (upcase name))

help: or provide a default:
   |
18 |   (upcase (or (get-name user) "Unknown"))
```

**Verify:** `dune test`; multiple help suggestions for nil handling

### R4: Undefined variable with suggestion

**Given**:
```elisp
(upcase strng)  ; typo for string
```
**When** type-checked
**Then** error:
```
error[E0425]: variable `strng` is not defined
  --> edit.el:102:13
   |
102|     (upcase strng)
   |             ^^^^^ not found in scope
   |
help: a variable with a similar name exists:
   |
102|     (upcase string)
```

**Verify:** `dune test`; Levenshtein-based suggestion for typos

### R5: Arity mismatch

**Given**:
```elisp
(+ 1)  ; + needs at least no args, but commonly 2+
(substring "hello")  ; needs at least 2 args
```
**When** type-checked
**Then** error:
```
error[E0061]: wrong number of arguments
  --> calc.el:15:3
   |
15 |   (substring "hello")
   |   ^^^^^^^^^^^^^^^^^^^
   |   expected: 2-3 arguments
   |   found: 1 argument
   |
note: substring signature:
      (substring STRING FROM &optional TO) -> String
```

**Verify:** `dune test`; shows expected vs actual arity

### R6: Signature mismatch

**Given** signature in `lib.tart`:
```elisp
(defun foo (int) -> string)
```
**And** implementation in `lib.el`:
```elisp
(defun foo (x) (+ x 1))
```
**When** type-checked
**Then** error:
```
error[E0308]: implementation does not match signature
  --> lib.el:10:1
   |
10 | (defun foo (x) (+ x 1))
   | ^^^^^^^^^^^^^^^^^^^^^^^
   | signature declares: (int) -> string
   | implementation has: (int) -> int
   |
  --> lib.tart:5:1
   |
 5 | (defun foo (int) -> string)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^ signature declared here
```

**Verify:** `dune test`; shows both signature and implementation locations

### R7: LSP diagnostic mapping

**Given** an error with primary and secondary spans
**When** converted to LSP diagnostics
**Then**:
- Primary span → `Diagnostic` with message
- Secondary spans → `DiagnosticRelatedInformation`
- Help → appended to message or `CodeAction`

**Verify:** `dune test`; LSP clients show related information

## Error Codes

| Code  | Category     | Description                      |
|-------|--------------|----------------------------------|
| E0308 | Type         | Type mismatch                    |
| E0317 | Type         | Incompatible branch types        |
| E0425 | Name         | Undefined variable               |
| E0426 | Name         | Undefined function               |
| E0061 | Arity        | Wrong number of arguments        |
| E0106 | Annotation   | Missing type annotation required |
| E0521 | Polymorphism | Type too specific for context    |

## Tasks

- [x] [R1] Implement type mismatch formatting
- [x] [R2] Implement branch mismatch formatting
- [x] [R3] Implement Option/nil error formatting
- [x] [R4] Implement typo suggestions (Levenshtein)
- [x] [R5] Implement arity error formatting
- [x] [R6] Implement signature mismatch formatting
- [x] [R7] Map to LSP diagnostic format

Run review agent after R1-R3 work (basic error formatting) before implementing R4-R6.
