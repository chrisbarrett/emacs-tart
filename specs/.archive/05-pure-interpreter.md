# Spec 05: Pure Elisp Interpreter

OCaml interpreter for a pure subset of Elisp, enabling macro expansion without
an Emacs subprocess.

**Dependencies:** Spec 04 parser complete.

## Goal

Implement an interpreter that can expand macros by evaluating defmacro bodies
in a controlled environment. The interpreter handles pure computation; I/O and
effects hit opaque boundaries requiring type annotations.

## Constraints

- **Pure**: No file I/O, network, process spawning, or Emacs-specific state
- **Hermetic**: Builds without Emacs; deterministic results
- **Integrated**: Shares type environment with type checker
- **Source tracking**: Maintains source locations through expansion

## Scope

**In scope (v1):**
- Core special forms: `quote`, `if`, `let`, `let*`, `lambda`, `progn`, `setq`
- `defmacro` registration and invocation
- Pure built-ins: `cons`, `car`, `cdr`, `list`, `append`, `nth`, `length`
- Arithmetic: `+`, `-`, `*`, `/`, `<`, `>`, `=`, etc.
- Predicates: `null`, `atom`, `listp`, `symbolp`, `stringp`, `numberp`
- List operations: `mapcar`, `mapc`, `assq`, `memq`
- Backquote expansion

**Out of scope (opaque boundaries):**
- `intern`, `make-symbol` (dynamic symbol creation)
- `eval` on computed forms
- `load`, `require` (file I/O)
- Buffer/window operations
- Process/network operations

## Output

```
tart/
├── lib/
│   ├── interp/
│   │   ├── value.ml       ; Runtime value representation
│   │   ├── env.ml         ; Environment (lexical + special vars)
│   │   ├── builtin.ml     ; Pure built-in functions
│   │   ├── eval.ml        ; Core evaluator
│   │   └── expand.ml      ; Macro expansion entry point
│   └── core/
│       └── core_ast.ml    ; Post-expansion core AST
└── test/
    └── interp/
        ├── eval_test.ml   ; Evaluation tests
        └── expand_test.ml ; Macro expansion tests
```

## Requirements

### R1: Value representation

**Given** Elisp values to represent
**When** the value type is defined
**Then** it supports: nil, t, integers, floats, strings, symbols, cons cells,
vectors, and closures (lambda + captured env)

**Verify:** `dune test`; values round-trip through pretty-printer

### R2: Core special forms

**Given** special forms: `quote`, `if`, `progn`, `let`, `let*`, `lambda`, `setq`
**When** evaluated
**Then** they follow Elisp semantics:
- `(quote x)` returns x unevaluated
- `(if test then else)` evaluates test, then appropriate branch
- `(progn e1 ... en)` evaluates all, returns last
- `(let ((x e1)) body)` binds x to e1 in body
- `(lambda (args) body)` creates closure
- `(setq x e)` mutates binding (lexical or special)

**Verify:** `dune test` with special form test cases

### R3: Function application

**Given** a function call `(f arg1 arg2 ...)`
**When** evaluated
**Then** f is evaluated to a closure or built-in, args are evaluated left to
right, and the function is applied

**Verify:** Lambda application tests; higher-order function tests

### R4: Pure built-in functions

**Given** pure built-ins are registered
**When** called
**Then** they behave like Emacs:
- `(cons 1 2)` → `(1 . 2)`
- `(car '(a b))` → `a`
- `(+ 1 2 3)` → `6`
- `(mapcar #'1+ '(1 2 3))` → `(2 3 4)`

**Verify:** Built-in test suite covering all registered functions

### R5: Macro registration

**Given** a `defmacro` form
**When** evaluated at top level
**Then** the macro is registered in the environment for later expansion

**Verify:** `(defmacro incf (x) ...)` followed by `(macroexpand '(incf y))`
produces expected expansion

### R6: Macro expansion

**Given** a form where the car is a registered macro
**When** `macroexpand-1` is called
**Then** the macro body is evaluated with args bound (unevaluated), and the
result replaces the original form

**Verify:** Expansion tests for common patterns: `when`, `unless`, `with-temp-buffer`

### R7: Backquote handling

**Given** backquoted forms with unquote and unquote-splicing
**When** evaluated
**Then** they produce the expected list structure:
- `` `(a ,x b)`` with `x=1` → `(a 1 b)`
- `` `(a ,@xs b)`` with `xs=(1 2)` → `(a 1 2 b)`

**Verify:** Backquote test suite covering nested cases

### R8: Source location threading

**Given** a macro expansion
**When** the result is produced
**Then** new syntax nodes carry source locations pointing to the macro call site
**And** the pre-expansion source is available for error messages

**Verify:** Location tests show macro-generated code points to call site

### R9: Opaque boundary handling

**Given** a call to an out-of-scope operation (`intern`, `load`, etc.)
**When** evaluation reaches it
**Then** evaluation stops with an "opaque boundary" marker
**And** the type checker treats the result as requiring annotation

**Verify:** `(intern "foo")` produces opaque value; type checker flags it

### R10: Full macro expansion

**Given** a top-level form with nested macros
**When** `macroexpand-all` is called
**Then** all macros are recursively expanded to core forms

**Verify:** `(when (foo) (incf x))` expands to `(if (foo) (progn (setq x (+ x 1))))`

## Tasks

- [ ] [R1] Define value type with all Elisp value kinds
- [ ] [R2] Implement special form evaluation
- [ ] [R3] Implement function application
- [ ] [R4] Register pure built-in functions
- [ ] [R5] Implement defmacro registration
- [ ] [R6] Implement macroexpand-1
- [ ] [R7] Implement backquote evaluation
- [ ] [R8] Thread source locations through expansion
- [ ] [R9] Define opaque boundary semantics
- [ ] [R10] Implement macroexpand-all
- [ ] Test with macros from `subr.el` and `cl-lib.el`

Run review agent after interpreter expands `when-let` correctly before
proceeding to Spec 06.
