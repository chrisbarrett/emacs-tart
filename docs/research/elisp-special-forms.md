# Emacs Lisp Special Forms: Typing Implications

Analysis of Elisp special forms for type system design. For each form: description,
typing implications, tractability rating, and examples where helpful.

**Tractability scale:**
- **Tractable:** Standard type system techniques apply directly
- **Hard:** Requires non-trivial extensions or approximations
- **Intractable:** Cannot be soundly typed without severe restrictions

---

## 1. Definition Forms

### 1.1 `defun`

```elisp
(defun NAME ARGLIST [DOCSTRING] [DECLARE] [INTERACTIVE] BODY...)
```

**Description:** Defines a named function. Creates a lambda and stores it in the
symbol's function cell. Arguments can include `&optional` and `&rest`.

**Typing implications:**

- Function type is `(-> arg-types... return-type)`
- `&optional` args: requires union with `nil` or default type
- `&rest` args: requires list type for variadic portion
- Top-level `defun` is generalization point (like ML `let`)
- Docstrings and `declare` forms are erased at type level
- `interactive` spec doesn't affect type, but signals "command" status

**Argument handling:**

```elisp
(defun foo (a &optional b &rest c) ...)
;; Type: (-> A (Optional B) (Rest C) R)
;; Or simplified: (-> A B* C* R) where B* means 0-or-1, C* means 0-or-more
```

**Tractability:** Tractable

Basic function typing is standard. Optional/rest arguments require care but
are well-understood (see TypeScript, Typed Racket). Key decision: whether to
model `&optional` as union-with-nil or as distinct optional type.

---

### 1.2 `defmacro`

```elisp
(defmacro NAME ARGLIST [DOCSTRING] [DECLARE] BODY...)
```

**Description:** Defines a macro. Body returns code to be substituted at call
site. Arguments can include `&optional`, `&rest`, and `&body`.

**Typing implications:**

- Macros operate at compile time on syntax, not values
- Cannot type macro *definitions* in the usual sense
- *Expansions* must be well-typed
- Two strategies:
  1. Expand first, type the expansion (simple, works for many macros)
  2. Provide type signature for macro "interface" (complex, limited)

**Tractability:** Hard to Intractable

Full macro typing requires dependent types or similar (macros are
code-generating functions). Practical approach: expand macros before
type-checking. For common macros (e.g., `when`, `unless`), provide built-in
typing rules. User-defined macros require expansion or stub signatures.

---

### 1.3 `defvar`

```elisp
(defvar SYMBOL &optional INITVALUE DOCSTRING)
```

**Description:** Declares a global variable. Marks symbol as "special" (dynamically
scoped). Only sets value if previously void.

**Typing implications:**

- Global mutable state with dynamic scope
- Value can be modified anywhere: type must be invariant or conservative
- Initial value provides type hint, but mutations can widen
- `defvar` without initvalue: type is `t` (unknown/any)
- Dynamic binding means type at use site may differ from definition

**Tractability:** Hard

Dynamic scope complicates flow-sensitive typing. Options:
1. Require type annotations for global variables
2. Infer most general type from all assignments (whole-program analysis)
3. Treat as `t` and require casts at use sites

Recommended: require type annotation in `.tart` signature file.

---

### 1.4 `defconst`

```elisp
(defconst SYMBOL INITVALUE [DOCSTRING])
```

**Description:** Defines a constant. Value is always set (unlike `defvar`).
Still dynamically scoped but intended to be immutable.

**Typing implications:**

- Simpler than `defvar`: value is set at definition
- Type can be inferred from `INITVALUE`
- "Constant" is advisory only (Elisp allows rebinding)
- For soundness, treat as immutable; warn on mutation

**Tractability:** Tractable

Infer type from initial value. If treated as truly constant, no mutation
complications.

---

### 1.5 `defcustom`

```elisp
(defcustom SYMBOL STANDARD [DOCSTRING] KEYWORD-ARGS...)
```

**Description:** User-customizable variable. Includes `:type` spec for
customize interface.

**Typing implications:**

- The `:type` keyword specifies a Customize type (not a programming type)
- Customize types overlap but don't align with type system types
- Can extract type hints: `:type 'string` implies `String`
- Complex customize types (e.g., `(choice string nil)`) need mapping

**Customize-to-type mapping:**

| Customize Type | Programming Type |
|----------------|------------------|
| `string`       | `String`         |
| `integer`      | `Integer`        |
| `boolean`      | `Bool` (or `(Or t nil)`) |
| `symbol`       | `Symbol`         |
| `(repeat X)`   | `(List X)`       |
| `(choice A B)` | `(Or A B)`       |
| `sexp`         | `t`              |

**Tractability:** Hard

Mapping customize types to real types is useful but not 1:1. Requires
maintaining a translation table. Some customize types have no good
programming type equivalent.

---

## 2. Binding Forms

### 2.1 `let`

```elisp
(let ((VAR1 VALUE1) (VAR2 VALUE2) ...) BODY...)
```

**Description:** Parallel binding. All `VALUEn` evaluated before any binding.
With lexical binding, creates lexical scope; with dynamic binding, temporarily
shadows dynamic values.

**Typing implications:**

- Standard typing with parallel binding semantics
- Let-polymorphism: generalize bound variables if values are syntactic values
- With lexical binding: straightforward lexical scoping
- With dynamic binding: bindings may escape (problematic)

**Example:**

```elisp
(let ((id (lambda (x) x)))
  (list (funcall id 1) (funcall id "a")))
;; id : forall a. a -> a (if generalized)
```

**Tractability:** Tractable (with lexical binding)

Standard Algorithm W handles `let`. Dynamic binding complicates: the value
at a call site may not match the value at definition. Recommendation: only
fully support lexical binding; treat dynamic variables conservatively.

---

### 2.2 `let*`

```elisp
(let* ((VAR1 VALUE1) (VAR2 VALUE2) ...) BODY...)
```

**Description:** Sequential binding. Each `VALUEn` can refer to earlier bindings.

**Typing implications:**

- Desugar to nested `let`:
  ```elisp
  (let* ((x 1) (y x)) body)
  ≡ (let ((x 1)) (let ((y x)) body))
  ```
- Type inference proceeds left-to-right
- Generalization at each binding (some systems defer to end)

**Tractability:** Tractable

Direct desugaring or sequential constraint generation. Well-understood.

---

### 2.3 `letrec`

```elisp
(letrec ((VAR1 VALUE1) ...) BODY...)
```

**Description:** Recursive binding. All variables in scope for all values.
Enables mutually recursive local functions.

**Typing implications:**

- All bindings mutually visible: can't infer types sequentially
- Requires simultaneous constraint solving
- For recursive functions: assume type, verify body, unify

**Example:**

```elisp
(letrec ((even? (lambda (n) (if (= n 0) t (odd? (1- n)))))
         (odd? (lambda (n) (if (= n 0) nil (even? (1- n))))))
  (even? 10))
```

**Tractability:** Tractable

Standard technique: assume fresh type variables for all bindings, infer
body types, unify. Same as ML `let rec`.

---

### 2.4 `lambda`

```elisp
(lambda ARGLIST [DOCSTRING] [INTERACTIVE] BODY...)
```

**Description:** Anonymous function. With lexical binding, creates closure
capturing free variables.

**Typing implications:**

- Standard function type `(-> arg-types... return-type)`
- No generalization at lambda (generalization at `let` only)
- Closure captures: types of free variables must be known
- With lexical binding: captured types are fixed at creation
- With dynamic binding: captured variables may change (unsound scenarios)

**Tractability:** Tractable (with lexical binding)

Standard lambda typing. Complications arise only with dynamic binding or
effects.

---

## 3. Control Flow

### 3.1 `if`

```elisp
(if COND THEN ELSE...)
```

**Description:** Conditional. Evaluates `COND`; if non-nil, evaluates `THEN`;
otherwise evaluates `ELSE` forms (implicit `progn`).

**Typing implications:**

- Return type: union of `THEN` type and `ELSE` type
- If no `ELSE`, type is `(Or THEN-type Nil)`
- **Occurrence typing opportunity:** In `THEN` branch, `COND` is truthy;
  in `ELSE` branch, `COND` is falsy (nil)

**Occurrence typing example:**

```elisp
(if (stringp x)
    (length x)        ;; here x : String
  (error "not string"))
```

**Tractability:** Tractable (basic), Hard (with occurrence typing)

Basic union typing is standard. Occurrence typing (type narrowing) is more
complex but well-studied (TypeScript, Typed Racket). Requires tracking which
predicates were tested.

---

### 3.2 `cond`

```elisp
(cond (CONDITION BODY...)
      (CONDITION BODY...)
      ...)
```

**Description:** Multi-way conditional. Tests conditions in order; evaluates
body of first true condition.

**Typing implications:**

- Desugars to nested `if`:
  ```elisp
  (cond (c1 e1) (c2 e2) (t e3))
  ≡ (if c1 e1 (if c2 e2 e3))
  ```
- Return type: union of all body types
- If no clause matches, returns nil (include in union)
- `(t ...)` clause: covers all remaining cases

**Tractability:** Tractable

Desugaring approach works well. Occurrence typing can thread through
multiple branches.

---

### 3.3 `when` and `unless`

```elisp
(when COND BODY...)
(unless COND BODY...)
```

**Description:** One-armed conditionals. `when` executes body if condition
true; `unless` if condition false.

**Typing implications:**

- `when`: return type is `(Or BODY-type Nil)`
- `unless`: return type is `(Or BODY-type Nil)`
- Desugars to `if`:
  ```elisp
  (when c body) ≡ (if c (progn body) nil)
  (unless c body) ≡ (if c nil (progn body))
  ```

**Tractability:** Tractable

Direct desugaring.

---

### 3.4 `and`

```elisp
(and FORMS...)
```

**Description:** Short-circuit conjunction. Returns nil if any form is nil;
otherwise returns value of last form.

**Typing implications:**

- If any form has type that includes nil, result includes nil
- If all forms are truthy types, result is type of last form
- Short-circuit: later forms only evaluated if earlier forms truthy
- Return type: `(Or Nil LAST-FORM-TYPE)` (simplified)

**Occurrence typing:**

```elisp
(and (stringp x) (> (length x) 0))
;; In second form, x is narrowed to String
```

**Tractability:** Tractable (basic), Hard (with occurrence typing)

Basic typing: union with nil. Full occurrence typing requires tracking
narrowing through short-circuit evaluation.

---

### 3.5 `or`

```elisp
(or FORMS...)
```

**Description:** Short-circuit disjunction. Returns first non-nil value;
otherwise nil.

**Typing implications:**

- Return type: union of all form types
- If all forms can be nil, result can be nil
- Common pattern: `(or x default)` for default values

**Occurrence typing:**

```elisp
(or (gethash key table) "default")
;; Result type: (Or VALUE-TYPE "default")
```

**Tractability:** Tractable

Union of all branch types.

---

## 4. Sequencing

### 4.1 `progn`

```elisp
(progn BODY...)
```

**Description:** Evaluates forms in sequence. Returns value of last form.

**Typing implications:**

- Type = type of last form
- Earlier forms evaluated for side effects; types discarded
- `(progn)` returns nil

**Tractability:** Tractable

Standard sequencing. Type of sequence is type of final expression.

---

### 4.2 `prog1`

```elisp
(prog1 FIRST BODY...)
```

**Description:** Evaluates all forms; returns value of first form.

**Typing implications:**

- Type = type of first form
- Later forms for side effects

**Tractability:** Tractable

---

### 4.3 `prog2`

```elisp
(prog2 FORM1 FORM2 BODY...)
```

**Description:** Evaluates all forms; returns value of second form.

**Typing implications:**

- Type = type of second form
- Rarely used; straightforward

**Tractability:** Tractable

---

## 5. Error Handling

### 5.1 `condition-case`

```elisp
(condition-case VAR BODYFORM HANDLERS...)
;; where HANDLER = (CONDITIONS BODY...)
```

**Description:** Error handler. Evaluates `BODYFORM`; if error matching
`CONDITIONS` is signaled, runs corresponding handler with error bound to `VAR`.

**Typing implications:**

- Return type: union of `BODYFORM` type and all handler body types
- `VAR` in handler has type based on condition (often just `t` or error type)
- Handler `(t ...)` catches all errors
- Protected form's type contributes only if no error occurs

**Control flow:**

```elisp
(condition-case err
    (may-signal-error)           ;; Type A
  (error (handle-error err)))    ;; Type B
;; Result type: (Or A B)
```

**Tractability:** Hard

Union typing is straightforward. The complexity:
1. Which conditions can be signaled? (Requires effect tracking or annotation)
2. Non-local control flow from `signal`/`error`
3. `VAR` type in handler is condition-specific

For v1: treat as union of body type and handler types; ignore condition specifics.

---

### 5.2 `unwind-protect`

```elisp
(unwind-protect BODYFORM UNWINDFORMS...)
```

**Description:** Cleanup form. Evaluates `BODYFORM`, then always evaluates
`UNWINDFORMS` (even on error/throw). Returns `BODYFORM` value.

**Typing implications:**

- Return type = `BODYFORM` type
- `UNWINDFORMS` evaluated for side effects; types discarded
- Like `try`/`finally` in other languages

**Tractability:** Tractable

Simple: type of body. Cleanup forms don't contribute to result type.

---

### 5.3 `signal` and `error`

```elisp
(signal ERROR-SYMBOL DATA)
(error FORMAT-STRING &rest ARGS)
```

**Description:** Signal an error condition. `error` is a convenience wrapper.

**Typing implications:**

- Return type is `Never` / bottom type (doesn't return normally)
- Enables type narrowing: code after `error` is unreachable
- `signal` with continuable errors: more complex (rare in practice)

**Example:**

```elisp
(if (null x)
    (error "x is nil")
  (use-x x))                ;; x : (Not Nil) here due to narrowing
```

**Tractability:** Tractable (for bottom type), Hard (for effect tracking)

Bottom type is well-understood. Full error effect tracking requires an
effect system.

---

## 6. Save/Restore Forms

These forms temporarily modify global state, execute body, then restore.
They are "effect-like" patterns that bracket a computation.

### 6.1 `save-excursion`

```elisp
(save-excursion BODY...)
```

**Description:** Saves point, mark, and current buffer. Executes body.
Restores saved state.

**Typing implications:**

- Return type = type of body (last form)
- Effect: temporarily modifies point/mark/buffer
- From type perspective: transparent wrapper
- For effect system: could track "excursion effect"

**Tractability:** Tractable (for types), Hard (for effects)

Pure type perspective: same as `progn`. Effect tracking: would need to
model buffer/point state.

---

### 6.2 `save-restriction`

```elisp
(save-restriction BODY...)
```

**Description:** Saves narrowing state (restriction). Executes body. Restores.

**Typing implications:**

- Return type = type of body
- Like `save-excursion`: transparent wrapper for types

**Tractability:** Tractable

---

### 6.3 `save-current-buffer`

```elisp
(save-current-buffer BODY...)
```

**Description:** Saves current buffer. Executes body. Restores current buffer.

**Typing implications:**

- Return type = type of body
- Transparent for typing

**Tractability:** Tractable

---

### 6.4 `save-match-data`

```elisp
(save-match-data BODY...)
```

**Description:** Saves match data (from regexp operations). Executes body.
Restores match data.

**Typing implications:**

- Return type = type of body
- Match data is global mutable state
- For effect system: "match-data effect"

**Tractability:** Tractable (types), Hard (effects)

---

### 6.5 Pattern: Save/Restore as Effect Bracketing

All save-* forms follow a pattern:

```
(save-STATE BODY) ≡
  let old = current-STATE
  try BODY
  finally restore-STATE(old)
```

**Type system implications:**

1. **For pure types:** Treat as `progn`-like; return type of body
2. **For effect system:** Could model as effect bracketing
3. **For occurrence typing:** State inside may differ from outside

Recommendation for v1: treat all save-* forms as transparent wrappers
returning body type. Effect tracking deferred.

---

## 7. Other Important Special Forms

### 7.1 `setq`

```elisp
(setq VAR VALUE VAR VALUE ...)
```

**Description:** Assignment. Sets variables to values (left to right).
Returns last value.

**Typing implications:**

- Mutation requires type consistency
- Value type must be subtype of variable's declared type
- For flow-sensitive typing: variable type may narrow/widen
- Return type = type of last value

**Tractability:** Tractable (basic), Hard (flow-sensitive)

Basic: require value type <= variable type. Flow-sensitive: track type
changes through assignments.

---

### 7.2 `quote`

```elisp
(quote OBJECT)
'OBJECT
```

**Description:** Returns `OBJECT` unevaluated.

**Typing implications:**

- Literal type: `'foo` has type `(Literal foo)` or just `Symbol`
- Quoted lists: `'(1 2 3)` has type `(List Integer)` or more precise
- Self-quoting objects (numbers, strings): quote is identity

**Tractability:** Tractable

Literal types are well-understood.

---

### 7.3 `function`

```elisp
(function FUNCTION)
#'FUNCTION
```

**Description:** Returns function object. For lambdas, signals "this is a
function" (affects compilation). For symbols, gets function binding.

**Typing implications:**

- `#'(lambda ...)` same as `(lambda ...)`
- `#'name` requires `name` to have function type
- Important for Lisp-2: distinguishes function namespace from variable

**Tractability:** Tractable

---

### 7.4 `while`

```elisp
(while TEST BODY...)
```

**Description:** Loop while `TEST` is non-nil.

**Typing implications:**

- Return type: always nil (or `Void`)
- `TEST` must have boolean-compatible type
- Loop-carried type changes: requires fixed-point or widening
- Occurrence typing in body: `TEST` known truthy

**Tractability:** Tractable (basic), Hard (with loop-carried typing)

Simple loops: return nil, ignore body type. Complex: track how types
evolve across iterations.

---

### 7.5 `catch` and `throw`

```elisp
(catch TAG BODY...)
(throw TAG VALUE)
```

**Description:** Non-local exit. `catch` establishes handler; `throw` jumps
to matching catch, returning value.

**Typing implications:**

- `catch` return type: union of body type and thrown values
- `throw` return type: bottom (never returns)
- Tag matching is dynamic: hard to track statically
- Similar challenges to exception typing

**Tractability:** Hard

Dynamic tag matching makes static typing difficult. Options:
1. Require tag types to be declared
2. Treat catch as returning `t` (any)
3. Use effect types for throws

---

## 8. Summary Table

| Form | Category | Tractability | Notes |
|------|----------|--------------|-------|
| `defun` | Definition | Tractable | Standard function typing |
| `defmacro` | Definition | Intractable | Expand before typing |
| `defvar` | Definition | Hard | Requires annotation |
| `defconst` | Definition | Tractable | Infer from value |
| `defcustom` | Definition | Hard | Map customize types |
| `let` | Binding | Tractable | Standard let-polymorphism |
| `let*` | Binding | Tractable | Sequential let |
| `letrec` | Binding | Tractable | Recursive binding |
| `lambda` | Binding | Tractable | Standard function type |
| `if` | Control | Tractable* | *Hard with occurrence typing |
| `cond` | Control | Tractable | Desugar to if |
| `when`/`unless` | Control | Tractable | Desugar to if |
| `and`/`or` | Control | Tractable* | *Hard with occurrence typing |
| `progn` | Sequencing | Tractable | Type of last form |
| `prog1`/`prog2` | Sequencing | Tractable | Type of nth form |
| `condition-case` | Error | Hard | Union + condition tracking |
| `unwind-protect` | Error | Tractable | Type of body |
| `save-*` | Save/Restore | Tractable | Transparent wrappers |
| `setq` | Mutation | Tractable* | *Hard flow-sensitive |
| `quote` | Literal | Tractable | Literal types |
| `function` | Literal | Tractable | Function namespace |
| `while` | Loop | Tractable | Returns nil |
| `catch`/`throw` | Control | Hard | Dynamic tag matching |

---

## 9. Recommendations for v1

### Prioritize (Tractable)

- All binding forms: `let`, `let*`, `letrec`, `lambda`
- Basic control flow: `if`, `cond`, `when`, `unless`
- Sequencing: `progn`, `prog1`, `prog2`
- Definition: `defun`, `defconst`
- Literals: `quote`, `function`
- Cleanup: `unwind-protect`, all `save-*` forms

### Support with Limitations (Hard)

- `defvar`: require type annotations
- `defcustom`: extract type hints where possible
- `condition-case`: union typing without condition specifics
- `and`/`or`: basic union typing, defer occurrence typing
- `setq`: simple type checking, defer flow-sensitivity

### Defer or Restrict (Intractable)

- `defmacro`: expand before typing; known macros get special rules
- `catch`/`throw`: conservative typing or require annotations
- Full occurrence typing: phase 2 feature

---

## References

- [GNU Emacs Lisp Reference Manual - Special Forms](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Forms.html)
- [GNU Emacs Lisp Reference Manual - Conditionals](https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html)
- [GNU Emacs Lisp Reference Manual - Local Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html)
- [GNU Emacs Lisp Reference Manual - Handling Errors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html)
- [GNU Emacs Lisp Reference Manual - Excursions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Excursions.html)
