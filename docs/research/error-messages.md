# Error Message Design for Type Checkers

Research on error message design for type systems, with focus on application to Emacs
Lisp type checking in tart.

## 1. Principles of Good Error Messages

### Elm's Philosophy

Elm is widely regarded as setting the gold standard for compiler error messages. The
core philosophy treats compiler errors as opportunities to educate developers rather
than cryptic status reports.

Key principles:

1. **First-person communication**: Use "I" to make errors feel conversational.
   - "I ran into a problem" rather than "Error occurred"
   - Creates a collaborative feel between developer and compiler

2. **Plain English over jargon**: Hide internal representations. Present the exact
   mistake in simple terms with suggestions for fixing it. A programmer "who just
   came out of bed after a night partying" should be able to understand it.

3. **Source-centric presentation**: Show the code the user wrote prominently. The
   error builds on their code, not abstract type expressions.

4. **Actionable guidance**: Don't just state problems; suggest fixes. Every error
   message should help the developer take a concrete next step.

5. **Educational opportunity**: For common mistakes, explain why something is wrong
   and teach language concepts inline.

John Carmack commented on Elm's error messages: "This should be an inspiration for
every error message."

### Rust's Approach

Rust's error message design follows a structured philosophy documented in RFC 1644:

1. **Code-centric display**: The fundamental observation is that errors should focus
   on the code the user wrote. Everything in the output builds on the source.

2. **Labels over prose**: Messages explaining why an error occurred appear as labels
   directly on the source code, not as separate paragraphs.

3. **Visual hierarchy**:
   - Primary labels: Coloured to match severity (red for errors, yellow for warnings),
     using `^^^` underline. Explain the "what".
   - Secondary labels: Blue text with `---` underline. Explain the "why".

4. **Separation of concerns**: Multiple errors should be clearly separate, not muddled
   together. Each error draws the eye to where it occurs.

5. **Accessibility**: Error messages must be readable without colours (for
   colour-impaired readers or monochrome terminals). Avoid Unicode and excessive
   ASCII art.

6. **Extended explanations**: The `--explain` flag provides longer, more verbose
   explanations for unfamiliar errors, keeping default output concise.

### What Makes Errors Actionable

Research and industry practice identify these characteristics of actionable errors:

1. **Specificity**: "Invalid input" is useless. "Expected a string but got an integer"
   is actionable. State the exact requirement that was violated.

2. **Location precision**: Point to the exact sub-expression where the error was
   detected. For type errors, also show related locations that contributed.

3. **Constructive suggestions**: Offer potential remedies, not just diagnosis.
   - Bad: "Type mismatch"
   - Good: "Type mismatch: expected String but found Integer. Did you mean to use
     `number-to-string`?"

4. **Preserve user intent**: Let users correct errors by editing their original input.
   Show what they wrote and what was expected alongside it.

5. **Minimal cognitive load**: Display errors adjacent to their source. Avoid forcing
   users to mentally map between error descriptions and code locations.

## 2. Error Message Structure

### Location Information

Every diagnostic needs precise location data:

```
file.el:42:15: error: ...
        ^     ^
        |     column (1-based)
        line (1-based)
```

For type errors, location is particularly important because errors may be detected far
from their cause. Include:

- **Primary location**: Where the type conflict was detected
- **Related locations**: Where conflicting constraints originated

Example structure:

```
foo.el:42:10: error[E0001]: type mismatch
   |
42 |   (concat name count)
   |                ^^^^^
   |                expected: string
   |                found: integer
   |
35 |   (let ((count 0))
   |                 - count defined as integer here
```

### The Error Message Itself

Structure messages in layers:

1. **Header**: Error code, severity, and one-line summary
   ```
   error[E0308]: type mismatch in function argument
   ```

2. **Primary span**: The code location with the main issue
   ```
      |
   42 |   (upcase count)
      |          ^^^^^ expected string, found integer
   ```

3. **Secondary spans**: Related code that explains why
   ```
      |
   35 |   (let ((count 0))
      |                - this is an integer
   ```

4. **Note**: Additional context that doesn't fit as a label
   ```
   note: `upcase` expects its argument to be a string
   ```

5. **Help**: Actionable suggestion
   ```
   help: use `number-to-string` to convert: (upcase (number-to-string count))
   ```

### Suggestions and Hints

Suggestions should be:

1. **Machine-applicable when possible**: If the fix is unambiguous, provide code that
   can be directly applied. LSP's "quick fix" actions depend on this.

2. **Clearly marked as suggestions**: Use "help:" or "suggestion:" prefixes.
   Don't present guesses as facts.

3. **Ranked by likelihood**: If multiple fixes are possible, order by probability.
   The most likely fix should come first.

4. **Context-aware**: Use information from the current scope.
   - Variable names that are close to what was typed
   - Functions that would produce the expected type
   - Common patterns in the codebase

Example with multiple suggestions:

```
error[E0425]: cannot find value `strng` in this scope
   |
42 |   (upcase strng)
   |          ^^^^^ not found in this scope
   |
help: a local variable with a similar name exists
   |
42 |   (upcase string)
   |          ^^^^^^
help: perhaps you meant the function `string`?
   |
42 |   (upcase (string ?c))
   |          ^^^^^^^^^^^^
```

### Related Locations

For complex type errors, show the chain of reasoning:

```
error[E0308]: mismatched types
  --> foo.el:50:3
   |
50 |   result
   |   ^^^^^^ expected `String`, found `Integer`
   |
note: expected due to return type
  --> foo.el:42:1
   |
42 | (defun my-func () : String
   |                     ^^^^^^ expected `String` because of this
```

Related locations help answer: "Why does the compiler think this should be X?"

## 3. Type Error Specific Patterns

### Expected vs Actual

The most common type error pattern. Key considerations:

1. **Consistent ordering**: Always use the same order (expected vs actual or actual vs
   expected). Rust and most languages use "expected X, found Y".

2. **User's perspective**: The "expected" type comes from context (function signature,
   variable declaration). The "actual" type comes from what the user wrote.

3. **Clear labelling**: Don't assume users know which is which.

```
error: type mismatch
   |
42 |   (+ 1 "two")
   |        ^^^^^
   |        |
   |        expected: number
   |        found: string
```

For Emacs Lisp, align with existing conventions:

```elisp
;; Emacs signals this at runtime:
(signal 'wrong-type-argument (list 'numberp "two"))
;; Message: Wrong type argument: numberp, "two"
```

Our static error should feel familiar:

```
error: wrong type argument
   |
42 |   (+ 1 "two")
   |        ^^^^^
   |        expected: number (for `+`)
   |        found: string
```

### Type Mismatch Formatting

When displaying complex types, apply formatting for readability:

1. **Highlight differences**: When comparing two types, emphasise what differs.

   ```
   expected: (List (Option String))
   found:    (List (Option Integer))
                          ^^^^^^^
   ```

2. **Abbreviate common prefixes**: For deeply nested types, collapse matching parts.

   ```
   expected: (-> (List ...) (Option (Result String Error)))
   found:    (-> (List ...) (Option (Result Integer Error)))
                                           ^^^^^^^
   ```

3. **Use vertical alignment**: For record types, align fields.

   ```
   expected type:
     { name : String
     , age  : Integer
     , email: String }

   found type:
     { name : String
     , age  : String    <- mismatch
     , email: String }
   ```

### Unification Failure Presentation

Unification errors are challenging because the conflict may arise from constraints
gathered across multiple locations. Best practices:

1. **Show the conflicting path**: Trace back to where each type came from.

   ```
   error: conflicting types for `x`
      |
   10 |   (let ((x (if condition 1 "one")))
      |                          ^  ^^^
      |                          |  this is a string
      |                          this is an integer
      |
   note: both branches of `if` must have the same type
   ```

2. **Don't blame one side arbitrarily**: Traditional unification has left-to-right
   bias, blaming the right side. Modern approaches type-check branches independently
   first, then report both when unification fails.

3. **Explain the constraint origin**: Why do these need to be the same type?

   ```
   error: type mismatch in if-expression
      |
      | (if condition
      |     1          ; branch 1: Integer
      |   "one")       ; branch 2: String
      |
   note: both branches of an `if` must have the same type so the result
         type is known regardless of which branch executes
   ```

### Higher-Rank Type Errors

Higher-rank polymorphism errors are notoriously confusing. Key strategies:

1. **Avoid jargon**: Don't mention "rank-2" or "impredicative" to users.

2. **Explain the actual problem**: The function needs to work for *any* type, but
   you're restricting it.

   ```
   error: type too specific
      |
   42 |   (run-with-state (lambda (s) (+ s 1)) initial)
      |                   ^^^^^^^^^^^^^^^^^^^^
      |                   |
      |                   this function only works with integers
      |
   note: `run-with-state` requires a function that works with any type,
         but this lambda only works with integers (due to `+`)

   help: if you need integer-specific state, use `run-with-int-state`
   ```

3. **Point to the restriction**: Show exactly which operation constrains the type.

4. **Suggest annotations if needed**: Higher-rank types often require explicit
   annotation.

   ```
   help: add a type annotation to make the polymorphism explicit:
      |
   42 |   (run-with-state (lambda ((s : forall a. a)) ...) initial)
   ```

## 4. Techniques

### Type Diffing

When types are large, compute and display only the differences:

```
error: type mismatch
   |
   | expected: (-> (List (Tuple String Integer)) (Option (Result String Error)))
   | found:    (-> (List (Tuple String Integer)) (Option (Result Integer Error)))
   |                                                             ^^^
   |                                                    differs here: Integer vs String
```

Implementation approach:

1. Walk both type trees in parallel
2. Identify the first point of divergence
3. Display abbreviated context with the difference highlighted
4. For multiple differences, list each separately

### Type Normalization for Display

Types may have internal representations that are confusing to display:

1. **Expand type aliases**: Show the user-facing name, not internal representation.
   ```
   ; Internal: TVar(42) unified with TApp(TConst("List"), [TConst("String")])
   ; Display: (List String)
   ```

2. **Simplify when possible**: `(Option (Option a))` might stay, but intermediate
   unification variables should be resolved.

3. **Preserve user names**: If the user wrote `MyType`, show `MyType`, not its
   expansion (unless expansion helps understanding).

4. **Configurable expansion**: Agda has debated whether to normalise types in errors.
   Consider offering both forms:
   ```
   expected: Buffer    (expands to: (Record (name String) (contents String) ...))
   found:    Integer
   ```

### Contextual Explanations

Explain errors in context of what the user was trying to do:

```
error: wrong number of arguments to `substring`
   |
42 |   (substring str)
   |   ^^^^^^^^^^^^^^^
   |   |
   |   given 1 argument, but `substring` requires 2 or 3
   |
note: `substring` extracts part of a string:
      (substring STRING FROM &optional TO)
      - STRING: the string to extract from
      - FROM: starting index (0-based)
      - TO: ending index (optional, defaults to end of string)

help: perhaps you meant:
   |
42 |   (substring str 0)        ; get from start
42 |   (substring str 0 10)     ; get first 10 characters
```

### Code Snippets

Best practices for including code in errors:

1. **Show enough context**: Include surrounding lines when they help.
   ```
      |
   40 |   (let ((name (get-name)))
   41 |     (when name
   42 |       (upcase name)))  ; error here
      |               ^^^^
   ```

2. **Truncate long lines**: Don't wrap or scroll horizontally.
   ```
   42 |   (very-long-function-call-that-extends... (problematic-arg)
      |                                             ^^^^^^^^^^^^^^^
   ```

3. **Highlight the precise span**: Use carets under the exact problematic expression.

4. **Number lines**: Line numbers help users find the location in their editor.

5. **Handle multi-line expressions**:
   ```
      |
   42 |   (my-function
   43 |     first-arg
   44 |     second-arg)
      |     ^^^^^^^^^^ type error here
   ```

## 5. LSP Diagnostic Formatting

### Diagnostic Structure

LSP defines the Diagnostic interface:

```typescript
interface Diagnostic {
  range: Range;                    // Where the diagnostic applies
  severity?: DiagnosticSeverity;   // Error, Warning, Info, Hint
  code?: integer | string;         // Diagnostic code (e.g., "E0308")
  codeDescription?: CodeDescription;  // URI to documentation
  source?: string;                 // Source identifier (e.g., "tart")
  message: string;                 // The diagnostic message
  tags?: DiagnosticTag[];          // Deprecated, Unnecessary
  relatedInformation?: DiagnosticRelatedInformation[];
  data?: any;                      // For code action preservation
}
```

### Severity Levels

Use severity levels appropriately:

- **Error** (1): Type mismatches, unbound variables, syntax errors. Code is incorrect.
- **Warning** (2): Unused variables, deprecated functions, suspicious patterns.
- **Information** (3): Style suggestions, refactoring hints.
- **Hint** (4): Very minor suggestions, often shown inline only.

For type errors, most will be Error. Reserve Warning for:
- Type widening that loses precision
- Implicit conversions
- Deprecated type usage

### Related Information

Use `relatedInformation` to show connected locations:

```typescript
{
  range: { start: { line: 41, character: 10 }, end: { line: 41, character: 15 } },
  severity: 1,
  code: "E0308",
  source: "tart",
  message: "type mismatch: expected String, found Integer",
  relatedInformation: [
    {
      location: {
        uri: "file:///path/to/foo.el",
        range: { start: { line: 35, character: 5 }, end: { line: 35, character: 20 } }
      },
      message: "count defined as Integer here"
    },
    {
      location: {
        uri: "file:///path/to/foo.el",
        range: { start: { line: 10, character: 0 }, end: { line: 10, character: 30 } }
      },
      message: "expected String due to function signature"
    }
  ]
}
```

This allows editors to:
- Show related locations inline
- Allow navigation to related positions
- Display the full error context

### Code Descriptions

Link error codes to documentation:

```typescript
{
  code: "E0308",
  codeDescription: {
    href: "https://tart.example.com/errors/E0308"
  }
}
```

Maintain an error index (like Rust's or Haskell's) with detailed explanations.

### Message Formatting

Keep messages concise for the diagnostic itself. The `message` field appears in:
- Editor hover tooltips
- Problems panel lists
- Inline error displays

Format for multiple contexts:
```
type mismatch: expected String, found Integer
```

Longer explanations belong in:
- Related information messages
- The error code documentation
- Hover or lightbulb actions

## 6. Examples

### Example 1: Basic Type Mismatch

**Bad** (typical ML-style):

```
Error: This expression has type int but an expression was expected of type string
```

Problems:
- No location shown
- "expression was expected" is passive and vague
- Which expression? Expected where?

**Good**:

```
error[E0308]: type mismatch
  --> init.el:42:10
   |
42 |   (upcase count)
   |          ^^^^^
   |          |
   |          expected: string
   |          found: integer
   |
note: `upcase` is defined as:
      (upcase STRING) -> String

help: convert the integer to a string first:
   |
42 |   (upcase (number-to-string count))
   |           ^^^^^^^^^^^^^^^^^^^^^^^^
```

### Example 2: Function Argument Mismatch

**Bad**:

```
Wrong number of arguments: 2
```

**Good**:

```
error[E0061]: incorrect number of arguments
  --> config.el:15:3
   |
15 |   (my-format template)
   |   ^^^^^^^^^^^^^^^^^^^^
   |   expected 2 arguments, found 1
   |
note: `my-format` signature:
      (my-format TEMPLATE ARGS) -> String
   |
 5 | (defun my-format (template args)
   |                            ^^^^ missing argument

help: provide the ARGS parameter:
   |
15 |   (my-format template '())
```

### Example 3: If-Branch Type Inconsistency

**Bad**:

```
Type error: branches of conditional have different types
```

**Good**:

```
error[E0317]: if branches have incompatible types
  --> utils.el:28:3
   |
28 |   (if (> n 0)
29 |       n
   |       ^ this branch has type: integer
30 |       "negative")
   |       ^^^^^^^^^^ this branch has type: string
   |
note: both branches of `if` must have the same type

help: perhaps return a string in both cases?
   |
29 |       (number-to-string n)

help: or return a number in both cases?
   |
30 |       -1)
```

### Example 4: Undefined Function

**Bad**:

```
Symbol's function definition is void: procss-buffer
```

**Good**:

```
error[E0425]: function `procss-buffer` is not defined
  --> edit.el:102:5
   |
102|     (procss-buffer)
   |      ^^^^^^^^^^^^^ not found
   |
help: a function with a similar name exists:
   |
102|     (process-buffer)
   |      ^^^^^^^^^^^^^^

help: did you forget to require a package?
   |
 1 | (require 'my-process-utils)
```

### Example 5: Complex Nested Type Error

**Bad**:

```
Cannot unify (List (Option String)) with (List (Option Integer))
```

**Good**:

```
error[E0308]: type mismatch in list element
  --> data.el:55:12
   |
55 |   (cons (Some 42) names)
   |              ^^
   |              expected: String (to match list element type)
   |              found: Integer
   |
note: `names` has type (List (Option String))
  --> data.el:50:3
   |
50 |   (let ((names (list (Some "Alice") (Some "Bob"))))
   |         ^^^^^ this list contains (Option String)
   |
help: if 42 should be a string:
   |
55 |   (cons (Some "42") names)
```

### Example 6: Higher-Order Function Type Error

**Bad**:

```
The type variable 'a' escapes its scope
```

**Good**:

```
error[E0521]: function type is too restrictive
  --> pipeline.el:33:5
   |
33 |     (map (lambda (x) (+ x 1)) items)
   |          ^^^^^^^^^^^^^^^^^^^^
   |          this function only accepts integers
   |
note: `map` expects a function that works with any element type:
      (map FUNC LIST) where FUNC : (a -> b)
   |
   | but the use of `+` restricts the input to integers

help: if `items` contains integers, add a type annotation:
   |
33 |     (map (lambda ((x : Integer)) (+ x 1)) items)
```

### Example 7: Record Field Type Error

**Bad**:

```
Type error: field 'age' has wrong type
```

**Good**:

```
error[E0308]: type mismatch in record field
  --> models.el:72:15
   |
72 |   (make-person :name "Alice" :age "thirty")
   |                                   ^^^^^^^^
   |                                   |
   |                                   expected: Integer
   |                                   found: String
   |
note: `person` record definition:
  --> models.el:5:1
   |
 5 | (defstruct person
 6 |   (name : String)
 7 |   (age : Integer))    ; age must be an integer
   |          ^^^^^^^

help: use an integer for age:
   |
72 |   (make-person :name "Alice" :age 30)
```

### Example 8: Occurrence Typing Not Applied

**Bad**:

```
Expected String but got (Option String)
```

**Good**:

```
error[E0308]: possible nil value
  --> format.el:18:13
   |
18 |   (upcase (get-name user))
   |           ^^^^^^^^^^^^^^^^
   |           |
   |           expected: String
   |           found: (Option String)
   |
note: `get-name` may return nil:
      (get-name USER) -> (Option String)

help: check for nil first:
   |
18 |   (when-let ((name (get-name user)))
19 |     (upcase name))

help: or provide a default:
   |
18 |   (upcase (or (get-name user) "Unknown"))
```

## Summary: Error Message Checklist

When implementing type error messages for tart, verify each error:

1. [ ] **Has a location**: Points to specific line and column
2. [ ] **Shows code context**: Displays the relevant source snippet
3. [ ] **Uses clear language**: Avoids jargon, uses expected/found consistently
4. [ ] **Explains why**: Labels show where types came from
5. [ ] **Is actionable**: Includes at least one fix suggestion
6. [ ] **Has an error code**: Allows lookup in documentation
7. [ ] **Works without colour**: Readable on monochrome terminals
8. [ ] **Handles multi-location errors**: Uses related information for complex cases
9. [ ] **Is concise**: Main message fits in one line; details are in notes/help
10. [ ] **Feels familiar**: Aligns with Emacs conventions where possible

## References

- [Elm Error Message Catalog](https://github.com/elm/error-message-catalog)
- [Rust RFC 1644: Error Format](https://rust-lang.github.io/rfcs/1644-default-and-expanded-rustc-errors.html)
- [Evolution of Rust Compiler Errors](https://kobzol.github.io/rust/rustc/2025/05/16/evolution-of-rustc-errors.html)
- [Shape of Errors to Come (Rust Blog)](https://blog.rust-lang.org/2016/08/10/Shape-of-errors-to-come/)
- [Haskell Error Index](https://errors.haskell.org/index.html)
- [Improving Type Error Messages in OCaml](https://arxiv.org/pdf/1512.01897)
- [Learning to Blame: Data-Driven Type Error Diagnosis](https://arxiv.org/abs/1708.07583)
- [LSP Specification 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [Writing Good Compiler Error Messages](https://calebmer.com/2019/07/01/writing-good-compiler-error-messages.html)
- [Un-obscuring GHC Type Error Messages](https://free.cofree.io/2020/09/01/type-errors/)
