# Spec 45: Source Excerpts in Error Messages

Elm-style friendly errors showing source code at error sites with visual underlines.

**Dependencies:** Spec 13 (error-reporting), Spec 35 (structured-errors)

## Goal

When users see a type error, they immediately understand what went wrong by seeing the actual code with visual markers—not just file:line:col coordinates they must navigate to manually.

## Constraints

| Constraint | Detail |
|------------|--------|
| Source reading | Must read source files to extract lines around errors |
| Graceful fallback | Missing/unreadable files degrade to current format |
| Multi-span | Show excerpts for primary span AND related locations |
| Provenance | Show which .tart file defines a violated signature |

## Background

### Output Formats

The CLI supports three output formats via `--format`:

| Format | Use case | This spec |
|--------|----------|-----------|
| `human` | Interactive terminal use (default) | Adds excerpts, colors, Elm prose |
| `compact` | IDE integration, grep-friendly | Unchanged (single-line) |
| `json` | Machine consumption, tooling | Unchanged (structured data) |

### Current State

Spec 13 describes the target format with source excerpts:

```
42 |   (upcase count)
   |           ^^^^^
   |           expected: String
```

Current `human` format only shows:

```
error[E0308]: type mismatch
  --> init.el:42:10
   |
   = expected: String
```

This spec implements the missing source excerpt rendering plus Elm-style prose for the `human` format only.

## Output

```
lib/
├── util/
│   ├── ansi.ml             ; ANSI color codes and TTY detection
│   └── ansi.mli
└── typing/
    ├── source_excerpt.ml   ; Source reading and rendering
    ├── source_excerpt.mli
    └── diagnostic.ml       ; (extended to use excerpts)
```

## Requirements

### R1: Source line extraction

**Given** a source span pointing to a file
**When** rendering an error
**Then** extract the relevant lines from the source file

```ocaml
val get_lines : file:string -> start_line:int -> end_line:int -> string list option
(* Returns None if file unreadable; Some lines otherwise *)
```

**Verify:** `dune test`; returns lines for valid files; None for missing files

### R2: Underline rendering

**Given** a span within a source line
**When** rendering the excerpt
**Then** show the line with carets underneath pointing to the span

```
42 |   (upcase count)
   |           ^^^^^
```

For multi-line spans, underline each line:

```
10 |   (if condition
   |   ^^^^^^^^^^^^^
11 |       then-branch
   |       ^^^^^^^^^^^
12 |       else-branch)
   |       ^^^^^^^^^^^^
```

**Verify:** `dune test`; underlines align with span columns

### R3: Line number gutter

**Given** source lines to display
**When** rendering
**Then** show line numbers in a consistent-width gutter

```
 9 |   (let ((x 1))
10 |     (+ x "foo"))
   |          ^^^^^
```

Gutter width adapts to largest line number displayed.

**Verify:** `dune test`; gutter aligns across all displayed lines

### R4: Elm-style prose headers

**Given** a type error
**When** formatting for display
**Then** use a friendly header with dashes spanning to location

```
-- TYPE MISMATCH ---------------------------------------- init.el:42:10

I found a type mismatch in this expression:

42 |   (upcase count)
   |           ^^^^^

This has type:

    Int

But `upcase` expects its argument to have type:

    String
```

**Verify:** Manual review; header format matches example

### R5: Conversational explanations

**Given** constraint context (FunctionArg, IfBranch, DeclaredReturn, etc.)
**When** formatting the error
**Then** use prose that explains the situation conversationally

For `FunctionArg`:
```
I found a type mismatch in this expression:

42 |   (my-fn 123 "hello")
   |              ^^^^^^^

The function `my-fn` expects argument 2 to be:

    Int

But this expression has type:

    String
```

For `IfBranch`:
```
The branches of this `if` have different types:

10 |   (if condition
11 |       42
   |       ^^ this is Int
12 |       "nope")
   |       ^^^^^^ this is String

Both branches must have the same type.
```

For `DeclaredReturn`:
```
The return type doesn't match the declaration:

 5 |   (declare (tart (int) -> string))

But the function body returns:

    Int
```

**Verify:** `dune test`; each context variant produces distinct prose

### R6: Provenance for .tart signatures

**Given** a type mismatch involving a function from a .tart file
**When** the signature has a known source location
**Then** show where the signature is defined

```
-- TYPE MISMATCH ---------------------------------------- init.el:42:10

I found a type mismatch...

42 |   (upcase count)
   |           ^^^^^

The signature for `upcase` is defined at:

  --> typings/emacs/30.1/subr.tart:127

127|   (defun upcase (string) -> string)
   |                  ^^^^^^
```

**Verify:** `dune test`; signature location shown when available

### R7: Related location excerpts

**Given** a diagnostic with related locations
**When** those locations have valid spans (not `<generated>`)
**Then** show source excerpts for them too

```
-- BRANCH MISMATCH -------------------------------------- utils.el:28:3

10 |   (if (> n 0)
11 |       n
   |       ^ this branch has type: Int

But this branch has a different type:

13 |       "negative")
   |       ^^^^^^^^^^ this is String
```

**Verify:** `dune test`; related spans shown with excerpts

### R8: Graceful degradation

**Given** a span pointing to a file that can't be read
**When** rendering the error
**Then** fall back to location-only format without crashing

```
-- TYPE MISMATCH ---------------------------------------- init.el:42:10

(source not available)

Expected: String
Found: Int
```

**Verify:** `dune test`; unreadable files produce fallback; no exceptions

### R9: Help suggestions with context

**Given** a diagnostic with help suggestions
**When** the help involves code transformation
**Then** show the suggestion with visual context

```
Hint: Convert the integer to a string:

42 |   (upcase (number-to-string count))
```

When no code transformation applies:
```
Hint: Check for nil first with `when-let` or provide a default with `or`.
```

**Verify:** `dune test`; code suggestions shown inline; prose suggestions shown as text

### R10: Non-human formats unchanged

**Given** `--format=compact` or `--format=json`
**When** formatting errors
**Then** preserve existing formats (no excerpts, no colors)

Compact:
```
init.el:42:10: error[E0308]: type mismatch [expected: String, found: Int]
```

JSON:
```json
{"kind":"type","code":"E0308","message":"type mismatch",...}
```

**Verify:** `./tart check --format=compact`; `./tart check --format=json`; output unchanged from current behavior

### R11: ANSI color output

**Given** `--format=human` (default)
**When** stdout is a TTY
**Then** use ANSI colors for visual distinction

| Element | Color |
|---------|-------|
| `error` | Red bold |
| `warning` | Yellow bold |
| `hint` | Cyan |
| Error code `[E0308]` | Red |
| `-->` location | Blue |
| Line numbers | Blue dim |
| Underline carets `^^^^` | Red |
| Type names | Green |
| Help text | Cyan |

When stdout is not a TTY (piped), emit plain text.

**Verify:** `./tart check bad.el` shows colors; `./tart check bad.el | cat` shows plain text

### R12: Syntax highlighting in excerpts

**Given** source code excerpts from `.el` files
**When** rendering with colors enabled
**Then** apply minimal Lisp syntax highlighting

| Element | Color |
|---------|-------|
| Parentheses | Default |
| Keywords (`defun`, `let`, `if`, etc.) | Magenta |
| Strings | Green |
| Comments | Gray/dim |
| Numbers | Cyan |
| Symbols after quote | Yellow |

Keep highlighting minimal—goal is readability, not full IDE colors.

**Verify:** Manual review; keywords and strings visually distinct

## Non-Requirements

- Caching source files across multiple errors (premature optimization)
- Showing more than 3 context lines around error (keep focused)

## Tasks

- [x] [R1] Implement `Source_excerpt.get_lines`
- [x] [R2] Implement underline rendering with caret alignment
- [x] [R3] Implement line number gutter formatting
- [x] [R4] Add Elm-style header formatting
- [x] [R5] Add conversational prose per constraint context
- [x] [R6] Track and display .tart signature provenance
- [x] [R7] Render excerpts for related locations
- [x] [R8] Add fallback for unreadable sources
- [x] [R9] Format help suggestions with context
- [x] [R10] Ensure compact and JSON formats unchanged
- [x] [R11] Implement `Ansi` module with color codes and TTY detection
- [x] [R11] Add colored output to diagnostic formatting
- [x] [R12] Add minimal Lisp syntax highlighting for excerpts
- [x] Write tests for excerpt rendering

**Status: Complete.** All requirements implemented in `source_excerpt.ml(i)`,
`ansi.ml(i)`, and `diagnostic.ml`. 12 unit tests in `source_excerpt_test.ml`.
