# Spec 38: AST Printer

Print `Sexp.t` to valid Emacs Lisp for round-trip testing.

- **Deps:** None

## Constraints

| Constraint | Detail |
|------------|--------|
| Semantic equivalence | Output re-parses to same AST (ignoring whitespace) |
| Emacs readable | Valid Elisp accepted by Emacs reader |
| Escape sequences | Correctly escape strings/characters |

## Output

```
lib/syntax/
├── print.ml
└── print.mli
```

## Requirements

### R1: Integer literals

**Given** `Sexp.Int` **When** printed **Then** valid Elisp integer

```ocaml
Int (42, _)   => "42"
Int (-17, _)  => "-17"
```

**Verify:** `(read "42")` in Emacs equals `42`

### R2: Float literals

**Given** `Sexp.Float` **When** printed **Then** valid Elisp float

```ocaml
Float (3.14, _) => "3.14"
Float (1e10, _) => "1e+10" or "10000000000.0"
```

**Verify:** `(read "3.14")` in Emacs equals `3.14`

### R3: Simple strings

**Given** `Sexp.String` with ASCII **When** printed **Then** double-quoted Elisp string

```ocaml
String ("hello", _) => "\"hello\""
```

**Verify:** `(read "\"hello\"")` in Emacs equals `"hello"`

### R4: String escapes

**Given** `Sexp.String` with special chars **When** printed **Then** Elisp escapes

| Char | Escape |
|------|--------|
| newline | `\n` |
| tab | `\t` |
| CR | `\r` |
| backslash | `\\` |
| quote | `\"` |
| bell | `\a` |
| backspace | `\b` |
| form feed | `\f` |
| escape | `\e` |

**Verify:** `(read "\"line1\\nline2\"")` equals `"line1\nline2"`

### R5: Non-ASCII strings

**Given** `Sexp.String` with non-ASCII **When** printed **Then** UTF-8 or `\xNN`/`\uNNNN`

**Verify:** `(read "\"...\")` in Emacs produces equivalent string

### R6: Symbols

**Given** `Sexp.Symbol` **When** printed **Then** valid Elisp symbol

```ocaml
Symbol ("foo-bar", _) => "foo-bar"
Symbol ("+", _)       => "+"
```

**Verify:** `(read "foo-bar")` equals `'foo-bar`

### R7: Keywords

**Given** `Sexp.Keyword` **When** printed **Then** leading colon

```ocaml
Keyword ("key", _) => ":key"
```

**Verify:** `(read ":key")` equals `:key`

### R8: Simple character literals

**Given** `Sexp.Char` with printable ASCII **When** printed **Then** `?c` syntax

```ocaml
Char (97, _)  => "?a"
Char (32, _)  => "?\\s" or "? "
```

**Verify:** `(read "?a")` equals `?a`

### R9: Escaped character literals

**Given** `Sexp.Char` requiring escape **When** printed **Then** escape sequence

| Char | Escape |
|------|--------|
| newline | `?\n` |
| tab | `?\t` |
| backslash | `?\\` |
| non-printable | `?\xNN` |

```ocaml
Char (10, _) => "?\\n"
Char (92, _) => "?\\\\"
```

**Verify:** `(read "?\\n")` equals `?\n`

### R10: Character modifiers

**Given** `Sexp.Char` with modifier bits **When** printed **Then** modifier syntax

| Modifier | Bit | Syntax |
|----------|-----|--------|
| Control | 0x1F mask | `?\C-x` |
| Meta | 0x8000000 | `?\M-x` |
| Shift | 0x2000000 | `?\S-x` |
| Hyper | 0x1000000 | `?\H-x` |
| Alt | 0x400000 | `?\A-x` |
| Super | 0x800000 | `?\s-x` |

```ocaml
Char (0x8000000 lor 120, _) => "?\\M-x"
Char (24, _)                => "?\\C-x"
```

**Verify:** `(read "?\\M-x")` equals `?\M-x`

### R11: Lists

**Given** `Sexp.List` **When** printed **Then** parenthesized, space-separated

```ocaml
List ([Symbol ("a", _); Int (1, _)], _) => "(a 1)"
```

**Verify:** `(read "(a 1)")` equals `'(a 1)`

### R12: Quote forms as reader macros

**Given** `Sexp.List` with quote/backquote/unquote/function **When** printed **Then** sugared syntax

| Form | Output |
|------|--------|
| `(quote x)` | `'x` |
| `(backquote ...)` | `` `... `` |
| `(unquote x)` | `,x` |
| `(unquote-splicing xs)` | `,@xs` |
| `(function f)` | `#'f` |

**Verify:** `(read "'x")` equals `(quote x)`

### R13: Vectors

**Given** `Sexp.Vector` **When** printed **Then** `#(...)` syntax

```ocaml
Vector ([Int (1, _); Int (2, _); Int (3, _)], _) => "#(1 2 3)"
```

**Verify:** `(read "#(1 2 3)")` equals `[1 2 3]`

### R14: Dotted pairs

**Given** `Sexp.Cons` **When** printed **Then** dotted pair syntax

```ocaml
Cons (Symbol ("a", _), Symbol ("b", _), _) => "(a . b)"
```

**Verify:** `(read "(a . b)")` equals `'(a . b)`

### R15: Improper lists

**Given** `Sexp.Cons` chain ending in non-nil **When** printed **Then** dotted tail

```ocaml
Cons (Int (1, _), Cons (Int (2, _), Int (3, _), _), _) => "(1 2 . 3)"
```

**Verify:** `(read "(1 2 . 3)")` equals `'(1 2 . 3)`

### R16: Error nodes

**Given** `Sexp.Error` **When** printed **Then** error indicator (not valid Elisp)

```ocaml
Error ("parse error", _) => "#<error: parse error>"
```

**Verify:** `dune build`; error nodes print without crash

### R17: Module interface

```ocaml
val to_string : Sexp.t -> string
```

**Verify:** `dune build`

### R18: Round-trip equivalence

**Given** valid Elisp **When** parsed and printed **Then** re-parsing = original AST

```
parse(source) = ast1
print(ast1) = printed
parse(printed) = ast2
ast1 = ast2  (structurally, ignoring spans)
```

**Verify:** `dune test`

## Non-Requirements

- Preserving whitespace/comments
- Pretty-printing with indentation
- Preserving numeric base (hex vs decimal)

## Tasks

- [x] [R17] Create `print.ml`/`print.mli`
- [x] [R1-2] Integer/float printing
- [x] [R3-5] String printing with escapes
- [x] [R6-7] Symbol/keyword printing
- [x] [R8-10] Character literals with modifiers
- [x] [R11-12] List printing with reader macro sugar
- [x] [R13] Vector printing
- [x] [R14-15] Cons/dotted pair printing
- [x] [R16] Error node printing
- [x] [R18] Round-trip tests with Emacs validation

## Status

Complete
