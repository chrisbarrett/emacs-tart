# Spec 58: Signature Syntax Table

Correct syntax table for `tart-signature-mode` so that delimiter-aware
commands (`check-parens`, `forward-sexp`, `show-paren-mode`,
`electric-pair-mode`) work with `.tart` file syntax.

- **Dependencies:**
  - [Spec 10](./10-emacs-integration.md) tart-mode.el exists

## Goal

Give `tart-signature-mode` its own syntax table with correct character
classes for tart's type syntax.  The existing
`inferior-tart-mode-syntax-table` already has the right definitions; the
signature mode should follow the same pattern.

## Context

`tart-signature-mode` derives from `lisp-mode`, which treats `|` as a
string-fence character (Common Lisp's `|symbol|` escape syntax).  In tart,
`|` is the union type operator (`(string | int)`).  This causes
`check-parens` to report false "unmatched bracket or quote" errors in 60 of
90 `.tart` files — any file with an odd number of `|` characters.

Additionally, `lisp-mode` treats `[]` as symbol constituents rather than
paired delimiters, so `forward-sexp` and `show-paren-mode` do not recognise
type quantifier brackets `[a b]`.

## Requirements

### R1: Pipe as symbol constituent

**Given** a `.tart` buffer with union types like `(string | int)`
**When** `check-parens` is invoked
**Then** no false errors are reported for `|` characters

**Verify:** Open a `.tart` file with an odd number of `|` operators; `M-x
check-parens` reports no errors (or reports only genuine mismatches)

### R2: Square brackets as paired delimiters

**Given** a `.tart` buffer with type quantifiers like `[a b]`
**When** point is on `[` and `forward-sexp` is invoked
**Then** point moves to after the matching `]`
**And** `show-paren-mode` highlights the matching bracket

**Verify:** Place point on `[` in `(defun identity [a] (a) -> a)`;
`C-M-f` moves to after `]`

### R3: Curly braces as paired delimiters

**Given** a `.tart` buffer with row types like `{name string age int}`
**When** point is on `{` and `forward-sexp` is invoked
**Then** point moves to after the matching `}`

**Verify:** Place point on `{`; `C-M-f` moves to after `}`

### R4: Standard Lisp syntax preserved

**Given** the new syntax table
**Then** parentheses `()`, semicolon comments, double-quote strings,
backslash escapes, and symbol-constituent characters (`-`, `_`, `*`, `+`,
`/`, `<`, `>`, `=`, `?`, `!`) retain their standard Lisp syntax classes

**Verify:** `check-parens` still catches genuinely unbalanced `()` in
`.tart` files; `;` still starts a comment

### R5: Ampersand and colon as symbol constituents

**Given** a `.tart` buffer with `&optional`, `&rest`, `&key`, or bounded
quantifiers `[(a : truthy)]`
**Then** `&` and `:` are symbol constituents so that `forward-sexp` treats
`&optional` and `:default` as single symbols

**Verify:** Place point before `&optional`; `C-M-f` moves past the whole
word

## Tasks

- [ ] [R1–R5] Define `tart-signature-mode-syntax-table` (modelled on
  `inferior-tart-mode-syntax-table`)
- [ ] [R1–R5] Set `:syntax-table` in `tart-signature-mode` definition
- [ ] [R1–R4] Manual smoke test: `check-parens` on prelude and buffer.tart
