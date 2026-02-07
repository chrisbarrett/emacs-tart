# Spec 57: Clause Diagnostics

Emit user-authored diagnostics from multi-clause signature matches.

**Dependencies:** [Spec 54] (multi-clause signatures), [Spec 51] (diagnostic severity)

## Links

### Deps
[Spec 51]: ./51-diagnostic-severity.md
[Spec 54]: ./54-multi-clause-signatures.md

### Related
[Spec 55]: ./55-plist-intrinsic.md
[Spec 56]: ./56-plist-type-overloading.md

## Goal

Let `.tart` signature authors attach diagnostics (errors, warnings, notes) to
function signature clauses, emitted when a matching clause is selected at a call
site.

## Rationale

Multi-clause signatures ([Spec 54]) pattern-match on argument types at call sites.
Some type patterns represent "works, but risky" usage that should surface a
message without rejecting the code. Currently the type-checker can only accept
silently or reject—clause diagnostics add graduated feedback.

This moves domain-specific type rules into `.tart` files, reducing the need for
compiler intrinsics. Library authors can express nuanced type behavior in
userland.

**Motivating example:** `plist-member` traverses a list looking for a key. It
works on malformed plists if the key appears before alternation breaks. Existing
code depends on this. Rather than a compiler intrinsic, the behavior is expressed
as:

```lisp
(defun plist-member [k v]
  (((plist k v) k) -> (plist k v))
  (((list (k | v)) k) -> (list (k | v))
    (warn "plist-member called on bare list; (plist _ _) provides alternation guarantees")))
```

First clause: well-typed plist, no diagnostic. Second clause: bare list, accepted
with a warning.

## Syntax

A diagnostic annotation follows the return type in a clause:

```
((params) -> return-type
  (SEVERITY fmt-string tvar ...))
```

Where:
- `SEVERITY` is one of `error`, `warn`, `note`
- `fmt-string` is a string literal with `%s` placeholders
- `tvar ...` are zero or more type variable names bound in the enclosing defun
  scope, substituted for `%s` in order

```lisp
;; No format args
(warn "prefer plist type for type-safe property access")

;; With format args referencing defun type variables
(warn "expected (plist _ _), got (list (%s | %s))" k v)
```

Clauses are ordered top-to-bottom ([Spec 54]). The first matching clause
determines both the return type and any diagnostic.

### Single-clause diagnostics

Diagnostic annotations also work on single-clause defuns:

```lisp
(defun old-fn (any) -> nil
  (warn "old-fn is deprecated; use new-fn"))
```

## Constraints

| Constraint         | Detail                                              |
|:-------------------|:----------------------------------------------------|
| Clause annotation  | Diagnostic is per-clause, not per-defun             |
| Call-site location | Diagnostic points to the call expression in `.el`   |
| Non-blocking       | `warn` and `note` do not affect return type         |
| Error recovery     | `error` emits diagnostic; return type still inferred|
| Severity mapping   | Maps to [Spec 51]: error→Error, warn→Warning, note→Info |
| Userland rules     | No compiler intrinsic needed for diagnostic logic   |

## Output

```
lib/sig/
├── sig_ast.mli      ; Add clause_diagnostic to defun_clause
└── sig_parser.ml    ; Parse (warn ...), (note ...), (error ...) in clauses
lib/sig/
└── sig_loader.ml    ; Carry diagnostic through to loaded signatures
lib/typing/
└── infer.ml         ; Emit diagnostic when clause matches at call site
```

## Requirements

### R1: Clause diagnostic AST

**Given** `sig_ast.mli`
**When** defining `defun_clause`
**Then** add an optional diagnostic annotation:

```ocaml
and clause_diagnostic = {
  diag_severity : diagnostic_severity;
  diag_message : string;       (* Format string *)
  diag_args : string list;     (* Type variable names for %s substitution *)
  diag_loc : span;
}

and diagnostic_severity = DiagError | DiagWarn | DiagNote

and defun_clause = {
  clause_params : sig_param list;
  clause_return : sig_type;
  clause_diagnostic : clause_diagnostic option;  (* NEW *)
  clause_loc : span;
}
```

**Verify:** `dune build`

### R2: Parse diagnostic forms

**Given** a clause body containing `(warn "msg" ...)`, `(note "msg" ...)`, or
`(error "msg" ...)`
**When** parsing a multi-clause or single-clause defun
**Then** parse the diagnostic form and attach it to the clause

Parsing rules:
- After `-> return-type`, check for a trailing list form starting with `warn`,
  `note`, or `error`
- Second element must be a string literal
- Remaining elements must be symbols (type variable names)
- At most one diagnostic per clause

**Verify:** `dune test`; diagnostic forms parse correctly

### R3: Format string validation

**Given** a diagnostic form `(warn "got %s where %s expected" a b)`
**When** parsing
**Then** validate:
- Number of `%s` placeholders matches number of type variable arguments
- Each argument name is bound as a type variable in the enclosing defun scope

**Verify:** Mismatched `%s` count produces parse error; unbound names produce
load error

### R4: Diagnostic emission at call sites

**Given** a function with diagnostic-annotated clauses
**When** multi-clause matching selects a clause with a diagnostic at a call site
**Then** emit the diagnostic:
- Severity from the annotation
- Message with `%s` replaced by the inferred types of referenced type variables
  (via `Types.to_string`)
- Location is the call expression span in the `.el` file being checked

**Given** `(warn "got %s" k)` where `k` unifies with `keyword` at the call site
**Then** emitted message is `"got keyword"`

**Verify:** `dune test`; diagnostics appear in type-checker output at correct
locations

### R5: Severity behavior

**Given** a clause with `(warn ...)` or `(note ...)`
**When** the clause matches
**Then** the diagnostic is emitted AND the clause's return type is used for
inference normally (non-blocking)

**Given** a clause with `(error ...)`
**When** the clause matches
**Then** the diagnostic is emitted as an error AND the clause's return type is
still used for error recovery (type inference continues)

**Verify:** `warn`/`note` do not affect return type; `error` produces a type
error while still yielding a return type

### R6: Interaction with [Spec 51] severity flags

**Given** diagnostics emitted from clause annotations
**When** formatted for output (CLI or LSP)
**Then** map to [Spec 51] severity levels:
- `error` → `Severity.Error`
- `warn` → `Severity.Warning`
- `note` → `Severity.Info`

**Given** CLI flags from [Spec 51] (`--warn-as-error`, `--ignore-warnings`)
**Then** clause diagnostics respect these flags identically to other diagnostics

**Verify:** `./tart check --ignore-warnings` suppresses clause warnings

### R7: Test fixtures

Add fixtures under `test/fixtures/typing/` with `.el` / `.expected` pairs:

- Single clause with `warn` (deprecated function pattern)
- Multi-clause with `warn` on fallback clause
- `error` clause that still infers return type
- Format string with type variable substitution
- Clause without diagnostic followed by clause with diagnostic

**Verify:** `dune test`; all fixtures pass

## Non-Requirements

- Multiple diagnostics per clause (use separate clauses for distinct messages)
- Diagnostic-only clauses with no return type (use `never` + `error`)
- Format specifiers beyond `%s` for type variable stringification
- User-defined error codes on clause diagnostics
- Diagnostics on `type`, `defvar`, or other non-`defun` declarations
- Conditional emission (if the clause matches, the diagnostic always fires)
- Diagnostics on virtual clauses generated by row-typed dispatch ([Spec 56])

## Design Notes

### Clause Matching Is Unaffected

Diagnostics do not alter clause matching order, success, or failure. A clause
matches based on its parameter types ([Spec 54]). If it matches AND has a
diagnostic, both the return type and the diagnostic are produced. The diagnostic
is a side-effect of clause selection, not a filter on it.

### Error Clauses vs Type Mismatches

An `(error ...)` clause is semantically different from a type mismatch:
- **Type mismatch:** no clause matches, generic unification error
- **`(error ...)`:** a clause DOES match, and the match itself is the problem

This distinction matters for error messages. A clause diagnostic explains the
domain-specific reason the usage is problematic. A type mismatch is structural.

### Deprecated Functions

Clause diagnostics naturally express function deprecation:

```lisp
(defun old-name (any) -> any
  (warn "old-name is deprecated since Emacs 29; use new-name"))
```

This uses the same mechanism and integrates with existing severity controls
(`--ignore-warnings`, `--warn-as-error`).

### plist-member Without Intrinsics

With [Spec 55] (plist intrinsic) and this spec, `plist-member` needs no special
compiler support:

```lisp
(defun plist-member [k v]
  ;; Well-typed plist: structural traversal is safe
  (((plist k v) k) -> (plist k v))
  ;; Bare list: works if key found before alternation breaks.
  ;; Warn since the caller may not realize the risk.
  (((list (k | v)) k) -> (list (k | v))
    (warn "plist-member on bare list; (plist _ _) enforces alternation")))
```

The type-checker matches against call-site argument types ([Spec 54]/[Spec 56]), selects
the appropriate clause, and emits the warning if the degraded path is taken. No
`infer_plist_member` intrinsic needed.

### Virtual Clause Interaction

Virtual clauses generated during row-typed dispatch ([Spec 56]) do not carry
diagnostics. Diagnostics are authored explicitly in `.tart` files and attached
only to clauses written by the signature author.

### Format String Simplicity

Format strings use `%s` exclusively. Substituted values are type representations
(`Types.to_string`). No type-level printf or complex formatting. If the message
needs to reference a compound type, describe it in the string literal:

```lisp
;; Reference constituent type variables, describe structure in prose
(warn "expected (plist %s %s), got bare list" k v)
```

### AST Representation

The diagnostic is an optional field on the existing `defun_clause` record. This
avoids restructuring the AST—clauses without diagnostics have `None`, clauses
with diagnostics have `Some { ... }`. The parser looks for a trailing form after
the return type.

### Loaded Representation

During signature loading (`sig_loader.ml`), the diagnostic annotation is
preserved through to the function's type environment entry. When `infer.ml`
performs clause matching, the selected clause's diagnostic (if any) is emitted
with the call-site span and the resolved type variable bindings.

### Subsumption of Existing Special Cases

The eq/eql disjointness check (`infer.ml:257-259, 1501-1571`) hard-codes
pattern matching on `(eq arg1 arg2)` calls and emits E0008 when the two argument
types are provably disjoint. The identity-safety aspect of this check could move
to `.tart`:

```lisp
(defun eq
  ((eq-safe eq-safe) -> bool)
  ((_ _) -> bool
    (warn "eq compares by identity; use equal for structural comparison")))
```

**Limitation:** The current check is more precise—it detects *disjointness*
between the two specific argument types (e.g., `(eq "hello" 42)` where string
and int can never be `eq`). That cross-argument relationship analysis requires
inspecting the resolved types of both parameters together, which clause patterns
cannot express—each parameter is matched independently. Clause diagnostics give
"non-identity-safe type" warnings, not "provably disjoint types" errors.

The eq identity-safety lint is still a net improvement over no check at all, and
the disjointness check could remain as a small compiler-side refinement if
needed. Alternatively, a future extension allowing clause guards that reference
multiple type variables could subsume the disjointness case entirely.

The row-typed field access functions (`alist-get`, `plist-get`, `gethash`,
`map-elt`; ~400 lines at `infer.ml:227-256, 1584-1967`) are already targeted by
[Spec 56] for migration to multi-clause `.tart` signatures. Clause diagnostics
complement this by enabling warnings for degraded paths—e.g., accessing a plist
key on a bare `(list _)` argument.

Other existing special cases (exhaustiveness E0400, missing signatures E0104,
kind checking E0300-E0302, undefined variables E0100) operate at the module or
declaration level, not at call sites. These are not candidates for clause
diagnostics.

## Tasks

- [x] [R1] Add `clause_diagnostic` and `diagnostic_severity` to `sig_ast.mli`
- [x] [R1] Update `defun_clause` with optional diagnostic field
- [x] [R2] Parse `(warn ...)`, `(note ...)`, `(error ...)` forms in
      `sig_parser.ml`
- [x] [R3] Validate `%s` count matches argument count
- [x] [R3] Validate arguments are bound type variables
- [x] [R4] Emit diagnostics during clause matching in `infer.ml`
- [x] [R4] Substitute type variable bindings into format strings
- [x] [R5] Wire severity: warn/note non-blocking, error blocks
- [x] [R6] Map to [Spec 51] severity; verify CLI flag interaction
- [x] [R7] Add test fixtures for clause diagnostics
- [ ] Write `plist-member` signature using clause diagnostics

**Status:** Complete. `plist-member` deferred: plist→list subsumption means
the bare-list clause is unreachable (the plist clause always matches via
widening).
