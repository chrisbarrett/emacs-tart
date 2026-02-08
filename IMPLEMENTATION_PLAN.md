# Specs 71, 72, 73 — Outstanding Work

> Completes all deferred items from [Spec 71](./71-lsp-server.md),
> [Spec 72](./72-cli.md), and [Spec 73](./specs/73-diagnostics.md). Nothing
> deferred.

## Audit Summary

All previous IMPLEMENTATION_PLAN tasks (1–16) are done. The LSP server has full
lifecycle, document sync, diagnostics, hover, definition, references, code
actions, completion, signature help, document symbols, rename, folding, semantic
tokens, inlay hints, type definition, workspace symbols, prepare rename, file
watching, workspace configuration, save events, concurrency, cancellation,
debounce, and progress reporting.

What remains: the deferred sections of all three specs.

### Spec 71 — LSP Server (11 deferred items)

| Item                                         | Current state                                                       |
| -------------------------------------------- | ------------------------------------------------------------------- |
| Workspace-wide find-references               | `find_references` walks current doc AST only                        |
| Cross-file goto-definition for `.el` targets | Resolves to `.tart` only, not other `.el` files                     |
| Workspace-wide rename                        | `handle_rename` uses single-doc `find_references`                   |
| Scope-aware completion                       | `completion.ml` prefix-filters all bindings, ignoring lexical scope |
| Semantic tokens delta                        | `semanticTokens/full` only, no `/delta`                             |
| Call hierarchy                               | Not implemented                                                     |
| Type hierarchy                               | Not implemented                                                     |
| Code lens                                    | Not implemented                                                     |
| Linked editing ranges                        | Not implemented                                                     |
| On-type formatting                           | Not implemented                                                     |
| Pull-based diagnostics                       | Not implemented                                                     |

### Spec 72 — CLI (1 deferred item)

| Item                                 | Current state                                                                     |
| ------------------------------------ | --------------------------------------------------------------------------------- |
| Debug-level unification tracing (R9) | Log infrastructure ready; zero `Log.debug` calls in `unify.ml` or `constraint.ml` |

### Spec 73 — Diagnostics (3 deferred items)

| Item                            | Current state                                                                                 |
| ------------------------------- | --------------------------------------------------------------------------------------------- |
| Reserved error codes (19 codes) | Variants missing from `diagnostic.ml` `error_code` type                                       |
| Info severity                   | Not in `Diagnostic.severity`; present as `Information` in `Protocol.diagnostic_severity` only |
| Clause diagnostics in LSP       | Clause diagnostics exist in `sig_ast.ml` but are not surfaced via `publishDiagnostics`        |

---

## Task 1 — Debug-Level Unification Tracing

**Spec:** [72](./specs/72-cli.md), R9.

**Status:** Logging infrastructure complete. No call sites in the hot loop.

**Changes:**

| File                       | Change                                           |
| -------------------------- | ------------------------------------------------ |
| `lib/typing/unify.ml`      | Add `Log.debug` calls at key decision points     |
| `lib/typing/constraint.ml` | Add `Log.debug` in constraint creation if useful |

**Instrumentation points in `unify.ml`:**

1. **`solve` (line ~702)** — log constraint count on entry.
2. **`unify` (line ~224)** — log both types on entry: `"Unify: %s ~ %s"`.
3. **Type variable linking (line ~250)** — log `"Link: _%d = %s"` when
   `tv := Link ty`.
4. **Occurs check (line ~63)** — log on failure only (avoids noise in the common
   path).
5. **`try_unify` (line ~766)** — log speculative attempt start; log
   success/rollback.
6. **`unify_param_lists` (line ~570)** — log rest-param handling and arity
   decisions.
7. **`unify_rows` (line ~491)** — log common fields and leftover fields.

All calls use `Log.debug` with `Printf.ifprintf` gating (zero-cost when
`--log-level` is not `debug`). Use `Core.Types.to_string` for type formatting
and `Constraint.to_string` for constraint formatting.

**Test:** Run `./specs/tart check --log-level=debug test/fixtures/typing/*.el`
and verify structured output appears on stderr.

---

## Task 2 — Reserved Error Codes

**Spec:** [73](./specs/73-diagnostics.md).

**Status:** 19 error codes specified but absent from `diagnostic.ml`.

**Changes:**

| File                        | Change                                                      |
| --------------------------- | ----------------------------------------------------------- |
| `lib/typing/diagnostic.ml`  | Add 19 variant constructors and `error_code_to_string` arms |
| `lib/typing/diagnostic.mli` | Expose new constructors                                     |

**Codes to add:**

| Category | Code  | Variant              | Description                            |
| -------- | ----- | -------------------- | -------------------------------------- |
| Name     | E0103 | `UndefinedField`     | Field not present in record type       |
| Name     | E0105 | `AmbiguousName`      | Name resolves to multiple definitions  |
| Arity    | E0202 | `MissingRequired`    | Required argument not provided         |
| Arity    | E0203 | `UnknownKeyword`     | Unknown keyword argument               |
| Pattern  | E0401 | `RedundantPattern`   | Pattern can never match                |
| Pattern  | E0402 | `InvalidPattern`     | Pattern syntax not supported           |
| Row      | E0500 | `MissingField`       | Required field not present             |
| Row      | E0501 | `DuplicateField`     | Field specified multiple times         |
| Row      | E0502 | `RowMismatch`        | Row types cannot be unified            |
| Row      | E0503 | `ClosedRowExtra`     | Extra field in closed row type         |
| Union    | E0600 | `UnionMismatch`      | Value does not match any union variant |
| Union    | E0601 | `EmptyUnion`         | Type subtraction produced empty type   |
| Union    | E0602 | `AmbiguousVariant`   | Cannot determine which union variant   |
| Module   | E0700 | `MissingModule`      | Required module not found              |
| Module   | E0701 | `CircularDependency` | Modules have circular require          |
| File     | E0800 | `FileNotFound`       | Source file does not exist             |
| File     | E0801 | `FileUnreadable`     | Source file cannot be read             |
| File     | E0802 | `ParseError`         | Syntax error in source file            |
| Version  | E0902 | `VersionParseFailed` | Package-Requires parse error           |

No constructor functions or emission sites are needed yet — only the variant
definitions and code-string mapping. Features that emit these codes will add
constructor helpers as they are built.

**Test:** Build succeeds; existing tests pass unchanged. Exhaustive `match` on
`error_code` in `error_code_to_string` confirms no missing arm.

---

## Task 3 — Info Severity

**Spec:** [73](./specs/73-diagnostics.md).

**Status:** `Diagnostic.severity` has `Error | Warning | Hint` but no `Info`.
The LSP protocol layer already has `Information` (value 3) in
`Protocol.diagnostic_severity`.

**Changes:**

| File                        | Change                                                                 |
| --------------------------- | ---------------------------------------------------------------------- |
| `lib/typing/diagnostic.ml`  | Add `Info` to `severity` type                                          |
| `lib/typing/diagnostic.mli` | Expose `Info`                                                          |
| `lib/lsp/server.ml`         | Map `Diagnostic.Info` to `Protocol.Information` in `diagnostic_to_lsp` |

**Test:** Build succeeds; no features emit `Info` yet, so existing tests are
unaffected. Add a unit test asserting `Info` maps to LSP severity 3.

---

## Task 4 — Clause Diagnostics in LSP

**Spec:** [73](./specs/73-diagnostics.md).

**Status:** `sig_ast.ml` defines `clause_diagnostic` with
`DiagError | DiagWarn | DiagNote` severity. `clause_dispatch.ml` resolves clause
diagnostics at call sites. These are collected during type inference but not
surfaced as LSP diagnostics.

**Changes:**

| File                        | Change                                                                        |
| --------------------------- | ----------------------------------------------------------------------------- |
| `lib/typing/diagnostic.ml`  | Add `of_clause_diagnostic` to convert `clause_diagnostic` → `Diagnostic.t`    |
| `lib/typing/diagnostic.mli` | Expose converter                                                              |
| `lib/lsp/server.ml`         | Include clause diagnostics in `publishDiagnostics` alongside type diagnostics |

**Severity mapping:**

| Clause severity | Diagnostic severity |
| --------------- | ------------------- |
| `DiagError`     | `Error`             |
| `DiagWarn`      | `Warning`           |
| `DiagNote`      | `Hint`              |

Clause diagnostics carry a message and a span from the call site. They do not
carry an error code (they are user-authored advisory messages from `.tart`
signatures, not compiler errors).

**Test:** Create a `.tart` fixture with a clause diagnostic annotation and a
matching `.el` file that triggers it. Assert the diagnostic appears in LSP
output with correct severity.

---

## Task 5 — Workspace-Wide Find References

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** `find_references` in `server.ml` walks only the current document's
AST. `Document.list_uris` can iterate open documents.

**Changes:**

| File                | Change                                                  |
| ------------------- | ------------------------------------------------------- |
| `lib/lsp/server.ml` | Extend `handle_references` to search all open documents |

**Approach:**

1. Resolve the symbol name at the cursor position (existing logic).
2. Iterate `Document.list_uris` to get all open documents.
3. For each open document, parse and call `find_references name sexps`.
4. Convert spans from each document to LSP locations with the correct URI.
5. Concatenate results, current document first.

**Scope:** Open documents only. Disk files not opened in the editor are not
searched — this matches Eglot's expectations and avoids unbounded I/O.

**Test:** Integration test: open two `.el` files that both use a symbol, request
references from one, assert results include locations from both.

---

## Task 6 — Cross-File Goto Definition for `.el` Targets

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** `handle_definition` resolves to `.tart` signatures via
`find_definition_in_signatures` but does not search other `.el` files.

**Changes:**

| File                | Change                                           |
| ------------------- | ------------------------------------------------ |
| `lib/lsp/server.ml` | Add `.el` file search as a third resolution step |

**Resolution chain (updated):**

1. Local definitions in the current document (unchanged).
2. Signature lookup in `.tart` files (unchanged).
3. **New:** Search all other open `.el` documents for a matching `defun`,
   `defvar`, or `defconst` with the target name.

**Scope:** Open `.el` documents only. Same boundary as workspace-wide
references.

**Test:** Integration test: open `a.el` defining `(defun my-fn ...)` and `b.el`
calling `(my-fn)`. Request definition from `b.el`, assert it resolves to the
defun in `a.el`.

---

## Task 7 — Workspace-Wide Rename

**Spec:** [71](./specs/71-lsp-server.md).

**Depends on:** Task 5 (workspace-wide references).

**Status:** `handle_rename` uses `find_references` for the current document
only.

**Changes:**

| File                | Change                                                                      |
| ------------------- | --------------------------------------------------------------------------- |
| `lib/lsp/server.ml` | Extend `handle_rename` to produce workspace edits across all open documents |

**Approach:**

1. Use the workspace-wide `find_references` from task 5.
2. Group results by URI.
3. Build a `workspace_edit` with `text_document_edit` entries for each affected
   document.

**Test:** Integration test: open two files sharing a symbol, rename from one,
assert edits span both documents.

---

## Task 8 — Scope-Aware Completion

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** `completion.ml` collects all bindings from local definitions,
signatures, and the type environment, filtering only by prefix. No lexical scope
analysis.

**Changes:**

| File                    | Change                                                     |
| ----------------------- | ---------------------------------------------------------- |
| `lib/lsp/completion.ml` | Filter candidates by lexical visibility at cursor position |

**Approach:**

1. After type-checking, walk the AST to determine which `let`/`let*` binding
   scope the cursor falls within.
2. Collect only the bindings visible at that scope level: function parameters,
   enclosing `let` bindings, top-level definitions, and signature imports.
3. Exclude bindings from sibling or later `let` clauses that are not yet in
   scope at the cursor position.

**Scope analysis:** Walk the parsed sexps depth-first. For each
`let`/`let*`/`lambda`/`defun` form, check whether the cursor position falls
within its body. If so, its bindings are in scope. The key function is a
position-containment check against `Syntax.Location.span`.

**Test:** Fixture where a `let` binding defines `x` inside a function. At a
position before the `let`, `x` should not appear in completions. Inside the
`let` body, it should.

---

## Task 9 — Semantic Tokens Delta

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** `semanticTokens/full` is implemented in `semantic_tokens.ml`. No
delta support.

**Changes:**

| File                          | Change                                                                                           |
| ----------------------------- | ------------------------------------------------------------------------------------------------ |
| `lib/lsp/semantic_tokens.ml`  | Cache previous token array per URI; compute delta on request                                     |
| `lib/lsp/semantic_tokens.mli` | Expose delta types                                                                               |
| `lib/lsp/protocol.ml`         | Add `semanticTokensDelta` result type and JSON encoding                                          |
| `lib/lsp/protocol.mli`        | Expose delta types                                                                               |
| `lib/lsp/server.ml`           | Route `textDocument/semanticTokens/full/delta`, update capability to `{ full: { delta: true } }` |

**Protocol:** `semanticTokens/full/delta` returns a `resultId` and an array of
`SemanticTokensEdit` (start, deleteCount, data). The server stores the previous
token array keyed by URI + `resultId`. On the next request, diff against the new
array and return edits. Fall back to a full response if no previous result is
cached.

**Test:** Integration test: open a file, request full tokens (get `resultId`),
edit the file, request delta with previous `resultId`, assert delta edits are
non-empty and correct.

---

## Task 10 — Call Hierarchy

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Not implemented. Requires a workspace-wide call graph.

**Changes:**

| File                         | Change                                                                                                             |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `lib/lsp/call_hierarchy.ml`  | **New.** Build call graph from open documents and signatures                                                       |
| `lib/lsp/call_hierarchy.mli` | **New.** Interface                                                                                                 |
| `lib/lsp/protocol.ml`        | Add `CallHierarchyItem`, `CallHierarchyIncomingCall`, `CallHierarchyOutgoingCall` types                            |
| `lib/lsp/protocol.mli`       | Expose call hierarchy types                                                                                        |
| `lib/lsp/server.ml`          | Add `callHierarchyPrepare`, `incomingCalls`, `outgoingCalls` handlers; register `callHierarchyProvider` capability |

**Architecture:**

1. `callHierarchy/prepare` returns a `CallHierarchyItem` for the function at the
   cursor (name, kind, URI, range, selection range).
2. `callHierarchy/incomingCalls` iterates all open documents, finds call sites
   of the target function, and returns caller items with call site ranges.
3. `callHierarchy/outgoingCalls` walks the body of the target function and
   returns callee items with call site ranges.

**Scope:** Open documents only for callers. Signature-defined functions appear
as callees even without open `.el` files.

**Test:** Integration test: `a.el` defines `(defun caller () (callee))`, `b.el`
defines `(defun callee ())`. Assert incoming calls on `callee` returns `caller`;
outgoing calls on `caller` returns `callee`.

---

## Task 11 — Type Hierarchy

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Not implemented. Requires subtype relationship tracking.

**Changes:**

| File                         | Change                                                                              |
| ---------------------------- | ----------------------------------------------------------------------------------- |
| `lib/lsp/type_hierarchy.ml`  | **New.** Extract subtype/supertype edges from type definitions                      |
| `lib/lsp/type_hierarchy.mli` | **New.** Interface                                                                  |
| `lib/lsp/protocol.ml`        | Add `TypeHierarchyItem` types                                                       |
| `lib/lsp/protocol.mli`       | Expose type hierarchy types                                                         |
| `lib/lsp/server.ml`          | Add `typeHierarchy/prepare`, `supertypes`, `subtypes` handlers; register capability |

**Architecture:**

Subtype relationships in tart come from union types and the numeric tower
(`Int <: Num`, `Float <: Num`). The type hierarchy provider:

1. `typeHierarchy/prepare` returns a `TypeHierarchyItem` for the type at the
   cursor.
2. `typeHierarchy/supertypes` walks the type definition to find parent types
   (e.g., `Int` → `Num`, concrete → union).
3. `typeHierarchy/subtypes` searches loaded signatures for types that are
   subtypes of the target (e.g., `Num` → `Int`, `Float`).

**Scope:** Types from loaded `.tart` signatures and the built-in type
environment.

**Test:** Request type hierarchy on `Int`, assert `Num` appears as supertype;
request subtypes of `Num`, assert `Int` and `Float` appear.

---

## Task 12 — Code Lens

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Not implemented. The spec noted "no clear use case identified."

**Use cases (now identified):**

1. **Reference count** — show "N references" above each `defun`/`defvar`,
   linking to find-references.
2. **Signature status** — show "missing signature" or "signature ✓" above each
   `defun`.

**Changes:**

| File                    | Change                                                                      |
| ----------------------- | --------------------------------------------------------------------------- |
| `lib/lsp/code_lens.ml`  | **New.** Compute lenses for top-level definitions                           |
| `lib/lsp/code_lens.mli` | **New.** Interface                                                          |
| `lib/lsp/protocol.ml`   | Add `CodeLens`, `CodeLensParams` types                                      |
| `lib/lsp/protocol.mli`  | Expose code lens types                                                      |
| `lib/lsp/server.ml`     | Add `textDocument/codeLens` handler; register `codeLensProvider` capability |

**Test:** Open a file with multiple `defun` forms, request code lenses, assert
one lens per definition with reference count and signature status.

---

## Task 13 — Linked Editing Ranges

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Not implemented. Spec noted "not applicable to Elisp."

**Reassessment:** Linked editing ranges are useful for `let` binding variable
names — editing the binding name should simultaneously edit all references
within the `let` body. This is applicable because Elisp `let` forms bind a name
in a lexically scoped region.

**Changes:**

| File                   | Change                                                                                          |
| ---------------------- | ----------------------------------------------------------------------------------------------- |
| `lib/lsp/server.ml`    | Add `textDocument/linkedEditingRange` handler; register `linkedEditingRangeProvider` capability |
| `lib/lsp/protocol.ml`  | Add `LinkedEditingRanges` type                                                                  |
| `lib/lsp/protocol.mli` | Expose type                                                                                     |

**Behaviour:** When the cursor is on a symbol inside a `let`/`let*` form:

1. Walk up to the enclosing `let` binding.
2. If the symbol matches a bound variable, return ranges for the binding site
   and all references within the `let` body.
3. For top-level `defun` names, return the defun name range plus all
   same-document references (reuse `find_references`).

**Test:** Cursor on `x` in `(let ((x 1)) (+ x 2))`, assert two linked ranges
returned (the binding and the reference).

---

## Task 14 — On-Type Formatting

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Not implemented. Spec noted "better handled by `indent-region`."

**Reassessment:** Minimal on-type formatting can complement `indent-region` by
auto-closing parentheses and adjusting indentation on `)` and newline. Emacs
users may disable this if they prefer manual control.

**Changes:**

| File                   | Change                                                                                                                        |
| ---------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `lib/lsp/server.ml`    | Add `textDocument/onTypeFormatting` handler; register `documentOnTypeFormattingProvider` with trigger characters `)` and `\n` |
| `lib/lsp/protocol.ml`  | Add `DocumentOnTypeFormattingParams`, `TextEdit` result types                                                                 |
| `lib/lsp/protocol.mli` | Expose types                                                                                                                  |

**Behaviour:**

- On `)`: if the closing paren balances a top-level form, insert a newline after
  it (if not already present).
- On `\n`: compute indentation for the new line based on paren depth and return
  an edit that sets the leading whitespace.

**Test:** Type `)` closing a `defun`, assert trailing newline is inserted. Type
newline inside a `let` body, assert correct indentation.

---

## Task 15 — Pull-Based Diagnostics

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Not implemented. Push model (`publishDiagnostics`) is implemented
and sufficient.

**Changes:**

| File                   | Change                                                                                                  |
| ---------------------- | ------------------------------------------------------------------------------------------------------- |
| `lib/lsp/server.ml`    | Add `textDocument/diagnostic` handler; register `diagnosticProvider` capability alongside existing push |
| `lib/lsp/protocol.ml`  | Add `DocumentDiagnosticParams`, `DocumentDiagnosticReport` types                                        |
| `lib/lsp/protocol.mli` | Expose types                                                                                            |

**Behaviour:** On `textDocument/diagnostic`, run the same type-check pipeline
used by `publishDiagnostics` and return a `fullDocumentDiagnosticReport` with
`kind = "full"`, `resultId`, and the diagnostics array. Cache `resultId` per URI
to support `unchanged` reports when diagnostics haven't changed.

The push model continues to operate alongside pull. Clients that support pull
will use it; clients that don't will continue to receive push notifications.

**Test:** Integration test: send `textDocument/diagnostic`, assert response
includes diagnostics matching what `publishDiagnostics` would send.

---

## Task 16 — Dynamic Registration

**Spec:** [71](./specs/71-lsp-server.md).

**Status:** Partially implemented (file watchers use dynamic registration in
`handle_initialized`). No general dynamic registration framework.

**Changes:**

| File                   | Change                                                                                                                                     |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `lib/lsp/server.ml`    | Track client capability for dynamic registration per feature; use `client/registerCapability` for features the client supports dynamically |
| `lib/lsp/protocol.ml`  | Add `Registration`, `RegistrationParams` types                                                                                             |
| `lib/lsp/protocol.mli` | Expose types                                                                                                                               |

**Behaviour:**

1. During `initialize`, parse `textDocument.*.dynamicRegistration` fields for
   each capability (completion, hover, definition, etc.).
2. For features where the client supports dynamic registration, omit the static
   capability from the initialize response.
3. After `initialized`, send `client/registerCapability` with the appropriate
   registration options.

This allows the server to update capabilities at runtime (e.g., change
completion trigger characters, add new file watchers) without requiring a
restart.

**Test:** Integration test: client advertises dynamic registration for
completion; assert initialize response omits `completionProvider`; assert
`client/registerCapability` is sent after `initialized`.

---

## Dependency Graph

```
1. Debug-level unification tracing     (independent)
2. Reserved error codes                (independent)
3. Info severity                       (independent)
4. Clause diagnostics in LSP           (depends on 3)
5. Workspace-wide find-references      (independent)
6. Cross-file goto-definition          (independent)
7. Workspace-wide rename               (depends on 5)
8. Scope-aware completion              (independent)
9. Semantic tokens delta               (independent)
10. Call hierarchy                     (depends on 5 for callers)
11. Type hierarchy                     (independent)
12. Code lens                          (depends on 5 for ref counts)
13. Linked editing ranges              (independent)
14. On-type formatting                 (independent)
15. Pull-based diagnostics             (independent)
16. Dynamic registration               (independent)
```

Tasks 1–3, 5–6, 8–9, 11, 13–16 are independent and can proceed in parallel. Task
4 depends on 3 (Info severity must exist before mapping `DiagNote`). Tasks 7,
10, and 12 depend on task 5 (workspace-wide references).

---

## Checklist

| #  | Task                            | Spec | New files                 | Key changed files                                | Status      |
| -- | ------------------------------- | ---- | ------------------------- | ------------------------------------------------ | ----------- |
| 1  | Debug-level unification tracing | 72   | —                         | `unify.ml`                                       | Done        |
| 2  | Reserved error codes            | 73   | —                         | `diagnostic.ml`, `diagnostic.mli`                | Done        |
| 3  | Info severity                   | 73   | —                         | `diagnostic.ml`, `diagnostic.mli`, `worker.ml`   | Done        |
| 4  | Clause diagnostics in LSP       | 73   | —                         | `diagnostic.ml`, `diagnostic.mli`, `server.ml`   | Done        |
| 5  | Workspace-wide find-references  | 71   | —                         | `server.ml`                                      | Done        |
| 6  | Cross-file goto-definition      | 71   | —                         | `server.ml`                                      | Not started |
| 7  | Workspace-wide rename           | 71   | —                         | `server.ml`                                      | Not started |
| 8  | Scope-aware completion          | 71   | —                         | `completion.ml`                                  | Not started |
| 9  | Semantic tokens delta           | 71   | —                         | `semantic_tokens.ml`, `protocol.ml`, `server.ml` | Not started |
| 10 | Call hierarchy                  | 71   | `call_hierarchy.{ml,mli}` | `protocol.ml`, `server.ml`                       | Not started |
| 11 | Type hierarchy                  | 71   | `type_hierarchy.{ml,mli}` | `protocol.ml`, `server.ml`                       | Not started |
| 12 | Code lens                       | 71   | `code_lens.{ml,mli}`      | `protocol.ml`, `server.ml`                       | Not started |
| 13 | Linked editing ranges           | 71   | —                         | `server.ml`, `protocol.ml`                       | Not started |
| 14 | On-type formatting              | 71   | —                         | `server.ml`, `protocol.ml`                       | Not started |
| 15 | Pull-based diagnostics          | 71   | —                         | `server.ml`, `protocol.ml`                       | Not started |
| 16 | Dynamic registration            | 71   | —                         | `server.ml`, `protocol.ml`                       | Not started |
