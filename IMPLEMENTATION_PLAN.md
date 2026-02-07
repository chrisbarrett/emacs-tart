# Implementation Plan: Refactoring Iteration

Gap analysis between codebase structure and emerging domain concepts.
Focused on modularisation, clarity, and test coverage.

## Analysis Summary

### Codebase metrics

| Module | Lines | Notes |
|--------|------:|-------|
| `typing/infer.ml` | 2264 | Largest; `clause_dispatch` and `row_dispatch` recently extracted |
| `sig/sig_loader.ml` | 1979 | Signature loading pipeline with ~5 internal subsystems |
| `lsp/server.ml` | 1737 | `code_action` recently extracted; many handler groups remain |
| `typing/diagnostic.ml` | 1155 | Error construction + 4 output formatters in one file |
| `sig/sig_parser.ml` | 1047 | Cohesive recursive-descent parser; low extraction value |

### Untested modules with high test value

| Module | Lines | Why test |
|--------|------:|---------|
| `typing/levenshtein.ml` | 89 | Pure algorithm, critical for UX, property-testable |
| `syntax/print.ml` | 149 | Output correctness, escape edge cases, round-trip verifiable |
| `typing/clause_dispatch.ml` | 228 | Recently extracted, complex dispatch logic |
| `typing/row_dispatch.ml` | 240 | Recently extracted, spec decision table |

### Dead code

- `form_cache.ml:23` — unused `module Exhaustiveness = Typing.Exhaustiveness`

---

## Iteration 1: Cleanup and quick wins

### Task 1.1: Remove dead import in form_cache.ml

- [x] Delete `module Exhaustiveness = Typing.Exhaustiveness` (line 23)
- [x] Build to confirm no breakage

---

## Iteration 2: Extract modules from `infer.ml`

Continue the pattern established by `clause_dispatch.ml` and
`row_dispatch.ml`. Target: reduce `infer.ml` from 2264 to ~1600 lines.

### Task 2.1: Extract `pcase_infer.ml` from `infer.ml`

Lines 829–1112 of `infer.ml` contain 8 mutually-recursive functions for
pcase pattern binding extraction and pcase form inference. They form a
self-contained subsystem operating on `Sexp.t` patterns.

- [ ] Create `lib/typing/pcase_infer.ml` + `.mli`
- [ ] Move: `extract_pattern_bindings`, `extract_backquote_bindings`,
      `extract_unquote_binding`, `extract_map_pattern_bindings`,
      `extract_map_pattern_constraints`, `infer_pcase_let`, `infer_pcase`
- [ ] Wire into `infer.ml` via module reference
- [ ] Update `lib/typing/dune` if needed
- [ ] Build + test to confirm no regressions

### Task 2.2: Extract `defun_infer.ml` from `infer.ml`

Lines 1996–2264 contain defun inference: `infer_defun_as_expr`,
`infer_defun`, `infer_defun_with_declaration`, `infer_defun_inferred`,
plus the `(declare (tart TYPE))` extraction helper.

- [ ] Create `lib/typing/defun_infer.ml` + `.mli`
- [ ] Move the defun inference cluster (~270 lines)
- [ ] Wire into `infer.ml`
- [ ] Build + test

---

## Iteration 3: Extract modules from `sig_loader.ml`

Target: reduce `sig_loader.ml` from 1979 to ~1350 lines by extracting
two cohesive subsystems.

### Task 3.1: Extract `sig_validation.ml` from `sig_loader.ml`

Lines 61–298 contain the entire signature validation subsystem:
`validate_type`, `validate_types`, `validate_params`,
`validate_binder_bounds`, `validate_clause`, `validate_defun`,
`validate_defvar`, `validate_type_decl`, `validate_import_struct`,
`validate_ctor`, `validate_data`, `validate_decl`,
`build_context`, `validate_signature`, `validate_signature_all`.

- [ ] Create `lib/sig/sig_validation.ml` + `.mli`
- [ ] Move validation functions (~240 lines)
- [ ] Wire into `sig_loader.ml`
- [ ] Build + test

### Task 3.2: Extract `sig_convert.ml` from `sig_loader.ml`

Lines 424–805 contain type conversion: `canonicalize_type_name`,
`sig_name_to_prim`, `substitute_sig_type`, `substitute_sig_param`,
`sig_type_to_typ_with_ctx` and the simplified interfaces.

- [ ] Create `lib/sig/sig_convert.ml` + `.mli`
- [ ] Move type conversion functions (~380 lines)
- [ ] Wire into `sig_loader.ml`
- [ ] Build + test

---

## Iteration 4: Extract modules from `diagnostic.ml` and `server.ml`

### Task 4.1: Extract `diagnostic_format.ml` from `diagnostic.ml`

Lines 700–971 contain output formatting that is distinct from diagnostic
construction: `format_pos`, `format_span`, `to_string`,
`to_string_human`, `to_string_compact`, `to_string_list`,
`error_type_of_code`, plus the JSON serializer at lines 1101–1155.

- [ ] Create `lib/typing/diagnostic_format.ml` + `.mli`
- [ ] Move formatting + JSON serialization (~330 lines)
- [ ] Keep diagnostic construction (types, constructors, context dispatch) in
      `diagnostic.ml`
- [ ] Wire together; update `diagnostic.mli` re-exports as needed
- [ ] Build + test

### Task 4.2: Extract LSP `completion.ml` from `server.ml`

Lines 1182–1389 contain the entire completion subsystem:
`extract_prefix_at_position`, `collect_local_completions`,
`collect_env_completions`, `filter_by_prefix`,
`deduplicate_completions`, `handle_completion`.

- [ ] Create `lib/lsp/completion.ml` + `.mli`
- [ ] Move completion functions (~210 lines)
- [ ] Wire into `server.ml` dispatch
- [ ] Build + test

### Task 4.3: Extract LSP `signature_help.ml` from `server.ml`

Lines 1391–1572: `find_call_context`, `param_to_label`,
`signature_of_function_type`, `handle_signature_help`.

- [ ] Create `lib/lsp/signature_help.ml` + `.mli`
- [ ] Move signature help functions (~180 lines)
- [ ] Wire into `server.ml` dispatch
- [ ] Build + test

---

## Iteration 5: Add unit tests for untested modules

### Task 5.1: Add unit tests for `levenshtein.ml`

Pure algorithm with well-defined interface — highest test value.

- [ ] Create `test/typing/levenshtein_test.ml`
- [ ] Test `distance`: identity (d(s,s)=0), symmetry, known pairs,
      empty strings, single edits
- [ ] Test `find_similar_names`: threshold scaling by length,
      sorting by distance, no matches case
- [ ] Test `suggest_name`: best match selection, None when no match
- [ ] Add to `test/typing/dune`

### Task 5.2: Add unit tests for `print.ml`

Output correctness is critical; escape sequences and modifiers are subtle.

- [ ] Create `test/syntax/print_test.ml`
- [ ] Test `escape_string_char`: all special escapes (\n, \t, \\, \")
- [ ] Test `escape_string`: strings with mixed escaping needs
- [ ] Test `print_char`: plain chars, control chars, modifier combos
      (Meta, Control, Shift, Hyper, Super, Alt)
- [ ] Test `Modifiers.extract`: round-trip modifier encoding
- [ ] Test `to_string`: quote/backquote/unquote reader macros,
      cons cells, vectors, keywords, nested lists
- [ ] Test round-trip: `to_string` then `parse_one` yields same AST
- [ ] Add to `test/syntax/dune`

### Task 5.3: Add unit tests for `clause_dispatch.ml`

Recently extracted; validates Spec 54 multi-clause dispatch.

- [ ] Create `test/typing/clause_dispatch_test.ml`
- [ ] Test `extract_arg_literal` for keyword and quoted symbol params
- [ ] Test `substitute_tvar_names` with various type shapes
- [ ] Test `try_dispatch` with single-clause, multi-clause,
      literal-matching, and no-match scenarios
- [ ] Test diagnostic resolution with format string placeholders
- [ ] Add to `test/typing/dune`

### Task 5.4: Add unit tests for `row_dispatch.ml`

Recently extracted; validates Spec 11 row accessor decision table.

- [ ] Create `test/typing/row_dispatch_test.ml`
- [ ] Test `get_config` for known accessors (alist-get, plist-get,
      gethash, map-elt, etc.) and unknown names
- [ ] Test row extraction functions with constructed types
- [ ] Test dispatch decision paths (Cases 1–5, R4–R8)
- [ ] Add to `test/typing/dune`
