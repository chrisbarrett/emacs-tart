# Implementation Plan — Incremental Sync Recovery

> Source: [Spec 71](./specs/71-lsp-server.md) §Robustness, task 3 from
> [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

When a client sends an incremental `didChange` with positions outside the
document bounds, `apply_single_change` returns `Error` and the server logs the
message but otherwise ignores the edit. The document is left at the old version,
creating silent state divergence — every subsequent incremental edit is applied
against stale text, compounding the problem.

The spec calls for two defences:

1. **Clamp** out-of-range edits to the document bounds so the edit can still be
   applied (best-effort).
2. **Detect version gaps** (incoming version ≠ stored version + 1) and log at
   info level so operators can diagnose sync issues.

## Current State

- `position_to_offset` returns `None` for out-of-range positions.
- `apply_single_change` propagates the `None` as `Error "... out of range"`.
- `apply_changes` returns the first `Error` immediately.
- `handle_did_change` in `server.ml` logs the error at info and drops the edit.
- No version gap detection exists.

## Tasks

### Task 1 — Clamp out-of-range positions in `position_to_offset`

**Files:** `lib/lsp/document.ml`, `lib/lsp/document.mli`

Change `position_to_offset` to never return `None`. When a position is past the
end of the document, clamp to `String.length text`. When a character offset is
past the end of a line, clamp to the line's byte length.

- Make `position_to_offset` return `int` (not `int option`).
- Update `apply_single_change` to remove the `None` match arms.
- Remove the `Error "Start/End position out of range"` paths — they become
  unreachable.
- Keep the `start > end` guard (invalid range).
- Keep the `end_offset > String.length text` guard but change it to clamp
  `end_offset` to `String.length text` rather than error.

**Tests:** Add to `test/lsp/document_test.ml`:

- `test_edit_past_end_clamps`: edit with start/end beyond document end → clamps
  to end, inserts at end.
- `test_edit_past_line_end_clamps`: character offset beyond line length → clamps
  to line end.

### Task 2 — Detect version gaps

**Files:** `lib/lsp/document.ml`, `lib/lsp/server.ml`

Add version gap detection to `apply_changes`:

- Before applying, check if `version <> doc.version + 1`. If so, return an
  enriched result that signals the gap (e.g., a separate variant or a warning
  string alongside `Ok`).
- In `handle_did_change`, log at info level when a version gap is detected:
  `"Version gap on %s: expected %d, got %d"`.
- Still apply the changes regardless — version gaps are informational, not
  blocking.

Design the return type as:
```ocaml
type apply_result = {
  warning : string option;
}
```

Change `apply_changes` to return `(apply_result, string) result`. The `Ok`
variant carries an optional warning string. `handle_did_change` logs the warning
when present.

**Tests:** Add to `test/lsp/document_test.ml`:

- `test_version_gap_detected`: apply changes with version jump (1 → 5) →
  succeeds with warning.
- `test_version_sequential_no_warning`: apply changes with version 1 → 2 → no
  warning.

### Task 3 — End-to-end integration test

**Files:** `test/lsp/server_test.ml`

Add an end-to-end test in the server test suite:

- `test_edit_past_end_succeeds`: send `didChange` with a range extending past
  document bounds → no crash, subsequent operations (hover, diagnostics) still
  work on the clamped document.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Clamp out-of-range positions | Done |
| 2 | Detect version gaps | Not started |
| 3 | End-to-end integration test | Not started |
