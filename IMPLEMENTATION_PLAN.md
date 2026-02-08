# Implementation Plan — Diagnostic Deduplication & Version Staleness

> Source: [Spec 71](./specs/71-lsp-server.md) §Robustness, task 6 (partial)
> from [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

Every `didChange` notification immediately triggers `publish_diagnostics` →
`check_document`, and during invalidation cascades, every dependent is also
re-checked and published. This produces two classes of waste:

1. **Identical diagnostics re-published.** If an edit doesn't change the
   diagnostic set (e.g., adding whitespace, editing a comment), the same
   diagnostics are sent to the client, causing redundant rendering.

2. **Stale diagnostics for superseded versions.** During rapid edits the server
   re-checks a document whose version has already been superseded by a later
   `didChange`. The resulting diagnostics are for stale text and may flash
   incorrect ranges before the next check completes.

The spec (§Robustness) calls for:

- **Identical diagnostics suppression:** compare the new diagnostic set against
  the last published set per URI; suppress if identical.
- **Version staleness check:** before publishing, verify the document version
  hasn't advanced; discard stale results silently.

These are achievable without concurrency (Task 4) and directly improve
correctness.

## Current State

- `publish_diagnostics` always writes the notification to the output channel.
- No record of previously published diagnostics exists.
- No version check between the version that triggered the check and the current
  document version.
- The `didChange` handler calls `publish_diagnostics server uri (Some version)`
  synchronously, so in the single-threaded model staleness only arises during
  invalidation cascades (dependent re-checks happen in sequence while the main
  URI may have received another `didChange` between iterations). However,
  installing the version check now prepares for Task 4 (concurrent requests).

## Tasks

### Task 1 — Track last-published diagnostics per URI

**Files:** `lib/lsp/server.ml`, `lib/lsp/server.mli`

Add a `last_diagnostics` hashtable to `server.t`:

```ocaml
last_diagnostics : (string, Protocol.diagnostic list) Hashtbl.t;
```

In `publish_diagnostics`, before writing the notification:

1. Look up the previous diagnostic list for this URI.
2. Compare against the new list. If structurally equal, skip the write and log
   at debug level: `"Suppressing identical diagnostics for %s"`.
3. Otherwise, store the new list and write the notification.

For the `didClose` case (publishing empty diagnostics), also update the cache.

Structural equality requires a comparison function for `Protocol.diagnostic`.
Add `diagnostic_equal : diagnostic -> diagnostic -> bool` and
`diagnostics_equal : diagnostic list -> diagnostic list -> bool` to
`protocol.{ml,mli}`. Compare all fields: range, severity, code, message,
source, related_information.

**Tests:** Add to `test/lsp/server_test.ml`:

- `test_identical_diagnostics_suppressed`: open a file with errors, note the
  diagnostics, send a whitespace-only didChange that doesn't alter the
  diagnostic set → verify only one `publishDiagnostics` notification is sent
  (not two).

### Task 2 — Version staleness check

**Files:** `lib/lsp/server.ml`

Before publishing diagnostics in `publish_diagnostics`, check that the document
version matches the version that was passed:

```ocaml
match Document.get_doc server.documents uri with
| Some doc when version = Some doc.version -> (* proceed *)
| Some doc when version = None -> (* proceed — didClose/explicit cases *)
| _ -> (* version mismatch — discard *)
```

If stale, log at debug level: `"Discarding stale diagnostics for %s (checked
v%d, current v%d)"` and skip the write.

This is most impactful during the invalidation cascade in `handle_did_change`:
while re-checking dependent documents, if the *triggering* document receives
another `didChange`, the dependents' versions haven't changed but the results
are based on outdated signature/graph state. The version check on the dependent
itself won't catch this, but it correctly catches the simpler case where the
same URI is re-checked.

**Tests:** Add to `test/lsp/server_test.ml`:

- `test_stale_diagnostics_discarded`: this is hard to test in the current
  synchronous model. Instead, verify the positive case: after a sequence of
  opens and changes, the final diagnostics match the final document state. This
  confirms the version plumbing is correct.

### Task 3 — Clear last-published diagnostics on close

**Files:** `lib/lsp/server.ml`

In `handle_did_close`, after publishing the empty diagnostic set, remove the URI
from `last_diagnostics`. This prevents stale entries from accumulating for
documents that are no longer open.

**Tests:** This is covered by existing `test_diagnostics_cleared_on_close`. Add
an assertion that reopening the same file re-publishes diagnostics (i.e., the
dedup cache doesn't suppress them after reopen).

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Track last-published diagnostics per URI | Done |
| 2 | Version staleness check | Done |
| 3 | Clear last-published diagnostics on close | Done |
