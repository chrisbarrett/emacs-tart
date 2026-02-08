# Implementation Plan — UTF-16 Position Encoding

> Source: [Spec 71](./specs/71-lsp-server.md), task 2 from
> [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Context

The LSP protocol uses UTF-16 code units for character offsets within lines.
The tart server currently approximates with byte offsets throughout:

- `server.ml:146` has a comment: "UTF-16 code units, but we approximate
  with bytes."
- `document.ml:81` `position_to_offset` treats `character` as a byte index.
- All positions returned via `range_of_span` pass byte columns straight
  through.

The low-level conversion functions (`utf16_offset_of_byte` and
`byte_offset_of_utf16`) are already implemented and tested in
`document.{ml,mli}` with 7 unit tests. What remains is wiring them into the
protocol flow.

There are two directions of conversion:

1. **Inbound (client → server):** Positions in requests (`hover`,
   `definition`, `references`, `completion`, `signatureHelp`, `rename`,
   `codeAction`) and `didChange` edits carry UTF-16 character offsets that
   must be converted to byte offsets before use.

2. **Outbound (server → client):** All positions in responses and
   notifications (`range_of_span`, code action edits, document symbols,
   rename edits) carry byte column offsets that must be converted to UTF-16
   before sending.

## Tasks

### 1. Add `position_encoding` to server state and negotiate during initialize

Store the negotiated position encoding in the server record. Parse
`general.positionEncodings` from client capabilities. Advertise the chosen
encoding in the initialize response.

**Changed files:**

| File | Change |
|------|--------|
| `protocol.mli` | Add `general_capabilities` with `position_encodings : string list option` to `client_capabilities`; add `position_encoding` field to `server_capabilities` |
| `protocol.ml` | Parse `general.positionEncodings` in `parse_client_capabilities`; encode `positionEncoding` in `server_capabilities_to_json` |
| `server.ml` | Add `position_encoding` field to `t`; negotiate encoding in `handle_initialize` |

**Negotiation logic:** If client advertises `utf-32`, prefer it (no
conversion needed — our byte offsets are effectively UTF-32 for ASCII, but
we still need conversion for multi-byte). Otherwise use `utf-16` (the
default). Store the result in `server.position_encoding`.

### 2. Add `line_text_at` helper to `document.ml`

The conversion functions need the text of a specific line. Add a helper
that extracts line text by line number from a document's text.

**Changed files:**

| File | Change |
|------|--------|
| `document.mli` | Add `val line_text_at : string -> int -> string option` |
| `document.ml` | Implement `line_text_at` — extract the nth (0-based) line from text |

### 3. Wire UTF-16 conversion into `position_to_offset` (inbound)

Make `position_to_offset` convert the `character` field from UTF-16 to
bytes before using it as an index into line text.

**Changed files:**

| File | Change |
|------|--------|
| `document.ml` | In `position_to_offset`, convert `pos.character` from UTF-16 to byte offset using `byte_offset_of_utf16` |
| `test/lsp/document_test.ml` | Add tests: incremental edit with multi-byte content using UTF-16 character positions |

### 4. Wire UTF-16 conversion into `range_of_span` (outbound)

`range_of_span` converts `Syntax.Location.span` (which has byte columns)
to `Protocol.range`. It needs the document text to convert byte columns to
UTF-16. Thread the document text through and apply `utf16_offset_of_byte`.

**Changed files:**

| File | Change |
|------|--------|
| `server.ml` | Change `range_of_span` to accept `~text:string`; look up line text and convert byte columns to UTF-16. Update all call sites (diagnostics, hover, definition, references, code actions, document symbols, rename). |

### 5. Add end-to-end LSP tests with multi-byte content

Add server-level tests that send multi-byte content (emoji, CJK) via
didOpen and verify that diagnostics, hover, and other responses report
positions in UTF-16 code units.

**Changed files:**

| File | Change |
|------|--------|
| `test/lsp/server_test.ml` | Add tests: hover on multi-byte line returns UTF-16 positions; diagnostics for multi-byte content use UTF-16 positions |

## Iteration Order

1. **Task 1** — Protocol negotiation (types + parsing + server state)
2. **Task 2** — `line_text_at` helper
3. **Task 3** — Inbound conversion (`position_to_offset`)
4. **Task 4** — Outbound conversion (`range_of_span`)
5. **Task 5** — End-to-end tests

Tasks 1–2 are prerequisites. Tasks 3–4 are the core work and can be done
independently once 1–2 are in place. Task 5 validates the full round-trip.
