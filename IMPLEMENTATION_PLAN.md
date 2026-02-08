# Implementation Plan — Workspace Configuration

> Source: Task 9 from [Spec 71](./specs/71-lsp-server.md)

## Problem

The server detects Emacs version and resolves typings paths at startup,
but there is no way for the user to override these via editor settings.
If detection fails or the user works with a different target version,
they must restart the server. There is also no way to add custom
signature lookup directories.

## Current State

- `server.ml`: `detect_emacs_and_build_config` runs once in `create`.
  `module_config` and `emacs_version` are immutable fields on `server.t`.
- `protocol.ml`: `initialize_params` has `process_id`, `root_uri`,
  `capabilities` — no `initializationOptions` field.
- `Search_path`: has `prepend_dir`, `of_dirs`, `with_emacs_version`.
- `Module_check`: has `with_search_path`, `with_search_dirs`,
  `with_stdlib`.
- No `workspace/didChangeConfiguration` handler exists.

## Design

### Settings

| Key | Type | Default | Effect |
|-----|------|---------|--------|
| `tart.emacsVersion` | `string` | auto-detect | Target Emacs version for typings |
| `tart.searchPath` | `string[]` | `[]` | Additional `.tart` lookup dirs (prepended) |

`tart.diagnostics.debounceMs` is deferred until concurrent processing
(task 4) enables timer-based debouncing.

### Where settings come from

1. **`initializationOptions`** in the `initialize` request — provides
   initial settings before the first type check.
2. **`workspace/didChangeConfiguration`** notification — applies
   settings changes at runtime without restart.

Both paths parse the same settings shape and call the same
`apply_settings` function.

### Applying settings

`apply_settings` on `server.t`:

1. Parse `tart.emacsVersion` string via `Emacs_version.parse_version`.
   If set and different from current, update `emacs_version` field.
2. Parse `tart.searchPath` array. Prepend each directory to the
   search path.
3. Rebuild `module_config` by re-running the same config-building
   logic as `detect_emacs_and_build_config`, but using the overridden
   version and additional dirs.
4. Invalidate all form caches and re-publish diagnostics for all open
   documents (settings change affects all type checking).

### Making server.t fields mutable

`module_config` and `emacs_version` must become `mutable` so
`apply_settings` can update them after initialization.

## Tasks

### Task 1 — Protocol types + settings parsing

**Files:** `lib/lsp/protocol.{ml,mli}`

- Add `tart_settings` type:
  `{ ts_emacs_version : string option; ts_search_path : string list }`.
- Add `parse_tart_settings : Yojson.Safe.t -> tart_settings` — extracts
  `tart.emacsVersion` (string or null) and `tart.searchPath` (string
  array or absent → `[]`). Tolerates missing keys gracefully.
- Add `initialization_options` field to `initialize_params` as
  `Yojson.Safe.t option`.
- Update `parse_initialize_params` to extract `initializationOptions`.
- Add `did_change_configuration_params` type with `settings` field
  (`Yojson.Safe.t`).
- Add `parse_did_change_configuration_params`.

### Task 2 — Server apply_settings + mutable config

**Files:** `lib/lsp/server.{ml,mli}`

- Make `module_config` and `emacs_version` mutable on `server.t`.
- Extract `build_config` helper from `detect_emacs_and_build_config`
  that takes `?emacs_version:version option` and
  `?extra_search_dirs:string list` parameters, so it can be reused
  with overrides.
- Add `apply_settings : t -> Protocol.tart_settings -> unit`:
  1. Parse emacs version string if provided.
  2. Rebuild config via `build_config` with overrides.
  3. Update `server.module_config` and `server.emacs_version`.
  4. Invalidate all form caches (`Form_cache.invalidate_all`).
  5. Re-publish diagnostics for all open documents.
- Wire `initializationOptions` into `handle_initialize`: after state
  transition, parse settings from init options (if present) and call
  `apply_settings`.
- Add `handle_did_change_configuration` notification handler: parse
  params, extract settings, call `apply_settings`.
- Add `"workspace/didChangeConfiguration"` to `dispatch_notification`.
- Add `Form_cache.invalidate_all` if it doesn't exist (clear all
  entries).

### Task 3 — Tests

**Files:** `test/lsp/server_test.ml`,
`test/lsp_support/lsp_client.{ml,mli}`

- Add `did_change_configuration_msg` to `lsp_client.{ml,mli}`.
- Tests:
  - `initializationOptions` with `emacsVersion` applied (verify
    diagnostics are published after initialize with custom version).
  - `workspace/didChangeConfiguration` with `searchPath` change
    (open doc, change config, verify diagnostics re-published).
  - Empty/null settings tolerated (no crash on `{}` or missing keys).
  - Unknown keys ignored (extra fields in settings don't cause errors).

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types + settings parsing | Done |
| 2 | Server apply_settings + mutable config | Done |
| 3 | Tests | Done |
