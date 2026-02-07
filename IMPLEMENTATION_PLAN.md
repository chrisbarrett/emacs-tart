# Implementation Plan: Spec 53 — Structured Logging

## Background

Spec 53 replaces ad-hoc logging with a unified `Log` module. Two
separate logging systems exist:

1. `lib/coverage/verbose_log.ml` — format-string based, gated by a
   `bool` threaded through call sites. Used by `main.ml` helpers
   (5 `open` sites, ~40 `verbose_log` calls).

2. `lib/lsp/server.ml` — string-based `log`, `debug`, `info` methods
   tied to server state. ~80 call sites inside server.ml. Has its own
   `log_level = Quiet | Normal | Debug`.

**What the spec wants:** A new `lib/log/` sub-library with global
mutable state (set once at CLI startup), format-string gating via
`Printf.ifprintf`, four levels (Quiet, Normal, Verbose, Debug),
optional JSON-lines output format, and global `--log-level`/`-v`/
`--log-format` flags on every subcommand.

**Key constraint:** Zero-cost when disabled — suppressed calls must
not allocate or format strings. The existing `verbose_log` already
does this correctly with `Printf.ifprintf`. The LSP server's `log`
function builds string first then gates — that's non-zero-cost.

**Migration complexity:**

- `main.ml` verbose_log calls: straightforward substitution of
  `verbose_log verbose` → `Log.verbose`. The `~verbose:bool` parameter
  threading can be removed since `Log` uses global state.
- `server.ml` log calls: ~80 sites use `debug server "..."` and
  `info server "..."` patterns (string, not format). These become
  `Log.debug "%s" msg` or better, convert inline string construction
  to `Log.debug "msg %s" arg` for zero-cost.
- The LSP server's own `log_level` type and `log`/`debug`/`info`
  functions are deleted. The `--log-level` arg on the `lsp` subcommand
  maps to the global `Log.set_level` call.
- `verbose_arg` currently used by coverage commands becomes the global
  `-v` shorthand. The coverage-specific `verbose` param threading is
  removed.

---

## Iteration 1: Create `lib/log/` module and global CLI flags

**What to build:**

1. Create `lib/log/dune` — new sub-library `tart.log` depending on
   `yojson`.

2. Create `lib/log/log.ml` and `lib/log/log.mli`:
   - `type level = Quiet | Normal | Verbose | Debug`
   - `type format = Text | Json`
   - Global refs: `current_level`, `current_format`
   - `set_level`, `set_format` mutators
   - `info`, `verbose`, `debug` using `Printf.kfprintf`/`Printf.ifprintf`
   - `debug_with_ctx` for optional JSON context

3. Add `tart.log` as dependency of `tart` (top-level lib/dune).

4. Wire global flags in `main.ml`:
   - Add `--log-level` enum (quiet/normal/verbose/debug)
   - Add `--log-format` enum (text/json)
   - Resolve `-v` flag: if set and `--log-level` is default, use
     Verbose; otherwise `--log-level` wins
   - Call `Log.set_level` and `Log.set_format` before dispatching
     each subcommand

5. Add `tart.log` dependency to coverage dune and lsp dune.

**Files:**
- `lib/log/dune` (new)
- `lib/log/log.ml` (new)
- `lib/log/log.mli` (new)
- `lib/dune` — add `tart.log` to libraries
- `lib/coverage/dune` — add `log` to libraries
- `lib/lsp/dune` — add `log` to libraries
- `bin/main.ml` — global flag definitions and wiring

**Verify:** `dune build`; `dune test`

---

## Iteration 2: Migrate coverage/main.ml from Verbose_log to Log

**What to build:**

1. Replace all `verbose_log verbose` calls in `main.ml` with
   `Log.verbose` calls. Remove `let open Tart.Verbose_log in` blocks.
   Remove `~verbose` parameter threading from helper functions
   (`find_typings_root`, `detect_emacs_version`, `log_typings_loading`,
   `scan_c_source_verbose`).

2. Update `run_coverage` and `run_emacs_coverage` — remove `verbose`
   parameter, update Cmdliner terms.

3. Delete `lib/coverage/verbose_log.ml` and `verbose_log.mli`.

4. Update `lib/coverage/dune` if needed.

5. Verify no remaining `Verbose_log` references: `grep -r Verbose_log`.

**Files:**
- `bin/main.ml` — bulk migration
- `lib/coverage/verbose_log.ml` — delete
- `lib/coverage/verbose_log.mli` — delete

**Verify:** `dune build`; `dune test`; `grep -r Verbose_log lib/ bin/`
returns nothing

---

## Iteration 3: Migrate LSP server to Log

**What to build:**

1. Remove `log_level` type and `log`/`debug`/`info` functions from
   `server.ml` and `server.mli`.

2. Remove `log_level` field from server `t` record and `create`
   parameter.

3. Replace all `debug server "..."` with `Log.debug "%s" "..."` or
   convert to format-string calls where feasible. Prioritize the ~10
   most common patterns; use `%s` wrapper for complex string-built
   messages.

4. Update `main.ml` `run_lsp`: remove `log_level` param from
   `Server.create` call. The global `Log.set_level` (wired in
   iteration 1) handles level configuration.

5. Remove `lsp_log_level_arg` from main.ml (superseded by global
   `--log-level`).

6. Update any tests that reference `Server.log_level`,
   `Server.Debug`, `Server.Normal`, `Server.Quiet`.

**Files:**
- `lib/lsp/server.ml` / `server.mli` — remove log infrastructure
- `bin/main.ml` — update `run_lsp`, remove `lsp_log_level_arg`
- `test/lsp/server_test.ml` (if it references log_level)

**Verify:** `dune build`; `dune test`

---

## Iteration 4: Spec status update

**What to build:**

1. Check all task boxes in specs/53-structured-logging.md.
2. Add Status section: "Complete".
3. Verify R8 (verbose logging shows signature loading) — already
   present from verbose_log migration.
4. R9 (debug logging for unification/resolution) — defer to future
   if no existing debug call sites exist in typing modules. Mark
   task as checked with note.
5. R10-R11 (stderr/stdout separation, quiet mode) — verify by
   inspection; Log module writes to stderr, results to stdout.

**Files:**
- `specs/53-structured-logging.md`

**Verify:** `dune test`; all tests pass
