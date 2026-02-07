# Implementation Plan: Spec 44 — Timing and Memory Statistics

## Background

Spec 44 adds opt-in timing and memory statistics to `check`, `eval`,
and `expand` subcommands. The existing `Log` module (Spec 53) already
provides `-v`/`--verbose` which gates at the `Verbose` level — timing
output uses `Log.verbose` so no new `-v` flag is needed. The only new
CLI flag is `--memory`.

**Key insight:** The spec's output example uses `[verbose]` prefix,
which maps directly to `Log.verbose` calls. Memory summary uses a
`[memory]` tag which we'll emit via `Log.info` (always shown when
`--memory` is set) or a dedicated emit function that bypasses log level
gating when `--memory` is active.

**Phases in `run_check`:**
1. Parsing (`Read.parse_file`)
2. Type inference (`Check.check_program`)
3. Diagnostic formatting

**Phases in `run_eval`:**
1. Parsing (`Read.parse_string`)
2. Evaluation (`Eval.eval_toplevel`)
3. Type inference (`Check.check_expr`)

**Phases in `run_expand`:**
1. Parsing (`Read.parse_file`)
2. Expansion (`Expand.expand_all`)

---

## Iteration 1: Timing module + `--memory` flag

**What to build:**

1. Create `lib/timing/dune` — new sub-library `tart.timing` depending
   on `unix`.

2. Create `lib/timing/timing.ml` and `lib/timing/timing.mli`:
   - `type t` (opaque, wraps float from `Unix.gettimeofday`)
   - `val start : unit -> t`
   - `val elapsed_s : t -> float` (seconds)
   - `val elapsed_ms : t -> float` (milliseconds)
   - `val format_duration : float -> string` (auto-scales:
     `"1.23s"` / `"45ms"` / `"123μs"`)

3. Create `lib/memory_stats/dune` — new sub-library
   `tart.memory_stats` (no extra deps, uses `Gc` stdlib).

4. Create `lib/memory_stats/memory_stats.ml` and
   `lib/memory_stats/memory_stats.mli`:
   - `type snapshot` (wraps `Gc.stat` result)
   - `type delta` (diffs of minor/major words, minor/major collections)
   - `val snapshot : unit -> snapshot`
   - `val diff : before:snapshot -> after:snapshot -> delta`
   - `val format_delta : delta -> string` (e.g., `"1.2MB alloc, 3 minor GC"`)
   - `val format_summary : unit -> string` (heap size, GC counts, total alloc)
   - `val format_bytes : float -> string` (auto-scales: `"1.2MB"` / `"45KB"`)

5. Add `tart.timing` and `tart.memory_stats` as dependencies of
   `tart` (lib/dune).

6. Re-export as `Tart.Timing` and `Tart.Memory_stats` in
   `lib/tart.ml` and `lib/tart.mli`.

7. Add `--memory` flag in `bin/main.ml`:
   - `memory_flag_arg`: `Arg.(value & flag & info ["memory"] ~doc)`
   - Wire into `check_cmd`, `eval_cmd`, `expand_cmd` terms

**Files:**
- `lib/timing/dune` (new)
- `lib/timing/timing.ml` (new)
- `lib/timing/timing.mli` (new)
- `lib/memory_stats/dune` (new)
- `lib/memory_stats/memory_stats.ml` (new)
- `lib/memory_stats/memory_stats.mli` (new)
- `lib/dune` — add dependencies
- `lib/tart.ml` / `lib/tart.mli` — re-exports
- `bin/main.ml` — `--memory` flag definition

**Verify:** `dune build`; `dune test`

---

## Iteration 2: Instrument `run_check` (R1, R3–R5, R8, R11)

**What to build:**

1. Wrap `check_file` phases with timing:
   - `Tart.Timing.start ()` before parsing
   - Log after parsing: `Log.verbose "Parsing %s... %s" file (format)`
   - `Tart.Timing.start ()` before type inference
   - Log after inference: `Log.verbose "Type inference... %s" (format)`

2. When `--memory` is set, take `Memory_stats.snapshot` before/after
   each phase. Append delta to the verbose line:
   `Log.verbose "Parsing %s... %s (%s)" file time_str mem_delta_str`

3. After all files, log total:
   `Log.verbose "Total: %s" (format total_elapsed)`

4. When `--memory` is set, emit memory summary after total:
   `Printf.eprintf "[memory] %s\n" (Memory_stats.format_summary ())`

5. For multiple files (R5), wrap each `check_file` call to show
   per-file timing.

6. Thread `memory` flag through `run_check` parameter.

**Files:**
- `bin/main.ml` — instrument `run_check` and `check_file`

**Verify:** `dune build`; `dune test`;
`./tart check -v test/fixtures/typing/core/basic.el 2>&1 | grep Parsing`

---

## Iteration 3: Instrument `run_eval` and `run_expand` (R9, R10)

**What to build:**

1. Instrument `run_eval`:
   - Time parsing, evaluation, type inference phases
   - Log each phase at verbose level
   - Log total at verbose level
   - When `--memory`, include deltas and summary

2. Instrument `run_expand`:
   - Time parsing (per file for `--load` files and target)
   - Time expansion
   - Log each phase at verbose level
   - Log total at verbose level
   - When `--memory`, include deltas and summary

3. Thread `memory` flag through both functions.

**Files:**
- `bin/main.ml` — instrument `run_eval` and `run_expand`

**Verify:** `dune build`; `dune test`;
`./tart eval -v '(+ 1 2)' 2>&1 | grep Total`

---

## Iteration 4: Polish formatting + spec status (R12)

**What to build:**

1. Verify human-readable units are properly scaled in all output.

2. Align output columns if feasible (phase name padding).

3. Verify stderr/stdout separation (R11): timing goes to stderr via
   Log.verbose (which uses `eprintf`), results go to stdout.

4. Check all task boxes in `specs/44-timing-stats.md`.

5. Add Status section: "Complete".

**Files:**
- `specs/44-timing-stats.md`

**Verify:** `dune test`; all tests pass
