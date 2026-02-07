# Spec 44: Timing and Memory Statistics

**Depends:** [Spec 36][] (cmdliner-cli)

## Links

### Deps
[Spec 36]: ./36-cmdliner-cli.md

| Constraint | Detail |
|------------|--------|
| Opt-in | Stats only when flags enabled |
| Zero-cost | No overhead when disabled |
| Granular | Per-phase and per-file breakdown |
| Composable | `--verbose` and `--memory` independent |

## Output

```
lib/timing/{dune,timing.ml,timing.mli}
lib/memory_stats/{dune,memory_stats.ml,memory_stats.mli}
bin/main.ml  ; instrumentation
```

## Requirements

| ID | Requirement | Verify |
|----|-------------|--------|
| R1 | `-v`/`--verbose` on check/eval/expand prints timing to stderr | `./tart check -v test.el 2>&1 \| grep -q "Total time"` |
| R2 | `--memory` prints GC stats to stderr, independent of `-v` | `./tart check --memory test.el 2>&1 \| grep -q "GC"` |
| R3 | `-v` shows total elapsed time (e.g., "1.234s", "45ms") | `./tart check -v test.el 2>&1 \| grep -E "Total.*[0-9]+"` |
| R4 | `-v` on check shows per-phase timing: Parsing, Macro expansion, Type inference | `./tart check -v test.el 2>&1 \| grep -q "Parsing"` |
| R5 | `-v` with multiple files shows per-file timing | `./tart check -v a.el b.el 2>&1 \| grep -c "\.el" \| grep -q "2"` |
| R6 | `--memory` shows GC summary: minor/major counts, heap size, total alloc | `./tart check --memory test.el 2>&1 \| grep -q "minor"` |
| R7 | `--memory` shows per-phase memory deltas | `./tart check --memory test.el 2>&1 \| grep -E "(Parsing\|Type inference).*alloc"` |
| R8 | `-v --memory` shows coherent combined output (time + memory per phase) | `./tart check -v --memory test.el 2>&1 \| grep -E "Parsing.*[0-9]+ms.*alloc"` |
| R9 | `eval -v` shows: Parsing, Evaluation, Type inference, Total | `./tart eval -v '(+ 1 2)' 2>&1 \| grep -q "Evaluation"` |
| R10 | `expand -v` shows: Parsing (per file), Expansion, Total | `./tart expand -v test.el 2>&1 \| grep -q "Expansion"` |
| R11 | Stats to stderr; normal output to stdout | `./tart eval -v '(+ 1 2)' 2>/dev/null` → "3 :: Int" only |
| R12 | Human-readable units (ns/us/ms/s, bytes/KB/MB), aligned output | Manual inspection |

## Implementation

### Timing

```ocaml
module Timing : sig
  type t
  val start : unit -> t
  val elapsed : t -> float       (* seconds *)
  val elapsed_ms : t -> float
  val format : float -> string   (* "1.23s" or "45ms" *)
end
```

Use `Unix.gettimeofday` or `Mtime`.

### Memory_stats

```ocaml
module Memory_stats : sig
  type snapshot
  val snapshot : unit -> snapshot
  val diff : before:snapshot -> after:snapshot -> delta
  val format_delta : delta -> string
  val format_summary : unit -> string
end
```

Use `Gc.stat` / `Gc.quick_stat`.

### Output Example

```
$ tart check -v --memory large.el
[verbose] Parsing large.el... 12ms (1.2MB alloc, 0 GC)
[verbose] Macro expansion... 5ms (0.3MB alloc, 0 GC)
[verbose] Type inference... 234ms (45MB alloc, 3 minor GC)
[verbose] Total: 251ms

[memory] Heap: 12MB, Minor GC: 3, Major GC: 0, Total alloc: 46.5MB
```

## Tasks

- [x] Add `-v`/`--memory` flags to check, eval, expand [R1-R2]
- [x] Create `lib/timing/` module
- [x] Create `lib/memory_stats/` module
- [x] Instrument `run_check` per-file and per-phase [R3-R5]
- [x] Instrument `run_eval` [R9]
- [x] Instrument `run_expand` [R10]
- [x] Add GC tracking per phase [R6-R7]
- [x] Coherent combined output [R8]
- [x] Verify stderr/stdout separation [R11]
- [x] Polish formatting [R12]

## Status

Complete
