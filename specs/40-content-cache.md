# Spec 40: Content-Addressable Cache

XDG-compliant content-addressable caching for incremental type-checking.

- **Deps:** None

## Constraints

| Constraint | Detail |
|------------|--------|
| XDG-compliant | `$XDG_CACHE_HOME/tart/` (fallback `~/.cache/tart/`) |
| Content-addressed | key = sha256(tart_binary + input_content) |
| Inspectable | Human-readable JSON format |
| Graceful | Missing cache = cache miss, not error |

## Output

```
lib/cache/
├── content_cache.ml
└── content_cache.mli
```

Cache structure:
```
$XDG_CACHE_HOME/tart/v1/
├── ab/ab3f...d2.json
└── cd/cd91...f7.json
```

## Requirements

### R1: XDG cache location

**Given** cache ops **When** determining dir **Then** `$XDG_CACHE_HOME/tart/` or `~/.cache/tart/`

```ocaml
val cache_dir : unit -> string
```

**Verify:** `XDG_CACHE_HOME=/tmp/test ./tart check file.el && ls /tmp/test/tart/`

### R2: Content-based key with dependency hashing

**Given** binary + input + dependencies **When** computing key **Then** sha256
of concatenated contents including transitive dependencies

```ocaml
val compute_key : binary:string -> input:string -> deps:string list -> string
(* => 64-char hex string *)
```

The key must incorporate the content of all transitive dependencies (`.tart`
signature files, prelude, etc.) so that changing a typing file invalidates
cached results for all `.el` files that depend on it. The dependency list
comes from the dependency graph ([Spec 27](./27-dependency-graph.md)).

**Verify:** `dune test`; same inputs = same key; different = different;
changing a `.tart` file invalidates dependents

### R3: Store results

**Given** key + data **When** `store` **Then** write to `v1/XX/XXXX...XX.json`

```ocaml
val store : key:string -> data:string -> unit
```

Two-char prefix directory prevents too many files per directory.

**Verify:** `dune test`; stored data at correct path

### R4: Retrieve results

**Given** key **When** `retrieve` **Then** `Some data` if cached, `None` if not

```ocaml
val retrieve : key:string -> string option
```

**Verify:** `dune test`; retrieve after store = `Some`; without = `None`

### R5: Graceful miss handling

**Given** missing entry, corrupted file, or I/O error **When** retrieve **Then** `None`

**Verify:** `dune test`; nonexistent key = None; corrupted file = None

### R6: Inspectable format

**Given** cached entry **When** examined **Then** valid JSON

```json
{
  "version": 1,
  "created_at": "2024-01-15T10:30:00Z",
  "data": "..."
}
```

**Verify:** `cat ~/.cache/tart/v1/ab/ab3f...d2.json | jq .`

### R7: Create directory on demand

**Given** cache dir missing **When** storing **Then** create directory structure

**Verify:** `rm -rf ~/.cache/tart && ./tart check file.el && ls ~/.cache/tart/v1/`

### R8: Atomic writes

**Given** concurrent access **When** writing same key **Then** write to temp, rename

**Verify:** `dune test`; parallel stores don't corrupt

### R9: Binary self-identification

**Given** need to hash binary **When** computing keys **Then** use `Sys.executable_name`

```ocaml
val binary_path : unit -> string
```

**Verify:** `dune test`; `binary_path` returns valid executable

### R10: Cache versioning

**Given** format may evolve **When** storing **Then** use `v1/` subdirectory

Old `v1/` entries remain valid; future `v2/` would coexist.

**Verify:** Files stored under `v1/`

### R11: Age-based eviction

**Given** cache entries older than threshold **When** eviction runs **Then**
delete entries with mtime > N days

```ocaml
val evict_older_than : days:int -> unit
```

Default threshold: 30 days. Content-addressed keys mean old file versions become
unreachable naturally; eviction reclaims disk space from stale entries.

**Verify:** `dune test`; entries older than threshold deleted; newer preserved

### R12: Eviction trigger

**Given** tart startup **When** cache directory exists **Then** run eviction
once per session

Eviction runs at most once per process to avoid repeated directory scans. Use a
marker file (`$CACHE_DIR/.last-eviction`) with mtime to skip if eviction ran
recently (e.g., within 1 hour).

```ocaml
val maybe_evict : unit -> unit
(* Runs eviction if not done recently *)
```

**Verify:** `dune test`; eviction runs on first call; skipped on subsequent calls

### R13: Eviction is best-effort

**Given** eviction failure (permissions, I/O error) **When** deleting entries
**Then** log warning, continue with remaining entries, don't fail the operation

Eviction must never block type-checking. Partial cleanup is acceptable.

**Verify:** `dune test`; read-only file doesn't crash eviction

## Non-Requirements

- Hit rate tracking
- Remote/shared cache
- Partial result caching

## Tasks

- [x] [R1] `cache_dir` with XDG
- [x] [R9] `binary_path`
- [x] [R2] `compute_key` with sha256
- [x] [R3, R7, R8] `store` with atomic writes
- [x] [R4, R5] `retrieve` with graceful errors
- [x] [R6, R10] JSON format with version
- [x] [R11] `evict_older_than` with mtime check
- [x] [R12] `maybe_evict` with marker file
- [x] [R13] Best-effort error handling in eviction
- [x] Tests
- [x] Integrate into type-checking

## Status

Complete. Cache is integrated into the `check` subcommand in `bin/main.ml`.
Cache key includes binary, input `.el` file, and all typings `.tart` deps
(c-core, lisp-core, sibling). Pass-only caching: clean files are cached,
files with errors are always re-checked. `--no-cache` flag available.
Eviction runs once per session via `maybe_evict`.
