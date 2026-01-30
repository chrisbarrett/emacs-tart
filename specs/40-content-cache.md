# Spec 40: Content-Addressable Cache

XDG-compliant content-addressable caching for incremental type-checking.

**Deps:** None

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

### R2: Content-based key

**Given** binary + input paths **When** computing key **Then** sha256 of concatenated contents

```ocaml
val compute_key : binary:string -> input:string -> string
(* => 64-char hex string *)
```

**Verify:** `dune test`; same inputs = same key; different = different

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

## Non-Requirements

- Cache size limits/eviction
- Hit rate tracking
- Remote/shared cache
- Partial result caching

## Tasks

- [ ] [R1] `cache_dir` with XDG
- [ ] [R9] `binary_path`
- [ ] [R2] `compute_key` with sha256
- [ ] [R3, R7, R8] `store` with atomic writes
- [ ] [R4, R5] `retrieve` with graceful errors
- [ ] [R6, R10] JSON format with version
- [ ] Tests
- [ ] Integrate into type-checking
