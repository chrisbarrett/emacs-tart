# Implementation Plan — Specs 94–101

Emacs coverage enhancements: auto-version detection, Elisp layer scanning,
table output, sorting/filtering, package coverage format, version resolution,
source acquisition, and remote source integration.

## Gap Summary

| Spec | Title | Status | Key Gaps |
|:-----|:------|:-------|:---------|
| [94](specs/94-emacs-coverage-auto-version.md) | Auto Version Detection | ~40% | Fallback chain exists in `emacs_coverage.ml`; not wired into `main.ml` |
| [95](specs/95-elisp-layer-scanning.md) | Elisp Layer Scanning | 0% | `Definition_extractor` and `Sig.Search_path.load_lisp_core` exist but are not used by `emacs_coverage.ml` |
| [96](specs/96-coverage-table-format.md) | Coverage Table Format | ~5% | Flat summary only; no per-file table, color, or JSON-with-uncovered |
| [97](specs/97-coverage-listing-options.md) | Coverage Listing Options | 0% | No `--sort`, `--reverse`, `--min/max-percentage`, positional args, drill-down |
| [98](specs/98-package-coverage-format.md) | Package Coverage Format | 0% | `lib/cli/coverage.ml` does not exist |
| [99](specs/99-emacs-version-resolution.md) | Version Resolution | 0% | No semver shorthand, `latest`, dev identifiers, SHA, or remote tag listing |
| [100](specs/100-emacs-source-acquisition.md) | Source Acquisition | 0% | No tarball download, shallow clone, or source cache layout |
| [101](specs/101-emacs-coverage-remote-sources.md) | Remote Source Integration | 0% | Orchestration of 99+100 into `emacs-coverage` not started |

## Dependency Graph

```
94 (auto-version) ─────────────────────┐
                                        ├─→ 101 (remote integration)
99 (version resolution) → 100 (source) ┘
                                            │
95 (elisp scanning) ──→ 96 (table format) ─┤
                                            ├─→ 97 (listing options)
                                            └─→ 98 (package coverage)
```

## Phase 1 — Elisp Layer + Auto-Version

### Task 1.1 — Elisp layer scanning in `emacs_coverage.ml` ([Spec 95](specs/95-elisp-layer-scanning.md))

**Files:** `lib/coverage/emacs_coverage.ml`, `lib/coverage/emacs_coverage.mli`

Add Elisp layer scanning alongside the existing C layer:

- Define `elisp_coverage_item` and `elisp_coverage_result` types (mirror the C
  equivalents) holding per-file results with filename, covered/uncovered/private
  lists.
- New `calculate_elisp_coverage` function:
  1. Glob `lisp/*.el` under the Emacs source directory.
  2. For each file, run `Definition_extractor.extract_from_file`.
  3. Load matching `lisp-core/{basename}.tart` via
     `Sig.Search_path.load_lisp_core` (already exists).
  4. Classify each definition as covered, uncovered, or private (`--`).
  5. Files without a matching `.tart` → 0/N public coverage.
- Add `elisp_results` field to the top-level result record returned to callers.
- Extend `summarize` to aggregate C + Elisp totals.

**Tests:** `test/coverage/emacs_coverage_test.ml` — add elisp coverage suite
using `with_temp_typings` extended with `lisp-core/` subdirectories and
temporary `.el` files.

### Task 1.2 — Auto-version selection ([Spec 94](specs/94-emacs-coverage-auto-version.md))

**Files:** `bin/main.ml`, `lib/coverage/emacs_source.ml`

- In `run_emacs_coverage`, when `--emacs-version` is absent, use the version
  from `Emacs_source.detect()` and pass it through the existing fallback chain
  in `Emacs_coverage.load_typings`.
- When `--emacs-version` is provided, it overrides the detected version.
- With `--verbose`, log `Detected Emacs version: V` and the fallback chain
  candidates to stderr.

**Tests:** `test/coverage/emacs_source_test.ml` — add test for version
extraction feeding into fallback chain.

## Phase 2 — Table Output + Formatting

### Task 2.1 — Per-file coverage table renderer ([Spec 96](specs/96-coverage-table-format.md))

**Files:** new `lib/coverage/coverage_table.ml` + `.mli`

Create a shared table renderer usable by both `emacs-coverage` and `coverage`:

- `file_row` record: `{ filename; private_count; public_covered; public_total;
  coverage_pct; uncovered_names }`.
- `table_config` record: `{ color : [ `Auto | `Always | `Off ]; format : [
  `Human | `Json ] }`.
- `render_table`: given `file_row list` and config, produce the aligned column
  output with FILENAME / PRIVATE / PUBLIC / COVERAGE headers.
- Color logic: green (≥95%), yellow (≥50%), red (<50%). Respect `--color` flag
  and TTY detection.
- `render_json`: produce the JSON structure from [Spec 96 R5](specs/96-coverage-table-format.md)
  with `emacs_version`, `files`, and `totals`.
- Default sort: `.c` first, then `.el`; alphabetical within groups.

Register in `lib/coverage/dune` and re-export via `lib/tart.mli`.

**Tests:** `test/coverage/coverage_table_test.ml` — table alignment, color
thresholds, JSON structure, sort order.

### Task 2.2 — Wire table output into `emacs-coverage` ([Spec 96](specs/96-coverage-table-format.md))

**Files:** `bin/main.ml`

- Replace the flat summary in `run_emacs_coverage` with
  `Coverage_table.render_table` / `render_json`.
- Add `--color` flag (`auto`/`always`/`off`) and `--format` flag
  (`human`/`json`) to the `emacs-coverage` subcommand.
- Build `file_row list` from the C + Elisp coverage results.

### Task 2.3 — Sorting, filtering, drill-down ([Spec 97](specs/97-coverage-listing-options.md))

**Files:** `lib/coverage/coverage_table.ml`, `bin/main.ml`

- Add `sort_rows` accepting `[ `Default | `Name | `Coverage | `Public |
  `Private ]` and a `reverse` flag.
- Add `filter_rows` for `--min-percentage` / `--max-percentage`.
- Positional argument interpretation:
  - Contains `*` or `?` → glob match on filename.
  - Contains `.el` or `.c` → exact filename match.
  - Otherwise → feature name match (basename without extension).
- Drill-down output: when positional args present, append
  `file:line:col: identifier` lines for uncovered identifiers.
- Drill-down JSON: add `uncovered_details` array per [Spec 97 R6](specs/97-coverage-listing-options.md).
- CLI flags: `--sort[=VALUE]`, `--reverse`, `--min-percentage=N`,
  `--max-percentage=M`.

**Tests:** add sorting, filtering, and drill-down cases to
`test/coverage/coverage_table_test.ml`.

### Task 2.4 — Package coverage format ([Spec 98](specs/98-package-coverage-format.md))

**Files:** `bin/main.ml` (or new `lib/cli/coverage.ml` if refactor warranted)

- `tart coverage` already exists in `main.ml`. Change its output to use
  `Coverage_table.render_table` with drill-down always enabled (narrow scope).
- Add `--color`, `--sort`, `--reverse`, `--min-percentage`, `--max-percentage`
  flags (identical to `emacs-coverage`).
- Preserve existing `--fail-under`, `--exclude`, and positional `PATH`
  arguments.

**Tests:** extend `test/coverage/coverage_report_test.ml` or
`test/coverage/report_format_test.ml` to verify table+drill-down output for
package coverage.

## Phase 3 — Remote Emacs Sources

### Task 3.1 — Version resolution ([Spec 99](specs/99-emacs-version-resolution.md))

**Files:** `lib/coverage/emacs_source.ml`, `lib/coverage/emacs_source.mli`

Add a `resolve_version` function:

- **Semver shorthand:** `"29"` → list remote tags, find latest `emacs-29.*`
  release tag. `"29.1"` → resolve to exact tag `emacs-29.1`.
- **`latest`:** find most recent stable release tag (highest semver).
- **Dev identifiers:** `"dev"`, `"devel"`, `"git"` → return `main` branch ref.
- **Git SHA:** 7+ hex chars → literal commit ref, no tag lookup.
- **Remote tags:** `git ls-remote --tags
  https://git.savannah.gnu.org/git/emacs.git` (filter `emacs-*` tags, strip
  `^{}` derefs, parse semver).
- **Failure:** print usage + 3 most recent tags; exit code 2.

Define `resolved_version` type: `Release of { tag; version }` | `Dev` |
`Commit of string`.

**Tests:** `test/coverage/emacs_source_test.ml` — mock git output; test semver
parsing, shorthand resolution, failure messages.

### Task 3.2 — Source acquisition ([Spec 100](specs/100-emacs-source-acquisition.md))

**Files:** `lib/coverage/emacs_source.ml`, `lib/coverage/emacs_source.mli`

Add `acquire_source` function:

- **Release versions:** download tarball from
  `https://ftp.gnu.org/gnu/emacs/emacs-{version}.tar.xz`. Fall back to shallow
  clone if no tarball exists.
- **Dev versions:** shallow clone depth=1 of Emacs git repo.
- **Arbitrary SHAs:** `git fetch` the specific commit.
- **Cache layout:** `$XDG_CACHE_HOME/tart/emacs-sources/{version-or-sha}/`.
  Use `Content_cache.cache_dir` for the XDG base.
- **Cache reuse:** releases are immutable once cached. Dev versions run
  `git fetch` each invocation.
- **Atomic writes:** acquire to a temp dir under the cache root, then
  `Unix.rename` to final path.

Reuse patterns from `emacs_corpus.ml` (`run_git`, `ensure_clone`) where
applicable.

**Tests:** `test/coverage/emacs_source_test.ml` — cache layout, atomic rename,
reuse-on-hit; mock or skip network-dependent tests.

### Task 3.3 — Wire remote sources into `emacs-coverage` ([Spec 101](specs/101-emacs-coverage-remote-sources.md))

**Files:** `bin/main.ml`

Orchestrate version resolution and source acquisition in
`run_emacs_coverage`:

1. If `--emacs-source PATH` provided → use it directly (no fetch).
2. Else if `--emacs-version V` provided:
   a. Resolve V via `Emacs_source.resolve_version`.
   b. If resolved version matches locally-detected version → use local source.
   c. Otherwise → `Emacs_source.acquire_source` → use cached tree.
3. Else if Emacs on PATH → auto-detect version and source (Spec 94 path).
4. Else → exit code 1 with error.
- Resolved version selects both the source tree and the typings directory
  (applying the fallback chain).
- `--verbose` logs: resolved version, cache hit/miss, source path.

**Tests:** integration-level tests in `test/coverage/emacs_source_test.ml`.

## Implementation Order

| Order | Task | Spec | Blocked by |
|:------|:-----|:-----|:-----------|
| 1 | 1.1 Elisp layer scanning | 95 | — |
| 2 | 1.2 Auto-version selection | 94 | — |
| 3 | 2.1 Table renderer | 96 | 1.1 |
| 4 | 2.2 Wire table into emacs-coverage | 96 | 2.1 |
| 5 | 2.3 Sorting, filtering, drill-down | 97 | 2.1 |
| 6 | 2.4 Package coverage format | 98 | 2.1 |
| 7 | 3.1 Version resolution | 99 | — |
| 8 | 3.2 Source acquisition | 100 | 3.1 |
| 9 | 3.3 Wire remote sources | 101 | 3.2, 1.2 |

Tasks 1.1, 1.2, and 3.1 can begin in parallel. Phase 2 (2.1–2.4) can proceed
in parallel with Phase 3 (3.1–3.2) once 1.1 completes.
