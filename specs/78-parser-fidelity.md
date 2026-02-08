# Spec 78 — Parser Fidelity

> Consolidates specs: [38](./.archive/38-ast-printer.md), [39](./.archive/39-emacs-reader-oracle.md), [41](./.archive/41-emacs-corpus.md), [42](./.archive/42-roundtrip-harness.md)

## Overview

Tart ensures its Elisp parser faithfully reproduces Emacs's own reader behaviour
through four interlocking components: an AST printer that converts parsed
S-expressions back to valid Elisp source, an Emacs reader oracle that invokes
Emacs in batch mode as a gold-standard reference, a corpus of real-world Emacs
Lisp files for broad coverage, and a round-trip test harness that ties the pieces
together to catch parser bugs systematically.

## AST Printer

`lib/syntax/print.ml` (`Print.to_string`) converts `Sexp.t` values to valid
Emacs Lisp strings. The output satisfies semantic equivalence: re-parsing the
printed text produces the same AST (ignoring spans). The printer is not a
pretty-printer; it emits minimal, correct Elisp with no attempt to preserve
whitespace, comments, or numeric base.

### Literals

| AST node | Output |
|----------|--------|
| `Int (42, _)` | `42` |
| `Float (3.14, _)` | `3.14` |
| `String ("hello", _)` | `"hello"` |
| `Keyword ("key", _)` | `:key` |
| `Symbol ("foo-bar", _)` | `foo-bar` |

Strings are escaped according to Elisp conventions: `\n`, `\t`, `\r`, `\\`,
`\"`, `\a`, `\b`, `\f`, `\e`. Non-ASCII characters use UTF-8 or `\xNN`/`\uNNNN`
escapes.

### Character Literals

Printable ASCII characters use `?c` syntax. Characters requiring escapes use
`?\n`, `?\t`, `?\\`, or `?\xNN`. Modified characters use Emacs modifier syntax:

| Modifier | Bit | Syntax |
|----------|-----|--------|
| Control | 0x1F mask | `?\C-x` |
| Meta | 0x8000000 | `?\M-x` |
| Shift | 0x2000000 | `?\S-x` |
| Hyper | 0x1000000 | `?\H-x` |
| Alt | 0x400000 | `?\A-x` |
| Super | 0x800000 | `?\s-x` |

The `Modifiers` sub-module exposes `extract` for decomposing a character code
into its base character and modifier list.

### Compound Forms

Lists print as parenthesised, space-separated elements. Reader-macro sugar is
applied where possible:

| Form | Output |
|------|--------|
| `(quote x)` | `'x` |
| `(backquote ...)` | `` `... `` |
| `(unquote x)` | `,x` |
| `(unquote-splicing xs)` | `,@xs` |
| `(function f)` | `#'f` |

Vectors print as `#(...)`. Cons cells print as dotted pairs `(a . b)`, and
improper lists print with a dotted tail `(1 2 . 3)`. Error nodes print as
`#<error: ...>` (not valid Elisp; intended for debugging only).

## Emacs Reader Oracle

`lib/oracle/emacs_reader.ml` invokes Emacs as a gold-standard reference. Every
oracle call uses `emacs --batch --quick` from the system PATH for minimal
startup time.

### Reading

`read_string` passes a single Elisp form to Emacs and returns its
`prin1-to-string` output. `read_file` reads all top-level forms from a file,
returning a list of canonical printed strings. Multi-expression files produce one
entry per form; comments are stripped by both Emacs and tart so they do not
affect comparison.

### Error Handling

The oracle captures four error classes:

| Variant | Meaning |
|---------|---------|
| `Read_error` | Emacs reader rejected the input |
| `Emacs_not_found` | No `emacs` on PATH |
| `Emacs_failed` | Non-zero exit with captured stderr |
| `Timeout` | Emacs exceeded the timeout (default 5 000 ms) |

Stderr is captured separately from stdout so warnings do not corrupt the parsed
output.

### Comparison

`lib/oracle/compare.ml` (`Compare.compare_string`, `Compare.compare_file`)
parses input with tart and reads it with Emacs, then compares canonical printed
forms. A light normalisation pass (`Compare.normalise`) strips insignificant
differences such as trailing whitespace and float formatting before comparison.
Results are `Match`, `Mismatch`, `Tart_error`, or `Emacs_error`.

Special read syntax---quote, backquote, function shorthand, and propertised
strings---is covered by the comparison.

## Corpus

`lib/corpus/emacs_corpus.ml` manages a shallow clone of the Emacs source
repository, stored at `$XDG_CACHE_HOME/tart/emacs-src/` (falling back to
`~/.cache/tart/emacs-src/`). Only a single version is checked out at a time.

### Clone and Checkout

The initial clone uses `git clone --depth 1` to reduce the multi-gigabyte Emacs
history to roughly 200 MB. Subsequent version switches use shallow tag fetches
(`git fetch --depth 1 origin tag <tag> --no-tags`) followed by checkout.
If the corpus directory already exists, no network activity occurs for listing or
path queries.

### Version Detection

When no explicit version is given, `detect_tag` queries the system Emacs via
`emacs --version` and maps the result to a git tag like `emacs-31.1`. The
`--emacs-version` CLI flag overrides this for CI environments where Emacs may not
be installed.

### File Listing

`list_el_files` returns absolute paths for all `.el` files in the corpus,
including generated files like `loaddefs.el`.

### CLI Subcommands

| Command | Action |
|---------|--------|
| `tart corpus checkout <ref>` | Fetch and checkout a tag, branch, or SHA |
| `tart corpus list` | Print all `.el` file paths |
| `tart corpus path` | Print the corpus directory path |
| `tart corpus clean` | Remove the corpus and report bytes freed |

### Error Handling

Network failures during clone or fetch produce clear messages. If a tag does not
exist, the error suggests available local tags. Exit codes: 0 success, 1 network
error, 2 invalid version/tag, 3 Emacs not found (auto-detect).

## Roundtrip Harness

`lib/roundtrip/roundtrip.ml` ties the printer, oracle, and corpus together into
an automated testing pipeline.

### Per-File Checks

`check_file` parses each `.el` file, prints every form back to Elisp via the AST
printer, re-parses the printed output, and compares the two ASTs with structural
equality (ignoring spans). `check_file_with_emacs` additionally runs the oracle
comparison to verify tart's output against Emacs's native reader.

### Failure Reporting

Failures include the absolute file path and error location. AST mismatches
produce a unified diff of expected vs actual printed output, with a form index
identifying which top-level form diverged.

### Caching

A content-addressable cache (SHA-256 of file contents) skips re-verification for
files that have already passed. On a cache hit, the harness returns `Cached`
immediately. Modified files trigger a full re-check and update the cache on
success.

### Corpus Run

`run_corpus` processes a list of files and collects results into a summary
reporting totals for passed, failed, and cached files, along with the list of
individual failures. The optional `~with_emacs:true` flag enables oracle
verification alongside structural round-trip checks.

### Integration

The harness integrates with `dune test` via tests in `test/roundtrip/`. A
standalone `scripts/run-roundtrip.sh` provides a `--with-emacs` flag for full
oracle runs outside the test suite.

## Deferred

- **CI integration** for oracle comparison (see
  [Spec 77](./77-testing-infrastructure.md)).
- **Parallel execution** in the roundtrip harness (sequential is adequate given
  caching).
