# Spec 53: Structured Logging

Unified logging facility with verbosity levels and switchable output format.

**Deps:** Spec 36 (cmdliner-cli), Spec 30 (verbose-coverage).
**Supersedes:** Spec 30 verbose_log module, LSP server ad-hoc logging.

## Goal

When a type-checker run produces confusing results, users and agents need
visibility into what happened internally—which signatures loaded, how symbols
resolved, why unification failed—without rebuilding tart or reading source.

## Constraints

| Constraint  | Detail                                                    |
|-------------|-----------------------------------------------------------|
| Unified     | Single logging module replaces verbose_log and LSP logger |
| Opt-in      | Default level is `normal`; no output change without flags |
| Zero-cost   | Disabled log calls must not allocate or format strings    |
| Stderr      | All log output goes to stderr; stdout reserved for results|
| Switchable  | Plain text by default; `--log-format=json` for machines   |
| Global      | `--log-level` and `--log-format` available on every subcommand |

## Output

```
lib/log/{dune,log.ml,log.mli}
bin/main.ml                    ; flag wiring, replaces verbose_log usage
lib/lsp/server.ml              ; migrated to Log module
lib/coverage/verbose_log.ml    ; deleted
lib/coverage/verbose_log.mli   ; deleted
```

## Log Levels

| Level   | Includes                | Use case                          |
|---------|-------------------------|-----------------------------------|
| quiet   | Nothing                 | Suppress all diagnostics          |
| normal  | Info, errors            | Default; operational messages     |
| verbose | + detailed operations   | Signature loading, path resolution|
| debug   | + trace-level internals | Unification steps, env lookups    |

Higher levels include all output from lower levels.

## Output Formats

### Plain text (default)

```
[info] Loading typings from /path/to/typings/emacs/31.0
[verbose] c-core/data.tart: 80 signatures
[debug] resolve: car -> (cons a b) -> a  (env depth 3)
[debug] unify: (List Int) ~ (List a) => a := Int
```

Prefix is the level name in brackets. One message per line.

### JSON lines (`--log-format=json`)

```json
{"level":"info","msg":"Loading typings from /path/to/typings/emacs/31.0"}
{"level":"verbose","msg":"c-core/data.tart: 80 signatures"}
{"level":"debug","msg":"resolve: car -> (cons a b) -> a","ctx":{"env_depth":3}}
{"level":"debug","msg":"unify: (List Int) ~ (List a) => a := Int"}
```

One JSON object per line. `ctx` field is optional, present when structured
context is available.

## Requirements

### R1: Log module with four levels

**Given** `lib/log/log.mli` is defined
**When** callers use `Log.info`, `Log.verbose`, `Log.debug`
**Then** each function gates on the configured level before formatting

**Verify:** `Log.debug` produces no output when level is `normal`

### R2: Zero-cost when disabled

**Given** log level is `quiet`
**When** `Log.info fmt args` is called
**Then** `fmt` and `args` are not evaluated (use `Printf.ifprintf` or similar)

**Verify:** No allocation observed for suppressed log calls

### R3: Global --log-level flag

**Given** any subcommand (check, eval, expand, lsp, coverage, etc.)
**When** `--log-level=verbose` is passed
**Then** messages at verbose level and below are emitted

**Verify:** `./tart check --log-level=verbose test.el 2>&1 | grep "\[verbose\]"`

### R4: Global --log-format flag

**Given** any subcommand
**When** `--log-format=json` is passed
**Then** each log line is a valid JSON object

**Verify:** `./tart check --log-level=debug --log-format=json test.el 2>&1 | head -1 | python3 -m json.tool`

### R5: -v shorthand for --log-level=verbose

**Given** any subcommand
**When** `-v` is passed
**Then** it is equivalent to `--log-level=verbose`

**Verify:** `./tart check -v test.el 2>&1 | grep "\[verbose\]"`

### R6: Replace verbose_log in coverage commands

**Given** coverage and emacs-coverage subcommands
**When** logging is enabled
**Then** they use `Log.verbose` and `Log.info` instead of `Verbose_log`
**And** `lib/coverage/verbose_log.{ml,mli}` are deleted

**Verify:** `grep -r "Verbose_log" lib/ bin/` returns no results

### R7: Replace LSP server logging

**Given** the LSP server
**When** it logs messages
**Then** it uses `Log.info` and `Log.debug` instead of its own `log` function
**And** `[tart-lsp]` prefix is replaced by the standard level prefix

**Verify:** `./tart lsp --log-level=debug 2>&1 | grep "\[debug\]"`

### R8: Type-checker logging at verbose level

**Given** `--log-level=verbose`
**When** `tart check` runs
**Then** output shows: typings path resolution, version detection, files loaded
with signature counts

**Verify:** `./tart check -v test.el 2>&1 | grep "signatures"`

### R9: Type-checker logging at debug level

**Given** `--log-level=debug`
**When** `tart check` runs
**Then** output additionally shows: symbol resolution lookups, unification
steps, environment operations

**Verify:** `./tart check --log-level=debug test.el 2>&1 | grep "unify\|resolve"`

### R10: Stderr/stdout separation

**Given** logging is enabled at any level
**When** any subcommand runs
**Then** log output goes to stderr
**And** command results (types, reports, expanded code) go to stdout

**Verify:** `./tart check -v test.el >out.txt 2>log.txt && ! grep "\[verbose\]" out.txt`

### R11: --log-level=quiet suppresses all diagnostics

**Given** `--log-level=quiet`
**When** any subcommand runs
**Then** nothing is written to stderr (except fatal errors from the OS)

**Verify:** `./tart check --log-level=quiet test.el 2>&1 >/dev/null | wc -l` outputs 0

### R12: Level names in --help

**Given** `tart --help` or `tart check --help`
**When** help is displayed
**Then** `--log-level` documents all four levels: quiet, normal, verbose, debug
**And** `--log-format` documents: text, json

**Verify:** `./tart --help 2>&1 | grep -E "quiet.*normal.*verbose.*debug"`

## Implementation Notes

### Module signature

```ocaml
module Log : sig
  type level = Quiet | Normal | Verbose | Debug

  val set_level : level -> unit
  val set_format : [`Text | `Json] -> unit

  (** Gated logging functions. Format string is not evaluated when level
      is below threshold. *)
  val info : ('a, out_channel, unit) format -> 'a
  val verbose : ('a, out_channel, unit) format -> 'a
  val debug : ('a, out_channel, unit) format -> 'a

  (** For structured JSON context on debug messages. *)
  val debug_with_ctx : (string * string) list -> string -> unit
end
```

Use mutable global state (`ref`) for level and format—set once at CLI startup
before any work begins. This avoids threading a log config through every
function signature.

### Wiring in main.ml

```ocaml
let log_level_arg =
  let doc = "Set log verbosity. $(docv) is $(b,quiet), $(b,normal) (default), \
             $(b,verbose), or $(b,debug)." in
  Arg.(value & opt log_level_enum Normal & info ["log-level"] ~doc ~docv:"LEVEL")

let log_format_arg =
  let doc = "Log output format. $(docv) is $(b,text) (default) or $(b,json)." in
  Arg.(value & opt log_format_enum `Text & info ["log-format"] ~doc ~docv:"FORMAT")

let verbose_arg =
  let doc = "Shorthand for --log-level=verbose." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)
```

Resolve `-v` vs `--log-level` in the term: if `-v` is set and `--log-level` is
at default, use Verbose; if both are explicit, `--log-level` wins.

### Migration path

1. Add `lib/log/` module
2. Wire global flags in `main.ml`
3. Replace `Verbose_log` call sites with `Log.verbose`
4. Replace LSP `server.log`/`server.debug` with `Log.info`/`Log.debug`
5. Delete `lib/coverage/verbose_log.{ml,mli}`
6. Add `Log.verbose` calls in type-checker for signature loading (R8)
7. Add `Log.debug` calls in type-checker for unification/resolution (R9)

## Tasks

- [ ] [R1-R2] Create `lib/log/` with `Log` module and `.mli`
- [ ] [R3-R5, R12] Add `--log-level`, `--log-format`, `-v` global flags in main.ml
- [ ] [R6] Migrate coverage commands from `Verbose_log` to `Log`
- [ ] [R7] Migrate LSP server to `Log`
- [ ] [R6] Delete `lib/coverage/verbose_log.{ml,mli}`
- [ ] [R8] Add verbose-level logging to type-checker (loading, resolution)
- [ ] [R9] Add debug-level logging to type-checker (unification, env lookups)
- [ ] [R10-R11] Verify stderr/stdout separation and quiet mode
- [ ] [R4] Implement JSON lines output format
