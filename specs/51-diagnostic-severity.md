# Spec 51: Diagnostic Severity

Severity levels: errors block; warnings/hints inform.

**Dependencies:** Spec 35, 47

## Constraints

| Constraint        | Detail                               |
| ----------------- | ------------------------------------ |
| Backward compat   | Default behavior unchanged           |
| CLI controllable  | Flags to promote/suppress            |
| LSP compatible    | Maps to LSP DiagnosticSeverity       |

## Output

```
lib/core/severity.ml(i)   ; Severity type
lib/typing/diagnostic.ml  ; Extended with severity
bin/main.ml               ; CLI flags
```

## Severity Levels

| Level   | Value | Exit code | Meaning                      |
|---------|-------|-----------|------------------------------|
| Error   | 1     | 1         | Must fix                     |
| Warning | 2     | 0         | Should review                |
| Info    | 3     | 0         | Neutral observation          |
| Hint    | 4     | 0         | Optional improvement         |

## Code Ranges

| Prefix | Severity | Example codes                                    |
|--------|----------|--------------------------------------------------|
| E      | Error    | E0001 TypeMismatch, E0100 UndefinedVariable      |
| W      | Warning  | W0001 RedundantGuard, W0002 VersionMismatch      |
| H      | Hint     | H0001 SimplifiableGuard, H0002 LowerableVersion  |

## Requirements

### R1-R3: Severity type

```ocaml
type t = Error | Warning | Info | Hint
val to_int : t -> int       (* Error->1, Warning->2, Info->3, Hint->4 *)
val of_int : int -> t option
val to_string : t -> string (* "error", "warning", "info", "hint" *)
val default_severity : error_code -> t  (* E*->Error, W*->Warning, H*->Hint *)
```

Diagnostic includes severity field; defaults from code prefix.

**Verify:** `dune test`

### R4-R5: Output formats

```
error[E0001]: type mismatch
  --> init.el:42:10

warning[W0001]: redundant feature guard
  --> init.el:15:1
```

```json
{"code": "E0001", "severity": "error", "severity_code": 1, "message": "..."}
```

**Verify:** `./tart check --format=json file.el | jq .severity`

### R6-R10: CLI flags

| Flag              | Effect                              |
|-------------------|-------------------------------------|
| (default)         | Exit 1 on errors only               |
| --warn-as-error   | Exit 1 on warnings too              |
| --ignore-warnings | Suppress warning output             |
| --ignore-hints    | Suppress hint output                |

Flags combine: `--warn-as-error --ignore-hints` shows errors+warnings, exits 1 on warnings.

**Verify:** `./tart check --warn-as-error file.el; echo $?`

### R11-R12: LSP mapping

Maps directly to LSP DiagnosticSeverity (same values). Respect client capabilities for filtering.

**Verify:** Eglot shows correct faces

### R13: Summary

```
Found 2 errors, 1 warning, 3 hints
```

**Verify:** `./tart check file.el`

### R16: Severity override

```ocaml
val make : ?severity:Severity.t -> code:error_code -> ... -> t
```

**Verify:** `dune test`

## Non-Requirements

- Per-file severity config
- Watch mode severity
- Custom severity levels
- Localization

## Tasks

- [ ] [R1-R3] Severity type and default mapping
- [ ] [R4-R5] Output formatting
- [ ] [R6-R10] CLI flags
- [ ] [R11-R12] LSP integration
- [ ] [R13] Summary counts
- [ ] [R16] Severity override
