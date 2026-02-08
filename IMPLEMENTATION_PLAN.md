# Implementation Plan — LSP Integration Test Harness

> Source: [Spec 71](./specs/71-lsp-server.md), task 1 from
> [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Context

`test/lsp/server_test.ml` already contains 65+ in-process integration tests
that exercise the full server loop. They use a temp-file-based pattern where
messages are concatenated into a single input buffer, the server runs to
completion, then all output is parsed at once. This works but has two
weaknesses:

1. **No subprocess testing** — the tests run `Server.create` / `Server.run`
   in-process, so they never exercise the actual `tart lsp` binary, CLI
   argument parsing, or process lifecycle (signals, stdin EOF, pipe semantics).

2. **Duplicated boilerplate** — every test rebuilds the init/shutdown/exit
   message sequence and re-implements output parsing. There is no shared LSP
   client abstraction.

This plan addresses both by extracting a reusable `Lsp_client` module and
adding a subprocess-based integration test suite.

## Tasks

### 1. Extract `Lsp_client` test helper library

Factor the repeated patterns from `server_test.ml` into a shared library
under `test/lsp_support/`:

**New files:**

| File | Purpose |
|------|---------|
| `test/lsp_support/lsp_client.ml` | Build messages, run server, parse responses |
| `test/lsp_support/lsp_client.mli` | Interface |
| `test/lsp_support/dune` | Library target `tart_lsp_test_support` |

**API sketch:**

```ocaml
(** Build JSON-RPC messages *)
val make_message : ?id:Yojson.Safe.t -> method_:string ->
  ?params:Yojson.Safe.t -> unit -> string

(** Standard message builders *)
val initialize_msg : ?id:int -> ?root_uri:string -> unit -> string
val initialized_msg : unit -> string
val shutdown_msg : ?id:int -> unit -> string
val exit_msg : unit -> string
val did_open_msg : uri:string -> ?version:int -> text:string -> unit -> string
val did_change_msg : uri:string -> version:int ->
  changes:Yojson.Safe.t list -> unit -> string
val did_close_msg : uri:string -> unit -> string
val hover_msg : id:int -> uri:string -> line:int -> character:int -> unit -> string
val definition_msg : id:int -> uri:string -> line:int -> character:int -> unit -> string
val references_msg : id:int -> uri:string -> line:int -> character:int -> unit -> string
val completion_msg : id:int -> uri:string -> line:int -> character:int -> unit -> string
val rename_msg : id:int -> uri:string -> line:int -> character:int ->
  new_name:string -> unit -> string

(** Run the server in-process with given messages, return parsed responses *)
val run_session : string list -> (int * Yojson.Safe.t list)

(** Run with standard init/shutdown wrapper *)
val run_initialized_session : string list -> (int * Yojson.Safe.t list)

(** Response query helpers *)
val find_response : id:int -> Yojson.Safe.t list -> Yojson.Safe.t option
val find_notification : method_:string -> Yojson.Safe.t list -> Yojson.Safe.t option
val find_diagnostics : uri:string -> Yojson.Safe.t list -> Yojson.Safe.t option
val response_result : Yojson.Safe.t -> Yojson.Safe.t
val response_error : Yojson.Safe.t -> Yojson.Safe.t option
```

### 2. Refactor `server_test.ml` to use `Lsp_client`

Rewrite the existing 65+ tests to use the shared helpers, drastically
reducing boilerplate. This is a pure refactor — no new test cases, no
behaviour changes.

**Changed files:**

| File | Change |
|------|--------|
| `test/lsp/server_test.ml` | Replace inline helpers with `Lsp_client` calls |
| `test/lsp/dune` | Add `tart_lsp_test_support` dependency to `server_test` |

### 3. Add subprocess integration test suite

Create `test/lsp_integration/` with tests that spawn `tart lsp` as a child
process and communicate via pipes.

**New files:**

| File | Purpose |
|------|---------|
| `test/lsp_integration/subprocess_client.ml` | Spawn tart binary, send/receive over pipes |
| `test/lsp_integration/subprocess_client.mli` | Interface |
| `test/lsp_integration/integration_test.ml` | Subprocess test cases |
| `test/lsp_integration/dune` | Build target with `%{bin:tart}` dependency |

**`subprocess_client` API:**

```ocaml
type t
val start : tart_bin:string -> t
val send : t -> string -> unit
val recv : t -> Yojson.Safe.t
val recv_all : t -> timeout_ms:int -> Yojson.Safe.t list
val shutdown : t -> int  (* returns exit code *)
```

**Test cases (from spec):**

- Protocol lifecycle: initialize handshake, shutdown/exit,
  exit-without-shutdown, unknown method → -32601
- Document sync: didOpen publishes diagnostics, didOpen with error,
  didChange incremental, didClose clears diagnostics
- Edge cases: empty file, comment-only file

### 4. Wire into CI

Ensure `dune test` runs both the refactored in-process tests and the new
subprocess tests. The subprocess tests depend on `%{bin:tart}` so dune
builds the binary automatically.

**Changed files:**

| File | Change |
|------|--------|
| `test/lsp_integration/dune` | `(deps (source_tree %{project_root}/typings))`, `(action (chdir %{project_root} ...))` |

## Iteration Order

1. **Task 1** — Extract `Lsp_client` library
2. **Task 2** — Refactor `server_test.ml` (verify all 65+ tests still pass)
3. **Task 3** — Subprocess integration tests
4. **Task 4** — CI wiring (likely just works via dune)

Tasks 1-2 are the bulk of the work and deliver immediate value (less
duplication, easier to add tests). Task 3 adds subprocess coverage. Task 4
is mechanical.
