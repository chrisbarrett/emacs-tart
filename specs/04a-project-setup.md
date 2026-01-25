# Spec 04a: OCaml Project Setup

Initialize the OCaml project structure with dune build system.

**Dependencies:** None. Run this before Spec 04.

## Goal

Establish the OCaml project foundation with build system, testing, and pre-commit
hooks.

## Output

```
tart/
├── flake.nix              ; Nix flake (already exists)
├── dune-project           ; Project metadata
├── tart.opam              ; Generated package file
├── bin/
│   ├── dune
│   └── main.ml            ; Entry point (placeholder)
├── lib/
│   ├── dune
│   └── tart.ml            ; Library entry (placeholder)
├── test/
│   ├── dune
│   └── test_main.ml       ; Test runner
├── .ocamlformat           ; Formatting config
└── .pre-commit-config.yaml ; Pre-commit hooks
```

## Requirements

### R1: Dune project initialized

**Given** a fresh directory
**When** the project is set up
**Then** `dune build` succeeds with no errors

**Verify:** `dune build` exit code 0

### R2: Library and binary targets

**Given** the dune configuration
**When** built
**Then** it produces:
- Library `tart` (in `lib/`)
- Executable `tart` (in `bin/`)

**Verify:** `dune exec tart -- --help` runs without error

### R3: Test framework configured

**Given** the test directory
**When** `dune test` runs
**Then** tests execute using alcotest (or similar)

**Verify:** `dune test` passes with placeholder test

### R4: Dependencies declared

**Given** the dune-project and flake.nix
**When** `nix develop` is entered
**Then** required packages are available:
- `menhir` (parser generator)
- `ppx_deriving` (derive show, eq)
- `yojson` (JSON for LSP)
- `alcotest` (testing)

**Verify:** `nix develop -c dune build` succeeds

### R5: Formatting enforced

**Given** `.ocamlformat` config
**When** `dune fmt` runs
**Then** code is formatted consistently

**Verify:** `dune fmt --check` exits 0

### R6: Pre-commit hooks

**Given** `.pre-commit-config.yaml` configured
**When** a commit is attempted
**Then** pre-commit runs: `dune build`, `dune test`, `dune fmt --check`

**Verify:** `pre-commit run --all-files` passes

## Tasks

- [x] [R4] flake.nix configured with dependencies
- [x] [R1] Create dune-project with lang 3.0+
- [x] [R2] Set up lib/ and bin/ with dune files
- [x] [R3] Add test/ with alcotest
- [x] [R5] Add .ocamlformat
- [x] [R6] Add .pre-commit-config.yaml with build/test/format hooks

Run `nix develop -c sh -c 'dune build && dune test'` before proceeding to Spec 04.
