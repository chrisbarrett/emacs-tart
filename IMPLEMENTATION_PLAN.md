# Implementation Plan

Based on specs 07-15, this plan prioritizes tasks by dependencies and impact.

## Phase 1: Complete Signature System (Spec 07)

Foundation for module boundaries and type-checked code.

### 1.1 Function and Variable Loading

- [x] [R5] Load `defun` signatures into type environment
- [x] [R6] Load `defvar` declarations into type environment
- [x] Verify: Type checker uses loaded signatures for calls

### 1.2 Type Aliases

- [x] [R7] Load type aliases (with definition) into type context
- [x] [R8] Load parameterized type aliases with instantiation
- [x] Verify: Aliases expand correctly in type expressions

### 1.3 Opaque Types

- [x] [R9] Load opaque types (no definition) as distinct abstract types
- [x] [R10] Handle opaque types with phantom type parameters
- [x] Verify: Opaque types not unifiable with other types

### 1.4 Module Directives

- [x] [R12] Implement `open` directive (import types, not re-export)
- [x] [R13] Implement `include` directive (inline and re-export)
- [x] Verify: Opened types available but not exported

### 1.5 Struct Imports

- [x] [R11] Implement `import-struct` to generate type, constructor, predicate,
      accessors
- [x] Verify: Struct accessor calls type-check based on slot types

### 1.6 Signature Search Path

- [x] [R15] Implement `tart-type-path` search path configuration
- [x] [R16] Implement module discovery order (sibling, search path, stdlib)
- [x] Verify: `(require 'cl-lib)` loads signatures from search path

### 1.7 Stdlib Signatures

- [x] [R17] Create `stdlib/builtins.tart` with arithmetic, list, string,
      predicate signatures
- [x] [R17] Create `stdlib/cl-lib.tart` with basic cl-lib signatures
- [x] [R17] Create `stdlib/seq.tart` with seq.el signatures
- [x] Verify: Built-in calls type-check correctly

## Phase 2: Migrate to Prek (Spec 16)

Faster git hooks improve every development iteration.

- [x] [R1] Add prek to flake.nix devShell packages
- [x] [R2] Update any docs referencing pre-commit
- [x] [R3] Run `prek install -f` to reinstall hooks
- [x] [R4] Verify all hooks pass with prek

## Phase 3: Emacs LSP Integration (Specs 09, 10)

Enable hover and diagnostics in Emacs via eglot.

### 3.1 CLI LSP Command

- [x] [R9] Wire `tart lsp` to start LSP server on stdio
- [x] [R10] Add `--port` option for TCP mode
- [x] Verify: LSP client connects and receives diagnostics

### 3.2 Eglot Integration

- [x] [R5] Add eglot server configuration for tart
- [x] [R8] Add customization options (`tart-executable`, etc.)
- [x] Verify: `M-x eglot` connects and hover shows types

### 3.3 Basic Documentation

- [x] [R11] Add setup instructions to README
- [x] Verify: README contains installation and usage examples

## Phase 4: Forall Inference (Spec 15)

Reduces boilerplate in signature files.

### 4.1 Implicit Quantification

- [x] [R1] Collect type variables in left-to-right first-occurrence order
- [x] [R2] Explicit quantifiers disable inference; error on unbound vars
- [x] Verify: `(defun seq-map (((a -> b)) (seq a)) -> (list b))` infers `[a b]`

### 4.2 Edge Cases

- [x] [R3] Handle phantom type variables (only in return type)
- [x] [R4] Distinguish quoted literals from type variables
- [x] [R5] Traverse nested arrow types for variable collection
- [x] [R6] Deduplicate quantifier list
- [x] Verify: Inference handles all grammar productions

## Phase 5: Module Boundaries (Spec 12)

Enables type-checked module interactions.

### 5.1 Basic Module Loading

- [x] [R1] Enable type checking for any `.el` file via LSP
- [x] [R2] Verify implementations match `.tart` signatures
- [x] [R3] Load signatures from search path for required modules
- [x] Verify: Mismatched implementations produce errors

### 5.2 Public/Private Distinction

- [x] [R5] Distinguish public (in `.tart`) vs internal (not listed) functions
- [x] [R8] Warn on functions defined but not in signature file
- [x] Verify: Internal functions inferred but not exported

### 5.3 Module Dependencies

- [x] [R6] Load signatures when `(require 'module)` is encountered
- [x] [R7] Handle autoloaded function lookup via prefix-based search
- [x] [R9] Handle circular module dependencies with lazy loading
- [x] Verify: Required module signatures available for type checking

## Phase 6: Error Reporting (Spec 13)

Improve error message quality.

### 6.1 Type Mismatch Formatting

- [x] [R1] Format type mismatch errors with expected/found
- [x] [R2] Format branch type mismatch errors
- [x] [R3] Format Option/nil errors with suggestions
- [x] Verify: Errors show context and suggestions

### 6.2 Name Errors

- [x] [R4] Implement Levenshtein-based typo suggestions
- [x] [R5] Format arity mismatch errors with signature
- [x] [R6] Format signature mismatch errors with both locations
- [x] Verify: Typos suggest similar names

### 6.3 LSP Integration

- [x] [R7] Map rich diagnostics to LSP format with related info
- [x] Verify: LSP clients show related information

## Phase 7: Emacs REPL Integration (Spec 10)

Interactive development workflow. All tooling lives in `tart-mode.el`, separate
from the runtime macros in `tart.el`.

### 7.0 File Split

- [x] Split current `tart.el` into `tart.el` (macros only) and `tart-mode.el`
      (dev tooling)
- [x] Verify: `(require 'tart)` loads only macros, no eglot/comint deps

### 7.1 Inferior Mode

- [x] [R1,R2,R9] Implement `inferior-tart-mode` with comint
- [x] [R3] Implement send-to-REPL commands
- [x] [R4] Implement type/expand inspection commands
- [x] Verify: REPL interaction works from elisp buffers

### 7.2 Minor Mode

- [x] [R7] Implement `tart-mode` minor mode with keymap
- [x] [R10] Add compilation-mode error parsing
- [x] Verify: Keybindings work, errors are clickable

## Phase 8: tart.el Runtime (Spec 14)

Inline type annotations in `.el` files. The runtime macros live in `tart.el`
(minimal, no dependencies). Development tooling is in `tart-mode.el` (see
Phase 7).

### 8.1 Macro Definitions (in tart.el)

- [x] [R8] Implement `tart` macro (expands to form)
- [x] [R8] Implement `tart-type` macro (expands to nil)
- [x] [R8] Implement `tart-declare` macro (expands to nil)
- [x] Verify: Macros expand correctly at runtime

### 8.2 Type Checker Recognition

- [x] [R1] Recognize `(declare (tart ...))` in function definitions
- [x] [R2] Add expression annotation checking (`(tart TYPE FORM)`)
- [x] [R3,R4] Track variable types from annotations
- [x] [R3] Check `setq`/`setf` against declared variable types
- [x] Verify: Type errors on annotation mismatches

### 8.3 Type Aliases

- [x] [R5,R6] Implement file-local type alias scope
- [x] [R7] Enforce invariance for parameterized types
- [x] Verify: Aliases usable in same file, not exported

### 8.4 Integration

- [x] [R10] Verify inline annotations match `.tart` declarations
- [x] [R9] Format error messages for annotation mismatches
- [x] Verify: Mismatched inline/`.tart` produces error

## Phase 9: ADT System (Spec 11)

Runtime representation and pattern matching.

### 9.1 Code Generation

- [x] [R1] Generate constructor functions from ADT definitions
- [x] [R2] Generate predicate functions
- [x] [R3] Generate accessor functions
- [x] [R6] Handle multi-field constructors (vectors)
- [x] [R7] Test recursive type handling
- [x] Verify: ADT construction/access works at runtime

### 9.2 Type Checking

- [x] [R4] Implement pcase type narrowing in branches
- [x] [R5] Implement exhaustiveness checking with warnings
- [x] Verify: Non-exhaustive matches produce warnings

## Phase 10: LSP Incremental (Spec 08)

Performance optimization for large codebases.

### 10.1 Query-Based Caching

- [x] [R9] Implement query-based caching for incremental type checking
- [x] Verify: Edit one function; others not recomputed (log check)

## Dependencies

```
Spec 07 (Signatures) ──┬─> Spec 15 (Forall Inference)
                       ├─> Spec 12 (Module Boundaries)
                       ├─> Spec 14 (tart.el Runtime)
                       └─> Spec 11 (ADT System)

Spec 08 (LSP) ─────────┬─> Spec 09 (CLI) ──> Spec 10 (Emacs)
                       └─> Spec 13 (Error Reporting)
```

## Phase 11: LSP Navigation Features (Spec 08 Phase 2)

Navigation and code intelligence features for IDE productivity.

### 11.1 Go to Definition

- [x] [R12] Implement `textDocument/definition` for function calls
- [x] [R13] Return definition location from defun spans
- [x] [R14] Handle cross-file definitions via signature lookup
- [x] Verify: Clicking on function name jumps to definition

### 11.2 Find References

- [x] [R15] Implement `textDocument/references` for symbols
- [x] [R16] Collect all references to a symbol across the document
- [x] Verify: Shows all usages of a function/variable

### 11.3 Code Actions

- [x] [R17] Implement `textDocument/codeAction` framework
- [x] [R18] Add "Extract function" refactoring
- [x] [R19] Add "Add type annotation" quickfix
- [x] Verify: Code actions appear on type errors

## Phase 12: LSP Completion and Symbols (Future)

Auto-completion and document structure for improved IDE experience.

### 12.1 Document Symbols

- [x] Implement `textDocument/documentSymbol` for outline view
- [x] Return defun, defvar, defconst declarations with their types
- [x] Include nested defuns (inner functions)
- [ ] Verify: Emacs imenu/outline shows document structure

### 12.2 Auto-Completion

- [x] Implement `textDocument/completion` for symbol names
- [x] Complete local variables in scope
- [x] Complete functions from loaded signatures (stdlib, requires)
- [x] Include type information in completion items
- [ ] Verify: Typing prefix shows completion candidates with types

### 12.3 Signature Help

- [x] Implement `textDocument/signatureHelp` for function calls
- [x] Show function signature when cursor is in argument list
- [x] Highlight current parameter position
- [ ] Verify: Typing `(mapcar |` shows signature with first param highlighted

### 12.4 Symbol Rename

- [x] Implement `textDocument/rename` for local symbols
- [x] Rename variables and function definitions within file
- [x] Verify: Renaming updates all references consistently

## Phase 13: Expanded Stdlib (Future)

Broader coverage of common Emacs packages.

### 13.1 Buffer and Window Operations

- [x] Add `stdlib/buffers.tart` for buffer manipulation functions
- [x] Add `stdlib/windows.tart` for window management
- [ ] Add `stdlib/frames.tart` for frame operations
- [ ] Verify: Buffer/window code type-checks correctly

### 13.2 File Operations

- [ ] Add `stdlib/files.tart` for file I/O
- [ ] Add `stdlib/directories.tart` for directory operations
- [ ] Verify: File manipulation code type-checks

### 13.3 Text Properties and Overlays

- [ ] Add `stdlib/text-properties.tart`
- [ ] Add `stdlib/overlays.tart`
- [ ] Verify: Text property code type-checks

### 13.4 Common Packages

- [ ] Add `stdlib/dash.tart` for dash.el
- [ ] Add `stdlib/s.tart` for s.el
- [ ] Add `stdlib/f.tart` for f.el
- [ ] Verify: Code using these packages type-checks

## Priority Order

1. **Phase 1**: Signature system (complete)
2. **Phase 2**: Prek migration - faster hooks improve every iteration
3. **Phase 3**: Emacs LSP integration - enables interactive development
4. **Phase 4**: Forall inference - reduces friction in writing signatures
5. **Phase 5**: Module boundaries - enables multi-file projects
6. **Phase 6**: Error reporting - improves developer experience
7. **Phase 7**: Emacs REPL - richer interactive workflow
8. **Phase 8**: Runtime features, ADTs
9. **Phase 9-10**: Performance and caching
10. **Phase 11**: LSP navigation features (go to definition, find references, code actions)
11. **Phase 12**: LSP completion and symbols (future)
12. **Phase 13**: Expanded stdlib coverage (future)
