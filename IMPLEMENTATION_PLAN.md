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
- [x] Verify: Emacs imenu/outline shows document structure (manual)

### 12.2 Auto-Completion

- [x] Implement `textDocument/completion` for symbol names
- [x] Complete local variables in scope
- [x] Complete functions from loaded signatures (stdlib, requires)
- [x] Include type information in completion items
- [x] Verify: Typing prefix shows completion candidates with types (manual)

### 12.3 Signature Help

- [x] Implement `textDocument/signatureHelp` for function calls
- [x] Show function signature when cursor is in argument list
- [x] Highlight current parameter position
- [x] Verify: Typing `(mapcar |` shows signature with first param highlighted (manual)

### 12.4 Symbol Rename

- [x] Implement `textDocument/rename` for local symbols
- [x] Rename variables and function definitions within file
- [x] Verify: Renaming updates all references consistently

## Phase 13: Expanded Stdlib

Broader coverage of common Emacs packages.

### 13.1 Buffer and Window Operations

- [x] Add `stdlib/buffers.tart` for buffer manipulation functions
- [x] Add `stdlib/windows.tart` for window management
- [x] Add `stdlib/frames.tart` for frame operations
- [x] Verify: Buffer/window/frame code type-checks correctly

### 13.2 File Operations

- [x] Add `stdlib/files.tart` for file I/O (includes directory operations)
- [x] Verify: File manipulation code type-checks

### 13.3 Text Properties and Overlays

- [x] Add `stdlib/text-properties.tart`
- [x] Add `stdlib/overlays.tart`
- [x] Verify: Text property code type-checks

### 13.4 Common Packages

- [x] Add `stdlib/dash.tart` for dash.el
- [x] Add `stdlib/s.tart` for s.el
- [x] Add `stdlib/f.tart` for f.el
- [x] Verify: Code using these packages type-checks

## Phase 14: Higher-Kinded Types (Spec 17)

Enable polymorphism over type constructors via kind inference.

### 14.1 Kind Representation

- [x] [R1] Add `kind` type to lib/typing/kind.ml
- [x] [R1] Add kind comparison and pretty-printing
- [x] Verify: `dune test`; kinds construct and compare

### 14.2 Kind Defaulting

- [x] [R5] Default existing type variables to kind `*`
- [x] Verify: Existing signatures unchanged

### 14.3 Kind Inference

- [x] [R2] Implement kind inference algorithm in kind_infer.ml
- [x] [R3] Add kind checking to type applications
- [x] Verify: HK type variable `f` inferred as `* -> *`

### 14.4 Explicit Annotations and Errors

- [x] [R4] Parse explicit kind annotations `(f : (* -> *))`
- [x] [R6] Implement kind error formatting
- [x] Verify: Kind mismatch shows expected/found

### 14.5 HK Instantiation

- [x] [R7] Update unification for HK instantiation
- [x] [R8] Test nested/partial type constructors
- [x] Verify: `fmap` instantiation preserves type safety

## Phase 15: Explicit Type Instantiation (Spec 18)

Enable explicit instantiation of polymorphic types at call sites.

### 15.1 Runtime Macro

- [x] [R5] Add `@type` macro to tart.el (expands to function call)
- [x] Verify: ERT tests; `(@type [int] identity 42)` expands to `(identity 42)`

### 15.2 Basic Instantiation

- [x] [R1] Recognize `@type` forms in infer.ml
- [x] [R1] Parse type arguments from vector using sig_parser
- [x] [R1] Apply explicit type arguments during inference
- [x] Verify: `(@type [int] identity 42)` type-checks

### 15.3 Partial Instantiation

- [x] [R3] Handle `_` placeholder for partial instantiation
- [x] Verify: `(@type [_ string] pair 1 "hi")` infers first arg

### 15.4 HK Instantiation

- [x] [R2] Test HK type constructor instantiation
- [x] Verify: `(@type [list int string] fmap ...)` works

### 15.5 Error Handling

- [x] [R4] Validate type argument arity
- [x] [R6] Format error messages with annotation context
- [x] Verify: Wrong arity and type mismatch errors show context

---

## Phase 16: Scoped Type Variables (Spec 19)

Enable type variables to be shared across multiple signatures within a scope.

### 16.1 Parsing and AST

- [x] [R1] Add TypeScope variant to sig_ast.ml
- [x] [R1] Parse type-scope blocks in sig_parser.ml
- [x] Verify: `dune test`; scoped blocks parse correctly

### 16.2 Scope Variable Binding

- [x] [R2] Implement scope variable binding during loading
- [x] [R3] Handle explicit forall inside scope (merges with scope vars)
- [x] [R8] Validate variable binding in scopes (error on unbound)
- [x] Verify: Scoped variables shared; external independent

### 16.3 Advanced Features

- [x] [R4] Integrate with kind inference for HK scoped variables
- [x] [R5] Implement nested scope shadowing
- [x] [R6] Handle opaque types in scopes
- [x] Verify: HK scopes and nesting work correctly

### 16.4 Integration

- [x] [R7] Export scoped declarations with correct polymorphic types
- [x] Verify: Exported functions usable from other modules

---

## Phase 17: Expanded Stdlib Phase 2 (Spec 20)

Additional commonly-used Emacs libraries for broader type checking coverage.

### 17.1 Hash Tables and Maps

- [x] [R1] Create stdlib/ht.tart with ht.el hash table library signatures
- [x] [R2] Create stdlib/map.tart with map.el generic map operations
- [x] Verify: ht.tart and map.tart parse and load

### 17.2 String and Utility Functions

- [x] [R3] Create stdlib/subr-x.tart with subr-x.el signatures
- [x] [R5] Create stdlib/rx.tart with rx-to-string signature
- [x] Verify: subr-x.tart and rx.tart parse and load

### 17.3 Process Management

- [x] [R4] Create stdlib/process.tart with process function signatures
- [x] Verify: process.tart parses and loads

### 17.4 Expanded cl-lib

- [x] [R6] Expand stdlib/cl-lib.tart with additional functions
- [x] Verify: Expanded cl-lib.tart loads

---

## Future Work (Requires New Specs)

The following areas are mentioned as future work in the specs:

1. **Additional Stdlib Coverage**
   - More Emacs packages (org-mode, magit, etc.)
   - Additional third-party package signatures

2. **Type Classes / Traits** (mentioned in Spec 17 Non-Requirements)
   - Typeclass/trait syntax
   - Associated types
   - Type families

## Priority Order

1. **Phase 1**: Signature system ✓
2. **Phase 2**: Prek migration ✓
3. **Phase 3**: Emacs LSP integration ✓
4. **Phase 4**: Forall inference ✓
5. **Phase 5**: Module boundaries ✓
6. **Phase 6**: Error reporting ✓
7. **Phase 7**: Emacs REPL ✓
8. **Phase 8**: tart.el Runtime ✓
9. **Phase 9**: ADT system ✓
10. **Phase 10**: LSP incremental ✓
11. **Phase 11**: LSP navigation features ✓
12. **Phase 12**: LSP completion and symbols ✓
13. **Phase 13**: Expanded stdlib ✓
14. **Phase 14**: Higher-Kinded Types ✓
15. **Phase 15**: Explicit Type Instantiation ✓
16. **Phase 16**: Scoped Type Variables ✓
17. **Phase 17**: Expanded Stdlib Phase 2 ✓
