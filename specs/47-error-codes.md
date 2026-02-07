# Spec 47: Error Code Registry

Central manifest of all tart error codes.

**Dependencies:** [Spec 13][] (error reporting), [Spec 35][] (structured errors)

## Links

### Deps
[Spec 13]: ./13-error-reporting.md
[Spec 35]: ./35-structured-errors.md

### Blocks
[Spec 50]: ./50-version-constraints.md
[Spec 51]: ./51-diagnostic-severity.md

## Goal

Define a canonical registry of error codes. All diagnostics reference this spec
as the source of truth. Codes are sequential from E0001.

## Constraints

- **Sequential**: Codes start at E0001, no gaps
- **Stable**: Once assigned, codes are never reassigned
- **Categorized**: Codes grouped by error category
- **Single source**: Implementation and docs derive from this spec

## Error Code Registry

### Type Errors (E0001–E0099)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0001 | TypeMismatch        | Expected one type, found another               |
| E0002 | BranchMismatch      | If/cond branches have incompatible types       |
| E0003 | InfiniteType        | Occurs check failed (recursive type)           |
| E0004 | SignatureMismatch   | Implementation doesn't match declared signature|
| E0005 | AnnotationMismatch  | Expression doesn't match tart annotation       |
| E0006 | ReturnMismatch      | Function body doesn't match declared return    |
| E0007 | UnificationFailed   | Types cannot be unified                        |
| E0008 | DisjointEquality    | eq/eql args are provably disjoint              |

### Name Errors (E0100–E0199)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0100 | UndefinedVariable   | Variable not in scope                          |
| E0101 | UndefinedFunction   | Function not in scope                          |
| E0102 | UndefinedType       | Type not in scope                              |
| E0103 | UndefinedField      | Field not present in record type               |
| E0104 | MissingSignature    | Function defined but not in .tart file         |
| E0105 | AmbiguousName       | Name resolves to multiple definitions          |

### Arity Errors (E0200–E0299)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0200 | WrongArity          | Wrong number of arguments to function          |
| E0201 | WrongTypeArity      | Wrong number of type arguments                 |
| E0202 | MissingRequired     | Required argument not provided                 |
| E0203 | UnknownKeyword      | Unknown keyword argument                       |

### Kind Errors (E0300–E0399)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0300 | KindMismatch        | Expected one kind, found another               |
| E0301 | InfiniteKind        | Occurs check failed at kind level              |
| E0302 | TypeArityMismatch   | Type constructor applied to wrong # of args    |

### Pattern Errors (E0400–E0499)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0400 | NonExhaustive       | Pattern match doesn't cover all cases          |
| E0401 | RedundantPattern    | Pattern can never match (already covered)      |
| E0402 | InvalidPattern      | Pattern syntax not supported                   |

### Row/Record Errors (E0500–E0599)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0500 | MissingField        | Required field not present                     |
| E0501 | DuplicateField      | Field specified multiple times                 |
| E0502 | RowMismatch         | Row types cannot be unified                    |
| E0503 | ClosedRowExtra      | Extra field in closed row type                 |

### Union Errors (E0600–E0699)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0600 | UnionMismatch       | Value doesn't match any union variant          |
| E0601 | EmptyUnion          | Type subtraction produced empty type           |
| E0602 | AmbiguousVariant    | Cannot determine which union variant           |

### Module Errors (E0700–E0799)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0700 | MissingModule       | Required module not found                      |
| E0701 | CircularDependency  | Modules have circular require                  |
| E0702 | SignatureNotFound   | No .tart signature file found                  |

### File Errors (E0800–E0899)

| Code  | Name                | Description                                    |
|-------|---------------------|------------------------------------------------|
| E0800 | FileNotFound        | Source file does not exist                     |
| E0801 | FileUnreadable      | Source file cannot be read                     |
| E0802 | ParseError          | Syntax error in source file                    |

## Requirements

### R1: Implementation derives from spec

**Given** the error code registry above
**When** implementing `lib/typing/diagnostic.ml`
**Then** error_code type matches this spec exactly

**Verify:** `dune test`; codes match between spec and impl

### R2: Unique code per error

**Given** an error condition
**Then** it has exactly one error code

**Verify:** No error condition maps to multiple codes

### R3: Stable codes

**Given** a code is assigned (e.g., E0001 = TypeMismatch)
**When** the error is renamed or refined
**Then** the code remains the same

**Verify:** Code assignments never change in git history

### R4: Error messages reference codes

**Given** any tart diagnostic output
**Then** it includes the error code: `error[E0001]: ...`

**Verify:** All error output includes bracketed code

## Status

Complete. All implemented codes match this registry. E0008 (DisjointEquality)
was added after initial spec draft. Clause diagnostics intentionally lack error
codes — they carry user-authored advisory messages from `.tart` signatures, not
compiler error conditions. Future codes (E0103, E0105, E0202, E0203, E0401,
E0402, E0500–E0503, E0600–E0602, E0700, E0701, E0800–E0802) are reservations
for features not yet implemented.

## Tasks

- [x] [R1] Update diagnostic.ml to use new codes
- [x] [R2] Ensure 1:1 mapping of conditions to codes
- [x] [R3] Add test preventing code reassignment
- [x] [R4] Verify all diagnostics include codes
