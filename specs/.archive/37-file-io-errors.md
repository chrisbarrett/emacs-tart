# Spec 37: File I/O Errors

Structured handling of file I/O errors with clear messages and actionable
suggestions.

- **Dependencies:**
  - [Spec 35](./35-structured-errors.md) (structured-errors)

## Goal

When users encounter file errors (not found, permission denied, etc.), they
immediately understand what went wrong and how to fix it.

## Constraints

| Constraint | Detail                                                         |
| ---------- | -------------------------------------------------------------- |
| Structured | Uses error types from [Spec 35](./35-structured-errors.md), not raw exceptions              |
| Contextual | Errors include what operation was being attempted              |
| Actionable | Suggests fixes for common mistakes (typos, missing extensions) |
| Consistent | Follows error format from [Spec 13](./13-error-reporting.md)                              |

## Output

```
lib/
└── errors/
    ├── file_error.ml      ; File error types and formatting
    └── file_error.mli
bin/main.ml                ; Updated to use file error handling
lib/sig/search_path.ml     ; Updated to return structured errors
lib/syntax/read.ml         ; Updated to return structured errors
```

## Requirements

### R1: File not found with suggestions

**Given** a file path `foo.el` that does not exist **When** the user runs
`tart check foo.el` **Then** error includes the path and suggests similar files
if any exist in the same directory

```
error[E0001]: file not found
  --> foo.el
   |
   | Cannot open 'foo.el': No such file or directory
   |
help: did you mean one of these?
   | - foobar.el
   | - foo-mode.el
```

**Verify:** `./tart check nonexistent.el` shows structured error message

### R2: Permission denied errors

**Given** a file `restricted.el` without read permissions **When** the user runs
`tart check restricted.el` **Then** error clearly states permission issue

```
error[E0002]: permission denied
  --> restricted.el
   |
   | Cannot read 'restricted.el': Permission denied
   |
help: check file permissions with: ls -la restricted.el
```

**Verify:** `dune test`; permission denied error is formatted correctly

### R3: Directory instead of file

**Given** a path `mydir/` that is a directory **When** the user runs
`tart check mydir/` **Then** error explains that a file was expected

```
error[E0003]: expected file, found directory
  --> mydir/
   |
   | 'mydir/' is a directory, not a file
   |
help: to check all .el files in a directory, use: tart check mydir/*.el
```

**Verify:** `./tart check lib/` shows directory error with suggestion

### R4: Signature file not found

**Given** a `.el` file that references a module without a `.tart` signature
**When** the search path cannot locate `module.tart` **Then** error indicates
the missing signature and search locations tried

```
error[E0004]: signature file not found
  --> init.el:5:1
   |
 5 | (require 'my-module)
   | ^^^^^^^^^^^^^^^^^^^^
   |
note: searched for 'my-module.tart' in:
   | - ./my-module.tart (sibling)
   | - typings/emacs/31.0/c-core/
   | - typings/emacs/31/c-core/
   |
help: create a signature file at ./my-module.tart
```

**Verify:** `dune test`; missing signature produces clear error

### R5: --load file not found in expand command

**Given** `tart expand --load macros.el file.el` where `macros.el` does not
exist **When** the command is executed **Then** error indicates which --load
file was not found

```
error[E0001]: file not found
  --> macros.el (from --load)
   |
   | Cannot open 'macros.el': No such file or directory
```

**Verify:** `./tart expand --load nonexistent.el test.el` shows specific error

### R6: Read error with context

**Given** a file read that fails partway through (e.g., disk error) **When** the
read operation fails **Then** error includes file path and operation context

```
error[E0005]: read error
  --> large-file.el
   |
   | Error reading 'large-file.el': Input/output error
   |
note: occurred while parsing file
```

**Verify:** `dune test`; I/O errors during read are caught and formatted

### R7: Similar filename suggestions

**Given** a typo in a filename like `init.l` instead of `init.el` **When** the
file is not found **Then** error suggests files with similar names (Levenshtein
distance <= 2)

```
error[E0001]: file not found
  --> init.l
   |
   | Cannot open 'init.l': No such file or directory
   |
help: did you mean: init.el
```

**Verify:** `dune test`; typo suggestions use Levenshtein matching

### R8: Missing .el extension suggestion

**Given** a file path `config` where `config.el` exists **When** the user runs
`tart check config` **Then** error suggests adding the `.el` extension

```
error[E0001]: file not found
  --> config
   |
   | Cannot open 'config': No such file or directory
   |
help: did you mean: config.el
```

**Verify:** `./tart check foo` suggests `foo.el` if it exists

### R9: Structured error type

**Given** the file error module **When** errors are created **Then** they use a
structured type integrating with [Spec 35](./35-structured-errors.md)

```ocaml
type file_error =
  | File_not_found of { path : string; suggestions : string list }
  | Permission_denied of { path : string }
  | Is_directory of { path : string }
  | Read_error of { path : string; message : string }
  | Signature_not_found of { module_name : string; search_paths : string list; span : Location.span option }
```

**Verify:** `dune build`; error type compiles

### R10: Error code mapping

**Given** file errors **When** formatted for display **Then** each error type
has a consistent error code

| Code  | Error Type          |
| ----- | ------------------- |
| E0001 | File not found      |
| E0002 | Permission denied   |
| E0003 | Is directory        |
| E0004 | Signature not found |
| E0005 | Read error          |

**Verify:** `dune test`; error codes are consistent

## Non-Requirements

- Recovery from transient I/O errors (retry logic)
- Network file system specific handling
- Symbolic link resolution messages

## Tasks

- [x] [R9] Create `lib/errors/file_error.ml` with structured error type
- [x] [R10] Implement error code mapping and formatting
- [x] [R7] Add Levenshtein-based filename suggestions (reuse from [Spec 13](./13-error-reporting.md) if
      available)
- [x] [R1, R8] Implement file not found with suggestions
- [x] [R2] Implement permission denied handling
- [x] [R3] Implement directory detection
- [x] [R6] Implement read error wrapping
- [x] [R4] Update `Search_path` to return structured errors for missing
      signatures
- [x] [R5] Update `bin/main.ml` expand command to use file errors
- [x] Update `lib/syntax/read.ml` to wrap I/O exceptions
- [x] Add tests for all error scenarios

## Status

Complete
