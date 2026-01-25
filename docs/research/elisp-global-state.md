# Emacs Lisp Global State: Typing Implications

This document analyses Emacs Lisp's global state mechanisms and their
implications for static type analysis. Each category includes a tractability
rating and recommendations for the type system.

## 1. Point and Mark

### State Tracked

- **Point**: Current cursor position in the current buffer, an integer >= 1
- **Mark**: The other end of the region, also an integer or nil
- **Region**: The area between point and mark

### Key Functions

```elisp
(point)                    ; -> integer
(point-min)               ; -> integer
(point-max)               ; -> integer
(mark)                    ; -> integer or nil
(mark-marker)             ; -> marker
(region-beginning)        ; -> integer
(region-end)              ; -> integer
```

### Movement Commands

Point is mutated implicitly by many functions:

```elisp
(goto-char POSITION)      ; sets point
(forward-char N)          ; moves point
(beginning-of-line)       ; moves point
(insert STRING)           ; moves point after inserted text
(delete-char N)           ; changes buffer and point relationships
```

### Type Implications

**Return types** are straightforward:
- `point`, `point-min`, `point-max` -> `Integer`
- `mark` -> `(Option Integer)` or `(U Integer Null)`
- Movement functions -> `Void` or `Unit`

**Challenges**:
1. Point validity depends on buffer contents: `1 <= point <= (point-max)`
2. After buffer modifications, previously-obtained point values may be invalid
3. Movement commands have side effects on the implicit current buffer

### Tractability: Tractable (for basic typing)

**Rationale**: Return types are well-defined. The relationship between point
values and buffer state is a refinement type problem beyond v1 scope.

**Approach**:
- Type movement functions as `(-> Void)`
- Type position-returning functions as `(-> Integer)`
- Do not attempt to track point validity (would require dependent types)
- Document that `save-excursion` restores point but this is not type-tracked


## 2. Match Data

### State Tracked

The match data is a vector of buffer positions storing the results of the most
recent regexp match:

- Group 0: entire match
- Groups 1-N: capture groups
- Each group has beginning and end positions

### Key Functions

```elisp
(match-string N)          ; -> string or nil
(match-string-no-properties N)  ; -> string or nil
(match-beginning N)       ; -> integer or nil
(match-end N)             ; -> integer or nil
(match-data)              ; -> list of markers/integers
(set-match-data LIST)     ; sets match data
```

### Implicit State Dependency

Match data is silently overwritten by any regexp operation:

```elisp
(when (string-match "\\(foo\\)" str)
  (do-something)          ; if this calls string-match, match data is clobbered
  (match-string 1 str))   ; may return wrong result or nil
```

### Save/Restore Pattern

```elisp
(save-match-data
  (string-match ...)      ; match data restored on exit
  (match-string 1))
```

### Type Implications

**Direct typing challenges**:
- `match-string` return depends on whether a match occurred
- Group number validity depends on the regexp used
- Match data can be invalidated by unrelated code

**The fundamental problem**: There is no static connection between the regexp
pattern and the match data access. The number of groups is determined by the
regexp string at runtime.

### Tractability: Intractable (for precise typing)

**Rationale**: The match data is implicit global state modified by many
functions. Tracking which regexp established the current match data and how
many groups it contains would require:
1. Dependent types keyed on the regexp string
2. Effect tracking for all regexp operations
3. Flow-sensitive analysis across function boundaries

**Approach for v1**:
- Type `match-string` as `(-> Integer (Option String))`
- Type `match-beginning`/`match-end` as `(-> Integer (Option Integer))`
- Accept that nil checks are required even when logically unnecessary
- Consider: special typing for common `(when (string-match ...) (match-string 1))` pattern via occurrence typing, but this is fragile


## 3. Current Buffer

### State Tracked

The "current buffer" is an implicit parameter to most buffer operations:

```elisp
(current-buffer)          ; -> buffer
(set-buffer BUFFER)       ; changes current buffer
(with-current-buffer BUF BODY...)  ; temporarily changes buffer
```

### Buffer-Local Variables

Variables can have buffer-local bindings:

```elisp
(defvar-local my-var nil)           ; always buffer-local
(make-local-variable 'my-var)       ; make buffer-local in current buffer
(buffer-local-value 'var buffer)    ; access specific buffer's binding
```

### Implicit Dependencies

Many functions implicitly operate on the current buffer:

```elisp
(point)                   ; position in current buffer
(buffer-string)           ; contents of current buffer
(insert ...)              ; modifies current buffer
(goto-char ...)           ; moves point in current buffer
```

### Type Implications

**Direct typing**:
- `current-buffer` -> `Buffer`
- `with-current-buffer` -> `(-> Buffer (-> A) A)` approximately

**Challenges**:
1. Most buffer operations have an implicit `Buffer` parameter
2. Buffer-local variables may have different values (or not exist) in different buffers
3. The current buffer can change at any time via `set-buffer`

### Tractability: Hard

**Rationale**: The current buffer is pervasive implicit state. Full tracking
would require:
- Effect system marking "reads current buffer" / "writes current buffer"
- Dependent types for buffer-local variable access
- Flow analysis for `set-buffer` and `with-current-buffer`

**Approach for v1**:
- Type `current-buffer` as `(-> Buffer)`
- Type `with-current-buffer` to ensure body receives buffer context
- Do not attempt to track buffer-local variable types per-buffer
- Treat buffer-local variables as having their declared type (unsound if different buffers have different types)


## 4. Selected Window and Frame

### State Tracked

- **Selected window**: The window where editing occurs
- **Selected frame**: The frame containing the selected window

```elisp
(selected-window)         ; -> window
(selected-frame)          ; -> frame
(select-window WINDOW)    ; changes selected window
(select-frame FRAME)      ; changes selected frame
```

### Temporary Selection Patterns

```elisp
(with-selected-window WINDOW BODY...)
(with-selected-frame FRAME BODY...)
(save-selected-window BODY...)
```

### Window/Frame Types

Windows and frames are opaque types with accessor functions:

```elisp
(window-buffer WINDOW)    ; -> buffer
(window-point WINDOW)     ; -> integer
(frame-selected-window)   ; -> window
(window-frame WINDOW)     ; -> frame
```

### Type Implications

**Direct typing is straightforward**:
- `Window` and `Frame` as opaque types
- Accessor functions have clear signatures
- `with-selected-*` forms are higher-order

**Challenges**:
1. Many commands implicitly use selected window (similar to current buffer)
2. Window/buffer relationships can change
3. Windows can be deleted, making previously-obtained window values invalid

### Tractability: Tractable

**Rationale**: Window and frame types are well-defined. The implicit
dependencies are similar to current buffer but less pervasive. Liveness is a
refinement concern.

**Approach**:
- Define `Window` and `Frame` as opaque types
- Type accessor functions normally
- Type `with-selected-window` as `(-> Window (-> A) A)`
- Do not track window liveness (would require linear types)


## 5. Dynamic Variables

### Mechanism

Emacs Lisp uses dynamic scope for variables declared with `defvar`:

```elisp
(defvar my-var nil "Documentation")  ; declares dynamic variable

(let ((my-var 42))                   ; dynamically binds my-var
  (some-function))                    ; sees my-var = 42

(setq my-var 10)                     ; sets global value
```

### defvar Without Value

A common pattern declares a variable without initializing it:

```elisp
(defvar some-external-var)           ; declared but not defined here
```

This signals "this variable exists and is defined elsewhere."

### let-Binding for Dynamic Scope

```elisp
(let ((case-fold-search nil)         ; affect all searches in body
      (default-directory "/tmp/"))   ; affect file operations in body
  BODY...)
```

### Tracking Dynamic Context

Functions may behave differently based on dynamically-bound variables:

```elisp
(defun my-search (pattern)
  (re-search-forward pattern))       ; behavior depends on case-fold-search
```

### Type Implications

**Direct typing challenges**:
1. A function's behavior depends on variables it doesn't lexically reference
2. `let` can bind any variable, changing downstream behavior
3. The type of a dynamic variable may effectively vary by call site

**Example problem**:
```elisp
(defvar my-handler nil)              ; sometimes a function, sometimes nil

(defun call-handler ()
  (when my-handler
    (funcall my-handler)))           ; type of my-handler varies
```

### Tractability: Hard

**Rationale**: Dynamic scope means any function can depend on non-local state.
Full tracking would require:
- Effect annotations for all dynamically-referenced variables
- Propagating dynamic context through call chains
- Handling `let`-bindings as context modifications

**Approach for v1**:
- Type dynamic variables with their declared type
- Assume dynamic variables maintain declared types (unsound if rebinding with different type)
- Consider: effect annotation for common dynamic dependencies like `default-directory`
- Special-case important variables (e.g., `case-fold-search : Boolean`)


## 6. Global State Patterns

### Mode Variables

Major and minor modes set global or buffer-local variables:

```elisp
(defvar org-mode nil)                ; non-nil when org-mode active
(defvar text-mode-hook nil)          ; hooks for text-mode

;; Mode activation
(org-mode)                           ; sets up buffer state
```

### Configuration Variables

User configuration via `defvar`:

```elisp
(defvar my-package-enabled t)
(defvar my-package-options '(:foo 1 :bar 2))
```

### defcustom Options

User-customizable variables with type specifications:

```elisp
(defcustom my-option nil
  "Documentation."
  :type '(choice (const nil)
                 (string :tag "Name"))
  :group 'my-package)
```

The `:type` specification provides type information we could potentially use.

### Type Implications

**Opportunities**:
- `defcustom :type` provides structured type information
- Mode variables often have boolean semantics
- Many configuration patterns are conventional

**Challenges**:
1. Mode variables may not have explicit type declarations
2. Configuration can be any Lisp value
3. `defcustom :type` is a widget spec, not a type spec (though related)

### Tractability: Partially Tractable

**Rationale**: Conventional patterns exist but aren't enforced.

**Approach**:
- Parse `defcustom :type` and translate to internal types where possible
- Type mode predicates (e.g., `derived-mode-p`) as `(-> Symbol Boolean)`
- Allow type annotations on `defvar` forms
- Default untyped `defvar` to declared initial value's type or `Any`


## 7. Process and File State

### default-directory

The current working directory for file operations:

```elisp
(defvar default-directory "/home/user/")

(let ((default-directory "/tmp/"))
  (find-file "foo.txt"))             ; opens /tmp/foo.txt
```

### Process Buffers

Processes are associated with buffers:

```elisp
(start-process NAME BUFFER PROGRAM &rest ARGS)
(process-buffer PROCESS)             ; -> buffer or nil
(get-buffer-process BUFFER)          ; -> process or nil
```

### File Handlers

File operations can be intercepted:

```elisp
(defvar file-name-handler-alist ...)

;; Handlers can make local files act like remote (TRAMP)
(find-file "/ssh:host:file")
```

### Type Implications

**Process types**:
- `Process` as an opaque type
- Process state (running, exited, etc.) as a refinement concern
- Process-buffer relationships can change

**File operations**:
- `default-directory` affects file path resolution
- File handlers can intercept any file operation
- Return values may differ based on handlers

### Tractability: Hard (for full tracking), Tractable (for basic types)

**Rationale**: Basic types for processes and files are straightforward. The
dynamic binding of `default-directory` and file handler interception make
precise tracking difficult.

**Approach**:
- Define `Process` as an opaque type
- Type file operations with their documented signatures
- Do not attempt to track `default-directory` effects
- Do not attempt to model file handler interception


## Summary Table

| Category            | Tractability | Approach                                    |
|---------------------|--------------|---------------------------------------------|
| Point and mark      | Tractable    | Simple types; ignore validity constraints   |
| Match data          | Intractable  | Option types; accept false positives        |
| Current buffer      | Hard         | Implicit parameter; unsound buffer-locals   |
| Selected window     | Tractable    | Opaque types; standard accessors            |
| Dynamic variables   | Hard         | Declared types; unsound rebinding           |
| Global state        | Partial      | Leverage defcustom; conventions             |
| Process/file state  | Hard/Tract.  | Basic types; ignore dynamic effects         |


## Recommendations for v1

### Do Type (Tractable)
1. Point/mark positions as `Integer`
2. Window and Frame as opaque types with typed accessors
3. Process as an opaque type
4. Buffer as an opaque type

### Accept Unsoundness (Hard, Pragmatic)
1. Buffer-local variables: assume declared type everywhere
2. Dynamic variables: assume declared type is maintained
3. Current buffer operations: type without tracking buffer identity

### Avoid (Intractable)
1. Match data group validity (would need dependent types)
2. Point validity relative to buffer size (refinement types)
3. Window/buffer liveness (linear types)
4. File handler effects (too dynamic)

### Future Considerations
1. **Effect annotations**: Mark functions that depend on specific dynamic state
2. **Occurrence typing**: `(when (match-string 1) ...)` could refine to non-nil
3. **Buffer context types**: `(with-current-buffer buf ...)` could provide guarantees
4. **Linear resources**: Windows, processes could be linear to track liveness
