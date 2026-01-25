# Emacs Lisp Data Structures: Typing Implications

This document analyzes Emacs Lisp's data structures and their implications for
a static type system based on System F with Hindley-Milner inference.

## 1. Cons Cells and Lists

### How It Works

The cons cell is the fundamental building block of Lisp data structures. A cons
cell contains two pointers: `car` (first element) and `cdr` (rest).

```elisp
;; Basic cons cell
(cons 1 2)        ; => (1 . 2) - improper list / pair

;; List construction
(cons 1 (cons 2 (cons 3 nil)))  ; => (1 2 3) - proper list
'(1 2 3)                         ; => (1 2 3) - quoted list literal

;; Heterogeneous lists (common in elisp)
'(name "Alice" age 30)           ; mixed types
'((key1 . val1) (key2 . val2))   ; alist
```

**Key characteristics:**

- **Proper lists** end with `nil`: `(1 2 3)` is `(1 . (2 . (3 . nil)))`
- **Improper lists** end with non-nil: `(1 2 . 3)`
- **nil** serves dual purpose: empty list `()` and boolean false
- **Heterogeneous by default**: Elisp lists routinely mix types

### Typing Strategy Recommendation

**Primary approach: Parametric list type with homogeneity assumption**

```
List a       -- homogeneous list of elements of type a
(List Int)   -- list of integers
```

**Challenge: Heterogeneous lists**

Elisp heavily uses heterogeneous lists. Strategies:

1. **Tuple types for fixed-length heterogeneous sequences**
   ```
   (Tuple Int String Bool)  -- (1 "hello" t)
   ```

2. **Union types for variable-length mixed lists**
   ```
   (List (Union Int String))  -- allows mixed int/string
   ```

3. **Structural typing for specific patterns** (alists, plists - see below)

**nil handling:**

```
nil : forall a. (List a)    -- nil is the empty list of any type
nil : Bool                  -- nil is also false

-- Requires union or overloading:
nil : forall a. (Union (List a) Bool)
```

**Improper list consideration:**

Improper lists are rare in modern Elisp. Recommend treating as a distinct type
or ignoring for v1:

```
(Pair a b)   -- (a . b) where b is not necessarily a list
```

### Tractability Rating

**Homogeneous lists**: Tractable - standard parametric polymorphism

**Heterogeneous lists**: Hard - requires union types, tuple types, or row
polymorphism; common patterns can be handled with special cases

**nil dual nature**: Hard - needs careful treatment; consider contextual typing
or explicit coercions

**Improper lists**: Tractable (by exclusion) - can be modeled as pairs or
ignored for v1

---

## 2. Vectors and Strings

### How It Works

Vectors are fixed-size, random-access arrays. Strings are specialized character
vectors.

```elisp
;; Vector literals and operations
[1 2 3]                    ; vector of integers
(make-vector 5 0)          ; vector of 5 zeros
(aref vec 0)               ; access element
(aset vec 0 42)            ; mutate element
(length vec)               ; size

;; Strings
"hello"                    ; string literal
(aref "hello" 0)           ; => ?h (character)
(length "hello")           ; => 5
(concat "a" "b")           ; string concatenation
```

**Key characteristics:**

- Fixed size at creation (though `vconcat` creates new vectors)
- O(1) random access
- Mutable elements
- Strings are character vectors but have specialized operations
- Characters are integers in Elisp

### Typing Strategy Recommendation

**Vector type:**

```
(Vector a)              -- homogeneous vector
(Vector Int)            -- vector of integers

aref : forall a. (-> (Vector a) Int a)
aset : forall a. (-> (Vector a) Int a Unit)
make-vector : forall a. (-> Int a (Vector a))
```

**String type:**

```
String                  -- primitive string type (not Vector Char)

;; String-specific operations
concat : (-> String String String)  -- variadic in practice
substring : (-> String Int Int String)

;; Character access returns Int (Elisp chars are ints)
aref : (-> String Int Int)
```

**Consideration: Vector vs String unification**

Could treat `String` as `(Vector Char)` but:
- Elisp chars are integers (no distinct Char type)
- String operations are specialized
- Recommend keeping `String` as distinct primitive

**Fixed-size vectors:**

For v1, do not track vector length in types. Length-indexed vectors (dependent
types) are out of scope.

### Tractability Rating

**Vectors**: Tractable - straightforward parametric type

**Strings**: Tractable - primitive type with specialized operations

**Character type**: Tractable (by design) - treat as Int, or introduce Char as
alias

**Mutability**: Tractable - no special handling needed; mutation is pervasive in
Elisp

---

## 3. Hash Tables

### How It Works

Hash tables provide O(1) key-value lookup with configurable equality tests.

```elisp
;; Creation
(make-hash-table :test 'equal)
(make-hash-table :test 'eq :size 100)

;; Operations
(puthash key value table)     ; insert/update
(gethash key table)           ; lookup (returns nil if missing)
(gethash key table default)   ; lookup with default
(remhash key table)           ; remove
(maphash fn table)            ; iterate

;; Hash table literal (Emacs 29+)
#s(hash-table data (key1 val1 key2 val2))
```

**Key characteristics:**

- Keys and values can be any type
- `:test` determines key equality (`eq`, `eql`, `equal`)
- `gethash` returns `nil` for missing keys (indistinguishable from nil value)
- Mutable by design

### Typing Strategy Recommendation

**Basic parametric type:**

```
(HashTable k v)         -- hash table with key type k, value type v

make-hash-table : forall k v. (-> (HashTable k v))
puthash : forall k v. (-> k v (HashTable k v) Unit)
gethash : forall k v. (-> k (HashTable k v) (Option v))
remhash : forall k. (-> k (HashTable k v) Unit)
```

**The nil problem:**

`gethash` returns `nil` for missing keys, conflating with `nil` values. Options:

1. **Option type with explicit handling**
   ```
   gethash : forall k v. (-> k (HashTable k v) (Option v))
   ```
   Requires wrapping; doesn't match runtime behavior exactly.

2. **Nullable type**
   ```
   gethash : forall k v. (-> k (HashTable k v) (Nullable v))
   ```
   Where `Nullable v = v | nil` (not quite right either).

3. **Accept unsoundness for v1**
   ```
   gethash : forall k v. (-> k (HashTable k v) v)
   ```
   Unsound but matches developer mental model.

**Recommendation:** Use Option type. The three-argument form with default can
have a refined type:

```
gethash : forall k v. (-> k (HashTable k v) v v)  -- with default
```

**Equality constraint:**

The `:test` parameter constrains valid key types. For v1, ignore this
constraint (allow any key type). Future: typeclass-like constraint.

### Tractability Rating

**Basic hash table type**: Tractable - standard Map/Dict pattern

**nil-as-missing handling**: Hard - fundamental Elisp idiom conflicts with
type safety; Option type is cleanest but changes semantics

**Equality test constraint**: Hard - needs typeclass-like mechanism; defer to v2

---

## 4. Association Lists (Alists)

### How It Works

Alists are lists of cons pairs, providing simple key-value association.

```elisp
;; Alist structure
'((key1 . value1) (key2 . value2) (key3 . value3))

;; Operations
(assoc key alist)              ; find pair by key (equal)
(assq key alist)               ; find pair by key (eq)
(alist-get key alist)          ; get value directly
(alist-get key alist default)  ; with default

;; Modification (returns new alist)
(cons (cons new-key new-val) alist)
(setf (alist-get key alist) value)  ; generalized setf

;; Common patterns
(add-to-list 'alist-var (cons key val))
```

**Key characteristics:**

- Linear search (O(n))
- First match wins (allows shadowing)
- Immutable operations return new lists
- Very common in Elisp for configuration, options, etc.

### Typing Strategy Recommendation

**Basic representation:**

```
(Alist k v) = (List (Pair k v))

;; Or as distinct type
Alist k v
```

**Operation types:**

```
assoc : forall k v. (-> k (Alist k v) (Option (Pair k v)))
assq  : forall k v. (-> k (Alist k v) (Option (Pair k v)))
alist-get : forall k v. (-> k (Alist k v) (Option v))
```

**String-keyed alists (very common):**

```
;; Special case for string keys
(Alist String v)
```

**Symbol-keyed alists:**

```
;; Configuration patterns
(Alist Symbol Any)  -- if we have Any type
(Alist Symbol v)    -- if homogeneous values
```

**Heterogeneous value challenge:**

Many alists have heterogeneous values:

```elisp
'((name . "Alice") (age . 30) (active . t))
```

Options:
1. **Union type**: `(Alist Symbol (Union String Int Bool))`
2. **Row type** (if implemented): typed field access
3. **Existential**: `(Alist Symbol (Exists a. a))` - limited utility

### Tractability Rating

**Homogeneous alists**: Tractable - `(List (Pair k v))`

**Heterogeneous alists**: Hard - requires row types or unions; very common in
practice

**Alist-as-record pattern**: Hard - needs row polymorphism for proper typing

---

## 5. Property Lists (Plists)

### How It Works

Plists are flat lists alternating between keys and values, typically using
keywords as keys.

```elisp
;; Plist structure
'(:name "Alice" :age 30 :active t)

;; Operations
(plist-get plist :name)           ; => "Alice"
(plist-put plist :new-key value)  ; returns modified plist
(plist-member plist :key)         ; check membership

;; Very common in function keyword arguments
(defun make-widget (&rest args)
  (let ((width (plist-get args :width 100))
        (height (plist-get args :height 50)))
    ...))
```

**Key characteristics:**

- Keys are typically keywords (symbols starting with `:`)
- O(n) linear search
- Alternating key-value structure
- Extremely common for keyword arguments and options

### Typing Strategy Recommendation

**Basic approach (limited):**

```
(Plist k v)  -- plist with key type k and value type v
```

**Row type approach (ideal):**

Plists are natural candidates for row polymorphism:

```
;; Row type notation (hypothetical)
{ :name : String, :age : Int, :active : Bool }

plist-get : forall r a. (-> { r | :key : a } Keyword a)
```

This would allow:

```
(defun process-person (p : { :name : String, :age : Int | r })
  ;; can access :name and :age, ignores other fields
  ...)
```

**Practical v1 approach:**

Without row types, options are limited:

1. **Treat as untyped**: `(Plist Keyword Any)`
2. **Define record types**: For common plist shapes, define nominal types
3. **Use structural types**: If implementing structural records

**Keyword argument handling:**

```elisp
(defun foo (&key name age) ...)
```

Ideally typed as:

```
foo : (-> :name String :age Int Result)
;; or with optional
foo : (-> :name (Option String) :age (Option Int) Result)
```

### Tractability Rating

**Basic plist type**: Tractable - but not very useful

**Row-typed plists**: Hard - requires row polymorphism implementation

**Keyword arguments**: Hard - needs row types or special function type syntax

**Potential for row types**: High - plists are the strongest argument for row
polymorphism in a typed Elisp

---

## 6. Symbols

### How It Works

Symbols are interned names with multiple associated values.

```elisp
;; Symbol structure
'my-symbol                       ; symbol literal
(intern "my-symbol")             ; create/retrieve symbol
(symbol-name 'my-symbol)         ; => "my-symbol"

;; Symbol cells
(symbol-value 'my-var)           ; variable value
(symbol-function 'my-func)       ; function value
(symbol-plist 'my-symbol)        ; property list

;; Keywords (self-evaluating symbols in keyword package)
:my-keyword                      ; keyword symbol
(keywordp :my-keyword)           ; => t

;; Symbol properties
(get 'my-symbol 'my-prop)        ; get property
(put 'my-symbol 'my-prop value)  ; set property
```

**Key characteristics:**

- Interned (unique per name in obarray)
- Multiple value cells: variable, function, property list
- Used as identifiers, enum values, keys
- Keywords are special self-quoting symbols

### Typing Strategy Recommendation

**Symbol as type:**

```
Symbol              ; the type of symbols
Keyword             ; subtype or distinct type for keywords
```

**Literal symbol types (singleton types):**

For precise typing of specific symbols:

```
'foo : (Literal 'foo)
'foo : 'foo           ; shorthand

;; Useful for enums
(deftype Direction (Union 'north 'south 'east 'west))
```

**Symbol-as-function-name:**

Symbols used with `funcall`:

```
(funcall 'my-func arg)
```

Requires tracking symbol -> function type association. For v1, likely unsound.

**Keyword handling:**

Keywords are frequently used in plists and alists:

```
:name : Keyword
;; or more precisely
:name : ':name    ; literal type
```

### Tractability Rating

**Symbol type**: Tractable - straightforward primitive type

**Keyword type**: Tractable - subtype or alias of Symbol

**Literal symbol types**: Tractable - useful for enums and plist keys

**Symbol value/function cells**: Hard - dynamic dispatch through symbols is
common but difficult to type

**Symbol properties**: Hard - arbitrary property attachment is untyped

---

## 7. Structures (cl-defstruct)

### How It Works

`cl-defstruct` provides record types with generated constructors and accessors.

```elisp
(cl-defstruct person
  name
  age
  (active t))  ; with default

;; Generated functions
(make-person :name "Alice" :age 30)  ; constructor
(person-name p)                       ; accessor
(person-age p)                        ; accessor
(setf (person-name p) "Bob")          ; setter
(person-p obj)                        ; predicate
(copy-person p)                       ; copier

;; With type declarations (not enforced at runtime)
(cl-defstruct person
  (name nil :type string)
  (age 0 :type integer))

;; Inheritance
(cl-defstruct (employee (:include person))
  department
  salary)
```

**Key characteristics:**

- Generates constructor, accessors, predicate, copier
- Optional type declarations (documentation only, not enforced)
- Single inheritance via `:include`
- Representation is vector by default

### Typing Strategy Recommendation

**Direct integration opportunity:**

`cl-defstruct` is the most type-friendly Elisp construct. Existing `:type`
declarations provide type information.

```elisp
(cl-defstruct person
  (name nil :type string)
  (age 0 :type integer))
```

**Type representation:**

```
;; Nominal record type
Person : Type
Person = { name : String, age : Int, active : Bool }

;; Constructor
make-person : (-> :name String :age Int :active Bool Person)

;; Accessors
person-name : (-> Person String)
person-age : (-> Person Int)

;; Predicate
person-p : (-> Any Bool)  ; or (-> a Bool) with type guard
```

**Inheritance:**

```
Employee <: Person   ; subtyping

employee-department : (-> Employee String)
person-name : (-> Person String)  ; works on Employee too
```

**Type extraction:**

Parse `cl-defstruct` forms to extract:
- Field names and types from `:type` declarations
- Default values (for optionality)
- Inheritance relationships

### Tractability Rating

**Basic struct types**: Tractable - direct mapping to record types

**Existing :type declarations**: Tractable - can be parsed and used

**Inheritance**: Tractable - straightforward subtyping

**Keyword constructor arguments**: Tractable - matches plist pattern

**Integration**: High priority - best path to typed Elisp records

---

## 8. EIEIO Classes

### How It Works

EIEIO (Enhanced Implementation of Emacs Interpreted Objects) provides CLOS-like
object orientation.

```elisp
(defclass person ()
  ((name :initarg :name
         :accessor person-name
         :type string
         :documentation "Person's name")
   (age :initarg :age
        :accessor person-age
        :type integer
        :initform 0))
  "A person class.")

;; Instance creation
(make-instance 'person :name "Alice" :age 30)
(person :name "Alice" :age 30)  ; shorthand

;; Method definition
(cl-defmethod greet ((p person))
  (format "Hello, %s" (person-name p)))

;; Inheritance
(defclass employee (person)
  ((department :initarg :department
               :type string)))

;; Multiple inheritance supported
(defclass manager (employee supervisor)
  ...)
```

**Key characteristics:**

- Full class-based OOP with slots
- `:type` specifier for documentation (not enforced)
- Multiple inheritance
- `cl-defmethod` for method dispatch
- Generic functions with multiple dispatch

### Typing Strategy Recommendation

**Class as type:**

```
Person : Type
Employee <: Person

;; Slot types from :type specifiers
person-name : (-> Person String)
person-age : (-> Person Int)
```

**Constructor typing:**

```
make-instance : forall c. (-> (Class c) ... c)
;; Or specific:
(person :name String :age Int) : Person
```

**Method typing:**

```
;; Generic function
greet : forall a. (Greetable a) => (-> a String)

;; Or simpler: just type the method for each class
greet : (-> Person String)
greet : (-> Employee String)  ; more specific
```

**Multiple dispatch complexity:**

```elisp
(cl-defmethod interact ((a person) (b person))
  ...)
```

Full multi-method typing requires:
- Tracking all method specializations
- Computing applicable methods
- Determining result type based on argument types

**Recommendation for v1:**

- Parse `defclass` for slot types
- Type accessors as simple functions
- Type methods with single dispatch only
- Defer multi-method dispatch to v2

### Tractability Rating

**Class types**: Tractable - nominal types with slots

**Slot type extraction**: Tractable - `:type` specifier is present

**Single inheritance**: Tractable - standard subtyping

**Multiple inheritance**: Hard - requires careful subtype handling

**Single-dispatch methods**: Tractable - like typed OOP

**Multiple dispatch**: Hard - complex dispatch semantics; defer

**Generic function typing**: Hard - needs typeclass-like mechanism

---

## 9. Buffers

### How It Works

Buffers are the central data structure for text editing in Emacs.

```elisp
;; Buffer operations
(current-buffer)                     ; get current buffer
(get-buffer "name")                  ; find by name
(get-buffer-create "name")           ; find or create
(with-current-buffer buf ...)        ; switch context

;; Buffer content
(buffer-string)                      ; entire content
(buffer-substring beg end)           ; region
(insert "text")                      ; insert at point

;; Point and markers
(point)                              ; current position
(point-min) (point-max)              ; buffer bounds
(goto-char pos)                      ; move point

;; Buffer-local variables
(make-local-variable 'my-var)
(setq-local my-var value)
(buffer-local-value 'var buf)

;; Major and minor modes
(fundamental-mode)
(with-current-buffer buf
  major-mode)                        ; => 'fundamental-mode
```

**Key characteristics:**

- Implicit current buffer context
- Buffer-local variable bindings
- Point (cursor) as implicit state
- Text properties on buffer content
- Mode-specific behavior

### Typing Strategy Recommendation

**Buffer as abstract type:**

```
Buffer : Type
(current-buffer) : Buffer
(get-buffer name) : (Option Buffer)
```

**Buffer operations:**

Many buffer operations have implicit context:

```
;; These operate on current buffer implicitly
(point) : Int
(buffer-string) : String
(insert str) : Unit
```

**With-current-buffer:**

```
with-current-buffer : forall a. (-> Buffer (-> a) a)
;; or as special form with context typing
```

**Buffer-local variables:**

Buffer-locals are dynamically scoped per-buffer. Typing options:

1. **Ignore buffer-local distinction**: Treat as regular variables (unsound)

2. **Effect typing**: Track buffer context
   ```
   insert : (-> String (Buffer!) Unit)  ; requires buffer context
   ```

3. **Context parameter**: Make buffer explicit
   ```
   insert : (-> Buffer String Unit)
   ;; But this doesn't match Elisp idiom
   ```

**Point as effect:**

Many functions implicitly use/modify point:

```elisp
(re-search-forward "pattern")  ; moves point, returns position or nil
(looking-at "pattern")         ; checks at point
```

For v1, likely cannot track point effects. Accept unsoundness.

**Mode-specific types:**

```elisp
;; In org-mode buffer
(org-entry-get nil "PROPERTY")  ; only valid in org buffers
```

Potential for buffer subtyping by mode:

```
OrgBuffer <: Buffer
(org-entry-get) : (-> OrgBuffer ...)
```

Complex; defer to v2+.

### Tractability Rating

**Buffer as type**: Tractable - abstract type

**Buffer operations**: Tractable - if ignoring implicit context

**Buffer-local variables**: Hard - dynamic scoping per buffer

**Point/mark effects**: Intractable for v1 - pervasive implicit state

**Mode-specific operations**: Hard - needs buffer subtyping

**with-current-buffer**: Tractable - as higher-order function

---

## Summary Table

| Data Structure | Basic Typing | Advanced Features | Tractability | Priority |
|----------------|--------------|-------------------|--------------|----------|
| Lists | `(List a)` | Heterogeneous, nil dual | Tractable/Hard | High |
| Vectors | `(Vector a)` | - | Tractable | Medium |
| Strings | `String` | - | Tractable | High |
| Hash tables | `(HashTable k v)` | nil-as-missing | Tractable/Hard | Medium |
| Alists | `(Alist k v)` | Row types | Tractable/Hard | High |
| Plists | `(Plist k v)` | Row types, kwargs | Hard | High |
| Symbols | `Symbol`, `Keyword` | Literal types | Tractable | High |
| cl-defstruct | Record types | Inheritance | Tractable | Critical |
| EIEIO | Class types | Multi-dispatch | Tractable/Hard | Medium |
| Buffers | `Buffer` | Context, modes | Tractable/Hard | Low |

## Recommendations for v1

### Must Have

1. **Homogeneous list type**: `(List a)` with standard operations
2. **Primitive types**: `String`, `Int`, `Float`, `Bool`, `Symbol`, `Keyword`
3. **Vector type**: `(Vector a)`
4. **cl-defstruct integration**: Parse and type existing structs
5. **Pair type**: `(Pair a b)` for cons cells
6. **Option type**: `(Option a)` for nullable returns

### Should Have

1. **Alist type**: `(Alist k v)` as `(List (Pair k v))`
2. **Hash table type**: `(HashTable k v)`
3. **Tuple types**: `(Tuple a b c)` for fixed heterogeneous sequences
4. **Union types**: `(Union a b)` for common mixed-type patterns
5. **Literal symbol types**: `'foo` as type for enums

### Could Have

1. **EIEIO class types**: Parse defclass, type accessors
2. **Buffer type**: Abstract type with basic operations
3. **Row types for plists**: Structural record types

### Won't Have (v1)

1. **Multiple dispatch typing**: Too complex
2. **Buffer-local variable tracking**: Needs effect system
3. **Point/mark effects**: Pervasive; accept unsoundness
4. **Mode-specific buffer subtypes**: Needs more design
5. **Length-indexed vectors**: Dependent types out of scope

## Open Questions

1. **nil handling**: How to reconcile nil-as-false and nil-as-empty-list?
   - Option A: Contextual typing (nil type depends on context)
   - Option B: Explicit coercions
   - Option C: Union type everywhere

2. **Row polymorphism priority**: Plists and keyword arguments strongly motivate
   row types. Is this worth the implementation complexity for v1?

3. **Struct inheritance**: How much subtyping to support? Width subtyping
   (structural) vs nominal subtyping?

4. **Alist key types**: Should we restrict alist keys to eq-comparable types?

5. **Buffer context**: Is it worth having a `BufferContext` effect or type
   parameter, or accept unsoundness?
