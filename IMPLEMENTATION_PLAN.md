# Implementation Plan: Spec 49 — Feature Guards

## Background

Currently all `require`'d modules are loaded eagerly at startup in
`module_check.ml`. Functions from optional packages (e.g. `json`,
`libxml`) are either always available or always missing. There's no
way to express "this function is available if `(featurep 'json)` is
true in the enclosing scope."

Feature guards add flow-sensitive availability tracking. Within a
`(when (featurep 'json) ...)` body, functions from `json.tart` become
available; outside that scope, they remain undefined.

### Architecture

The existing narrowing system (`narrow.ml`) handles predicate-based
type narrowing — `(stringp x)` narrows `x` to `string` in the
then-branch. Feature guards are structurally similar but operate on
the *environment* rather than individual variable types: they add or
remove function/variable bindings.

Key data flow:

```
1. Guard recognition (guards.ml)
   - Detect (featurep 'X), (fboundp 'f), (boundp 'v), (require 'X)
   - Return guard_info: what feature/symbol becomes available

2. Feature environment (feature_env.ml)
   - Track which features/symbols are proven available
   - Extend type_env with feature-gated bindings

3. Inference integration (infer.ml)
   - In if/when/unless/cond/and/or: detect guards in conditions
   - Create extended env for guarded branches
   - Propagate early-return guards to rest of scope

4. Module resolution (module_check.ml)
   - Pre-load optional module signatures into a "pending" pool
   - Guards unlock pending signatures into the active env
```

### Key design decisions

1. **Lazy activation, not lazy loading**: All `.tart` files are parsed
   at startup into a pending pool. Guards move bindings from pending
   to active. This avoids I/O during inference.

2. **Inline-only** (R17): Like predicate narrowing, guards must be
   inline `(when (featurep 'X) ...)` — storing the result in a
   variable doesn't work.

3. **featurep activates all bindings from X.tart**; `fboundp`/`boundp`
   activate only the named function/variable.

4. **require at top level** (not inside a conditional) activates
   unconditionally (already works today via `extract_requires`).
   `(require 'X nil t)` (soft require) activates nothing unless
   guarded.

---

## Iteration 1: Guard pattern recognition module

**What to do:**

1. Create `lib/typing/guards.ml` and `guards.mli`:

   ```ocaml
   type guard =
     | FeatureGuard of string        (* (featurep 'json) *)
     | FboundGuard of string         (* (fboundp 'json-parse-string) *)
     | BoundGuard of string          (* (boundp 'json-null) *)
     | BoundAndTrueGuard of string   (* (bound-and-true-p my-var) *)

   val analyze_guard : Syntax.Sexp.t -> guard option
   (** Detect guard pattern in a condition expression.
       Returns None if the expression is not a recognized guard. *)

   val analyze_guards : Syntax.Sexp.t -> guard list
   (** Detect guards in a condition, including combined guards
       from (and guard1 guard2 ...). Returns empty list if none. *)
   ```

2. `analyze_guard` matches:
   - `(featurep 'X)` or `(featurep (quote X))` → `FeatureGuard X`
   - `(fboundp 'f)` or `(fboundp (quote f))` → `FboundGuard f`
   - `(boundp 'v)` or `(boundp (quote v))` → `BoundGuard v`
   - `(bound-and-true-p v)` → `BoundAndTrueGuard v` (note: v is
     bare symbol, not quoted — it's a macro)

3. `analyze_guards` additionally handles `(and g1 g2 ...)` by
   collecting guards from each conjunct (R16).

4. Add `tart.guards` to `lib/typing/dune` dependencies.

5. Add unit tests in `test/typing/guards_test.ml`.

**Files:**
- `lib/typing/guards.ml` — new
- `lib/typing/guards.mli` — new
- `lib/typing/dune` — add module
- `test/typing/guards_test.ml` — new
- `test/typing/dune` — add test

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Iteration 2: Feature environment module

**What to do:**

1. Create `lib/typing/feature_env.ml` and `feature_env.mli`:

   ```ocaml
   type t
   (** Feature environment tracking available features and
       pending (guarded) bindings. *)

   val empty : t

   val add_pending_feature : string -> Core.Type_env.t -> t -> t
   (** Register a feature's bindings as pending (not yet proven). *)

   val add_pending_fn : string -> Core.Type_env.scheme -> t -> t
   (** Register a single function binding as pending. *)

   val add_pending_var : string -> Core.Type_env.scheme -> t -> t
   (** Register a single variable binding as pending. *)

   val activate_feature : string -> t -> Core.Type_env.t ->
     Core.Type_env.t
   (** Activate all bindings for a feature into the type env. *)

   val activate_fn : string -> t -> Core.Type_env.t ->
     Core.Type_env.t
   (** Activate a single function binding if pending. *)

   val activate_var : string -> t -> Core.Type_env.t ->
     Core.Type_env.t
   (** Activate a single variable binding if pending. *)

   val is_feature_available : string -> t -> bool
   (** Check if a feature's bindings have been registered
       (pending or active). *)
   ```

2. Internal representation: maps from feature name to
   `(string * scheme) list` for fn/var bindings. Maps from
   symbol name to scheme for individual fn/var bindings.

3. `activate_feature` iterates the feature's bindings and calls
   `Type_env.extend_fn` / `Type_env.extend` for each.

4. Add unit tests in `test/typing/feature_env_test.ml`.

**Files:**
- `lib/typing/feature_env.ml` — new
- `lib/typing/feature_env.mli` — new
- `lib/typing/dune` — update
- `test/typing/feature_env_test.ml` — new
- `test/typing/dune` — update

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Iteration 3: Thread feature_env through inference

**What to do:**

1. Add `feature_env : Feature_env.t` field to the inference
   environment. This requires extending the env that flows through
   `infer`. Two options:

   a. Add to `Type_env.t` — simplest, keeps everything in one place
   b. Thread as a separate parameter — more explicit

   Use option (a): add `feature_env` field to `Type_env.t`.

2. Update `Type_env.empty` and `Type_env.of_list` to initialize
   `feature_env` to `Feature_env.empty`.

3. Add `Type_env.with_feature_env` setter and `Type_env.feature_env`
   getter.

4. In `module_check.ml`, after loading c-core and lisp-core:
   - Scan for optional modules that have `.tart` files but are not
     in the `require` list
   - Load their signatures via `Search.load_module` into a temporary
     env
   - Register those bindings as pending via `Feature_env.add_pending_feature`
   - Store the feature_env in the type_env passed to `Check.check_program`

5. For now, don't activate anything — just register pending
   bindings. This iteration validates the plumbing without changing
   behavior.

**Files:**
- `lib/core/type_env.ml` — add feature_env field
- `lib/core/type_env.mli` — add feature_env accessors
- `lib/typing/module_check.ml` — register pending features

**Verify:** `nix develop --command dune test --force 2>&1` — all
tests pass unchanged (no activation yet).

---

## Iteration 4: Guard activation in conditional forms

**What to do:**

1. In `infer.ml`, modify `infer_if`, `infer_when`, `infer_unless`,
   `infer_cond`, `infer_and` to detect guards:

   - After `Narrow.analyze_condition`, also call
     `Guards.analyze_guards condition`
   - If guards are detected, create an extended env with activated
     bindings for the then-branch:
     ```ocaml
     let guarded_env =
       List.fold_left (fun env guard ->
         match guard with
         | FeatureGuard name ->
           Feature_env.activate_feature name
             (Env.feature_env env) env
         | FboundGuard name ->
           Feature_env.activate_fn name
             (Env.feature_env env) env
         | BoundGuard name ->
           Feature_env.activate_var name
             (Env.feature_env env) env
         | BoundAndTrueGuard name ->
           Feature_env.activate_var name
             (Env.feature_env env) env
       ) env guards
     in
     ```
   - Use `guarded_env` for inferring the then-branch body

2. For `infer_unless`: the body is the *else* branch, so guards
   in the condition should NOT be activated (R12).

3. For `infer_if`: activate in then-branch only; else-branch sees
   unguarded env (R12: negated guards).

4. For `infer_cond`: each clause gets its own guard activation
   from its test expression.

5. For `infer_and`: propagate guards to later operands (R10).

6. For `infer_or`: detect `(or (featurep 'X) (error ...))` pattern
   — if first arg is a guard and second is `(error ...)`, activate
   guard for rest of scope (R9). This requires propagating the
   guard through `infer_progn` to subsequent forms.

**Files:**
- `lib/typing/infer.ml` — guard detection in conditional forms

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Iteration 5: Require handling and early-return guards

**What to do:**

1. Handle `(require 'X)` at the expression level in `infer.ml`:
   - Match `(require 'X)` (2 args, no noerror flag) as a statement
     that unconditionally activates feature X (R5)
   - Match `(require 'X nil t)` (3 args, noerror=t) as soft require
     — activates nothing unless guarded (R6)

2. For early-return patterns (R9, R11):
   - `(unless (featurep 'X) (error ...))` — after this form, X is
     proven for rest of scope. Detect in `infer_progn`: if a form
     is `(unless guard error-form)`, activate guard for subsequent
     forms.
   - `(or (featurep 'X) (error ...))` — same effect when used as
     a statement. Detect in `infer_progn`.

3. Thread guard activations through `infer_progn` by modifying the
   env for subsequent forms when early-return guards are detected.

**Files:**
- `lib/typing/infer.ml` — require handling, early-return guards

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Iteration 6: Module resolution for feature names (R13)

**What to do:**

1. In `module_check.ml`, implement feature-to-module resolution:
   - `(featurep 'json)` → look for `json.tart` in the search path
   - Use existing `Search.load_module` with the feature name

2. Scan code for guard patterns (`featurep`, `fboundp`, `boundp`)
   to discover which optional modules to pre-load:
   - Walk the AST before type-checking
   - Collect all feature names from `(featurep 'X)` calls
   - Collect all symbol names from `(fboundp 'f)` and `(boundp 'v)`
   - Pre-load corresponding `.tart` files as pending

3. For `fboundp`/`boundp`, the module name isn't directly available.
   Use the existing `extract_module_prefixes` logic to guess the
   module from the symbol name (e.g., `json-parse-string` → `json`).

**Files:**
- `lib/typing/module_check.ml` — feature scanning and pre-loading

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Iteration 7: Test fixtures

**What to do:**

1. Create `test/fixtures/typing/core/feature_guard_basic.{el,expected}`:
   - `(when (featurep 'json) (json-parse-string "{}"))` → PASS
   - Unguarded `(json-parse-string "{}")` → FAIL (undefined)

2. Create `test/fixtures/typing/core/feature_guard_fboundp.{el,expected}`:
   - `(when (fboundp 'json-parse-string) ...)` → PASS
   - Other json functions still unavailable

3. Create `test/fixtures/typing/core/feature_guard_require.{el,expected}`:
   - `(require 'json)` then call → PASS
   - `(require 'json nil t)` then call → FAIL
   - `(when (require 'json nil t) (json-parse-string "{}"))` → PASS

4. Create `test/fixtures/typing/core/feature_guard_control.{el,expected}`:
   - if then-branch: PASS; else-branch: FAIL (R7, R12)
   - cond clauses: each independent (R8)
   - and propagation (R10)

5. Create `test/fixtures/typing/core/feature_guard_early_return.{el,expected}`:
   - `(unless (featurep 'json) (error ...))` then call → PASS (R11)
   - `(or (featurep 'json) (error ...))` then call → PASS (R9)

6. Create `test/fixtures/typing/core/feature_guard_inline.{el,expected}`:
   - `(let ((avail (featurep 'json))) (when avail ...))` → FAIL (R17)

Note: These fixtures require a `json.tart` file in the typings. If
one doesn't exist, create a minimal one with `json-parse-string` and
`json-null` for testing.

**Files:**
- `test/fixtures/typing/core/feature_guard_*.{el,expected}` — new

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Iteration 8: Spec status update

**What to do:**

1. Check all task boxes in `specs/49-feature-guards.md`
2. Add Status section: Complete (note any deferred items)
3. R14 (redundant guard warning) deferred — requires Spec 50
   version constraint integration
4. R15 (macro transparency) is inherent — macros expand before
   type-checking

**Files:**
- `specs/49-feature-guards.md` — check boxes, update status

**Verify:** All tests pass.
