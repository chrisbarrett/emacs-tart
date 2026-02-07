# Implementation Plan: Feature Guards (Spec 49)

Flow-sensitive type narrowing for runtime feature detection (`featurep`,
`fboundp`, `boundp`, `bound-and-true-p`, `require`).

## Analysis

### Current Architecture

**Module loading** happens in `module_check.ml` at the top level:
1. `extract_requires` finds `(require 'X)` forms in the sexp list
2. `Search.load_module` resolves `X.tart` → loads signatures into env
3. `Check.check_program` type-checks with that pre-populated env

**Type narrowing** happens in `infer.ml` via `narrow.ml`:
1. `Narrow.analyze_condition` detects predicate calls in conditions
2. `narrow_env_from_analysis` produces then/else environments
3. `infer_if`, `infer_when`, etc. use narrowed envs for branches

Feature guards reuse the narrowing pattern but operate on
**name availability** rather than type intersection:
- `(featurep 'json)` → make all json.tart names available in then-branch
- `(fboundp 'f)` → make function `f` available in then-branch
- `(boundp 'v)` → make variable `v` available in then-branch

### Key Design Decisions

1. **Guard analysis in narrow.ml** — extend `condition_analysis` with a
   `FeatureGuard` variant alongside existing `Predicate`/`Predicates`
2. **Lazy signature loading** — `featurep` guard triggers loading of
   `X.tart` at inference time (not pre-loaded like `require`)
3. **Environment extension** — guards add names to the env (vs narrowing
   which refines existing types)
4. **No new module files** — the spec suggests `feature_env.ml` and
   `guards.ml` but the existing `narrow.ml` + `infer.ml` pattern is
   simpler and more consistent

### Scope Deferral

- R14 (redundant guard warning) requires version constraint propagation
  (Spec 50) — defer
- R15 (macro transparency) — already works since macros expand before
  type checking

---

## Iteration 1: Guard pattern recognition

Extend `narrow.ml` to recognize feature guard patterns in conditions.

### Task 1.1: Add guard types to narrow.ml

- [ ] Add `guard_info` type: `FeatureGuard of string` | `FboundGuard of string` | `BoundGuard of string` | `BoundTrueGuard of string`
- [ ] Add `Guard of guard_info` and `Guards of guard_info list` to `condition_analysis`
- [ ] Add `analyze_guard` function: recognizes `(featurep 'X)`, `(fboundp 'f)`, `(boundp 'v)`, `(bound-and-true-p v)` patterns
- [ ] Extend `analyze_condition`: try guard analysis when predicate analysis yields NoPredicate
- [ ] Handle `(and ...)` containing mixed predicates and guards
- [ ] Update narrow.mli
- [ ] Build + test

### Task 1.2: Unit tests for guard recognition

- [ ] Test `analyze_condition` with `(featurep 'json)` → `Guard (FeatureGuard "json")`
- [ ] Test `(fboundp 'json-parse-string)` → `Guard (FboundGuard "json-parse-string")`
- [ ] Test `(boundp 'json-null)` → `Guard (BoundGuard "json-null")`
- [ ] Test `(bound-and-true-p my-var)` → `Guard (BoundTrueGuard "my-var")`
- [ ] Test `(and (featurep 'json) (stringp x))` → mixed result
- [ ] Test `(featurep x)` (non-literal) → NoPredicate
- [ ] Build + test

---

## Iteration 2: Signature loading for feature guards

Wire guard recognition into the inference engine so that guards
extend the environment with loaded signatures.

### Task 2.1: Add module loader callback to infer

- [ ] Add `load_feature : (string -> Env.t -> Env.t) option` field to a
      new `infer_config` record (or pass as labeled param)
- [ ] Thread the loader through `infer` → control flow functions
- [ ] When a `FeatureGuard "X"` is detected, call `load_feature "X" env`
      to get an env extended with X.tart signatures
- [ ] `FboundGuard "f"`: look up f in the loaded module env, extend
      only function f
- [ ] `BoundGuard "v"`: look up v in the loaded module env, extend
      only variable v
- [ ] `BoundTrueGuard "v"`: extend v with type `t` (truthy)
- [ ] Build + test

### Task 2.2: Wire loader in module_check.ml

- [ ] Create `make_feature_loader` in module_check.ml using
      `Search.load_module`
- [ ] Pass loader into `Check.check_program` → `Infer`
- [ ] Ensure loaded features are cached (avoid re-loading json.tart
      for every `(featurep 'json)` call)
- [ ] Build + test

---

## Iteration 3: Control flow propagation

Extend all control flow inference functions to handle guards.

### Task 3.1: Guard env application in infer.ml

- [ ] Add `apply_guard_narrowing`: given guard_info + loader + env,
      returns (then_env, else_env) where then_env has names,
      else_env unchanged
- [ ] Add `narrow_env_from_analysis` handling for `Guard`/`Guards`
      variants (compose with existing predicate narrowing)
- [ ] `infer_if`: then-branch gets guarded env, else-branch unchanged
- [ ] `infer_when`: body gets guarded env
- [ ] `infer_unless`: body gets **unguarded** env (R12: negated guard)
- [ ] `infer_cond`: each clause independently guarded
- [ ] `infer_and`: guards propagate to later operands (R16)
- [ ] Build + test

### Task 3.2: Require as top-level guard

- [ ] In `infer` main dispatch, intercept `(require 'X)` as expression
- [ ] Hard require `(require 'X)`: load X.tart into env, return
      feature symbol type; env persists for subsequent forms
- [ ] Soft require `(require 'X nil t)`: return result type but
      **don't** extend env (needs guard)
- [ ] Soft require as condition `(when (require 'X nil t) ...)`:
      treat as FeatureGuard in then-branch
- [ ] Note: top-level `(require 'X)` already handled by module_check;
      this handles require inside control flow
- [ ] Build + test

---

## Iteration 4: Test fixtures

### Task 4.1: Feature guard test fixtures

- [ ] `feature_guard_basic.{el,expected}`: featurep in when/if unlocks
      names (PASS); unguarded call errors (FAIL with UNDEFINED)
- [ ] `feature_guard_fboundp.{el,expected}`: fboundp unlocks single
      function; other names from same module still unavailable
- [ ] `feature_guard_boundp.{el,expected}`: boundp unlocks variable
- [ ] `feature_guard_negated.{el,expected}`: else-branch of if with
      featurep does NOT have guarded names (R12)
- [ ] `feature_guard_and.{el,expected}`: combined guards via and (R16)
- [ ] `feature_guard_require.{el,expected}`: hard require extends env;
      soft require needs guard
- [ ] Build + test

---

## Iteration 5: Spec completion

### Task 5.1: Documentation and inline-only

- [ ] R17 is inherent: guards only recognized inline (same as predicates)
- [ ] Create `feature_guard_no_stored.{el,expected}`: stored featurep
      result does NOT unlock names
- [ ] Check all task boxes in specs/49-feature-guards.md
- [ ] Update status (Complete, except R14 which needs Spec 50)
- [ ] Build + test
