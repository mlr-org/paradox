# Dormant values: implementation plan

Status: implemented in the current worktree with focused validation complete,
2026-07-26. The replacement-candidate freeze, full release gates, downstream
waves, and remote tracker updates remain deliberately deferred until the other
pre-release package-facing todos have converged.
Scope: paradox 2.0.0, before first public release. The implementation changes
package-facing source after the last sealed candidate and therefore requires a
**new release candidate** and a full re-run of the gate matrix per
`design/release-2.0.0.md` and `AGENTS.md`. During this development batch, only
focused validation is intended because further pre-release todos remain; that
focused evidence is not a substitute for the future candidate gates. Agents
must never perform remote writes.

Related reading: `design/contract-first-2.0.0.md` (contract to be amended),
`design/compatibility.md` (break table to be amended), issue #265, PR #275,
PR #343, mlr-org/mlr3pipelines#101. An empirical baseline for every claim in
this plan is in the Appendix.

## 1. Summary of the change

Checked value assignment (`$values <-`, `set_values()`) stops rejecting
entries whose *dependencies* are unsatisfied. Such entries are stored as
**dormant** values: retained in `$values`, excluded from `$get_values()`
output, and reactivated automatically when a later assignment makes their
dependencies hold. Type/bounds/special-value/`custom_check` validation,
unknown-ID rejection, sanitization, TuneToken admission, and the atomic
graph-wide transaction semantics of assignment are unchanged.

Simultaneously, dependency evaluation everywhere becomes **default-aware**
(issue #265, resolved in favor of consulting defaults): a parent that is
absent from the evaluation basis but has a recorded `default` satisfying the
condition makes the child active.

`$constraint` callbacks receive the **dependency-filtered (active) subset**
of the configuration instead of the raw list.

There is exactly one activity engine (section 3.1) shared by every consumer.
No policy flag: the behavior is unconditional.

Explicitly **out of scope** (do not implement here): printer changes marking
dormant entries (possible later, separate PR); warnings/messages on storing
dormant values; any change to `Design`/`$transpose()`, samplers, NA handling
in `check_dt`, TuneToken admission shapes, `$deps` table shape, `$add_dep()`
RHS feasibility, bulk `$deps <-` semantics, `$has_deps`; the pre-existing
wart that `to_tune()` tokens can sit on dependency-inactive params and
`$search_space()` then tunes a parameter that is dormant at train time.

## 2. Locked decisions (maintainer-approved; do not re-litigate)

1. **#265 resolved default-aware.** Dependency evaluation consults recorded
   defaults for absent parents, transitively (spec in 3.1). `NoDefault`
   never satisfies.
2. **Constraint input is filtered.** `$constraint` (and the collection child
   / shadow constraint adapters) receive only the active entries of the
   configuration being validated.
3. **No printer change** in this change set.
4. Explicit `$check()/$assert()/$test()/$check_dt()` keep `check_strict =
   TRUE` defaults and validate a *point* — everything present must be
   active — so `ps$check(ps$values)` is intentionally **not** an invariant
   once dormant entries exist. Document this.
5. Point evaluation is **store-blind**: `$check(xs)` never consults the
   set's stored `$values`, only `xs` plus defaults. (bbotk's
   `Objective$eval_many` explicit asserts depend on point semantics;
   corpus refs in Appendix A3.)

## 3. Normative semantics

### 3.1 Activity

Given an evaluation basis `x` (a named list) and a ParamSet(-graph) schema,
a parameter `p` is **active** iff every dependency row `(p, on, cond)`
is satisfied. A row is satisfied as follows:

1. Validate the dependency path first; an active-path cycle is a hard error.
2. If `x[[p]]` is a TuneToken, the row is skipped (treated as satisfied).
3. Otherwise, if `on` is itself not active w.r.t. `x` (recursive), the row is
   **unsatisfied** — regardless of any value or default `on` carries.
   (The existing filter is already transitive in this sense; keep it.)
4. Otherwise determine the effective operand for `on`:
   - `x[[on]]` if present and not a TuneToken;
   - if `x[[on]]` is a TuneToken: the row is **skipped** (treated as
     satisfied) — this matches the documented `$check_dependencies()`
     semantics ("A dependent value or its parent supplied as a TuneToken is
     skipped", `R/ParamSet.R:786-789`) and must stay identical;
   - else `default(on)` if the recorded default is not `NoDefault`
     **[NEW — the #265 change]**;
   - else the row is **unsatisfied**.
5. With an operand chosen, the row is satisfied iff the closed built-in
   comparator (`C_condition_test_builtin` semantics) accepts it. Operand
   admission stays exactly as today: a stored special value (e.g. `NULL`)
   or an opaque `ParamUty` operand keeps its current comparator behavior —
   pin with characterization tests (6.T14/T15), do not change.

Multiple rows on one child are conjunctive (unchanged). BASE dependency cycles
can currently be constructed; mutation admission is unchanged in this task.
Every unified activity consumer must detect an active-path cycle and fail with
a deterministic cycle error, as the Design path already does, rather than loop
or return a partial mask. Dangling parents (possible via bulk `$deps <-` or
`allow_dangling_dependencies` subsets) evaluate as "absent": default if
recorded, else unsatisfied — pin current dangling behavior first (6.T16) and
preserve anything that would otherwise change design/`flatten()` behavior.

**Evaluation basis per consumer:**

| Consumer | Basis `x` |
|---|---|
| `$check/$assert/$test`, each row of `$check_dt`, `$check_dependencies` | the candidate point `xs` (never stored `$values`) |
| `$get_values()` filtering; its `check_required` exemption | the stored `$values` (translated whole-graph state for COLLECTION/SHADOW) |
| `presence = "required"/"all"` exemption inside check-family | the candidate point `xs` |
| constraint filtering (3.4) | the complete configuration being validated at that site |

The design/sampler row-activity engine (`src/design_dependencies.c`)
operates on complete sampled rows where every parent has a value, so
defaults can never apply; its semantics are therefore unchanged. Document
this exemption in the code where the two engines meet, and add an assertion
test that grid/random designs are byte-identical before/after (6.T22).

### 3.2 Assignment (checked)

`C_param_set_assign_values_checked` (entered from `R/ParamSet.R:1113`; the
same wrapper serves COLLECTION and SHADOW):

- Keep: outer-shell admission, unknown-ID rejection, per-entry
  type/bounds/special/`custom_check` validation (this **includes dormant
  entries** — the store stays domain-sound; `p_uty` custom checks still run
  for dormant entries, document that), sanitization, token admission,
  clear-values canonicalization, graph planning, dedup/last-owner
  semantics, generation checks, reentrancy error, the allocation- and
  callback-free commit wave, atomicity on failure.
- Remove: the dependency-feasibility rejection of the resulting state.
  Assignment must not evaluate activity at all **except** when a constraint
  exists (next section). This is a small perf win on the mutation hot path.
- Unchecked assignment (`C_param_set_store_values`) is already tolerant;
  unchanged.
- `$add_dep()` still does not validate stored values (now harmless: a
  violating store is simply a store with dormant entries). Unchanged.

### 3.3 `$get_values()`

`C_param_set_get_values` (`R/ParamSet.R:553`): the `remove_dependencies =
TRUE` filter switches to the unified default-aware activity kernel with
basis = stored values. It is already transitive and must remain so.
`remove_dependencies = FALSE` returns the raw store including dormant
entries (unchanged). `check_required` runs against the filtered view:
required-but-inactive params are exempt (today's behavior, Appendix A1.T3);
note the tightening in 3.5.

### 3.4 Constraint input

At every constraint invocation site, compute the active subset of the
configuration being validated (basis per 3.1 table) and pass **only active
entries** to the callback:

- BASE check-family and `$test_constraint`/`$test_constraint_dt`
  (`C_param_set_test_constraint_builtin` / `..._dt_builtin`,
  `R/ParamSet.R:700/724`): filter `x` / each row.
- Checked assignment with a constraint present: compute activity of the new
  complete state once, filter, call the snapshotted constraint exactly once
  (existing invocation point; only the input changes).
- COLLECTION live check/test/assignment graph sites compute activity on the
  full translated collection configuration (collection-level cross-set deps
  apply), then each child constraint receives its **child-scope slice of the
  active entries, unprefixed** — preserving the v2 fix that child constraints
  see unprefixed child values. Detached BASE checking filters before invoking
  `C_param_set_collection_detached_constraint`.
- SHADOW graph sites compute activity over the complete merged origin
  configuration and pass already filtered hidden/visible slices to
  `C_param_set_shadow_constraint`, which retains its merge-and-call role.
  (Deps never cross the visible/hidden boundary, so the partition is
  well-defined.)
- Carrier ABI caution: exact detached collection and Shadow callback carriers
  have no schema. They must keep exactly their current plan fields; activity is
  computed at the authoritative check/test/assignment graph site, never stored
  in the plan or reimplemented in a carrier evaluator.

### 3.5 Intentional behavior changes to record (NEWS + differential rows)

Each of these is a deliberate consequence; write one NEWS bullet and one
differential expected-difference row per item:

1. Checked assignment accepts dependency-unsatisfied entries (dormant
   values); error → success.
2. Check-family with defaults: a point missing a parent whose default
   satisfies now passes strict check (`#265`); previously an error string.
3. `presence = "all"/"required"` tightening: a child whose absent parent
   has a *satisfying default* now counts as active and can be demanded
   where it was previously exempt. (Observable mainly for
   `presence = "required"`; for `"all"` the absent parent itself usually
   already fails the check.)
4. `get_values(check_required = TRUE)` tightening, symmetric to (3).
5. Constraints receive filtered input; a constraint that inspected
   inactive/absent-parent entries sees a different list.
6. `$check(ps$values)` is no longer guaranteed `TRUE`; docs updated.
7. Diagnostics: the "can only be set if …" message fragment must survive
   verbatim for the still-failing cases (downstream matches on it —
   Appendix A3). For the new failure case "parent absent and its default
   does not satisfy", either reuse the existing "'x' is not set at all"
   wording or extend it with the default; if extended, keep the leading
   fragment stable.

### 3.6 `$search_space()` interaction (bounded investigation, then fix)

`get_tune_ps` (`R/ParamSet.R:1279` onward) reconstructs dependency rows
from detached Domain requirements. Determine the current rule for a
dependency whose parent is **outside** the search space: (a) parent has a
fixed stored value, (b) parent unset. Extend rule (b) with the same
activity semantics: unset parent whose default satisfies ⇒ dependency
satisfied ⇒ keep the param, drop the dep row; unset parent without
satisfying default ⇒ current behavior (do not invent new behavior beyond
the default extension). Add tests for both (6.T20). If the current code
already errors on (b), keep erroring when the default does not satisfy.

## 4. Code map and implementation order

Native entry points (verified in source):

- `src/paramset_check.c` (~126k): check kernel; dependency diagnostics at
  `src/paramset_check.c:2962` and `:2972` (two sites — expected to be the
  point-check and assignment paths; the assignment one is the code to
  remove/bypass). Hosts or calls: `C_param_set_check_builtin`,
  `C_param_set_check_dt_builtin`, `C_param_set_check_dependencies_builtin`,
  `C_param_set_assign_values_checked`, `C_param_set_get_values`,
  `C_param_set_test_constraint_builtin`, `C_param_set_test_constraint_dt_builtin`.
- `src/design_dependencies.c`: vectorized design/row activity engine (do
  not change semantics; see 3.1).
- `src/builtin_condition.c` / `C_condition_test_builtin`: comparator
  (unchanged; used by the activity kernel).
- Collection/shadow constraint evaluators: `src/paramset_collection_*.c`,
  shadow plan evaluator behind `C_param_set_shadow_constraint`.
- R wrappers needing doc/comment updates: `R/ParamSet.R` (the transaction
  comment block at 1101-1116 currently says the transaction "validates
  strict dependency/constraint semantics" — rewrite; roxygen for `values`,
  `set_values`, `check*`, `get_values`, `test_constraint*`, `constraint`),
  `R/ParamSetCollection.R`, `R/ParamSetShadow.R`, `R/Domain.R` (`depends`
  docs), `vignettes/indepth.Rmd` (Dependencies + Values sections).

Order of work:

1. **Locate and unify the activity kernel.** Identify every current
   dependency-evaluation site (check kernel, get_values filter,
   check_dependencies, assignment path). Extract ONE list-basis activity
   routine (parameterized by basis; token-skip; transitive; then add
   default-aware operand selection). The contract's one-semantic-engine
   rule applies: after this change there must not be two point/list
   dependency evaluators. Confirm the get_values filter and the check
   kernel end up on the same routine.
2. Assignment: drop the dependency rejection; wire constraint filtering
   (only when a constraint exists).
3. Check-family + presence/required exemptions on the unified kernel
   (point basis).
4. get_values filter + check_required on the unified kernel (store basis).
5. Constraint adapters (BASE, dt, collection live/detached, shadow).
6. `$search_space()` rule per 3.6.
7. R docs/comments, vignette, NEWS.
8. Tests (section 6), including updating the characterization tests that
   encode strict assignment.
9. Contract documents: `design/contract-first-2.0.0.md` (transaction and
   check semantics, the two cold-family descriptions if they mention
   strictness), `design/compatibility.md` (new break-table rows for 3.5),
   `design/architecture.md`, `AGENTS.md` non-negotiables (the value-
   assignment bullet and the "one native semantic implementation" bullet
   gain the activity-kernel wording).
10. Process: differential expected-difference rows; benchmark additions
    (section 7); full local gate re-run; new candidate freeze; downstream
    waves; hosted portability re-run.

## 5. Cautions

- **Memory gates.** `paramset_check.c` edits require re-running the
  combined GCT/Valgrind/rchk memory validation. Respect the PROTECT
  discipline; remember the `schedule_vector()` ALTREP-duplicate lesson
  (an ALTREP `Duplicate` may return its input; never use pointer equality
  to decide protection release).
- **Atomicity.** Activity computation and constraint filtering belong to
  the validation phase; the commit wave stays allocation- and
  callback-free. Reentrancy/generation-check semantics unchanged.
- **Encodings.** The UTF-8/Latin-1 dependency-diagnostic tests
  (`tests/testthat/test-native-paramset-value-mutation.R:296` ff.) must
  stay green; dep diagnostics move out of assignment but remain in
  check-family with identical wording.
- **Store-blind points.** Easy to get wrong: when checking a point, the
  kernel has access to the capsule's stored values — it must not use them.
  Test 6.T8 guards this.
- **Where deps live is where filtering happens.** A collection-level
  (cross-set) dep filters collection-level reads only; a child set read
  directly returns its raw/child-filtered values (Appendix A1.T7). This is
  intended; document it in `ParamSetCollection` docs (one sentence) because
  mlr3pipelines' planned auto-branch-deps sit at collection level while
  PipeOps read their own sets.
- **Downstream messaging.** Carry into `compat/downstream-pr-handoff.md`
  notes: (a) mlr3 may want `Learner$hash`-style hashes over `get_values()`
  if canonicalization over dormant entries is desired (raw `$values` feeds
  hashes today — Appendix A3); (b) the AutoTuner refit crash class
  (`mlr3tuning/R/AutoTuner.R:422`) disappears; (c) `lrn(...)`/`$configure()`
  no longer error on dependent params — with default-aware evaluation the
  common svm-style cases now behave *correctly* instead.
- **miesmuschel divergence.** Its legacy shadow calls an explicit
  `self$assert(rhs)` before writing and stays strict; resolved by its v2
  migration branch to paradox's `ParamSetShadow`. No paradox action; note
  it in the handoff.
- The exact replacement-candidate corpus corrected an error in this plan:
  `TEST_MAKE_PS2()` actually recorded `default = "a"` for `xx`. Its two
  explicit point assertions therefore correctly became default-aware and
  failed their old error expectations. The small mlr3tuning adaptation removes
  that accidental helper default, restoring the tests as useful store-blind
  `NoDefault` regressions on both Paradox versions. A separate TuneToken-child
  expectation is version-gated because Paradox 2 deliberately skips the
  incoming edge and permits later dormant storage.

## 6. Test matrix

New tests (t = testthat unless noted). Activity kernel, via public surface:

- T1 transitive chain: A off, B stored-but-inactive, C depends B==stored →
  C inactive (regression of Appendix A1.T1).
- T2 default-aware: parent absent, default satisfies → child active in
  `get_values`, `check`, `check_dt` row, `check_dependencies`.
- T3 default-aware negative: parent absent, default does NOT satisfy →
  inactive/error; message fragment preserved.
- T4 NoDefault: parent absent, no default → inactive/error (guards the
  mlr3tuning/bbotk contract).
- T5 stored value overrides default: parent stored non-default,
  unsatisfying, though default would satisfy → child inactive (and the
  converse).
- T6 recursion with defaults, jakob-r's #265 puzzle as a named test:
  A `p_lgl(default = TRUE)`; B `p_lgl(default = TRUE, depends = A == FALSE)`;
  C depends B == TRUE. With empty basis: A active, B inactive (A's default
  is TRUE), C inactive regardless of B's default. Assert exactly this.
- T7 diamond/multi-row conjunction; CondAnyOf; multiple parents; a BASE cycle
  admitted through the existing mutation surface fails safely in every
  activity consumer.
- T8 store-blindness: set holds satisfying stored parent; `check(xs)` with
  xs lacking the parent (parent has no default) still errors.
- T9 token parent skip: parent = TuneToken in basis → edge skipped, in all
  four check-family consumers and in get_values filtering.
- T10 assignment accepts dormant (single, bulk, `.insert` TRUE/FALSE);
  bounds/type violations on dormant entries still rejected; unknown IDs
  still rejected; atomicity on mixed valid/invalid input.
- T11 reactivation: switch parent, dormant child returns in `get_values`;
  in-place update of a dormant value now works (was Appendix A2.P3 error).
- T12 collection graph-wide: cross-set dormant store via checked
  assignment; identity re-assignment `psc$values = psc$values` succeeds
  with dormant present (flips Appendix A1.T5); shadow write-through with
  dormant among visible and hidden values.
- T13 constraint filtering: BASE (constraint sees only active entries;
  ordering/once-per-validation unchanged), dt per-row, collection child
  constraint receives unprefixed active child slice, shadow merged-then-
  filtered; a constraint reading a dormant id sees it absent.
- T14 characterization (pin, no change): special-value parent operand
  (e.g. stored `NULL` via `special_vals`) in condition evaluation.
- T15 characterization (pin, no change): dependency on a `ParamUty` parent
  with plain-atomic vs opaque stored value — record current comparator
  behavior, whatever it is.
- T16 characterization (pin, no change): dangling-parent evaluation after
  `$deps <-` bulk assignment and `allow_dangling_dependencies` subset —
  then extend for the default-aware rule if the parent has a default.
- T17 presence tightening: `presence = "required"` demands a
  default-active child; `presence = "all"` cases.
- T18 `get_values(check_required = TRUE)`: required dormant exempt;
  required default-active demanded; required active-and-unset still errors
  (Appendix A1/A2.P5).
- T19 lifecycle: serialize/unserialize, deep clone, `all.equal` (dormant
  values compare — two sets differing only in dormant entries are unequal),
  `upgrade_paradox_object[_graph]` idempotent pass-through of dormant
  states, collection admission, `subset(keep_trafo=...)` variants.
- T20 `$search_space()` out-of-space parent rule per 3.6 (both branches).
- T21 `$check(ps$values)` non-invariant documented behavior: a legal
  dormant store fails strict check and passes
  `check(ps$get_values())` and `check(..., check_strict = FALSE)`.
- T22 design/sampler unchanged: grid/random/hierarchical designs on
  dep-bearing sets identical pre/post change (seeded).
- T23 encoding: dep diagnostics with UTF-8/Latin-1 ids/levels through
  check-family (existing tests keep passing).
- T24 `check_dt` stays point-strict per row: row with parent mismatched
  and child non-NA fails strict `check_dt`; NA-cells still mean absent.

Updates to existing tests: any characterization/contract test asserting
assignment-time dependency errors or raw constraint input (grep
`tests/testthat/test-characterization-*`, `test-native-paramset-value-
mutation.R`, `test-contract-*` for dependency-related assignment cases);
paradox's own suite otherwise has essentially no such expectations (all
"can only be set" tests go through `$check()` — verified).

Downstream verification (after local gates): re-run at least mlr3, bbotk,
mlr3tuning, mlr3pipelines, miesmuschel, mlr3mbo, mlr3learners, mlr3fselect
waves from `.local/compat/github-release-refresh-20260720/`. The first exact
replacement run intentionally exposed four obsolete expectations—three in
mlr3tuning and one inactive spline degree in mlr3pipelines. After the small
dual-version adaptations recorded in `compat/downstream-pr-handoff.md`, expect
zero remaining dormant-contract failures.

## 7. Benchmarks

- Expect the checked-assignment workloads to improve slightly (dependency
  evaluation removed); `get_values` on dep-bearing sets gains a default
  lookup — must stay within its tier.
- The release workload set lacks dependency-rich coverage (noted in
  `design/release-2.0.0.md`). Add two workloads: (a) checked bulk
  assignment on a 64-param set with a deep dependency chain, (b)
  `get_values()` on the same set with mixed active/dormant/default-active
  entries. Wire them through `benchmarks/workloads.R` + regression policy.
- The seven contract-reset integrity tiers are unrelated; do not touch.

## 8. Documentation and process checklist

- [x] Roxygen: `values`, `set_values`, `check/assert/test(_dt)`,
      `check_dependencies`, `get_values`, `test_constraint(_dt)`,
      `constraint` (filtered input), `Domain(depends)`; regenerate docs
      (note AGENTS.md roxygen-8.0.0 caveat: doc refresh needs the
      disposable debug build).
- [x] `vignettes/indepth.Rmd`: Dependencies section gains a "dormant
      values" paragraph + default-aware statement; Values section drops the
      claim that dependency constraints are enforced on assignment.
- [x] NEWS: one bullet per 3.5 item, under a "Dependency semantics" block.
- [x] `design/contract-first-2.0.0.md`, `design/compatibility.md` break
      table, `design/architecture.md`, `AGENTS.md` non-negotiables updated.
- [x] Differential harness: expected-difference rows for 3.5.
- [ ] Full local gates per `design/validation.md`; **new candidate freeze**
      (new ref under `refs/paradox-release/`); downstream waves; hosted
      Windows/macOS re-run on a new portability companion; ledger update.
- [x] `compat/downstream-pr-handoff.md`: add the three downstream notes
      from section 5.
- [ ] Close/annotate trackers on merge: #265 and PR #275 (implemented,
      default-aware), PR #343 (completed via read-side filtering + dormant
      storage), mlr-org/mlr3pipelines#101 paradox-side unblocked; verify
      and probably close miesmuschel#65 (NA-in-`assert_dt` already
      tolerant in v2).

Focused development evidence (not release-candidate evidence), 2026-07-26:

- the seven changed native translation units pass strict C17 syntax checks
  with `-Wall -Wextra -Wpedantic -Wconversion -Wshadow
  -Wstrict-prototypes -Wmissing-prototypes -Werror`;
- all 22 dormant-value contract blocks pass (156 expectations), and the
  package-wide isolated unit inventory is green after rerunning its sole
  optional ConfigSpace file with the already-cached local Python environment
  (effectively 90 files, 6,528 passes, five skips, no failures/errors/warnings);
- the six selected Paradox 1 differential cases are exact reviewed
  differences with no fingerprint mismatch or unexpected delta; and
- benchmark inventory/policy and worker-validation self-tests pass, including
  exact result validation for both new dependency-rich workloads. No complete
  benchmark, downstream, memory, or compatibility matrix was run in this
  development batch, by maintainer request.

## 9. Acceptance criteria

1. All tests in section 6 pass; full paradox suite green on both gate R
   versions.
2. `grep`-level proof of one activity kernel: the dependency-row
   evaluation loop exists once and is called from check-family,
   get_values, check_dependencies, and constraint filtering.
3. Memory gates (GCT/Valgrind/rchk) clean for the new candidate.
4. Differential run: only the reviewed 3.5 rows differ from v1.
5. Benchmark: all rows within tier, including the two new dep workloads.
6. Downstream waves: no unexplained failures vs the current ledger baseline;
   the mlr3tuning, mlr3pipelines, and miesmuschel dual-version adaptations
   exercise the intended dormant contract.
7. Docs/NEWS/contract updated; new candidate frozen and recorded.

## Appendix: verified baseline (2026-07-25/26, candidate 8797f11 + corpus release-refresh-20260720)

A1. Empirical probes against the installed v2 build (scripts:
`branch_dep_test.R`, `dormant_probe.R`, `dormant_probe2.R` in the session
scratchpad; reproduce freely):

- T1 The `get_values` filter is already transitive and default-blind.
- T2 Parent-unset-with-satisfying-default: child silently dropped by
  `get_values`; strict check errors with "…'x' is not set at all. Try
  setting 'x'…". (The case #265/default-aware fixes.)
- T3 `required` × deps already tolerant: required-but-inactive not
  demanded by `get_values(check_required=TRUE)` or `presence` modes.
- T4/P1 `check_dt` treats NA as absent universally (inactive AND active
  params; strict and non-strict).
- T5 Dormant state (created via `assert_values = FALSE`) survives
  serialize/clone/all.equal(clone)/collection admission/shadow/subset and
  `upgrade_paradox_object`; strict `check(own values)` fails; checked
  identity re-assignment fails today (flips after this change).
- T6 `to_tune()` tokens can already be stored on dependency-inactive
  params; `$search_space()` then contains the param without the dep.
  (Pre-existing; out of scope.)
- T7 Cross-set (collection-level) deps do not filter child-level
  `get_values` reads.
- P2 Reactivation on switch-back already works when the final state is
  coherent. P3 In-place update of a dormant value errors today.
- P7 `Design$transpose()` drops NAs; whole-list REPLACE assignment loops
  survive branch switches today; incremental `set_values` loops error.

A2. Paradox test suite: all "can only be set" expectations go through
`$check()` (`tests/testthat/test_deps.R`,
`test-native-paramset-value-mutation.R:344-351`); no assignment-time
dependency-error expectations found.

A3. The initial downstream source scan incorrectly predicted **0 breaking test
expectations**. Exact candidate execution found four expectation-only changes:
three mlr3tuning assertions (including the two explicit point checks whose
helper parent actually had `default = "a"`) and mlr3pipelines' inactive
splines `degree` constructor assertion. No runtime implementation failed.
Small dual-version test adaptations preserve strict NoDefault point checking,
the TuneToken-child rule, and raw-versus-filtered dormant storage. The broader
evidence still holds: mlr3tuning disables assignment validation in the tuning
loop (`ObjectiveTuning.R:66` `assert_values = FALSE`) and reset+replaces per
evaluation (`mlr3/R/worker.R:490-491`,
`ObjectiveTuningAsync.R:26-30`); AutoTuner refit assigns checked
(`mlr3tuning/R/AutoTuner.R:422`) and can hard-error today on hierarchical
spaces (class of crashes removed by this change); raw
`$values` feeds hashes (`mlr3/R/Learner.R:844`, `HotstartStack.R:235-240`,
`mlr3pipelines/R/PipeOp.R:414` → `Graph.R:494` → `GraphLearner.R:373`,
`mlr3torch/R/LearnerTorch.R:418`); learners consume filtered values
(`get_values(tags=)`) in train/predict throughout; the svm canonical case
is `mlr3learners/R/LearnerClassifSVM.R:32` (`cost` depends
`type == "C-classification"`, `type` default `"C-classification"`);
historical strictness workarounds: `mlr3pipelines/R/PipeOpLearnerCV.R:184-190`,
`miesmuschel/inst/tinytest/test_ParamSetShadow.R:255`,
`mlr3mbo/R/AcqOptimizer.R:132-139`. No downstream code uses
`check_strict =` or `remove_dependencies =` explicitly anywhere.
