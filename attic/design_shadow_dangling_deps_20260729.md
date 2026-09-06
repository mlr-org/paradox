# Design: ParamSetShadow tolerates dangling dependencies (2026-07-29)

Status: IMPLEMENTED on 2026-07-29 over `92a9d6b`, with the recommended
resolution of every decision point (D1--D6). Follows the 2026-07-29 review
conversation on `92a9d6b`; the probe scripts referenced below live in the
session scratchpads of that conversation. See §7 for what the implementation
found that this plan did not anticipate.

## 0. Summary and motivation

A dangling dependency — a dep row whose `on` names no existing parameter,
created with `allow_dangling_dependencies = TRUE` — is a first-class v2
concept: the constructor (`ParamSet$new(..., allow_dangling_dependencies =)`,
`ps(..., .allow_dangling_dependencies =)`), `$add_dep()`, `$subset()`, and
`$flatten()` all speak it, the bulk `$deps<-` snapshot validates only the
dep's *child* id (`paramset_mutate.c` `snapshot_dependencies`), and every BASE
and COLLECTION read surface tolerates the row (check answers "never
satisfiable", values store dormant, designs produce `NA`, upgrade migrates,
clone/serialize round-trip). The one outlier is `ParamSetShadow`:

- `validate_related_state` (`paramset_shadow.c:355`, used by both
  `validate_base_origin` and `validate_shadow_template`) requires every dep's
  `on` to be a parameter, so a shadow over a BASE origin carrying a perfectly
  valid dangling dep dies with "Corrupt ParamSetShadow BASE origin state";
- `filter_dependencies` (`paramset_shadow.c:653`) classifies `on` against
  *visible ids only*, so over a COLLECTION origin the same row is
  misdiagnosed as "reach across shadow bounds" (the parameter is not hidden;
  it does not exist);
- under the live-schema heal, an existing working shadow whose origin *later*
  gains a dangling dep goes permanently dark with the corruption message.

miesmuschel's v1 ParamSetShadow tolerated all of this (its crossing check was
the XOR `(id %in% shadowed) != (on %in% shadowed)`, which an absent `on`
passes; construction worked, `$deps` showed the row verbatim, the view
enforced it live, and `add_dep` honored the flag). The v2 refusal is
therefore a v1 regression, not a documented policy.

Tolerating dangling deps in shadows also composes exactly with the new live
"origin minus hidden" semantics: the hidden set is fixed at construction from
then-existing ids and origin ids are unique, so **a dangling dep that later
resolves always resolves to a visible parameter** — the view picks it up
automatically, the same way it picks up a parameter the origin gains.

## 1. Grounded current state (evidence)

### 1.1 The three shadow sites

| Site | Behavior today |
|---|---|
| `validate_related_state` (`paramset_shadow.c:355-387`) | conjunct `related_ids_are_known(&parameter_ids, dependencies->on, ...)` refuses any absent `on`. Callers: `validate_shadow_template` (:424, the shadow's own payload) and `validate_base_origin` (:485, BASE-origin admission). Shadow-local; no other file uses it. |
| `filter_dependencies` (`paramset_shadow.c:653-706`) | per row computes `id_visible`/`on_visible` against visible ids; `id_visible != on_visible` errors "Params %s have dependencies that reach across shadow bounds"; both-hidden rows drop silently; both-visible rows are kept. Absent is indistinguishable from hidden. Single caller: `assemble_shadow_core` (:1120), which already has `source_params` (the origin id universe) in scope. |
| `add_dep` SHADOW branch (`paramset_mutate.c:638-698`) | validates the flag then discards it; requires `id` *and* `on` visible ("Shadow dependencies must stay inside the visible schema"); delegates to the origin with `FALSE`. |

### 1.2 What already tolerates dangling rows (verified, no work needed)

- Check plan: per-node validation resolves only the dep's **id**
  (`paramset_check.c:808-816`); an unresolved parent short-circuits at
  evaluation ("TuneToken children and dangling parents deliberately
  short-circuit", `design_dependencies.c:498`). A SHADOW node is a single
  node whose local table *is* the visible flat table, so a visible dangling
  row flows through checking exactly like BASE.
- Dormant-value store, `$get_values()` activity filtering, design/sampler
  generation (`NA` column), `$qunif`, `as.data.table`, `print`, deep clone,
  serialization, `upgrade_paradox_object()`.
- `$subset()` refuses-by-default with a named error and a
  `allow_dangling_dependencies = TRUE` escape (v1 parity, incl. refusing
  *pre-existing* dangling rows); `$flatten()` always tolerates.
- `$search_space()` guard "Dangling dependencies not allowed: Dependencies on
  %s dangling." (`R/ParamSet.R:1078-1081`) protects the tune ps;
  `get_tune_ps` carries only deps among tuned ids, so a dangling dep on the
  source set is silently dropped from the search space — identical in v1.

### 1.3 v1 facts (paradox 1.0.1 `main`, miesmuschel 0.0.4-3 sources, probed)

- BASE: dangling dep = the dependent parameter is un-settable through checked
  paths until `on` exists and is satisfied; never an object-level error.
  (v2 differs deliberately via dormant values: the checked store *accepts*
  the dependent value as dormant.)
- COLLECTION: `map_values` renamed only ids it knew; a dangling `on` passed
  upward **verbatim, un-prefixed**, and `$check` resolved it against the
  collection-level flat namespace — the mlr3pipelines union pattern (child
  declares `on = "t.y"`, the sibling arrives later, the dep starts being
  enforced).
- miesmuschel shadow: tolerated absent `on` everywhere; live; `add_dep`
  honored the flag. One v1 bug NOT to reproduce: `sh$add_dep(id, on =
  <hidden>, TRUE)` planted a *crossing* dep in the origin which the view then
  silently filtered — a state its own constructor refused.
- v1 designs hard-errored on dangling deps with a raw internal checkmate
  message; v2 produces `NA` columns (better, but an undocumented
  differential).

### 1.4 The blocking discovery: v2's dangling-`on` resolution scope is incoherent

Probed on `92a9d6b` (collection `s(x)` with `x -> "t.y"` dangling, then
`$add(ps(y = ...), "t")`):

| Consumer | Resolution scope for a dangling `on` |
|---|---|
| `$deps` getter (`translate_dependency_id`, `paramset_collection_deps.c:10-38`) | first matching ancestor, then outward-prefixed; verbatim if never matched. Displays `t.y`; in a nested collection displays `o.b.y`. |
| `$check`/`$test` (`paramset_check.c:1396-1414`) | **owning node only** (`local_param_row`); a sibling-named `on` never resolves — "'t.y' is not set at all" even when `t.y = 2` is supplied. |
| designs / `SamplerUnif` / `$get_values` | flat at the reading root (they consume the getter's translated table; `design_dependencies.c:216,498,523`). Enforce the dep. |
| `ps_union` | works, but only because v2's `ps_union` returns a flattened plain ParamSet. |

Consequence on one object: `co2$check(co2$get_values())` is FALSE while
`co2$subset(c("s.x", "t.y"))$check(<the same list>)` is TRUE. v1's collection
check resolved flat and enforced the dep, so this is a **pre-existing
v1-parity break** (present before `2bc63a1`), and it kills the "dangling dep
resolves when the union completes" pattern on live collections.

This must be settled *before* the shadow work: a shadow over a collection
projects the getter-translated table, so its own (BASE-like) check would
enforce a dep the origin collection's check ignores — the shadow would become
a fourth scope.

### 1.5 A `2bc63a1` regression found on the way

`paramset_to_configspace()` on a set with a dangling dep now fails with a bare
`subscript out of bounds` from `parent_classes[[on]]` (the ParamLgl
stringification added in `2bc63a1`). The path never worked — v1 failed
python-side — but the failure moved earlier and lost its name.

## 2. Decision points

**D1 (prerequisite). Unify the dangling-`on` resolution scope.**
Recommendation: the translation-walk semantics the getter and the design
plan already implement — resolve `on` through `translate_dependency_id`
(nearest enclosing namespace, then outward-prefixed), then look it up in the
reading root's flat table; a miss means never-satisfiable. Concretely: the
check plan builder resolves a locally-unresolvable `on` through the same walk
instead of giving up at the owning node. This restores v1's union-resolution
behavior on live collections (v1 was verbatim-then-flat; the walk is a
superset that additionally resolves nested cases the getter already displays
as resolved) and makes check/get_values/designs/getter agree. It is a
behavioral change: deps that were silently never-satisfiable in collection
`$check` become enforced. That is the v1 behavior and what every other v2
consumer already does.

**D2 (the ask). Shadow classification rules.** For each origin dep row, with
`V` = visible ids, `O` = current origin ids (post-heal), `H` = retained
hidden set:

| dep `id` | dep `on` | behavior |
|---|---|---|
| visible | visible | keep (unchanged) |
| visible | in `O`, not visible (hidden) | crossing error (unchanged) |
| visible | not in `O` | **keep as dangling row (new)** |
| hidden | visible | crossing error (unchanged; both directions stay errors) |
| hidden | hidden | drop silently (unchanged) |
| hidden | not in `O` | drop silently (new case; the dep governs a hidden parameter, the origin's business) |

Membership in `O` is tested before membership in `H`, so a hidden id that
vanished from a forged/legacy origin cannot resurrect a crossing error for a
name that no longer exists. Invariant (test it): a dangling `on` can never
later resolve to a hidden parameter — `H` is a subset of construction-time
origin ids and origin ids are unique, so a later-gained id is always visible.

**D3. `add_dep` through the view honors the flag.** `on` visible → as today.
`on` hidden → crossing error regardless of the flag (deliberately not
reproducing miesmuschel's escape hatch). `on` absent from the origin →
with `allow_dangling_dependencies = TRUE`, delegate to the origin *with
TRUE*; with FALSE, the BASE-parity error ("`on` is not a parameter in this
ParamSet"). Requires distinguishing hidden from absent, i.e. reading the
origin's params — the branch already fetches the origin for delegation.

**D4. Crossing-error message.** Today: "Params %s have dependencies that
reach across shadow bounds", pinned by `test-ParamSetShadow.R:124,130,134`
and `test-upgrade-paradox-object.R:1597` (both directions). Once absent no
longer lands here, the message is only ever about hiddenness; recommend
naming both ends and the direction ("Dependency of 'x' on 'h': 'h' is hidden
by this ParamSetShadow" / "hidden 'q' depends on visible 'p'"). Optional —
check `design/compatibility.md` for a ledgered differential row before
changing; update the four pins.

**D5. `paramset_to_configspace()` guard** (the §1.5 regression): a named
error — "cannot export dependency of 'a' on 'future': no such parameter" —
before `parent_classes[[on]]` is indexed. Independent of the rest; smallest
possible fix.

**D6. Differential rows.** (a) v2 designs produce `NA` where v1 designs
raised a raw internal assertion — keep, add the differential/NEWS row.
(b) checked `$values<-` stores a dep-inactive value as dormant where v1
refused — presumably already ledgered with dormant values; verify a dangling
parent is covered by that wording. (c) `$search_space()` silently dropping a
source-set dangling dep is v1-identical; one doc sentence.

## 3. Implementation plan (ordered)

**Phase 0 — D1 alignment (own commit).**
`paramset_check.c`: where the plan builder resolves a dep's `on`
(`:1396-1414`), on local miss run the same owner-to-root translation walk the
getter uses, then resolve against the root table; miss = never-satisfiable
(today's behavior). `translate_dependency_id` is static in
`paramset_collection_deps.c` — export it (header touch ⇒ `rm -f src/*.o`
before rebuild) or extract into `paramset_domain_common`. The check graph has
the owner chain; mind that the walk must use the *snapshot* tables of the
admitted generation, not live re-reads. New regression file pinning the
coherence quadruple (check == get_values == design == getter on one object;
the `co2$check(co2$get_values())` probe is the flagship), the live union
pattern (dep enforced only after `$add` supplies the sibling), the nested
`o.b.y` case, and the wrong-space control (`on = "y"` child-local spelling
never resolves — in either version).

**Phase 1 — shadow tolerance.**
1. `validate_related_state`: drop the `dependencies->on` conjunct (both
   callers need the relaxation: BASE-origin admission and the shadow's own
   payload, which now legitimately carries dangling rows). Keep the
   `dependencies->ids` conjunct.
2. `filter_dependencies(source, visible_ids, origin_ids, ...)`: implement the
   D2 table. `assemble_shadow_core` passes `source_params->ids`; no other
   signature changes.
3. `add_dep` SHADOW branch per D3.
4. Messages per D4 (+ the four test pins).
5. Docs: the two ParamSetShadow paragraphs that currently say a dangling
   dependency cannot be declared through a view; roxygen for `add_dep`; NEWS;
   the AGENTS.md "derived schema stays live" battery gains a dangling clause;
   `design/contract-first-2.0.0.md` Shadow section. Roxygen regen is the
   documented disposable-debug-build dance.

**Phase 2 — D5 guard in `R/config_space.R`** (can ride with either commit).

## 4. Watch-outs

- **The relaxed validator also loosens the shadow's own payload admission**:
  a hand-forged shadow capsule with dangling rows becomes admissible. Every
  consumer of shadow `.deps` is id-keyed (verified in §1.2), so the worst a
  forged absent-`on` row can do is be never-satisfiable — but re-verify the
  activity kernel and the constraint adapter's hidden-values plan never index
  by `on` without a found-check.
- **Shadow-over-collection input space**: `filter_dependencies` sees the
  origin's aggregated table in getter-translated spellings. After Phase 0
  that space is coherent with `visible_ids` and with the origin's flat ids;
  without Phase 0 the shadow would enforce deps the origin's check ignores.
  Do not reorder the phases.
- **Refresh paths**: deps are not in the schema slice; they are re-filtered
  on every rebuild (both the slice-unchanged template-reuse path and the full
  rebuild), and the signature-match early return keeps the payload as-is.
  Dangling rows must survive all three — the classification is pure, so this
  is a test obligation, not a code one.
- **Stamp discipline**: no new stamping sites; the filter runs inside the
  existing epoch-guarded builds. Keep it allocation-order-neutral.
- **`Rf_error` inside `filter_dependencies`** runs mid-build with protected
  intermediates — the existing error style there is fine; don't add
  allocations between the count pass and the fill pass without re-balancing
  PROTECTs.
- **Message pins**: the four crossing-message pins, and the upgrade-path pin
  at `test-upgrade-paradox-object.R:1597`, must move with D4 in the same
  commit.
- **Performance**: one extra membership test per dep row only when `on` is
  not visible; shadow value hot paths untouched (deps filter runs on rebuild
  only). Run the balanced benchmark pairs anyway (repo rule); watch
  `shadow_values_live` and the collection read rows; Phase 0 adds a
  translation walk per *locally-unresolvable* dep row in check-plan builds —
  measure a collection check with many ordinary deps to show the walk is not
  entered for resolved rows.
- **Build hygiene**: exporting the translate walk touches a header — clean
  `src/*.o` before rebuilding, or stale objects will fake corruption.
- **No registration/arity changes** anywhere in the plan — no probe or
  coverage-tsv churn expected; assert that at the end
  (`environment/native-routine-coverage.tsv` diff must be empty).

## 5. Tests that need particular care

1. **Coherence quadruple** (Phase 0): one object, four consumers, one
   answer; plus the live union-resolution pattern and its nested variant;
   plus `co2$check(co2$get_values())` as the regression anchor.
2. **The two conversation probes as regressions**: construction over a
   dangling-dep BASE origin; an existing shadow whose origin gains a dangling
   dep later (was: permanently dark with a corruption message; now: usable,
   dep visible and enforced-as-never-satisfiable).
3. **Live resolution through the view**: dangling dep, then the origin gains
   the target id — the view's check flips from never-satisfiable to
   conditional enforcement without any explicit refresh call; the same under
   `gctorture()`; the same when the origin is a collection and the target
   arrives via `$add()` (heal + resolution in one read).
4. **Crossing stays refused, both directions**, at construction and when the
   origin gains the crossing dep later; hidden-id dangling rows stay
   invisible and harmless. If D4 changes the message, the four pins move.
5. **`add_dep` matrix through the view**: {visible, hidden, absent} ×
   {TRUE, FALSE} — six cells, each with the exact error or the origin-side
   effect asserted (incl. that absent+TRUE plants the dep in the origin and
   both the origin and a second shadow observe it).
6. **Parity rows**: for BASE-origin and COLLECTION-origin shadows, `$deps`,
   `$check`, `$test`, `$get_values`, checked `$values<-` (dormant store),
   `$subset` (named refusal + flag), `$flatten`, designs/sampler (`NA`
   column), `$qunif`, `$search_space` (silent-drop parity), `print`,
   `as.data.table` — each equal to the origin's own answer modulo hiding.
7. **Round trips**: serialize and `clone(deep = TRUE)` of shadows with
   dangling rows (both origin kinds), then re-derivation afterwards.
8. **Migration**: upgrade of a legacy graph containing a shadow-shaped node
   over a dangling-dep origin (today the preflight refuses via the relaxed
   validators' old behavior); the read-only preview path must tolerate
   without installing.
9. **Never-hidden-resolution invariant**: attempt to re-add a hidden id to
   the origin (duplicate refusal proves the invariant holds by construction).
10. **Fuzzer extension** (optional): teach the randomized invariant fuzzer
    from the 07-28 hunt to sprinkle dangling rows.

## 6. Verification

Full `scripts/native-check --mode strict-gcc --mode asan --mode ubsan --tests
full` (as-cran + depends-only included), the instrumented root-carrier driver
if `core_state.c`/graph code is touched (Phase 0 touches the check builder —
run it), balanced benchmark pairs with the in-process median-of-7
confirmation for any non-flat row, and a `diff` of
`environment/native-routine-coverage.tsv` proving no registration churn.

## 7. What the implementation added or corrected

- **A fourth resolution site.** §1.4 named the `$deps` getter, `$check`, and
  the flat consumers. `collection_active_constraint_row()`
  (`paramset_trafo.c:1486`) is a fifth consumer and a *second* per-node
  resolver: it decides which values reach each child constraint and resolved
  `on` inside the owning node only. Left alone it would have disagreed with
  the fixed check (a value the check calls active would have been filtered out
  of the constraint's input), so Phase 0 unified it through the same exported
  walk. The check-graph site could not share the collection-graph helper --
  it has its own node type -- so it re-expresses the walk over the check
  graph's precomputed `root_ids`, with a comment binding the two.
- **A fifth message pin.** §4 listed four pins for the crossing message.
  `test-native-paramset-mutation.R:452` pinned the *`add_dep`* message
  (`"visible schema"`) as well, and it is the one D3 replaces.
- **`add_dep` messages.** D3 fixed the policy but not the wording. The view
  now says `'<id>' is hidden by this ParamSetShadow` for an endpoint it hides
  on the `id` side, the shared crossing message for a hidden parent, and the
  BASE-parity `` `id`/`on` is not a parameter in this ParamSet`` for an absent
  one. The one shared crossing sentence lives in `paramset_shadow.h` as
  `PARADOX_SHADOW_CROSSING_MESSAGE` so the projection and the declaration
  cannot drift apart.
- **Stale contract text.** The `### SHADOW` sections of
  `design/contract-first-2.0.0.md` and `design/architecture.md` still described
  the pre-`2bc63a1` frozen visible schema. They were corrected along with the
  dependency clauses.
- **Cost.** No hot path changed: the extra origin-ID membership test runs only
  for a row that already leaves the visible schema, the outward walk only for a
  parent its own node does not know, and `validate_related_state` now performs
  one *fewer* scan per shadow refresh.

## 8. Out of scope

Full ConfigSpace export of dangling deps (only the D5 named error), any
change to `$search_space()` semantics beyond documentation, memoisation of
occurrence-based graph expansion, and the `$deps<-`-on-derived-kinds surface
(stays refused).
