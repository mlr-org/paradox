# Migration-layer hunt follow-ups (2026-07-29)

Deep-dive verdicts on the five low-confidence migration-layer leads from the
2026-07-28 bug hunt (the "came back 1/2" list). Three were real defects and
were fixed immediately; the remaining two were correct-but-sharp behavior
written up here for a ruling, which the maintainer gave the same day:
implement the registration guard (item 4) and the recommended gateway path
(item 5). Both are now implemented; their sections keep the analysis and
record how each landed. Line references are to the tree at `224a7ec` plus
this batch.

## Fixed on the branch (for the ledger)

### 1. `upgrade_paradox_object()` returned its prepared graph unvalidated

Lead: "doesn't run the joint graph validation that
`upgrade_paradox_object_graph()` does, so the same legacy fixture passes one
API and aborts the other." **Confirmed, with a concrete witness** (the
unnamed-values fixture below): the recursive API's commit phase proves every
prepared root jointly (`.upgrade_paradox_validate_current_roots`) before the
first original shell changes, while `.upgrade_paradox_graph()` handed
`session$prepared[[1L]]` to the caller with no such barrier. A preparation
defect therefore failed *closed* in one API and *open* in the other — the
single-object API returned a split-brain object whose `$values`, `print()`,
`$clone()`, and `generate_design_random()` worked while `$check()`,
`$get_values()`, `$set_values()`, and `$subset()` reported a corrupt capsule.

Fix: `.upgrade_paradox_graph()` now runs the same joint validation over
`session$prepared` before returning, failing as
`"Cannot upgrade Paradox object at x: prepared/current roots failed joint
validation (...)"` (the helper gained a `failure` clause parameter; the commit
sites keep their wording). Regression test: "the single-object API refuses to
return an invalid prepared graph" (surgically re-poisons preparation through a
rebound `.upgrade_paradox_values`).

### 2. Carrier recognition disagreed with the crate-rebuild decision

Lead: two guards on the legacy `psc_extra_trafo`/`psc_constraint` crate
disagree. **Confirmed as an input-mutation bug.**
`.upgrade_paradox_strip_legacy_extra_trafo()` rebuilds a recognized collection
crate with a fresh environment only when `typeof(sets_with_trafos) == "list"`
and `is.function(psc_extra_trafo)`; `.upgrade_paradox_callback_carriers()`
recognized the crate on shape alone. A template-shaped wrapper whose `psc_*`
binding is not a function (tampered/degenerate fixture; no genuine Paradox 1
serialization produces one) was therefore *counted as a carrier crate but not
rebuilt*, and `.upgrade_paradox_rebind_callback_carriers()` then wrote the
prepared children **into the serialized object's own closure environment**:

- `upgrade_paradox_object()` violated its "input is never mutated" contract
  (`serialize()` before/after differed; the legacy crate env ended up holding
  the prepared current children, shared with the returned object);
- in the recursive API the same write happened during *prepare*, violating
  "build every replacement before touching a serialized shell" — an abort
  after that point left the input silently tampered.

Fix: recognition in `.upgrade_paradox_callback_carriers()` now requires
exactly the rebuild predicate, so "counted as carrier" ⟺ "rebuilt with a
fresh environment". An unrecognized wrapper is carried like any other opaque
callback (documented single-object semantics: "does not traverse arbitrary
callback environments"), and the recursive API still reaches the captured
children through native closure discovery and upgrades them in place.
Regression test: "carrier recognition agrees with the strip rebuild decision".

### 3. Zero-length unnamed legacy `$values` poisoned the capsule

Lead: possibly unreachable because 1.0.1 canonicalises to `named_list()`.
**Reachability confirmed as tampered-fixture-only, but the failure mode was
fail-open, which this layer treats as a defect.** Only the zero-length unnamed
list passes `.upgrade_paradox_values()`'s name checks with `names()` `NULL`
(any longer unnamed list aborts); `param_set_core_replace()` installs fields
verbatim, and `paradox_domain_validate_values()` requires the names attribute
outright, so the prepared capsule was admitted at build time and rejected by
every later graph admission (the split-brain object of item 1).

Fix: `.upgrade_paradox_values()` installs the validated names
unconditionally, so the copy is canonical (`named_list()`-shaped) even when
the fixture stored a bare `list()`. Both APIs now migrate the fixture
successfully. Regression test: "a zero-length unnamed legacy values list
upgrades to the canonical named list".

## Ruled and implemented (2026-07-29)

### 4. The upgrader registry admits provably-unsatisfiable bridge registrations

Lead (old line 1642): "an additive ParamSetShadow subclass classes as BASE in
shell_auth while its capsule stays SHADOW; fails closed with a clean error."
The code at that line is **correct and fails closed**; a comment now records
the invariant chain at the check. But the investigation surfaced the sharper
underlying fact, proven and demonstrated live:

- `paradox_param_set_class_kind_raw()` classes a registered legacy vector
  `c(<owner>, "ParamSet", "R6")` as SHADOW **iff** `<owner>` is exactly
  `"ParamSetShadow"` (`src/shell_auth.c:57`); every other owner label classes
  as BASE with one additive label.
- Graph admission accepts a shell only when class kind equals capsule kind
  (`src/paramset_check.c:1285`, `src/shell_auth.c:306`).
- `.upgrade_paradox_build_owner()` requires a replacement result to carry a
  SHADOW capsule, and overwrites an additive result's `.core` with the BASE
  capsule.

Therefore a **"replacement" bridge is satisfiable only for
`legacy_class = c("ParamSetShadow", "ParamSet", "R6")`** (the miesmuschel
case), and an **"additive" bridge is unsatisfiable for exactly that vector**.
Yet `register_paradox_object_upgrader()` and
`.paradox_validate_object_upgrader_entry()` accept both doomed combinations
(demonstrated: the entry validator returns them as valid). A bridge author who
registers one discovers the impossibility only per-object at migration time,
as `"owner upgrader ... failed"`/`"corrupt current state capsule (Corrupt
ParamSet state: capsule kind disagrees with shell class)"` — a message that
blames corruption when the actual mistake is the registration.

**Implemented** (rejects only configurations that can never migrate a single
object): `register_paradox_object_upgrader()` enforces

```
(migration_kind == "replacement") == (legacy_class[[1L]] == "ParamSetShadow")
```

with direction-specific messages ("A replacement upgrader can only be
registered for the legacy class vector ..." / "An additive upgrader cannot be
registered for the legacy class vector ..."). Deliberate scoping decision:
the guard lives in `register()` only, NOT in
`.paradox_validate_object_upgrader_entry()` — the entry validator guards
registry *shape* integrity and is also the seam the test suite uses to inject
synthetic entries (`with_replacement_owner_upgrader` and the
inspection-envelope tests); a well-shaped but semantically dead entry is not
"corrupt", and `.upgrade_paradox_build_owner()`'s kind checks plus graph
validation remain the fail-closed backstop for anything injected past
registration (a comment at that site records the chain). Docs updated:
roxygen paragraph on `register_paradox_object_upgrader()` (Rd regenerated),
the NEWS registry bullet, and the registration list in
`design/contract-first-2.0.0.md`. Tests: the `retired_bindings` acceptance
probe in `test-upgrade-registry.R` moved to the one satisfiable replacement
vector (doubling as its positive test), and both refusal directions are
pinned.

### 5. Serialized Paradox 1 `Design`: `transpose(trafo = TRUE)` fails with a capsule error, not the upgrade hint

Lead: "a serialized paradox-1 Design re-enters the 2.0.0 namespace through
`.__Design__transpose` and runs today's implementation against yesterday's
object." **The direct rebinding itself is correct** — Design and the Sampler
family kept their Paradox 1 public field layout, so today's bodies read a
serialized v1 shell fine; a comment at the rebinding branch
(`R/leanify_paradox.R`) now records that premise and the boundary below.

The genuine gap is the embedded **legacy `param_set`** those shells carry:

- Every Sampler path touches it through R6 accessors (`$length`, `$ids()`,
  `Design$new()`'s asserts), so a legacy shell lands in the ParamSet gateways
  and gets the actionable "Upgrade the containing object with
  `upgrade_paradox_object_graph(x)`" error, honoring
  `paradox.legacy_object_action = "upgrade"`.
- `Design$transpose()` goes native immediately. Demonstrated live on a
  v1-layout Design carrying a legacy ParamSet:
  `transpose(trafo = FALSE)` **silently works** (the data path never needs the
  capsule); `transpose(trafo = TRUE)` (the default) fails with
  `"Corrupt ParamSet shell in Design transformation"` — corruption-flavored,
  no upgrade hint, and the auto-upgrade option is never consulted.
  (`paradox_domain_private_environment()` requires a `.core` binding;
  a legacy private has none.)

`upgrade_paradox_object_graph()` on the container heals everything (discovery
reaches the Design's `param_set` field and upgrades it in place), so this is a
first-use UX gap, not a correctness gap.

**Implemented** (the recommended option): a gateway on the **unversioned**
`.__Design__transpose` binding itself
(`.paradox_make_design_transpose_gateway()` installed by the direct-bind
branch of `.paradox_leanify_package()`), not inside `Design$transpose()`.
Current v2 objects are leanified to the versioned
`.__paradox2_Design__transpose` target and never resolve the old name, so
only serialized Paradox 1 / pre-release shells enter here — the hot path pays
**zero** and no benchmark question arises. The gateway checks
`self$param_set` currency (`.paradox_gateway_current_core`), runs the
standard first-use flow — actionable error by default,
`upgrade_paradox_object_graph(self)` under
`paradox.legacy_object_action = "upgrade"` — and forwards to the versioned
target. Two consequences accepted deliberately:

- a graph-healed v1 Design still calls the old name forever (its serialized
  stubs are never rewritten), so the "param_set already current → forward"
  fast path is load-bearing, exactly as in the ParamSet-family gateways;
- `transpose(trafo = FALSE)` on an *unupgraded* v1 Design used to work
  silently (data-only path) and now raises the actionable first-use error
  unless auto-upgrade is on — a narrowing that matches the ParamSet-family
  first-use precedent and removes the argument-dependent half-working state.

Tests: "current Design stubs bypass the transpose gateway" and "serialized
Design transpose gets the first-use gateway" in
`test-upgrade-paradox-object-graph.R` (error → auto-upgrade → healed fast
path, identity preserved). The Sampler old names keep the plain alias — every
Sampler path reaches the embedded ParamSet through R6 accessors and already
lands in the ParamSet gateways; extending the gateway shape over that whole
surface (so first-use errors name the member actually called) remains an
option if a native-direct Sampler path ever appears.

Rejected alternatives, for the record: message-only native rejections (would
leave `trafo = FALSE` silently working and the auto-upgrade option inert) and
documented status quo.
