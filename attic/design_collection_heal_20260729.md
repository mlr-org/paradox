# Design: live rebuild ("heal") for stale ParamSetCollection flattens

2026-07-29, Claude. Status: **implemented.** The decisions below were settled
by the maintainer as D1 = live, D2 = same batch, D3 = keep `.v1` (and no
intra-2.x migration machinery — none existed), D4 = implement.
Baseline: `paradox_c` at `d89d58b` plus the uncommitted 2026-07-28 batch
(punctuation, barren prune, conflict warning, Domain messages).

## 0. What the implementation does differently

Recorded here because the design text below is the pre-implementation
argument, not the landed shape.

- **`.edges` is a tolerant cache, not a coupled field.** It is validated for
  shape only, never against the current `.sets` length. An edge list installed
  without a matching record simply makes the node stale, which keeps the
  adversarial `.sets`-forging probes reporting the cycle they are about rather
  than an edge-record error, and keeps the record's failure mode "rebuild"
  instead of "corrupt".
- **The stamp mixes the capsule's own address into the stored value.** R's
  `attr<-` copies the address slot verbatim when it duplicates a referenced
  external pointer, so an address-independent stamp would have let a rewritten
  duplicate inherit the original's proof. A SHADOW additionally
  reauthenticates its refresh signature on the fast path, because an ordinary
  in-place `attr<-` changes no address at all.
- **No preview heal.** Migration preflight may not install anything into a
  current shell, so it keeps previewing SHADOW generations offside and does
  not re-flatten collections; see the migration bullet below for what a stale
  collection now reports there. A single read of the object heals it.
- **`$tags<-` on a derived node became durable rather than refused.** This
  case (§G5 of the reconnaissance, not in the original design) had no answer in
  the design: the flatten would silently discard the assignment. It is now
  anchored in a `tag_override` inside `.edges` -- the node's own answer for
  exactly the IDs it named, re-applied after every re-derivation, with the sets
  left untouched. Tags are the one derived field a derived node may own
  outright, which also means two views over one set can tag the same parameter
  differently. IDs no assignment named stay derived. `ParamSetShadow$tags<-`,
  previously read-only, gained the same behavior. A field-level capsule
  replacement now also carries a SHADOW's snapshot carrier onto the replacement
  generation -- a latent gap `$tags<-` on a shadow was the first caller to
  reach.
- **The SHADOW `.edges` record** carries the origin schema slice as well as the
  hidden ID set, so an unchanged projection is carried forward with its field
  objects intact. Without that, every value commit behind a Shadow would look
  like a schema change to a collection above it.
- **Two epochs, not one.** §3.3's single schema epoch governs COLLECTION
  flattens; D4 needs a second, all-changes epoch for SHADOW projections, since
  a Shadow caches values. Each capsule compares against the epoch its own kind
  is invalidated by, so a tuning loop's value commits never touch a collection
  stamp.
- **Migration meets the heal in two places.** Paradox 1 let a caller set tags
  on a collection itself, and consumed the `tag_sets`/`tag_params` flags at
  `$new()`/`$add()` while keeping only their output. The upgrader therefore
  reads those flags back off the tags they generated and records them in
  `.edges`, so a migrated collection keeps generating them; tags a Paradox-1
  caller set directly on the collection that no set accounts for are preserved
  as that collection's own `tag_override`. And because
  migration preflight may install nothing, it is the one boundary that cannot
  re-flatten a stale collection: it now says so ("read it once ... before this
  operation") instead of reporting the mismatch as capsule corruption.
- **Benign drift does not restamp.** §3.2 updated `edges.stamped[i]` in place
  when only a child's values had moved. The implementation leaves the record
  alone and re-compares the three schema-slice pointers on the next walk
  instead: in-place mutation of a frozen generation would have to reason about
  payload fields shared by `shallow_duplicate` across generations and across a
  deep clone, and the comparison it saves is three pointer loads. The cost is
  that one superseded child generation stays reachable from the parent's
  record until that parent re-flattens.

This is the design for ledger item 2: a contained `ParamSetCollection` whose
child changes schema after construction. The chosen direction was "live
rebuild". This document grounds that in the actual architecture, extends the
problem inventory (two further instances of the same staleness class were
found while designing), proposes a concrete mechanism, and lists the
decisions that are policy rather than engineering.

---

## 1. Problem inventory (measured, not assumed)

Probes: `scratchpad/probe_v2.R` against the current build,
`scratchpad/probe_101.R` against `main` (1.0.1-9000) installed into a scratch
library.

### 1a. The known case: `inner$add()` under an ancestor

```r
inner = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
outer = ParamSetCollection$new(list(o = inner))
inner$add(ps(y = p_dbl(0, 1)), "b")
```

| operation on `outer` | v2 today | 1.0.1 |
|---|---|---|
| `$ids()` | stale (`o.a.x`) | stale (`o.a.x`) |
| `$values` (get) | **error** "Corrupt … child capsule state" | half-live: shows `o.b.y = …` once inner has a value — a name `outer`'s own schema does not contain |
| `$check(list(o.a.x = .5))` | **error** "child schema size mismatch" | `TRUE` (new param invisible) |
| `$check(list(o.b.y = .5))` | error (same) | "Parameter 'o.b.y' not available" |
| `$values <- list(o.b.y = .5)` | error (same) | assertion error, not settable |
| `$deps` | **error** | live |
| `as.data.table()` / `$lower` / `$tags` | stale, no error | stale, no error |

Two conclusions. First, v2's traversing reads *already detect* the
inconsistency (the "Corrupt …" errors are the detection working); only the
O(1) flatten reads are silently stale. Second, **1.0.1 is not a
correctness reference here** — it is half-live (values leak through under
names `$ids()` doesn't list; schema-consuming operations refuse them). The
"matches 1.0.1" phrasing in the option text was wrong about this corner.
The design target is therefore: *behave as if the ancestor had been freshly
constructed from its current children* — which agrees with 1.0.1 everywhere
1.0.1 agrees with itself, and is consistent where 1.0.1 is not.

### 1b. New finding: `$tags<-` on a contained child

`$tags<-` exists on base sets (`paradox_param_set_set_tags`) and replaces the
child's `.tags`. The parent's flatten caches child tags, so:

```r
chB = ps(z = p_dbl(0, 1)); parB = ParamSetCollection$new(list(s = chB))
chB$tags = list(z = "newtag")
parB$tags                    # stale: s.z has character(0)
parB$ids(tags = "newtag")    # character(0) — silently wrong
```

**No path ever errors** — unlike `$add`, the traversals don't validate tag
identity, so this is silent wrong answers on both v2 and 1.0.1. Any fix for
1a that keys on "child core changed" fixes this one for free; a fix
special-cased to `$add` would not.

Everything else a child can change post-construction is already read live by
the parent and is correct on both versions: `$add_dep`/`deps<-` (parent
`$deps` traverses), `$extra_trafo<-`, `$constraint<-` (traversals read the
callback slots live), `$values<-` (values are never cached in a collection).
Base sets have no `$add`. So the *complete* set of flatten-staling child
mutations is: **collection `$add`** (changes `.params/.tags/.trafos/
.translation/.sets`) and **`$tags<-`** (changes `.tags`).

### 1c. Policy gap: shadows freeze their visible schema

```r
sh = ParamSetShadow$new(innerE, character(0))   # innerE is a collection
innerE$add(ps(y = p_dbl(0, 1)), "b")
sh$ids()                 # "a.x" — frozen, no error
sh$check(list(b.y = .5)) # "Parameter 'b.y' not available"
```

The shadow's refresh machinery *notices* the origin change (signature
mismatch) and rebuilds — but rebuilds against its construction-time template:
`paramset_check.c` states the policy, "A shadow's schema is immutable but its
effective values, dependencies, and constraint are a package-owned live
view." Same for origin `$tags<-`: the shadow's tags stay frozen. This is
internally consistent (a frozen subset stays a valid subset — origins can
only grow), but it diverges from the miesmuschel reference, where the visible
set is computed live as "origin minus hidden". Decision point D1 below.

`clone(deep = TRUE)` isolates correctly today (the clone transaction rewrites
the whole node graph); nothing to fix there.

---

## 2. Architectural facts the design stands on

These make the "elegant" version possible; all verified in source.

1. **State generations are immutable; mutation = rebind.** A `.core` is a
   NULL-address `EXTPTRSXP` whose protected slot is the whole ten-field
   payload. Every mutator (`paradox_param_set_core_replace`, collection
   `$add`, values commits, shadow refresh) builds a **new** external pointer
   and rebinds `private$.core`. An old core is a frozen generation. Therefore
   **"capsule generation" already has a canonical runtime representation: core
   identity.** A version counter does not need to be invented — the pointer
   *is* the cookie. (`Rf_shallow_duplicate` in `core_replace` also means
   fields the update didn't touch keep their SEXP identity across
   generations — a values-only rebind preserves `.params/.tags/.trafos`
   objects. That distinguishes "values drift" from "schema change" by
   pointer comparison alone.)

2. **Graph edges are shells, resolved live.** `PARADOX_CORE_SETS` holds the
   child R6 environments; every traversal resolves shell → current
   `.core`. That is why parents *see* child changes at all (as errors,
   today), and why no parent back-pointer is needed to *find* fresh child
   state — only to be *notified*, which lazy validation makes unnecessary.

3. **The choke points already exist.** ~25 native semantic entry points
   (params, domains, check, get/store values, qunif, trafo, deps, subset,
   sampler, design, collection construct/add, deep-clone snapshot) already
   call `paradox_core_refresh_shadow(self, private)` before doing work, and
   it no-ops for non-SHADOW kinds. The R-side raw reads (`$lower`,
   `$length`, `$levels`, …) all funnel through `private$.state()` →
   `C_param_set_core_state`. Extending "refresh" to COLLECTION kind at the
   existing gate — plus one hook inside the two state accessors — covers
   every entry; there is no site-enumeration risk of the kind the back-pointer
   design would have.

4. **The precedent is in-tree.** Shadows already do lazy validate-and-rebuild
   keyed on pointer identity: their core carries a signature attribute of
   (shell, core) pairs over the whole origin graph; `shadow_refresh_
   authoritative` compares and rebuilds on mismatch, with a commit/preview
   split (`paradox_shadow_preview_authoritative`) for read-only paths that
   must not bind. The collection design below is the same idea with two
   corrections appropriate to collections: per-edge instead of transitive
   signatures, and schema-slice instead of whole-core comparison (collections
   must *not* invalidate on values drift, shadows must — they cache values).

5. **Rebuild code already exists.** `$add` builds a child's flatten slice via
   `build_collection_static_state(singleton, tag_sets, tag_params, postfix)`
   — the constructor's own builder — and appends with
   `append_collection_table`. A full re-flatten of one collection from its
   current children is a fold of exactly those calls. No new flatten logic.

6. **The only unrecoverable rebuild input is the per-edge tag flags.**
   `tag_sets`/`tag_params` are consumed at `$new`/`$add` and discarded. They
   must be per-**edge**, not per-param (an empty child added with
   `tag_params = TRUE` has no params row to hang them on until it grows —
   which is precisely the scenario being fixed). Everything else needed for
   rebuild is persisted: edge names (`names(.sets)`, `translation$owner_name`),
   `.postfix`, and the children themselves.

---

## 3. Recommended design

**Lazy bottom-up self-heal at the existing refresh gates, keyed on core
identity per edge, with a global schema-epoch fast path.** This is the
"invalidation cookie" instinct made concrete: the cookie is the child's core
pointer; the counter exists only to make "nothing changed anywhere" a
single-compare fast path.

### 3.1 New persisted state

Payload schema grows 10 → 11 fields: `.edges` (after `.postfix`).
For a COLLECTION core:

```
.edges = list(
  stamped    = <VECSXP, length n_children>,  # child core consumed at flatten
  tag_sets   = <LGLSXP, length n_children>,
  tag_params = <LGLSXP, length n_children>
)
```

`NULL` for BASE and SHADOW cores. This solves the tag-flag persistence
problem (per-edge, so the empty-child case works, and two edges sharing one
child with different flags work) and gives the validator its ground truth.
`stamped[i]` holding the old core keeps at most one superseded generation
per child alive between walks (the old payload shares everything but the
replaced field with the current one), released at the next walk.

Mechanical cost: `core_field_names`/`exact_names`/`FIELD_COUNT`, the six
`fields[PARADOX_CORE_FIELD_COUNT]` construction sites (copy-all-then-override
loops carry it automatically), `param_set_core_new()` on the R side, schema
validators, and the version-tag question (D3).

### 3.2 The heal walk

One new native routine, structurally a sibling of
`paradox_core_validate_graph_path` (same shell-identity visit table, same
DONE-memo so shared subtrees are walked once, same rooting discipline):

```
heal(node):                     # post-order
  for child in node.sets:
    heal(child)                                    # may rebind child .core
  if node is COLLECTION:
    for i, child in enumerate(node.sets):
      cur = child.private$.core                    # current generation
      if cur == node.edges.stamped[i]: continue    # nothing changed
      if schema_slice(cur) == schema_slice(stamped[i]):   # pointers:
          node.edges.stamped[i] = cur              #   .params,.tags,.trafos
          continue                                 # benign values drift
      mark node stale
    if stale:
      rebuild flatten from current children + .edges flags   # §3.4
      bind new core (commit mode) / use temporary (preview mode)
  if node is SHADOW: existing refresh (unchanged mechanism)
```

- **Benign drift** (a child's `values<-` rebound its core, schema fields
  SEXP-identical) refreshes the stamp in place and does not rebuild. This is
  what makes the design safe for the hot loop: tuning writes values every
  iteration; flattens must not churn.
- Bottom-up means every `.core` bind installs an *individually consistent*
  node, so an interrupt mid-heal leaves a partially-healed graph in which
  every healed node is correct and the rest heal at the next entry. No
  transaction needed.
- The walk runs no user code (the flatten builders are pure C), so there is
  no reentrancy window.
- Cycle admission is unchanged: `$add` keeps its downward reachability check
  (adding X under S creates a cycle iff S is reachable from X — no parent
  pointers needed), and the walk keeps the ACTIVE-colouring cycle error as
  defense in depth.

### 3.3 The fast path: schema epoch in the address slot

Without a fast path, every read walks O(distinct nodes): ~0.15–0.3 µs/node
(two env lookups + binding read + a few compares), i.e. ~3–6 µs on a 20-node
graph. Traversing reads (values/check/trafo: 20–200 µs) would not notice;
`$ids()`-class reads (5.5 µs) would. Hence:

- One package-global `uintptr_t` **schema epoch G**, bumped exactly where a
  core is installed over an existing binding with a *schema field* replaced
  by a *different object* — decidable mechanically inside the two canonical
  installers (`core_replace` sees update names; `$add`/shadow-commit know
  what they changed). Schema fields: `.params .tags .trafos .sets
  .translation .postfix .edges`. Values commits don't bump. No per-mutator
  discipline: a future mutator that installs cores through the canonical
  installers is covered by construction.
- Each core's **verified stamp S lives in the external pointer's address
  slot** (`R_SetExternalPtrAddr`, read back as `uintptr_t`). Entry gate:
  `S == G` → skip the walk entirely (~2 ns); else walk, then stamp every
  visited node `S := G`.

Why the address slot is the right home and not a payload field:

- **Serialization-correct by construction.** R serializes external pointers
  with a NULL address, so a loaded object's stamp is automatically "never
  verified" — no unserialize hook, no risk of a stale saved stamp colliding
  with a fresh counter. And since extptrs and environments *are*
  reference-tracked by R serialization, the `.edges$stamped` cores keep
  their identity relative to the children within one saved object: the
  first post-load walk is a cheap validate-and-restamp, not a rebuild.
  (One narrow exception: an object saved *between* a benign values drift and
  the walk that would have restamped it rebuilds once after load — the
  payload-field comparison can't survive serialization. Harmless: identical
  result, construction-grade cost, once.)
- **Zero-allocation restamping.** Reads may restamp without creating cores
  (a payload-field stamp would either allocate a new payload per
  revalidation or be shared across generations by `shallow_duplicate`).
- It does not conflict with the "no native resource" property: the slot
  stores an integer, never a dereferenced pointer, still no finalizer. The
  `addr == NULL` clause in `paradox_core_is_valid` is reinterpreted as
  "addr is the session-local verification stamp".

Overflow: irrelevant on 64-bit (2⁶⁴ schema mutations); on a 32-bit platform a
wrap could theoretically alias — document, don't engineer around.

Cost profile:

| situation | cost |
|---|---|
| steady-state read, any kind | + one global load + compare (~2 ns; < 0.1 % even on `$ids()`) |
| read after a schema mutation anywhere | one walk per entered root: ~0.15–0.3 µs × distinct nodes, then restamped |
| read of an actually-stale ancestor | + rebuild of that node's own flatten ≈ its construction cost (ms-scale for large sets), bottom-up, each node at most once per mutation |
| `values<-` / `$check` tuning loop | unchanged (no G bump, no walk) |
| readonly entries (check preview) on a stale graph | heal into a temporary, not bound — recomputed per call until a commit-mode entry binds (same accepted model as shadow preview today) |

### 3.4 Rebuild

Per stale collection: fold `build_collection_static_state` over the current
children as singletons with their stored per-edge flags (exactly `$add`'s
code path), append, run the existing duplicate-ID admission, assemble the new
payload (copy-all-then-override), stamp fresh `.edges`. Reuses the current
builders; a later vectorized-flags variant of the constructor builder is an
optimization, not a requirement.

**Deferred errors are inherent and must carry context.** If `inner$add`
introduced an ID that collides inside `outer`, the *rebuild* discovers it at
`outer`'s next read. The duplicate-ID error must say which edge changed and
which ID collides ("while refreshing 'outer' after contained set 'b'
changed: duplicate ID 'o.b.y'"), because the triggering statement is far from
the mutation. 1.0.1 never errors here (it silently keeps inconsistent
tables); erroring-with-context is strictly better. Only the eager variant
(§4.3) could report at `$add` time — the one real advantage it has.

### 3.5 What this fixes, in one mechanism

- 1a: ancestors of a grown collection heal (ids, values, check, deps,
  as.data.table, `$lower`, setting the new param through the ancestor).
- 1b: `$tags<-` on a contained child (a schema-slice change like any other).
- 1c: shadows over grown collections — automatically *if* D1 chooses live
  schema; the shadow's existing signature machinery sees the healed
  collection core and rebuilds against current origin params.
- The existing "Corrupt …" traversal checks stay as defense-in-depth; after
  healing they are unreachable through supported use, which is exactly what
  an internal-invariant error should be.

---

## 4. Alternatives considered

### 4.1 Always-walk (no epoch) — the fallback

Drop G and S; run the §3.2 walk at every entry. Simpler (no global, no
address-slot reinterpretation); identical healing semantics. Cost: O(distinct
nodes) per read — a few µs on deep graphs, worst felt on `$ids()`-class
reads (~doubling at 20 nodes, growing linearly). Acceptable as a first
landing if the epoch layer is deferred (D2): the epoch is a pure overlay on
this design, addable without reshaping anything.

### 4.2 Transitive signatures per collection (shadow-style)

Store the whole-subtree (shell, core) pair list on every collection, compare
flat. Rejected: O(subtree) storage per nesting level; conflates values drift
with schema change (shadows *need* that — they cache values; collections
must not, or tuning loops rebuild flattens every iteration); and it saves no
walking, since producing the "current" pair list is itself the walk.

### 4.3 Eager push invalidation (parent back-references)

Weak-ref registry of parents per node; `$add`/`tags<-` walks up and rebuilds
or dirties ancestors. Honest advantages: O(1) reads with no epoch, and
collision errors surface *at the mutation site* (better UX than §3.4's
deferred error). Rejected because the cost lands in the wrong places:
registry maintenance at every containment/clone/GC event, dead-entry
sweeping, and — decisive — the failure mode inverts: miss one *registration*
site and silent staleness returns, whereas in the lazy design a missed
*entry* site is impossible by construction (all entries already pass the
refresh gate) and the traversal validators would still catch inconsistency.
The lazy design's deferred-error corner is mitigated by error context; it
does not justify the registry.

### 4.4 Seal instead of heal

`$add`/`tags<-` error when the target is contained anywhere. ~50 lines, kills
the bug class, but forbids the mlr3pipelines-style workflow the live-rebuild
decision explicitly chose to support. Off the table per that decision; noted
only because sealing *nothing else* means every future schema mutator must
go through the canonical installers to stay covered — which is already the
architecture's rule.

---

## 5. Decisions needed (policy, not engineering)

- **D1 — shadow visible schema: live or frozen?** Today: frozen at
  construction (§1c), internally consistent but a frozen island once
  collections heal, and it diverges from miesmuschel's "origin minus hidden,
  computed live". **Recommendation: live** — uniform with the heal, and the
  refresh machinery already rebuilds from current origin params; the change
  is to stop pinning the template's param table (and to reuse prior field
  objects when the origin's schema slice is unchanged, so shadow refreshes
  keep field identity and don't masquerade as schema changes to collections
  above them — that identity-preservation is required for §3.3's G-bump rule
  to stay quiet under value churn regardless).
- **D2 — land with or without the epoch layer?** Recommendation: design for
  it, land it in the same batch (it is small: one counter, one slot, one
  compare per entry), benchmark both gates like the last batch did. If it
  must be cut for risk, 4.1 is the same design minus the overlay.
- **D3 — version tag.** Schema goes to 11 fields. The `paradox.core.*.v1`
  tag strings were never in a release; either bump to `.v2` (honest, churns
  fixtures) or keep `.v1` (nothing serialized exists in the wild).
  Recommendation: keep `.v1` until the first public release freezes the
  format; the tag exists for *post-release* evolution.
- **D4 — optional follow-up, out of scope here:** the same stamp slot could
  gate shadow refresh with a second all-mutations epoch, removing today's
  per-entry origin-graph walk (part of the shadow 4.6× overhead). It weakens
  the per-entry corrupt-shell revalidation shadows currently repeat, so it
  should be its own considered change, not a rider.

## 6. Scope map

| unit | change |
|---|---|
| `core_state.{c,h}` | 11th field; validators; epoch global + stamp helpers; `paradox_core_refresh(self, private)` generalizing the shadow-only gate |
| new `paramset_heal.c` (or into collection_construct) | the walk (§3.2), ~sibling of `validate_graph_path` |
| `paramset_collection_construct.c` | `.edges` at `$new`/`$add`; rebuild = fold of existing builders; contextual duplicate-ID message |
| `paramset_shadow.c` | D1 (unpin template schema; identity-preserving refresh); comment correction in `paramset_check.c` ("schema is immutable" no longer holds) |
| ~25 native entries + `C_param_set_core_state` | call the generalized gate (mostly a rename) |
| `R/ParamSet.R` | `param_set_core_new` gains `.edges` |
| tests | the ledgered battery: collection-in-collection, collection-in-shadow, growing shadow origin, deep nesting, aliased/shared child + `$add`, values on new params through the ancestor, every read/write surface after the change, `$tags<-` staleness, collision-at-heal error text, interrupt-mid-heal, serialization round-trip, clone interplay, gctorture |
| benchmarks | same A/B harness as the 2026-07-28 batch; gates: `values<-`/check unchanged within noise, `$ids()` unchanged with epoch |

Estimated effort: the mechanism is mostly recombination of existing pieces
(walk skeleton, builders, refresh gates); the schema change is mechanical but
wide; the test battery is the bulk of the work, deliberately.
