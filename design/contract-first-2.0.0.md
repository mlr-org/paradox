# Paradox 2 contract-first architecture

## Status and precedence

This document is the normative architecture and compatibility contract for the
first public Paradox 2 release. It supersedes the compatibility-first native
candidate formerly described by the repository. `design/architecture.md` now
maps this contract to implementation, `design/compatibility.md` records the
intentional public boundary, and `design/release-2.0.0.md` is the active release
ledger. Git history retains the replaced compatibility-first narratives; they
do not constrain this implementation.

No previously frozen 2.0.0 candidate is releaseable under this contract. A new
candidate must implement this design and receive source-bound verification from
the beginning. Toolchain, dependency, corpus, and harness caches may be reused
when their authenticated keys remain valid, but results about older package
bytes may not be relabeled as evidence for the replacement.

The contract reset is intentionally bounded. It spends the unreleased major
version break on removing unused extension and mutation promises that otherwise
force every ordinary operation to authenticate a second, mutable R execution
surface. It is not permission for further discretionary performance work once
the design below is implemented. The one explicitly bounded final performance
batch was specified before implementation in
`design/final-performance-implementation-plan.md`; it preserves every contract
here and is now closed at commits `81cbccf` and `387c1cd`.

It also spends that break now on one strict structural-object boundary. Exotic
ALTREP/S4 shells and duplicate R/native admission have no known maintained
consumer and impose dispatch, multi-observation, and dual-authority costs. They
are rejected in this release rather than temporarily retained and broken again
later. Ordinary documented containers and stable ALTREP semantic vectors remain
supported at the exact positions defined below.

## Public shell and opaque payload

`ParamSet`, `ParamSetCollection`, and `ParamSetShadow` remain serializable R6
objects with their established public constructors, class vectors, methods,
active bindings, reference behavior, and shallow/deep cloning semantics where
those operations make sense for the node kind. R6 is the public shell, not the
state model or execution engine.

Every new shell owns one package-created, versioned payload capsule in a private
binding. The capsule has these invariants:

- It is composed solely of ordinary R objects and is therefore visible to R's
  garbage collector, cloneable, and serializable. An external pointer is never
  the sole source of capsule/model truth.
- Its physical list/environment layout, field names, attributes, indices, and
  derived caches are opaque package internals. Downstream code may not read or
  write them as an extension mechanism.
- Its header identifies a package schema version and exactly one node kind:
  `BASE`, `COLLECTION`, or `SHADOW`. Unknown versions and kinds fail with a
  deterministic upgrade/state error before semantic work begins.
- Permanent semantic vectors stored in a current capsule are canonical
  ordinary, non-ALTREP vectors with exact types, lengths, names, and ownership.
  The sole representation exception is base R's canonical compact
  `data.frame` row-names metadata; it is never treated as a semantic column,
  indexed through an ALTREP accessor, or exposed as package state. Mutable
  state is changed only by package operations, which update any generation or
  derived-cache metadata transactionally.
- A schema marker is an admission hint, not a memory-safety boundary. Native
  code still validates every type, length, index, arithmetic bound, and graph
  relationship needed before access. Forged or corrupt state must error, never
  over-read, write out of bounds, or segfault.

The v1 capsule retains no general derived index or topology plan. Those plans
are operation-local scratch; the sole persistent derived cache is the exact
SHADOW generation signature specified below. Direct private mutation is
unsupported, so the engine does not repeatedly parse R6 closure bodies, method
tables, active-binding registries, or private-table aliases merely to detect it.

Legacy R6 bindings/tables named `.params`, `.values`, `.tags`, `.deps`,
`.trafos`, `.sets`, and similar are not a public or downstream ABI. The current
opaque payload deliberately reuses several of those field names as one internal
schema; that does not preserve an equivalent readable or writable legacy
representation. Public accessors continue to return compatible values and
tables.

Consumers needing a detached search space without transformations call
`$subset(..., keep_trafo = FALSE)`. The additive final argument defaults to
`TRUE`; `FALSE` removes both selected per-parameter transformations and the
extra transformation callback in the one native subset transaction, without
changing the independently controlled constraint. It applies uniformly to
BASE, COLLECTION, and SHADOW. Direct mutation of a reconstructed Domain's
`.trafo` field, or acceptance of the malformed Domain produced by such a
mutation, is not a compatibility requirement. All three subset control flags
are exact attribute-free, non-missing logical scalars. COLLECTION callback
detachment follows the callbacks retained by the admitted BASE result and does
not reinterpret the original controls through an R generic.

The one stateful shell-policy exception is the documented public
`assert_values` field. It determines whether `$values<-` enters the checked or
unchecked native value-store operation; it is not parameter, value, callback,
or graph authority. R6 serialization and cloning retain it, and semantic
equality compares it because it changes subsequent public assignment behavior.
It remains outside the fixed capsule payload and does not create a second value
implementation: both selected stores are native. Here and below, “complete
state” means complete capsule/model state subject to this sole shell policy.

### Chosen capsule representation

For 2.0.0 this design choice is fixed, even though it remains an internal ABI.
The private `.core` binding contains an external-pointer shell. Its
tag is one of `paradox.core.base.v1`, `paradox.core.collection.v1`, or
`paradox.core.shadow.v1`; its protected slot is the complete serializable
ordinary-R payload; its address slot carries only the session-local
verification stamp described under *Derived schema* below. There is no
unmanaged allocation, address dereference, or finalizer. The shell supplies an opaque, difficult-to-forge type boundary while
the protected ordinary-R graph remains the sole source of capsule/model truth
and round trips through R serialization.

The v1 payload is a fixed named eleven-field list: `.params`, `.values`,
`.tags`, `.deps`, `.trafos`, `.extra_trafo`, `.constraint`, `.sets`,
`.translation`, `.postfix`, and `.edges`. These names are package-internal
schema, not downstream accessors.
`BASE`, `COLLECTION`, and `SHADOW` use the same physical schema and interpret
only the fields appropriate to their kind. This intentionally avoids three
partially duplicated state implementations.

`.edges` is the derivation record of a node whose schema is derived rather
than owned. It is `NULL` for `BASE`. For `COLLECTION` it holds, per edge in
`.sets` order, the exact child capsule generation that edge was flattened from
and that edge's `tag_sets`/`tag_params` flags -- flags no flattened table can
recover, because an edge whose child is still empty contributes no row. For
`SHADOW` it holds the origin schema slice the visible tables were projected
from plus the retained hidden ID set, which is what makes the visible schema
"origin minus hidden" computed live rather than frozen at construction. The
record is a cache: an absent or mismatched one makes the node stale, and the
next read re-derives it.

Both derived kinds additionally record a `tag_override`. Tags are the one
derived field a derived node may own outright: a `$tags<-` assignment on a
`COLLECTION` or `SHADOW` is stored as that node's own answer for exactly the
IDs it named, survives every later re-derivation, and leaves the sets the
schema is derived from untouched -- so two views over one set may tag the same
parameter differently. An ID no assignment named stays derived.

#### Derived schema

A `COLLECTION`'s flattened schema and a `SHADOW`'s projection are derived from
other nodes and are kept current lazily. Every semantic entry point passes one
gate that walks the graph below it in post-order, re-flattens a `COLLECTION`
whose child's `.params`/`.tags`/`.trafos` slice has moved, and refreshes a
`SHADOW`. The result is what the node would be if it had just been constructed
from its current children. Because each installed generation is individually
consistent, an interrupt or a deferred name collision leaves a partially healed
graph in which every healed node is correct and the rest heals at the next
entry; no transaction is needed. Parent back-references are deliberately not
used: a missed *entry* is impossible (every entry passes the gate), whereas a
missed *registration* would silently reintroduce the staleness.

Two session-global epochs make "nothing changed anywhere" a single comparison.
A capsule installation that changes a derived-schema input advances the schema
epoch; any other semantic installation advances the state epoch; a cache
refresh advances neither. Each capsule records the epoch at which its own
subtree was proven to agree -- mixed with the capsule's own address, so a
capsule duplicated by an ordinary R attribute assignment is unverified -- in
the external pointer's address slot, the one place R guarantees to reset on
unserialize. The stamp proves that nothing was installed; it never records
that a caller or a node may be trusted, and a `SHADOW` still reauthenticates
its refresh signature on every entry.

The protected eleven-field payload is the complete capsule/model state, subject
only to the public `assert_values` shell policy described above. A SHADOW core
also carries exactly one package-private attribute,
`.paradox.shadow.snapshot.v1`, containing an ordinary attribute-free list of
alternating origin-graph shells and the exact capsule generations from which
the current payload was derived. This is derived, package-rebuildable cache
data, not parallel authority: `.sets[[1L]]` is the sole origin edge, and the
signature contains neither a second origin edge nor callbacks/factories. Every
current SHADOW core must nevertheless carry the exact signature; missing or
extra metadata is corrupt rather than an invitation to guess or replay.
Native refresh validates the complete attribute and every referenced capsule
before using it. An unchanged signature returns the current capsule; a changed
signature rebuilds from the selected graph and fixed factories resolved from
the locked Paradox namespace; a malformed signature errors without replay.
Deep clone rebuilds the signature against its memoized cloned graph.
Serialization may preserve the signature because its identities are serialized
as part of the same ordinary graph, but it remains validated and may be rebuilt
only by a package-controlled construction, refresh, clone, or upgrade path.

Canonical table fields are plain base `data.frame` column stores with no key,
secondary index, spare-column capacity, or data.table self-reference. Native
code reads their columns directly after complete shape validation. Public table
accessors construct data.table facades whose mutable shells and columns are
newly owned or detached at the boundary. A native producer may complete a
genuinely fresh package-owned facade directly; caller-owned/public-ingress
tables retain the defensive finalizer. Consequently there is no load-time
data.table-layout probe, version-specific index synthesis, or permanent table
facade to maintain. Required table attributes and their values are contractual
by name, but their pairlist order is not legacy API; native grid facades use
one fixed `row.names`/`class`/`names` construction order rather than
reproducing R-version-specific `data.table::CJ()` order.

Every supported mutation constructs a replacement capsule and swaps the
private `.core` binding only after validation. Reads retain the capsule selected
at operation entry. This makes the object generation itself the snapshot token,
eliminates in-place private-table aliasing, and gives reentrant operations an
unambiguous old-or-new state without a second R implementation.

## Node model

The three node kinds share one native operation interface and one graph model.
Shared children are valid; a node encountered twice on one active path is a
cycle and produces a deterministic error. Native semantic/admission traversals
are iterative and use checked `R_xlen_t` arithmetic rather than the C stack.

### `BASE`

A `BASE` node owns one parameter schema in public parameter order. Its payload
contains the built-in type code and canonical structural metadata required by
the native engine, plus mutable values, dependency edges, transformations,
constraint, tags, and other supported public state. Public setters are the only
mutation boundary and update the payload atomically.

### `COLLECTION`

A `COLLECTION` node owns ordered references to child nodes. Each capsule
generation owns an immutable prefix/postfix translation snapshot; construction
and `$add()` create a validated replacement capsule instead of mutating that
snapshot in place. `$add()` is one native transaction: it snapshots and
validates the current and proposed-child graphs (following SHADOW origin edges),
rejects pre-existing corruption and any edge that would create a cycle, builds
the complete replacement, rechecks every graph generation used by the plan,
and commits only the collection capsule. A failure or reentrant graph mutation
therefore leaves the old generation unchanged. It does not copy
child values, dependencies, transformations, or constraints into a second
canonical representation. Those dynamic properties are read from the child
snapshots selected for the current operation. Shared-child DAGs retain one
child identity and receive deterministic public ordering; cycles are rejected.

Public collection results preserve the established translated IDs, nested
order, explicit named `NULL`, named empty results, and shallow sharing of
opaque leaves while returning independently owned mutable shells.

Collection callback behavior has one native implementation. Live
`$extra_trafo`/`$constraint` access and detached subset/flatten callbacks use
package-owned thin closures that retain only an exact validated owner/mapping
plan and enter one registered native evaluator family; SHADOW views of
COLLECTION origins use the same engine. Child callbacks are selected from the
admitted capsule snapshot rather than an overridden child R6 method.
Translation, collision handling, child-result cardinality/name admission,
merging, and scalar logical constraint admission are never duplicated in R.
Authoritative collection check/test/assignment sites evaluate activity in the
full translated namespace before each child constraint receives its active
unprefixed child slice. Detached BASE checking performs the same filtering
before invoking a detached carrier. The fixed schema-free callback carrier ABI
stores no activity mask and contains no duplicate activity evaluator.
Collection extra-transformation results first contain retained/untransformed
inputs in input order, followed by changed child outputs in callback-plan order
and each callback's result order. Child-owned inputs omitted by their callback
disappear. A changed name that collides with a retained input is an error.

### `SHADOW`

`ParamSetShadow` becomes an exported, package-owned Paradox node rather than a
third-party subclass that reconstructs ParamSet private tables. Its public
constructor remains `ParamSetShadow$new(set, shadowed)`, and its class and
constructor source API remain available to miesmuschel.

A `SHADOW` node owns exactly one origin edge and the fixed construction-time
visible parameter schema in capsule `.params`. `shadowed` is constructor input,
not retained authority. Current origin IDs outside that fixed visible schema
form the hidden complement; no parallel visible-ID or hidden-ID field is
stored. In particular:

- `$origin` is read-only and returns the exact origin object.
- Visible `$values` reads are derived from the current origin values. Assigning
  visible values writes through to the origin while preserving every hidden
  origin value.
- Dependencies, constraint, individual transformations, and `extra_trafo` are
  live origin semantics. An operation snapshots and filters their then-current
  values; it does not retain copied private tables in the shadow.
- A BASE-origin constraint closure retains exactly the admitted callback and
  hidden-value snapshot in a two-field plan. Its native evaluator manually
  forms the already activity-filtered hidden-first/visible-second callback
  input, without `c()` or S3 dispatch, preserves opaque leaf identity, calls
  once, and requires one non-missing logical result. The authoritative Shadow
  graph site filters before constructing/invoking this exact schema-free plan;
  the adapter does not own another activity engine. COLLECTION-origin adapters
  use the shared native collection evaluator family and the same upstream
  filtering rule where applicable.
- A dependency crossing the visible/hidden boundary is rejected. This is
  checked at construction and again when a live dependency snapshot is used,
  so later origin mutation cannot create ambiguous shadow semantics.
- The visible parameter schema—including ID order, type, storage, bounds,
  levels, tolerance, grouping, tags, defaults, and other structural metadata—is
  fixed at shadow construction. Later origin schema changes do not silently
  alter the view; callers construct a new shadow when they want a new
  structural view. A current origin that no longer supplies a compatible
  visible ID is corrupt/unsupported and produces a deterministic error; newly
  added collection IDs simply remain outside the fixed visible schema.
- Clone and serialization preserve the documented origin/view relationship.
  Deep cloning duplicates the graph according to ordinary Paradox deep-clone
  semantics while preserving shared-node identity within the cloned graph.
- A SHADOW may directly wrap a BASE or COLLECTION. Direct SHADOW-to-SHADOW
  origins are rejected; callers form the combined visible partition over the
  ultimate origin instead of adding a redundant live-view layer.

This node supplies the speed-sensitive behavior required by miesmuschel without
making third-party access to Paradox private storage a supported API.

## Additive-only ParamSet-family R6 inheritance

Third-party inheritance from `ParamSet`, `ParamSetCollection`, and
`ParamSetShadow` is supported only for additive subclasses. Such a subclass may
call `super$initialize()`, add public/private fields, and add methods or active
bindings whose names do not replace a Paradox core ParamSet binding. This keeps
bbotk's `Codomain` source model viable. It does not restrict the documented
`Sampler` extension API, whose subclasses implement their own sampling
behavior.

The following are not extension contracts:

- overriding a Paradox constructor or core method/active binding;
- replacing, unlocking, reparenting, delaying, or making active a generated
  Paradox binding;
- replacing the payload capsule or one of its descendants;
- depending on the generated wrapper body, formals, owning enclosure, private
  registry, insertion order, lock state, or `env.profile()` representation.

The native engine need not detect those modifications in order to reproduce
their effects. A direct call with corrupt or forged inputs remains memory-safe,
but semantic compatibility for a modified shell is not promised. Behavior that
needs a new core node belongs in Paradox itself, as `SHADOW` does.

## Closed Domain and Condition systems

Domain and Condition dispatch are closed over package-owned kinds. The exported
function names remain stable, but they are ordinary closed dispatch functions,
not third-party S3 registration points.

The supported Domain kinds are `ParamDbl`, `ParamInt`, `ParamFct`, `ParamLgl`,
and `ParamUty`. A future package-owned type is a deliberate maintainer change to
one central kind table and every operation required by that kind; there is no
third-party native plug-in ABI and no slow S3 semantic engine.

Standalone built-in Domain objects retain their documented outward class and
table shape. Constructing a ParamSet validates and snapshots their semantic
contents into the `BASE` capsule. Later mutation of the source Domain or a
detached Domain returned by an accessor does not mutate that ParamSet.

Numeric short-form constructors perform bounds admission, materialization of
admitted atomic ALTREP semantic inputs, logscale normalization, mapping-callback
selection, and row construction in one registered native operation. Their
structural shells and metadata are never materialized as ALTREP. Exact zero-row Domain and
zero-dimensional grid behavior also belongs to the native engines; it is not
an R-side special-case implementation.
A `ParamFct` with `character()` levels is canonical. Operations that request
zero rows preserve a typed `character(0)` factor column; nonempty quantile or
uniform-sampling requests error before indexing a level or entering the RNG.

`generate_design_grid()` is one complete native graph operation, not a nominal
Cartesian generator followed by an R/data.table normalization engine. It
deduplicates realized built-in axes, makes stored values fixed axes, traverses
only dependency-valid branches, and preserves the established first nominal
occurrence of every final row. Its optional ceiling is defined over that final
realized row count. Dependency graph planning and built-in comparison are
shared with Design dependency masking. A nominal zero axis still produces a
typed empty grid even if fixed, after dependency topology has been validated.
Package-created final grid data alone enters the R6 Design shell with a
namespace-owned prepared token; that token is not a public bypass for arbitrary
Design input. Valid non-storage/`NULL`/S4 fixed specials retain exact identity
as single list-column leaves, while a fixed TuneToken errors because it is not a
concrete design value.

Canonical semantic admission of one built-in Domain row has exactly one native
owner. The constructor's final-state assertion, ParamSet construction, and an
ObjectTuneToken carrying a Domain all call that owner for kind/storage,
grouping, cargo, bounds/tolerance, levels, special values, default, tags,
transformation, requirements, and initialization invariants. An operation may
separately validate the outward table/class container at its boundary; after
extracting the row it receives an admitted built-in kind and validated fields
and must not reproduce those semantic rules. This division keeps outward
Domain-facade corruption diagnostics exact without creating a second Domain
engine.

All outward Domain shells, table/class/name structures, row containers, cargo
containers, interpreted cargo entries, and other interpreted metadata are
ordinary non-ALTREP and non-S4 objects. For `ParamDbl`, `ParamInt`, `ParamFct`,
and `ParamLgl`, the outer `special_vals` list, its names, and list metadata are
likewise structural ordinary non-ALTREP/non-S4 objects. Each typed special leaf
is rejected if it is ALTREP, before an
element is observed. An admitted S4 special leaf for one of these typed kinds
is an opaque pointer-identity token: only that exact object can match it. An S4
`default` or `init` is therefore accepted only when pointer-identical to an
already admitted special leaf; otherwise it is invalid. This preserves the
explicit special-value identity exception without asking C to interpret or
dispatch on an S4 object. `ParamUty` remains different by design: its value,
default, initial, and special leaves are opaque, may be S4, and retain identity.
Its Paradox-1 special membership is preserved as base `identical()` against
each admitted special leaf, including S4 leaves. That comparison is the sole
narrow observation of opaque ParamUty specials and invokes no S3/S4 method.
Its Domain shell and structural metadata remain ordinary non-ALTREP/non-S4.

`p_uty(custom_check=)` remains the supported validation escape hatch for opaque
values. Its callback is part of the ordinary callback contract, not a new
Domain type. Existing `special_vals`, transformations, defaults, initial
values, aggregation, and internal-tuning callbacks remain supported where
their public constructors document them. Package-interpreted callbacks retain
identity from the point of admission: admission recursively discards
`srcref`, `srcfile`, and `wholeSrcref` representation metadata from a stored
copy while retaining the exact enclosing environment. A callback with no such
metadata is stored at pointer identity. The admission-time debugging opt-out is
`options(paradox.strip_srcrefs = FALSE)`. This does not weaken the opaque-leaf
contract: function-valued `$values`, defaults, special values, initialization
leaves, and other user payload remain untouched by source-reference
normalization. The separately specified legacy graph migration may still
identity-migrate a legacy ParamSet shell reached inside such a payload.

The supported dependency conditions are exactly `CondEqual` and `CondAnyOf`.
Their exported constructor names, `Constructor$new` adapter, class vectors,
two-element list shape, readable/mutable `rhs`, formatting, printing, vector
comparison, and ordinary serialization remain compatible. Adding a dependency
validates and snapshots the condition kind and operand into the capsule;
subsequent mutation of the source condition or a detached `$deps` result does
not mutate the ParamSet. `condition_test()` and `condition_as_string()` remain
exported names but do not call `UseMethod()`. Standalone `condition_test()`
uses the same exact built-in admission and comparison semantics as dependency
execution, with an optimized native vector kernel for direct input. A built-in
RHS must be an attribute-free logical, integer, double, or character vector
without missing values; `CondEqual` requires one element and `CondAnyOf` a
non-empty unique vector. Admission roots the selected RHS and copies each
element once before the strict ordinary-vector capsule validator runs. The
direct `x` accepts `NULL` or a plain vector of the same four kinds with no
semantic attribute other than names. Stable ALTREP operands are materialized
once at native admission. A classed vector, array, or other attributed operand
is rejected instead of invoking an `Ops`, `%in%`, or other S3 method.
Condition shells and their structural class/name metadata must be ordinary
non-ALTREP/non-S4. The admitted atomic RHS and direct operand may be stable
ALTREP as above but also reject the S4 bit explicitly; S4 is never an opaque
Condition operand.
Exact admission may return the already validated RHS in operation-local
workspace while its dependency/Condition graph remains rooted. Reusing that
borrowed pointer is part of the single validator, not a cache or a second
admission policy.
`condition_as_string()` remains one cold R presentation operation, not another
condition evaluator. `ParamSet$add_dep()` and dependency assignment reject an
unknown or malformed condition immediately instead of storing an extension for
later dispatch.

TuneToken admission is likewise closed, but its representation remains a
package-defined internal format rather than an extension API. A supported token
is one ordinary non-ALTREP list of exactly two elements named `content` and `call`, with
only its exact names and class attributes. `call` is one attribute-free,
non-missing character value. The accepted class vectors are exactly:

- `c("FullTuneToken", "TuneToken")`;
- `c("RangeTuneToken", "TuneToken")`;
- `c("ObjectTuneToken", "TuneToken")`;
- `c("InternalTuneToken", "FullTuneToken", "TuneToken")`;
- `c("InternalTuneToken", "RangeTuneToken", "TuneToken")`.

Full content is the exact named list `{logscale}`. Range content is
`{lower, upper, logscale}`, where each bound is one non-missing integer/double
or `NULL`. An Internal Full or Range token has false `logscale` and may append
one `aggr` function. Object content is one exactly admitted bounded,
value-producing built-in Domain or an exact `c("ParamSet", "R6")` shell whose
ordinary enclosure/private bindings identify itself and a canonical BASE core.
A ParamUty Domain is unbounded and is not Object-token Domain content. A
zero-level `ParamFct` remains canonical for typed empty operations but is not a
value-producing tuning range. Another bounded typed Domain may still retain
admitted opaque leaves, and the BASE-ParamSet form may produce an
opaque target value. A COLLECTION, SHADOW, or additive ParamSet subclass is not
Object-token content. A name attached to a scalar bound
or flag by ordinary R indexing is representation-only and is discarded from
the native snapshot. Every interpreted token shell, container, class/name
vector, scalar, Domain structure, and ParamSet shell is explicitly non-ALTREP
and non-S4. A
subclass, extra/reordered field or class, extra
attribute, malformed call/content, or recursively attached metadata is rejected
at the fixed root before arbitrary traversal. Opaque documented leaves retain
identity. Users construct tokens with `to_tune()`; these exact admission rules
do not make the representation a manual construction contract.

Exact creator provenance is not part of native admission. Proving that an R6
environment was literally returned by one generator would reintroduce mutable
method/body/enclosure authentication. A shell alias that preserves the exact
genuine BASE `self`/`private`/core linkage may therefore be structurally
indistinguishable and pass. This is safe because native checking and conversion
never call an alias method or use its public surface as authority; they retain
only the selected canonical capsule generation. The representation remains
unsupported for manual construction, but documentation and diagnostics must not
promise to detect every indistinguishable alias by creator provenance.

An `ObjectTuneToken` whose content is a ParamSet has a two-stage contract with
one owner for each question. `$check()` and checked value assignment validate
and root the exact token plus current BASE capsule natively, require its schema
to be nonempty and bounded, and execute no candidate callback. Each admitted
live candidate yields a private rooted `{shell, private, core}` generation
receipt. The check engine reauthenticates every receipt after callback-capable
validation. Checked assignment retains those receipts through replacement
construction and performs a final allocation-free, non-forcing identity scan
immediately before its allocation-free commit. If any candidate binding or core
changed, the nested mutation wins and the outer operation errors without
committing.

Explicit `$search_space(values=)` input enters the same exact native structural
boundary. Before R conversion or a candidate callback begins, C replaces each
live BASE candidate in the admitted token snapshot with a sealed, single-use
BASE subset capability. The package constructor consumes that capability to
create the detached search-space ParamSet; R never calls or rereads the original
candidate shell. The `values` container itself is either an ordinary non-ALTREP
named list or an ordinary non-ALTREP S3-classed named list with only names/class attributes. Its outer class is
discarded before native token selection, no `[` method dispatches, and S4 or
other semantic container attributes reject.

The deterministic `$search_space()` conversion is the sole operation that
evaluates the candidate's transformation and establishes its one-dimensional
output and target-Domain compatibility. A structurally valid but
output-incompatible candidate therefore may be stored and fails when the search
space is requested.
Paradox 1 performed that callback-dependent plausibility work during checking;
retaining that timing would require an unsnapshotted R preflight before the
native atomic commit. Corrupt candidate state still errors during native
admission and leaves stored values unchanged.

Malformed exact-token or Domain structure is a hard public-boundary error, even
when discovered during `$check()`. It is not demoted to the character diagnostic
used for an ordinary value that is structurally admissible but infeasible for
its target Domain. This distinction keeps unsupported/corrupt structure out of
the value-result protocol and supplies no fallback path.

## Preserved outward compatibility

The contract reset changes extension and mutation mechanisms, not the ordinary
Paradox user model. Subject to the legacy migration boundary, Paradox 2
preserves:

- exported constructor/function names and normal argument meanings, including
  `ParamSet$new()`, `ps()`, `psc()`, `c.ParamSet()`, `ps_union()`, and
  `ps_replicate()`;
- package-owned R6 class vectors, public method and active-binding names,
  reference behavior, and supported shallow/deep clones;
- public parameter order, names, types, attributes, factor levels, bounds,
  tolerances, tags, defaults, initial values, and printable representations for
  ordinary inputs and stable ALTREP inputs in documented semantic atomic
  positions;
- `$values$x <- value`, named-list assignment, unset-via-`NULL`, explicit named
  `NULL`, required tags, TuneTokens, presence modes, sanitization, constraints,
  dependencies, transformations, dormant storage with filtered reads, and
  internal-tuning behavior;
- ordinary non-ALTREP S3-classed named configuration-list containers, with the
  outer class ignored and removed at native admission rather than used for
  dispatch, including explicit `$search_space(values=)` input;
- representation-only names on scalar Domain constructor inputs, which are
  removed from the owned canonical row while classes/other attributes remain
  unsupported;
- semantic `all.equal()` comparison of BASE, COLLECTION, and SHADOW objects
  through a detached graph projection rather than private R6 environments. It
  compares node class/`assert_values`, params, values, tags, dependencies,
  BASE callbacks, COLLECTION children, complete SHADOW origin state, and
  canonical shared-node topology;
- public data-table-shaped schemas and list columns, subject to the detached
  facade and data.table >= 1.18.4 boundary below;
- collection prefix/postfix spelling, nested order, shared-child DAG behavior,
  detached `flatten()`/union results, named empty results, and opaque leaf
  references;
- the intentional bug fixes recorded in the historical compatibility document.

Ordinary built-in validation failures are classified once in C and retain
informative checkmate-style categories and established consumer-relied-on
fragments for missingness, type/shape, integerish values, bounds, and factor
membership. The diagnostic formatter runs only after native admission fails;
successful checks neither construct messages nor enter R or checkmate.
An unknown parameter ID is likewise formatted only after its exact native hash
lookup fails. The failure-only C matcher uses the actual unknown name,
case-insensitive partial edit distance, the established 20% query-length
threshold, stable parameter order for ties, and at most three candidates.
Partial matching gives collection-added prefixes/postfixes low cost. This fixes
Paradox 1's accidental use of the unknown entry's integer position as the
distance query without adding work to successful assignment or checking.
Byte-identical reproduction of every checkmate quirk, internal error priority
that depends on a side-effecting promise, `conditionCall()`, implementation
frames, and unsupported exotic-object behavior is not an outward requirement.
Every intentional differential from Paradox 1 receives a NEWS entry and a
focused regression test.

## Dependency activity, dormant values, and filtered constraints

Dependency activity has one native list-basis semantic kernel. The check
family, `check_dependencies()`, `$get_values()` dependency filtering, and every
authoritative constraint check/assignment site all call that kernel; none
maintains a second point or stored-value evaluator in R or C.

For one named evaluation basis `x`, a parameter is active only when every one
of its dependency rows is satisfied. Evaluation is recursive and conjunctive.
After cycle validation, a child supplied as a TuneToken skips its incoming
rows. Otherwise an inactive parent makes the row unsatisfied regardless of any
value or default it carries. For an active parent, an explicit value in `x` is
the operand; a TuneToken parent skips the row. If an active parent is absent
from `x`, its recorded default is the operand unless that row contains
`NoDefault`; an absent `NoDefault` parent leaves the row unsatisfied. The
built-in closed Condition comparator remains the sole operand evaluator.
Explicit values therefore override defaults, and a default on an inactive
parent cannot reactivate its descendants. Multiple rows on one child remain
conjunctive. A dangling parent for which the admitted graph has no
Domain/default is absent and unsatisfied. Existing BASE dependency
mutation can construct a cycle and is unchanged by this feature. Every activity
consumer detects an active-path cycle and raises a deterministic error; it
never loops, overflows the C stack, or returns a partial mask.

The basis is operation-specific and never guessed:

- `$check()`, `$assert()`, `$test()`, each `$check_dt()` row, and
  `$check_dependencies()` use only the candidate point plus recorded defaults.
  They never consult stored `$values`.
- `$get_values(remove_dependencies = TRUE)` uses the raw stored values plus
  recorded defaults. Its `check_required` decision is made after filtering.
- A raw getter call that requests no dependency filtering and has no required
  parameters does not invoke the activity kernel. It still admits the complete
  parameter/dependency/Condition snapshot and therefore retains every
  corruption boundary.
- `presence = "required"` and `"all"` use the candidate point basis. A
  satisfying default can therefore make an absent child active and required
  where a default-blind check formerly exempted it.
- A constraint uses the complete configuration presented at that invocation,
  plus recorded defaults only for activity. Defaults influence which supplied
  entries are active; they are not synthesized into the callback's list.

Checked `$values <-` and `set_values()` assignment no longer reject a
Domain-valid entry merely because its dependency is unsatisfied. Such an entry
is **dormant**: it remains in the raw `$values` store, is omitted by the default
`$get_values(remove_dependencies = TRUE)` view, and reappears automatically
when a later assignment makes it active. `remove_dependencies = FALSE` exposes
the complete raw store. Dormant values still undergo the same type, bounds,
special-value, ParamUty `custom_check`, sanitization, TuneToken, unknown-ID, and
structural admission as active values. Assignment without a constraint does not
compute activity. The checked and unchecked stores retain their existing
graph-wide planning, generation receipts, deterministic last-owner semantics,
and allocation- and callback-free atomic commit.

This deliberately separates a legal store from a valid explicit point.
Check-family defaults remain `check_strict = TRUE`, and every supplied point
entry must be active. Consequently `$check(ps$values)` is not an invariant:
raw `$values` may legally include dormant entries, while
`$check(ps$get_values())` or an explicitly non-strict point check can succeed.
Dependency diagnostics for the still-failing point cases retain the established
`"can only be set if"` fragment.

Every constraint callback receives only the active entries of its candidate
configuration. BASE scalar/table checking filters each candidate point;
checked assignment filters the complete proposed state before its one
snapshotted callback invocation. An authoritative COLLECTION graph operation
computes activity in the full translated namespace, including cross-child
dependency rows, then passes each child constraint only its active child-scope
entries with collection affixes removed. The authoritative SHADOW graph
operation computes activity over merged hidden and visible origin state and
passes already filtered slices to the existing hidden-first/visible-second
adapter. Exact detached collection and Shadow carriers have no schema, so they
do not and must not reimplement activity; their owning BASE/graph check site
filters before invoking them. Activity remains evaluation-time state and is not
added to carrier plans or the Shadow capsule.

Dependency filtering is owned where the dependency row lives. A collection-
level cross-child edge affects a collection-level `$get_values()` read; reading
one child directly applies only that child's dependency graph. The specialized
Design/sampler row masker remains separate because it operates on complete
sampled rows in which every parent already has a value, so a default cannot be
selected. It must remain behaviorally equivalent to the list kernel on that
complete-row domain and is not permission for a second point/list engine.

## One native semantic engine

There is one semantic implementation for current objects. Thin R wrappers do
only work that intrinsically requires R language semantics—such as capturing
`depends`, constructor representations, or `...`—and then enter one registered
native operation. There is no parallel R/checkmate/data.table/S3 execution
engine whose result, callback order, and diagnostics must be authenticated and
reproduced.

Standalone Domain checks and ParamSet scalar/table checks share the same
package-owned built-in value classifier and failure-only formatter. The
classifier is semantic authority; the formatter reports its recorded category
and values. Neither native validation path calls checkmate, revalidates in R,
nor maintains a second copy of the admission rules for presentation.

The same native operations consume `BASE`, `COLLECTION`, and `SHADOW` payloads.
Node-specific planning is an internal switch inside the engine, not a public
method replacement protocol. Unsupported arguments and corrupt current state
produce deterministic package errors; they do not return a sentinel that
restarts the operation through a second implementation.

Native code may evaluate documented user callbacks through R. It may also use
ordinary R helpers for genuine language capture or error construction. It must
not call checkmate or data.table to implement a hot semantic operation. Shipped
code is portable C99 and supports R >= 3.6. Equivalent API spellings are
centralized in `src/r_api_compat.c`; current R retains its public,
allocation-free ordinary-frame fast path and no semantic translation unit
gains a parallel old-R engine. Authentication keeps one conservative rooting
proof across all supported branches because hostile class metadata can
allocate during facade admission, and rejects recognized callback-backed user
databases before binding inspection.

R 3.6--4.1 has no public non-evaluating single-binding existence query. Cold
optional lookups therefore use `base::exists(..., inherits = FALSE)`, which
does not invoke active bindings. Required authenticated ordinary-frame binding
snapshots remain allocation-free. A terminal genuine-shell optional receipt
scan cannot allocate, so on those runtimes it uses the
header-declared/exported `R_HasFancyBindings()` only to fail closed for a fancy
frame before selecting a stored cell. R 3.6--4.5 uses the declared/exported
`Rf_findVarInFrame` to retrieve that stored frame cell. R 3.6--4.4 may then
inspect a returned `PROMSXP` through the header-declared/exported
`R_PromiseExpr`, `PRENV`, and `PRVALUE` without forcing it. R 4.5's compiled-
code policy classifies those accessors as non-API and offers no replacement,
so recursive migration fails closed when it reaches a promise and requests
that the operation be performed under R 4.0--4.4 or R >= 4.6. Ordinary
factory-created callbacks can retain formal promises in their lexical frames
even when the formal is forced or unused; this boundary therefore does not
require an explicit delayed binding.
R >= 4.6 uses only the documented experimental binding classifier and delayed/
forced-binding/dots accessors. None of the three detached-promise accessors is
locally declared or present in an R >= 4.5 DSO, and a `PROMSXP` reached outside
one of the R >= 4.6 public binding boundaries is opaque. An
R-level `substitute()` is non-forcing but returns a promise expression rather
than a binding-kind/generation receipt; it cannot distinguish that expression
from a realized language/symbol value and therefore makes receipt scans and
recursive graph discovery unsound.
Likewise, public `R_getVar` is absent from every pre-4.6 DSO: without
`R_GetBindingType` it may force the very delayed binding being inspected. The
R >= 4.6 source contains exactly three reviewed `R_getVar` call sites, one in
the direct-value facade and two in graph direct/forced-value branches. Every
call follows the classifier; the DSO inventory contains one undefined-symbol
row.
The registered direct-binding projection preserves the API classification:
realized language objects and symbols are returned as values, while delayed
promises carrying language/symbol expressions remain promises and are never
evaluated. Expression or R storage type is not a substitute for binding kind.

Option access does not enlarge that exception ledger. The shared bounded
simple-Domain renderer uses public `base::getOption()` through the facade on
R 3.6--4.4 and documented `Rf_GetOption1` on R >= 4.5. Runtime symbol gates
forbid the latter in old DSOs and require one occurrence in newer DSOs; only
the option snapshot is versioned, not the representation engine.

Every exceptional symbol, version range, source occurrence, and rationale is
listed exactly in `environment/r-api-exceptions.tsv` and must pass raw-token,
DSO, pinned-header, and real-runtime audits before freeze. These entries are not
a CRAN allowlist and authorize neither another internal API nor an alternate
semantic path. The exact R 4.5.2 runtime also runs its own
`tools:::check_compiled_code()` over the installed package and retains a
manifest-bound zero-issue receipt. R 3.6 also exposes no function accessor for
an active binding.
Direct and recursive legacy ParamSet-family migration fail closed if active-
binding inspection is required and ask the user to migrate under R >= 4.0; they
never invoke or silently skip the binding. Paradox-1 ParamSet-family R6 shells
themselves contain active bindings, so their practical object/graph migration
requires R >= 4.0. Exact built-in current Paradox-2 shells retain recursive
traversal through their authenticated capsule. Their package active facades are
opaque on R 3.6: unsupported in-place replacement of such a facade cannot be
distinguished when all exact shell receipts remain intact, and nodes reachable
only from the replacement closure are not traversed. The binding is never
invoked. Additive shells and modifications that fail exact authentication
instead fail closed. This
limitation does not affect current-object operations, idempotent current-object
conversion, standalone legacy Domain/Condition conversion, or graphs without
arbitrary active bindings.

Other old-header adaptations remain public and inline: fresh raw/complex
destinations use direct vector access before the element-setter declarations
exist, and the default `identical()` flag value is used before its named macro
appears. Collection parameter reads pass the already admitted, rooted core
directly to the shared params loader; no temporary environment, repeated
capsule lookup, or `R_NewEnv()` compatibility implementation exists.

Exactly two narrow cold semantic-orchestration families are allowed to use R;
each has one implementation and neither is a fallback. The first is internal
tuning, where documented cargo callbacks and lexical R closure environments are
the operation itself. Its three public operations are
`$aggr_internal_tuned_values()`,
`$disable_internal_tuning()`, and `$convert_internal_search_space()`. Each is
one implementation, captures all required capsule cargo, translation, Domain,
and owner-value state before its first callback, executes only the documented
cargo callbacks, and commits through the native value/capsule mutation
boundary. After native subset/flatten has produced canonical detached state,
`ParamSetCollection$flatten()` may additionally traverse its validated
translation snapshot and rebind `cargo` callbacks to the flattened namespace.
Its sole canonical write is a package-owned replacement of the already
detached result's rewritten `cargo` column. This family neither repeats
structural admission nor selects a competing graph/check/value/callback engine,
and it is never a fallback.

The second is exact-TuneToken `$search_space()` conversion. Before R sees a
token, native admission validates and roots its exact fixed shape, built-in
kind, and target Domain. Any exact BASE ParamSet content is replaced with a
sealed single-use subset capability, and native code returns that one stable
operation snapshot. The cold conversion switches only over those built-in
kinds, evaluates the documented transformation/output-compatibility question,
and constructs the outward search-space ParamSet. It does not dispatch through
third-party TuneToken, Domain, or candidate methods, reread a live candidate,
revalidate token/Domain structure in R, or compete with another native/R
conversion implementation. Callback-based plausibility sampling and its exact
RNG restoration belong only to this conversion, not to native structural
admission.

Deep clone is separate cold R6 shell-lifecycle orchestration. Its explicit work
stack preserves shell identity and shared graph topology, but all capsule
validation, installation, generation checks, and Shadow signature rebuilding
remain native. It is therefore not another semantic exception: internal tuning
and exact-token search-space conversion are the complete cold R *semantic*
orchestration boundary, while clone and detached equality are shell/presentation
glue with no independent admission or mutation contract. The one-way legacy
migration utility is likewise outside this current-operation count. It
authenticates a retired private schema and orchestrates current constructors/
native validation plus R6 shell transplant; it is never an alternate
implementation or fallback for a current capsule operation.

Cold outward glue need not invent a native duplicate. In particular,
`all.equal.ParamSet()` compares ordinary detached views produced by the native
readers using base R's equality machinery. The view is a flat ordinary graph
record with `root`, ordered `nodes`, and per-node edge kind, names, and canonical
node IDs, built by an explicit work stack. Thus independently built equivalent
DAGs compare equal, shared and duplicated topology differ, and an active-path
cycle errors. Derived COLLECTION/SHADOW callback adapters are excluded in favor
of the complete child/origin state that defines them. The method neither walks
private R6 environments nor revalidates capsule semantics, and there is no
competing C equality implementation or fallback selection.

`ParamSet$check_dependencies()` is not cold presentation glue. It is a strict
native dependency-only operation using the same admitted capsule graph, point
mapping, and dependency kernel as `$check()`. Its input is an ordinary uniquely
named base list with no class or semantic attributes. It diagnoses unknown IDs
even when no dependency rows exist, skips an edge when its child or parent is a
TuneToken, applies the shared default-aware activity rules, and returns `TRUE`
or the first dependency diagnostic. It is a store-blind point operation and
never fills the candidate from stored `$values`. Paradox 2 does not preserve
the former R data.table/pmap traversal or its newline-collapsed multi-error
result.

`ParamSet$test_constraint()` and `$test_constraint_dt()` likewise enter the
same native graph planner, point admission, and constraint kernel as the full
checks. `assert_value = FALSE` skips Domain-value validation but not structural
point admission. Every constraint receives only the active subset selected by
the shared default-aware activity kernel. With validation enabled, the table
method snapshots the input, validates every row before executing any constraint
callback, and then evaluates the operation's snapshotted constraint set exactly
once per row in order. A callback mutation cannot replace the callback selected
for later rows of that operation. The table method retains its documented
data.table-only boundary. There is no R scalar or per-row constraint
implementation.

Tag projection/replacement, dependency-table snapshot/projection/replacement,
dependency append, and BASE constraint/extra-transformation callback
replacement are registered native mutation boundaries. They validate and own
the complete replacement before swapping a capsule. Dependency-table snapshot
and bulk `$deps <-` are callback-free structural operations: they admit only
exact closed Conditions, require valid child IDs and reject self-edges, but
retain the established dangling-parent behavior and do not require Condition
RHS values to remain feasible. Thus copying a dependency graph after narrowing
a parent Domain preserves its exact predicate; a wholly impossible predicate
means that its child is always inactive. `$add_dep()` is deliberately stricter:
the shared check kernel verifies RHS feasibility and, if that validation
callback changes the target capsule, the nested mutation wins and the outer
append errors without overwriting it. `SHADOW$add_dep()` routes to this strict
native append only after proving both endpoints remain in the fixed visible
schema. These operations have no R/checkmate/data.table mutation planner and no
shared feasibility/fallback mode.

`$has_deps` is a registered scalar reader, not an alias for
`nrow(self$deps)`. A BASE validates its canonical dependency table directly. A
SHADOW performs its one authoritative live refresh and validates that selected
table. A COLLECTION admits the complete capsule graph, including corruption
and cycle checks, and reads only the root subtree dependency count. The reader
does not construct detached dependency columns or a data.table facade, cache
graph validity, or select a reduced-integrity collection path.

## Operation snapshot and callback contract

Each public operation is one transaction with these phases:

1. The thin wrapper captures only documented language arguments and outward
   representation metadata. This capture is not semantic admission and may
   observe an input before native code does.
2. Ordinary arguments are forced and validated once in their documented
   left-to-right order. Interpreted outer shells are required to be ordinary
   non-ALTREP/non-S4 and are rejected before semantic observation, except for
   the documented public-table and exact `set_values(.values=)` one-snapshot boundaries
   specified below. At native admission, supported ALTREP atomic semantic
   inputs are materialized once into independently rooted ordinary snapshots
   during this phase. Those snapshots, not previously captured representation
   text, are semantic authority.
3. The engine validates the current payload graph and captures one operation
   snapshot of all structural state, values, dependencies, callbacks, mapping
   decisions, and mutation generations that the operation can observe.
4. The operation executes against that snapshot. User callbacks run in their
   documented row/parameter order and receive the documented values and named
   arguments; constraint callbacks receive the dependency-active subset
   specified above.
5. A mutating operation verifies that every capsule generation on which its
   write depends is unchanged, then commits all package-owned state atomically
   in an allocation- and callback-free section. Failure before commit leaves
   the capsule unchanged.

A callback may mutate external R state or call back into Paradox. The nested
operation observes the state current at its own entry. Mutations made by a
callback can affect a later public operation, but they do not replace callbacks,
tables, methods, dependencies, or filters already captured by the enclosing
operation. The package never falls back and replays a callback after execution
has begun.

If a reentrant callback mutates a capsule that an enclosing mutating operation
intends to commit, the generation check raises one deterministic
concurrent-mutation error instead of overwriting the nested operation. The
nested mutation
and other callback side effects remain visible; the enclosing operation is not
retried. A read-only enclosing operation may finish from its already captured
snapshot.

ParamSet-bearing ObjectTuneTokens add one input-generation dependency that is
not part of the target graph. Native admission roots an exact receipt for each
selected live candidate before any ParamUty/constraint callback. Public checking
reauthenticates those receipts after all callback-capable work. Checked value
assignment retains them through sanitized-result and replacement construction,
checks target generations, then performs one final nonallocating receipt scan
as the last operation before the commit wave. That last scan uses only
non-forcing ordinary-binding lookup and identity; it cannot execute candidate
code. A finalizer or callback that changed a candidate at any intervening
allocation point is therefore detected without replay or partial commit.

Warnings and errors from callbacks propagate once and in callback order.
Ordinary built-in value failures preserve informative checkmate-style
categories and established message fragments through the native formatter.
The package does not promise byte-identical wording for every checkmate quirk,
`conditionCall()`, stack shape, local variable names, or the results of
`substitute()`, `sys.call()`, and `parent.frame()` inside an implementation
frame. Side-effecting promises are not a mechanism for changing the state seen
by later rows of one native operation.

A transformation result and every non-table transformation input must have an
ordinary non-ALTREP, non-S4 outer list shell. A documented data-frame input may
also use the suffix-classified top-level ALTREP table boundary, which is materialized once
before the same structural validation. Admitted semantic atomic leaves and
data-frame columns may be stable ALTREP and are materialized once. A base
`ParamSet` extra transformation may
return an unnamed list. The result remains unnamed, preserving the established
one-dimensional `to_tune(ParamSet)` and public callback behavior. If a base
result supplies names, they must be complete and unique. A child extra
transformation evaluated as part of a `ParamSetCollection` must return complete
unique names, because the engine cannot translate an unnamed child result into
the collection namespace. Both cases are admitted by the same native
transformation engine; there is no R-side repair or replay.

The callable closures returned for live or detached collection callbacks are
public R functions for compatibility, but their bodies are native-entry
wrappers, not semantic implementations. A detached closure's exact three-field
plan records translation plus callback carriers and their owner indices. An
extra-transformation carrier also owns the detached BASE shell required for its
documented `param_set` callback argument; a constraint carrier needs no such
shell. The plan cannot use a later core-method override as an alternate
callback engine.

## ALTREP boundary

ALTREP is supported by materialize-once semantics, not by a decline-and-replay
fallback. Stable implementations are supported, including base ALTREP compact
sequences such as `1:n`. Here stable means that merely observing length,
elements, attributes, or representation does not itself change the answers to
later observations in the same external state. The semantic guarantee begins
at native admission, after a thin R wrapper may already have performed
documented language or printable-representation capture. An accessor may
allocate or reenter R, so the native engine never sizes from one ALTREP
observation and fills from another. It materializes each admitted public
semantic input exactly once into a protected ordinary vector, validates and
plans from that vector, and retains it until the last use.

This support is positional, not structural. Configuration/search-space and
transformation list shells, ParamSet constructor `params` lists, Domain/
Condition/TuneToken/capsule shells, Domain cargo containers and interpreted
cargo entries, internal table shells, row containers, class/name vectors,
dimnames, and other list metadata are interpreted structure and must be
ordinary non-ALTREP and non-S4 objects. The six documented public-table
ingresses—`check_dt`, `test_constraint_dt`, `qunif`, data-frame `trafo` input,
`Design$transpose()`, and Design dependency planning—share one native
classifier. It recognizes an ordinary well-formed class vector whose terminal
suffix is `"data.frame"` or `c("data.table", "data.frame")`, then applies the
attributes allowed for that recognized table kind. Leading additive classes
are representation-only: native admission never dispatches through them. The
classifier does not copy or materialize an ordinary shell merely to remove the
prefix, and semantic snapshots ignore it. When a top-level ALTREP shell already
requires materialization, that owned snapshot drops the prefix and installs the
canonical suffix. Every class
label is non-missing, non-empty, non-bytes, and unique. The reserved labels
`"data.table"` and `"data.frame"` may occur only in the recognized terminal
suffix; reversed, duplicated, and non-suffix reserved-label forms reject.
Names and classes are ordinary, attribute-free character vectors. An empty
`structure(list(), class = "data.frame", row.names = ...)` may omit its names
attribute, preserving the base-compatible zero-column spelling.

A data.table's optional `.internal.selfref` is an attribute-free, non-S4,
nonobject external pointer; `sorted` is an attribute-free ordinary character
vector; and `index` is an ordinary non-S4, nonobject integer(0) carrier. Any
attributes below that `index` carrier are uninspected data.table cache payload.
Paradox never consumes a key, index, or self-reference cache; all three
carriers are discarded from an owned ALTREP-shell snapshot and ignored by an
ordinary-shell semantic snapshot. Package-owned capsule tables and outward
facades continue to use canonical ordinary metadata and never retain this
ingress exception.

The raw `row.names` attribute is a non-S4, non-object, attribute-free integer
or character vector. Ordinary compact `c(NA_integer_, -n)` and
`c(NA_integer_, n)` forms decode to `n`; other ordinary values contribute only
their length. Stable integer and character ALTREP row names are admitted with
one Length observation and no Elt observation because labels have no semantics
for these operations. Each row-consuming path compares the captured count with
its admitted columns. Direct `trafo` and Design dependency planning when there
are no dependency rows validate the row-name structure but do not observe
columns merely to authenticate an unused dimension. A zero-column data.frame
therefore retains its declared rows: `Design$transpose()` produces one empty
configuration per row, whereas an unclassed empty list still represents zero
rows.

An admitted suffix-classified top-level VECSXP ALTREP is materialized once. Its
owned snapshot installs only the canonical recognized class suffix; an ordinary
admitted shell is not copied merely to remove inert leading classes. Native
admission owns names and classes before a callback-capable row-name Length or
top-shell Length/Elt observation, calls top-shell Length once and Elt once per
column, preserves column identities, canonicalizes row names from the captured
count, and drops ignored data.table caches. Base R's lazy attribute-only
duplicate is the common motivating case; admission does not depend on its
current internal width threshold. Semantic atomic columns may themselves be
stable ALTREP. Raw attribute selection uses `R_mapAttrib()` on R >= 4.6 and one
exact, ledgered `ATTRIB` traversal on R 3.6--4.5; neither route evaluates R code, calls
data.table, or supplies a fallback engine. This narrow table exception does not
extend to general list, row, callback-result, or package-state shells.
Direct checked and unchecked `$values <-` reject an outer ALTREP
shell before observing its length, names, or elements. They canonicalize the
Paradox-1 clear-values spellings—`NULL`, an ordinary attribute-free zero-length
atomic/expression vector, or an accepted empty list container—to a named native
`list()`. `set_values(.values=)` is
the single general-list exception: the native merge owns one snapshot of that
supplied shell before it interprets names or values. That narrow exception does
not authorize ALTREP list shells at `$check()`, `$search_space()`, direct value
assignment, or any capsule boundary and does not create a fallback engine.

A custom ALTREP whose `Length`, element, data-pointer, or attribute observation
changes its later answers between R-side capture and native admission is
hostile and outside the semantic compatibility contract. Paradox neither tries
to make constructor text agree with that later snapshot nor treats printed
representation metadata as semantic authority. Such an input may be rejected,
or it may be admitted according to the one native snapshot; either way it must
not select an R replay path or cause Paradox itself to crash or corrupt memory.
An implementation that violates the R C API or crashes inside its own accessor
cannot be sandboxed by Paradox and is outside this guarantee. Exact Paradox-1
behavior for these state-changing objects is deliberately not reproduced.
R did not expose VECSXP ALTREP classes before R 4.3, so the adversarial
list-ALTREP fixture is unavailable on R 3.6 while the production list-ALTREP
branch is vacuous. Atomic ALTREP admission and fixtures remain part of the
R-3.6 contract.

Materialization occurs before the payload snapshot. If an ALTREP accessor
reenters Paradox and performs a valid mutation, that mutation is therefore part
of the state captured by the outer operation rather than a reason to replay the
input through another path.

Current capsules do not store ALTREP permanent semantic vectors. Base R's
compact `data.frame` row-names metadata is the sole exception and is never
indexed as semantic data. Setters and constructors normalize admitted inputs
before commit. ALTREP element semantics that fail native materialization or
validation produce an error before the operation snapshot is committed; they
do not invoke an alternate implementation.

Opaque `ParamUty` values, defaults, initial values, `special_vals`, environments,
external pointers, S4 objects, and other leaf payloads are not recursively
materialized or copied. They are independently rooted and passed to their
documented callback or result unchanged. ParamUty special membership is the
sole narrow observation: it uses base `identical()` against each admitted leaf,
including S4, without S3/S4 dispatch. Materialize-once applies to each admitted
atomic vector whose elements, length, names, or attributes Paradox itself must
observe. Conversely, an ALTREP special leaf for ParamDbl/ParamInt/ParamFct/
ParamLgl is rejected before observation; it is not normalized into a typed
special. An admitted S4 special for those kinds matches only by pointer identity,
and an S4 default/init is valid only when it is that same object. These narrow
leaf rules do not permit ALTREP or S4 Domain, Condition, TuneToken, ParamSet,
internal/package-state table, class/name, dimnames, cargo, or other interpreted
structure. The documented public-table shell exception remains exactly the one
specified above.

All existing ownership rules remain: no raw vector pointer crosses an
allocating or callback-capable boundary, every allocated object is protected,
independently replaceable children receive independent roots, output arithmetic
is checked, and interrupts are polled in long loops.

## data.table boundary

Paradox 2 requires data.table 1.18.4 or newer. data.table is an outward
interoperability dependency, not an internal state or execution engine:

- no data.table object is stored in the payload capsule;
- native operations do not use data.table joins, grouping, setters, or private C
  APIs;
- public accessors that historically return a data.table continue to return a
  valid, independently owned data.table facade with the documented columns,
  types, order, row names, key/index behavior where public, and a usable
  `.internal.selfref`;
- mutating a returned table with `data.table::set()` or `:=` affects that
  detached result and never mutates the capsule unless an explicit Paradox
  setter is called.

`Design$data` remains an intentionally outward, publicly mutable data.table,
not a hidden capsule table. A Design operation treats its then-current columns
as public input and snapshots/materializes them once before entering the engine;
user mutations are visible to the next operation. Paradox does not keep a
second internal data.table or a derived semantic copy that can diverge from the
public field.

The pre-1.18 `alloc.col()` compatibility bridge and its version-dependent path
are removed. Shipped C may study data.table's public object behavior but may not
include or copy its private APIs.

## Serialization and legacy object-graph migration

Current-schema objects use ordinary `serialize()`/`unserialize()` and
`saveRDS()`/`readRDS()` behavior. Serialization includes the versioned payload
and graph, and round trips preserve node kind, public behavior, callbacks,
reference topology, and explicit named `NULL` values.

The first public 2.0.0 release has two deliberately different explicit
migration boundaries for objects serialized by Paradox 1.x or the superseded
compatibility-first 2.0.0 implementation.

`upgrade_paradox_object(x)` remains the pure single-object converter:

- Current-schema objects are accepted idempotently.
- Canonical legacy built-in Domain and Condition objects are normalized to their
  current closed forms; canonical `ParamSet` and `ParamSetCollection` graphs are
  validated and rebuilt with current capsules.
- The input is never mutated. Legacy graph sharing and cycles are detected;
  valid shared-child identity is preserved in the rebuilt graph and cycles are
  rejected.
- Public values, tags, dependencies, transformations, constraints, prefix and
  postfix rules, and callback closures are preserved when they can be mapped to
  the closed current model.
- Unknown Domain or Condition classes, replaced core methods, malformed private
  state, and legacy third-party R6 subclasses not covered by an exact
  registered owner bridge fail with a path-specific diagnostic.
- The upgrader never executes a legacy operation merely to discover its state.

During pure and graph preparation, package-interpreted legacy callbacks undergo
the same source-reference normalization as fresh callbacks. Only exact known
Paradox-1 package-generated wrapper shapes are authenticated: categorical
mapping, collection-flattened `in_tune_fn`, tuning-ParamSet transformation, and
detached collection transformation/constraint adapters. An authenticated
detached adapter contributes its captured ParamSet carrier list as explicit
migration dependencies, preserving aliases. Before R observes that list, one
registered C boundary rejects ALTREP, S4, and object shells and shallow-copies
an ordinary list with optional ordinary names. The wrapper is always rebuilt
in a fresh closure environment before carrier rebinding, so neither the pure
converter nor graph preflight mutates serialized input. With
`paradox.strip_srcrefs = FALSE`, that rebuilt wrapper retains source metadata
but does not retain wrapper pointer or environment identity. No arbitrary
callback environment becomes a migration edge.

The recognition templates deliberately preserve the wrappers' free carrier
symbols byte-for-byte. Their narrow `utils::globalVariables()` declaration is
for codetools only: admitted fresh environments supply the values at execution
time, and no package-namespace fallback is added. Binding dummy values or
otherwise rewriting a template body would weaken compatibility by making the
exact serialized wrapper fail authentication.

`upgrade_paradox_object_graph(x)` is the identity-preserving recursive
migration boundary for a containing object:

- It returns `x` invisibly. Every admitted legacy ParamSet-family R6
  environment is transplanted in place, so references held by an enclosing R6
  object, closure, attribute, collection edge, or another alias remain valid.
  The public `assert_values` policy is preserved for built-in, additive-owner,
  and replacement-owner shells.
  Standalone Domain/Condition normalization remains in the pure converter; the
  graph crawler does not replace arbitrary immutable leaves inside containers.
- Native discovery is iterative, pointer-memoized, interruptible, and visits
  each reachable object at most once. It follows list/vector elements,
  pairlists, calls and expressions; all attributes including S4 slots;
  environment bindings and enclosing parents; active-binding functions without
  invoking the binding; closure environments, formals, bodies and bytecode
  expressions; and forced or unforced binding/`...` promise structure without
  forcing an unforced promise where the runtime has a policy-compliant
  inspection API. A forced binding promise contributes its stored value and
  expression; an unforced one contributes its expression and evaluation
  environment on R 3.6--4.4 and R >= 4.6. R 4.5 fails recursive migration
  closed on a reached promise with an R 4.0--4.4 or R >= 4.6 instruction.
  Ordinary factory callback frames can retain such formal promises. R >= 4.6 treats a
  detached `PROMSXP` outside a binding/dots cell as opaque.
  R 3.6 cannot retrieve an arbitrary active-binding function and therefore
  fails this migration closed with an R >= 4.0 upgrade instruction rather than
  invoking or silently omitting the binding. Exact built-in current Paradox-2
  shells have one narrow exception: authenticated capsule authority is
  traversed directly while locked methods and package active facades remain
  opaque. Unsupported relocked method and active-facade replacements cannot be
  distinguished from generated code on that runtime and are likewise opaque.
  Direct bindings are classified by the native binding API, never by evaluating
  or inspecting a substituted R expression. A realized `LANGSXP` or `SYMSXP`
  is therefore a value, while a delayed promise whose expression is a language
  object or symbol remains a promise; both are traversed according to their
  actual binding kind without forcing.
- `.GlobalEnv`, every attached search-path environment (including Autoloads),
  package and namespace environments, imports environments, base, and the empty
  environment are traversal boundaries. Imports classification requires an
  ordinary raw scalar `name` with the `imports:` prefix and
  `R_BaseNamespace` as the direct parent; a user environment cannot become a
  boundary by spoofing descriptive metadata alone. Thus a closure made by
  `crate()` or a
  local R6 private enclosure is searched, but reaching package/global
  infrastructure cannot expand the migration to the whole session.
- Weak-reference internals and generic external-pointer address/tag/protected
  slots are opaque. Attributes remain ordinary graph edges. The one protected
  slot exception is an authenticated Paradox `.core`, whose ordinary-R payload
  is traversed so a legacy shell hidden in a current v2 capsule value or callback
  closure is not missed.
- Discovery is followed by complete legacy/current authentication, owner-hook
  inspection, semantic preparation, offside construction, dependency-plan
  validation, and shell-shape auditing. Current ParamSet-family authentication
  has one native inert owner: its class is an ordinary, attribute-free,
  non-ALTREP vector of unique nonempty labels ending in
  `c("ParamSet", "R6")`; an immediately preceding
  `"ParamSetCollection"` or `"ParamSetShadow"` selects that family, and
  earlier nonreserved labels are additive R6 subclass layers. With no family
  marker the shell is BASE. The family must agree with a canonical package core
  (including exact Shadow metadata), and the shell's `assert_values` binding
  must be an exact attribute-free, non-missing `logical(1)`.
  No
  serialized method, active binding, or user callback is executed to inspect
  legacy state. Current ParamSet-family shells are discovery candidates too:
  their complete capsule graphs and capsule-kind/class agreement are validated
  in preflight, although they are omitted from the commit order. Thus corrupt
  current state anywhere in the selected graph cannot be hidden behind a
  shallow external-pointer/carrier check or allow a valid legacy sibling to
  mutate first.
- A current Shadow is validated against its authoritative live origin without
  mutating the selected shell: native code retains the selected private
  `.core` as the source-generation receipt, separately builds the semantic
  preview core, and derives collection callback detachment from the already
  admitted graph rather than rereading child shells. After offside preparation
  and transplant planning, every prepared/current root is admitted by one
  native all-roots operation and all selected generations pass one
  allocation-free receipt scan before the first transplant.
- Commit is post-order and monotonic. A child is transplanted first; only then
  is its prepared parent's child/origin edge rebased to the identity-preserved
  original shell. A rebase may allocate or invoke a registered replacement
  factory, so all already-current identity roots plus that newly rebased
  prepared root are jointly validated immediately before the parent is
  transplanted. An unrebased parent remains an offside template until its own
  turn. The transplanted original then joins the current identity-root set,
  which is jointly validated again. Each completed shell has current
  enclosures, `self`/`private`/`super` links, methods/active bindings, capsule,
  class, and `assert_values` state and is independently valid. An ordinary
  semantic, bridge, or shape failure is preflight-only and leaves the graph
  untouched. A catastrophic allocation failure or interrupt during commit may
  leave a prefix of valid current nodes; retrying the idempotent operation
  recognizes those nodes and completes the remainder. This does not promise
  rollback across a pending finalizer from an unrelated user object:
  `suspendInterrupts()` does not suppress finalizer execution at R evaluator
  safe points. If such a finalizer deliberately mutates a selected root inside
  the R-level binding wave, the post-transplant joint scan detects and errors,
  but a completed transplant stays completed and the externally corrupted
  graph is not promised to be retryable.

Current R6 methods never pay a migration dispatch. Paradox's package-local
leanifier stores their bodies under versioned `.__paradox2_*` namespace names,
and newly constructed shells call those names directly. Historical
`.__ParamSet*`/`.__ParamSetCollection*` targets, plus unversioned
`.__ParamSetShadow*` targets emitted by pre-release Paradox 2, are cold
gateways. An authenticated capsule-backed shell, including the actual
pre-release Shadow payload, forwards directly without consulting the legacy
option or requiring an owner registry. The native gateway applies the same
ordinary additive family-suffix classifier, requires the exact `assert_values`
binding and canonical matching core, follows the authenticated superclass chain
to the enclosure that defines the requested historical family target, and
returns one rooted context receipt. It never evaluates the serialized stub's
still-lazy `private` or `super` arguments, forwards a guessed top enclosure
slice, or rereads the shell in R after authentication. A shell cannot borrow
another current object's enclosure. Otherwise the default
`getOption("paradox.legacy_object_action", "error")` produces a precise error
that names `upgrade_paradox_object_graph()`. With
`options(paradox.legacy_object_action = "upgrade")`, the gateway silently
upgrades the reached shell graph by identity, resolves the new enclosure, and
continues the originally requested operation. No direct
`mlr3misc::leanify_package()` target is current authority.

Legacy third-party ParamSet subclasses are admitted only through
`register_paradox_object_upgrader()`. Registration is exact and narrow:

- the full class vector is exactly
  `c(<one owner class>, "ParamSet", "R6")`; there is no S3 dispatch,
  superclass search, partial match, or arbitrary subclass chain;
- registration must originate in that owner package's currently loaded
  namespace and records only the owner name, exact class, `"additive"` or
  `"replacement"` migration kind, namespace-local inspector/rebuilder names,
  and replacement-only retired public bindings;
- hooks are resolved anew from that authenticated namespace. Paradox never
  stores or executes an inspector/rebuilder function recovered from serialized
  bytes;
- Paradox authenticates the common R6 shell and package provenance; the
  inspector authenticates any owner-specific state it reads. An additive
  inspector returns an empty named dependency list; its rebuilder receives the
  authenticated prepared BASE object and cannot retire bindings. A replacement
  inspector returns exactly one ParamSet-family dependency named `origin`; its
  rebuilder must construct the exact registered class around a canonical
  current `ParamSetShadow` capsule and can retire explicitly declared old-only
  active fields;
- public or private R6 finalizers are outside the owner-migration contract:
  their registration belongs to one environment identity and transplanting
  temporary/current enclosures could cause premature or repeated cleanup;
- undeclared owner fields, an unknown exact class, altered owner methods, a
  stale/unloaded registering namespace, malformed hook results, and conflicts
  fail closed during preflight.

Package provenance is identity-based, not label-based. A built-in legacy
method enclosure must be parented by the exact current Paradox namespace. An
owner bridge must be parented first by the exact namespace incarnation retained
in its registry entry and then by that same exact Paradox namespace.
`isNamespace()` and `environmentName()` alone are insufficient because an
ordinary environment can reproduce their descriptive metadata.

bbotk's legacy `Codomain` is the maintained additive registry case.
miesmuschel registers its exact legacy Shadow as a replacement by Paradox's
package-owned `ParamSetShadow`; `params_unid` and `set_id` are explicitly
retired rather than retained as parallel state. Unknown third-party subclasses
remain unsupported.

Historical `mbo_config` RDS files are mandatory upgrade fixtures. Old objects
may be passed to either appropriate upgrader after `readRDS()`. Direct first use
errors by default; transparent first-use migration is available only through
the explicit option above. Package documentation must show recursive upgrade
for containing objects and explain the opt-in policy. Release harnesses obtain
these bytes from the exact commit/tree
jointly selected by the reviewed repository ledgers, stage one read-only
receipted bundle, and must execute rather than environment-skip the fixture
test; a mutable checkout path is not release provenance.

## Downstream bridge order

The coordinated downstream transition happens in this order:

1. Paradox implements and exports the package-owned `ParamSetShadow`, capsule
   nodes, pure and recursive upgraders, exact owner registry, versioned current
   lean targets/cold historical gateways, and closed functions while still
   carrying version `2.0.0` as an unreleased candidate.
2. bbotk changes its one private `ParamSetCollection$.sets` read to the public
   `$sets` accessor and verifies that additive `Codomain` inheritance works
   without a core override. bbotk registers the exact legacy
   `c("Codomain", "ParamSet", "R6")` class with an inert inspector and current
   additive rebuilder; Paradox injects the authenticated prepared BASE state.
   It also reserves every historical owner-local `.__Codomain__*` lean target
   as a cold default-error/opt-in-upgrade gateway, including `$clone()`, which
   otherwise need not reach an inherited Paradox method.
   Its native local-search code also roots the
   detached public `$data` and `$deps` snapshots for the entire lifetime of
   every stored column/Condition pointer; Paradox 2 no longer leaves a private
   alias that accidentally keeps those facades alive.
3. mlr3mbo uses the public
   `$subset(..., keep_trafo = FALSE)` boundary on Paradox 2 instead of mutating
   detached/private Domain transformation storage. Its Paradox-1 paths remain
   unchanged.
4. miesmuschel publishes a dual-version bridge. With Paradox 2 it selects and
   re-exports Paradox's `ParamSetShadow` generator, registers its exact legacy
   class as a replacement migration, and supplies owner-local cold gateways if
   a historical override reaches an owner namespace target before an inherited
   Paradox gateway. With Paradox 1.x it retains its legacy class. Its tests
   exercise migration by identity, retired `params_unid`/`set_id` errors,
   `$origin`, visible-value write-through, hidden-value preservation, live
   dependencies/constraint/transformations, and cross-boundary rejection on
   both branches.
5. celecx, mlr3, and mlr3fselect keep their runtime behavior unchanged.
   Diagnostic-only version gates introduced for the early generic Paradox-2
   messages are removed where the centralized native formatter restores the
   established ordinary-value fragments. celecx retains its independent
   cycle/dependency bridge. mlr3pipelines may avoid pinning internal call shape,
   and additionally owns a real GraphLearner deep-clone repair where an R6 value
   in `state$param_vals` was shared; the former ParamSet private layout merely
   masked that alias in its test helper. mlr3fda may retain snapshot variants
   for call-frame differences, which are not part of the diagnostic contract.
   ConfigSpace and bbotk verify retained closed Condition function names and
   built-in shapes; ConfigSpace rejects an unknown condition explicitly rather
   than interpreting it as `CondAnyOf`.
6. Dual-compatible downstream releases or reviewed candidate branches are
   available before Paradox 2 is submitted. The Paradox release gate tests
   those exact bridge revisions, then tests the wider priority-zero/one corpus.
7. Only after the downstream bridges and Paradox 2 are public may downstream
   packages remove their Paradox-1 compatibility branches on their own release
   schedules.

The obsolete `smashy` copy may adopt the same bridge, but it is not allowed to
restore private-layout compatibility to the Paradox contract.

## Verification policy for the replacement candidate

Correctness claims attached to a semantically different superseded package
payload are historical. The replacement requires fresh source-bound evidence,
except where the release ledger retains an independently replayed proof that two
refs build the same complete package payload and explicitly scopes reuse to
inputs unaffected by their tooling-only diff. Development validation must
maximize information per wall time and avoid repeating full gates while the
contract is still moving.

The package suite must contain contract tests for:

- exact capsule schema/version rejection and corrupt-state no-segfault behavior;
- `BASE`, shared/cyclic `COLLECTION`, and live-origin `SHADOW` semantics;
- additive subclasses and deterministic non-support for core override/private
  replacement;
- closed Domain and Condition acceptance/rejection, including proof that
  constructor, ParamSet, and ObjectTuneToken paths share the sole built-in
  Domain-row semantic admission owner; Object-token Domain tests admit bounded
  built-in Domains, reject unbounded `ParamUty`, and exercise opaque leaf
  identity through a bounded typed Domain;
- every exact built-in TuneToken class/content shape, scalar-name normalization,
  explicit search-space input, serialization, and rejection of subclasses,
  extra/reordered metadata, malformed calls/content, and deep/cyclic forgery
  without arbitrary traversal; exact BASE Object content accepts no collection,
  shadow, or additive subclass, safe genuine-core aliases invoke no methods,
  generation receipts survive allocation/finalizers, and search consumes sealed
  single-use capabilities;
- ordinary non-ALTREP/non-S4 structural admission across Domains, Conditions,
  TuneTokens, ParamSet candidates/constructor lists, value/search containers,
  transformation input/results, Domain cargo, internal table shells, dimnames,
  and list metadata, plus the shared strict public-table classifier at all six
  ingresses. Public-table coverage includes canonical and additive terminal
  class suffixes, no prefix-induced ordinary-shell copy, ALTREP-snapshot class
  canonicalization without dispatch, rejection of
  malformed/reversed/non-suffix/reserved/duplicate class vectors, one-shot
  top-shell ALTREP materialization, allowed/discarded data.table cache carriers, ordinary and
  compact row-name forms, stable integer/character ALTREP row names with one
  Length and no Elt, missing/S4/attributed/mismatched row-name rejection,
  mutable shared-name reentry, and zero-column data.frame row counts;
  typed special-leaf ALTREP rejection, pointer-only typed S4 special/
  default/init matching, and opaque ParamUty S4 leaves with base-`identical()`
  special membership and no dispatch;
- malformed exact-token/Domain structure raises a hard boundary error while an
  ordinary infeasible value retains the check-diagnostic protocol;
- operation-entry forcing, snapshot, callback order, mutation visibility, and
  no-replay behavior;
- one shared default-aware list-basis activity kernel across scalar/table point
  checks, dependency-only checking, stored-value filtering, and BASE/
  COLLECTION/SHADOW constraint evaluation. Coverage includes transitive chains,
  conjunctions/diamonds, explicit-value precedence, `NoDefault`, TuneToken
  endpoints, dangling parents, safely rejected admitted BASE cycles,
  store-blind points, collection-level ownership, and equivalence with complete
  sampled rows;
- checked storage of Domain-valid dormant values without dependency rejection,
  including dormant custom/type/bounds validation, atomic mixed failures,
  reactivation, raw-versus-filtered reads, presence/required tightening,
  serialization/clone/equality/upgrade, and the documented non-invariant that
  a legal raw store need not pass strict `$check()`;
- filtered constraint inputs for BASE scalar/table points, checked assignment,
  translated COLLECTION child slices, and merged SHADOW origin state, with
  existing callback count/order/reentry and opaque-leaf guarantees retained;
- one-observation native materialization for stable/base ALTREP under
  allocation, finalizers, and reentry, plus no-crash/no-corruption behavior for
  hostile state-changing custom ALTREP across the R-capture/native boundary;
  direct checked/unchecked value assignment rejects an outer ALTREP before
  observation and natively canonicalizes the Paradox-1 empty spellings
  (`NULL`, an ordinary attribute-free zero-length atomic/expression vector, or
  an accepted empty list container), while only `set_values(.values=)`
  exercises the structural one-snapshot exception;
- detached data.table 1.18.4+ facades, documented data.frame/data.table inputs
  including suffix-classified top-level ALTREP shells, additive presentation
  classes, and base R's lazy duplicate,
  stable semantic ALTREP columns, row-consuming versus non-row-consuming count
  checks, canonical ordinary package metadata, and the absence of internal
  data.table state;
- current serialization, pure upgrades, and identity-preserving recursive
  upgrades of pinned Paradox-1 fixtures, `mbo_config`, nested/shared/cyclic
  containing graphs, callbacks/closures/attributes/environments/promises,
  current-core payloads, traversal boundaries, default/opt-in gateways, exact
  owner bridges, retired fields, and rejected extensions;
- every exact reviewed downstream bridge contract recorded in
  `compat/github-bridge-provenance.tsv`, including public Shadow/subset use,
  detached-snapshot ownership, diagnostic-only adaptations, and independent
  downstream bug fixes.

During implementation, stop at the first failing layer: parse/static harness
checks, directly affected tests, one strict compiler build, then the complete
Paradox unit suite. Collect a bounded batch of failures and repair a coherent
cause before rerunning only affected targets. Do not run a full `R CMD check`,
consumer corpus, memory suite, documentation suite, or benchmark as an inner
loop.

Once one clean Git ref implements the complete contract, validate in this
dependency/acceptance order; independent long stages may overlap after their
prerequisites and candidate bytes are frozen:

1. strict GCC and Clang C99 builds, registered-routine/export audit, static
   analyzers, and the complete package suite;
2. R 3.6.3, R 4.0.5, R 4.3.3, R 4.5.2, and development-R execution plus
   compilation against all seven pinned R 3.6.0--4.6.1 header axes; the exact
   raw-attribute/hot-closure-formals/stored-binding/promise exception ledger,
   raw-token/version-gated DSO audit, and option-access symbol policy; the
   authenticated R 3.6 complete-test closure and separate exact declared-floor
   smoke with `R_DEFAULT_PACKAGES` isolated; and the full-only sealed
   R 4.0.5-to-R 3.6.3 serialization handoff;
3. a new normalized upstream differential whose intentional deltas describe
   this contract reset rather than the superseded seven-delta policy;
4. focused bridge rows, then all priority-zero/one GitHub and CRAN/Bioconductor
   consumers;
5. GCT, Valgrind, bounded rchk, ASan, and UBSan using direct coverage of every
   registered routine and allocation/callback hazard;
6. package checks, examples, vignettes, manuals, pure/recursive/first-use/
   owner-bridge legacy upgrade workloads, and the active book/website
   documentation;
7. current Windows x86-64, exact Windows x86-64 R 3.6.3/Rtools35, and real
   Apple-silicon ARM64 CI against the exact candidate;
8. representative paired benchmarks, alone on an idle host, after behavior and
   bytes freeze; accept their release conclusion only after every correctness
   gate is green.

Use `scripts/environment/resource-jobs` for parallel admission and retain its
report. Reuse authenticated dependency, toolchain, compiler-header, and
consumer-install caches whose byte-affecting keys still match. Build/install
the candidate once from clean source for each distinct
R/compiler/instrumentation profile, and share that immutable installation among
tests or gates that authenticate the identical profile and package bytes. Do
not carry forward a development DSO or any package installation, differential
result, memory report, documentation result, or benchmark whose key includes
different superseded package bytes. Refs with a sealed identical built payload
follow the narrow evidence-reuse rule in the validation and release ledgers;
donor execution identities are never rewritten.

The bounded final performance batch is now complete. Performance work is
frozen again; further source changes reopen only evidence that actually depends
on those changed bytes, but the first contract-first candidate necessarily
reopens the complete release matrix.

## Implementation choices that are not public contracts

Profiling is closed for the 2.0.0 implementation after the retained batches in
`design/final-performance-implementation-plan.md`. In addition to the earlier
low-risk wins (empty constructor storage, resolved collection rows, one Shadow
refresh, and scalar `$has_deps`), the final batch adds one operation-local
getter ID index, one exact dependency/Condition admission whose RHS is reused,
one-pass exact Condition attributes, native completion of fresh
Domain/dependency facades, a single-use internal SamplerUnif ownership handoff,
and bounded collection-graph scratch. The final wide-getter residual is the
single exact dependency/Condition and parameter-table integrity pass; removing
it would change this contract rather than remove duplication.

A sparse search-target projection moved a maintained end-to-end workload by
only about 2%, and a bulk-dependency constructor transaction improved
representative xgboost construction by only about 6--7% despite a larger
requirement-heavy microbenchmark gain. Both experiments remain rejected for
this release because their extra semantic and validation surface is not
low-hanging; neither is an omitted compatibility break or unfinished public
contract.

The final paired policy does not pretend that Paradox 1 performed Paradox 2's
new integrity work. It assigns finite contract-reset budgets only to the direct
live Shadow value read and three direct collection value reads, which validate
an origin signature/generation or complete capsule DAG. All real consumer
operations and filtered getters retain strict ordinary tiers. These rows remain
visible and reviewable rather than waived, and their wider v1-relative budgets
are expected to disappear once Paradox 2 is the release baseline.

The chosen v1 representation is recorded above so maintainers do not recreate
competing state engines. It is still not a downstream contract: a later
Paradox release may change the carrier, schema tag, cache layout, field names,
or translation representation together with its upgrader and tests. The native
engine may be split across readable C translation units. Internal choices must
satisfy the observable contract and ownership rules, but their exact bytes and
names are not promised to external code.
