# Paradox 2 implementation architecture

This document maps the normative contract in
[`contract-first-2.0.0.md`](contract-first-2.0.0.md) to the chosen source
architecture. It replaces the superseded compatibility-first design. Git
history contains that design and its measurements; none of its R6-surface or
fallback mechanisms are current requirements.

The first public 2.0 release deliberately establishes the strict structural
boundary described below. Exotic ALTREP/S4 shells are not retained as a
temporary compatibility layer: no maintained consumer needs them, and doing so
would preserve multiple-observation and duplicate-admission complexity.

## Shape of the system

```text
public R constructor/method/active binding
                    |
       language capture and argument order only
                    |
            registered .Call operation
                    |
       validate + snapshot capsule graph once
                    |
        BASE / COLLECTION / SHADOW planning
                    |
      native kernel + documented R callbacks
                    |
       detached R / data.table outward result
```

There is one semantic path. A native routine either returns the operation's
documented result or raises an error. It never returns a private sentinel that
causes R to repeat the operation through checkmate, data.table, S3 dispatch, or
a second R implementation.

## R6 shell and capsule

`ParamSet`, `ParamSetCollection`, and `ParamSetShadow` keep their public R6
class vectors, constructors, methods, active bindings, reference semantics,
serialization, and supported clone behavior. Each package-created object owns
one private `.core` binding. It is necessarily replaceable by package mutation
transactions, but downstream replacement is unsupported. Core logic ignores
generated wrapper bodies, method registries, closure environments, and legacy
private fields.

`.core` is a NULL-address `EXTPTRSXP`. It has no unmanaged memory and no
finalizer. Its tag is exactly one of:

- `paradox.core.base.v1`;
- `paradox.core.collection.v1`;
- `paradox.core.shadow.v1`.

The protected slot is the sole complete serializable capsule/model truth: an
ordinary, exactly named ten-element list containing `.params`, `.values`,
`.tags`, `.deps`, `.trafos`, `.extra_trafo`, `.constraint`, `.sets`,
`.translation`, and `.postfix`. The identical physical schema avoids three
subtly different state implementations. Node-kind validation decides which
fields are meaningful.

The documented public `assert_values` field is the sole stateful R-shell policy
outside that model. It selects checked versus unchecked native value-store
entry, is retained by ordinary R6 clone/serialization, and is included in
semantic equality, but it is not schema/value/graph authority and does not
alter the fixed payload ABI.

A SHADOW external pointer additionally has exactly one internal attribute,
`.paradox.shadow.snapshot.v1`. It is an exact ordinary alternating
shell/capsule-generation list for the origin graph that produced the current
payload. This attribute is a derived cache only: `.sets[[1L]]` remains the sole
origin edge, callback factories are resolved from the locked package namespace
only on construction or a cache miss, and the attribute contains no semantic
callback or parallel origin. Unchanged refresh performs identity validation and
returns immediately. Deep clone rebuilds the signature for cloned identities;
ordinary serialization can retain it with the serialized graph. Any extra,
missing, or malformed attribute is corrupt state and errors without replay.

This physical layout is documented for maintainers, not exposed as a
downstream ABI. Native code validates the tag, list shape, field types, table
shapes, lengths, names, indices, graph edges, and operation-specific invariants
before access. A forged tag is therefore an error boundary, not a memory-safety
shortcut.

Mutations shallow-copy the payload, replace changed ordinary children, create a
new capsule shell, and swap `.core` only after validation. An operation retains
the capsule selected at entry, so reentrant code observes a precise old or new
generation instead of partially mutated private tables.

## Canonical state

The `.params`, `.tags`, `.deps`, `.trafos`, and `.translation` stores are exact
plain base `data.frame`s with canonical column types, ordinary non-ALTREP
semantic columns, compact base row names, and class `data.frame`. They have no
key, secondary index, `truelength` capacity contract, or `.internal.selfref`.

Values and callback lists are ordinary named lists. Structural strings and
vectors are canonical owned vectors. Every interpreted list/table/Domain/
Condition/TuneToken/capsule shell, ParamSet constructor `params` list,
transformation input/result shell, Domain cargo container/interpreted cargo
entry, row, dimnames, class/name vector, and other list metadata object is
ordinary non-ALTREP and non-S4, except for the top-level shell of a documented
public table input. One shared classifier serves `check_dt`,
`test_constraint_dt`, `qunif`, data-frame `trafo`, Design transpose, and Design
dependency planning. It recognizes a well-formed ordinary class vector ending
in `"data.frame"` or `c("data.table", "data.frame")` and then enforces the
attributes allowed for that kind. Leading additive classes are presentation
metadata only: they never select dispatch. The classifier does not copy or
materialize an ordinary shell merely to remove the prefix, and semantic
snapshots ignore it; the already-required snapshot of an ALTREP shell installs
only the canonical suffix. Class labels are
non-missing, non-empty, non-bytes, unique,
and may not place the reserved data.frame/data.table labels outside their
terminal suffix. Names/classes and data.table cache carriers are ordinary
structure; ignored `.internal.selfref`, `sorted`, and `index` carriers are
discarded. Raw row names are attribute-free, nonobject, non-S4 integer or
character vectors. Ordinary compact positive/negative counts are decoded, and
a stable integer/character ALTREP row-name vector contributes one Length and no
Elt because its labels are ignored.

An admitted suffix-classified top-level VECSXP ALTREP is copied with one Length and one Elt
per column while retaining column identities. Names and classes are owned
before callback-capable row-name or top-shell observation, and the owned shell
gets the canonical recognized class suffix and canonical row names from the
captured count. Ordinary additive-class shells are not copied. Row-consuming operations
compare this count with admitted column lengths; direct `trafo` and a no-edge
Design dependency plan do not observe columns merely to compare an unused
dimension. A zero-column data.frame may omit names and retains its row count,
so Design transpose emits one empty configuration per row; an unclassed empty
list remains zero rows. Base R's lazy attribute-only duplicate is the common
top-shell case, but the contract does not depend on its implementation
threshold. Admitted semantic atomic columns may be stable ALTREP; canonical
capsule columns and facade metadata remain ordinary owned snapshots. Opaque leaves
such as ParamUty values, environments, and
external pointers are rooted but not recursively copied or interpreted.
ParamUty value/default/init/special leaves are the opaque S4 exception, with
special membership preserved through base `identical()` and no S3/S4 dispatch.
A typed-Domain S4 special is a pointer-identity token; typed default/init may
use it only when it is that exact admitted special leaf.

Public accessors build independently owned objects. Accessors historically
returning a data.table attach the public `c("data.table", "data.frame")` class
and a valid self-reference only after every column shell is detached. Mutating
the result with `set()` or `:=` cannot mutate the capsule. `Design$data` remains
an intentionally public mutable data.table and is treated as operation input,
not internal state.

There is one ledgered cold presentation-only data.table identity lookup in
`R/ParamSet.R`. It resolves the unexported `.reassign_extracted_table` and the
exported `set` R functions from the data.table namespace and compares
them with functions already on the active call stack so an extracted detached
`$params` facade can retain historical `:=`/`set()` reassignment behavior. It
calls neither function and supplies no semantic admission, mutation engine,
private C API, capacity contract, or data.table-version bridge. No other
unexported data.table lookup is authorized.

Native subset assembly is also the sole owner of transformation removal.
`$subset(..., keep_trafo = TRUE)` retains the selected per-parameter
transformation rows and `extra_trafo` callback by default. With `FALSE`, the
same transaction constructs an empty canonical transformation table and a
`NULL` extra callback, while `keep_constraint` remains independent. BASE,
COLLECTION, and SHADOW enter this boundary and return a detached BASE result;
there is no Domain-table mutation or R-side Domain reconstruction alternative.
The three subset control flags are exact attribute-free non-missing logical
scalars. COLLECTION's cold callback-detachment wrapper follows the callbacks
actually retained in the admitted BASE result; it does not apply `!` or any
other generic to the original flag objects.
This is the supported replacement for mlr3mbo's former mutation of private
Domain `.trafo` storage.

`all.equal.ParamSet()` is intentionally ordinary S3 comparison glue over these
detached native projections. Each node record contains class and
`assert_values`, params, values, tags, dependencies, and BASE callbacks;
COLLECTION records expose named child edges and SHADOW records their origin
edge. The result is a flat ordinary `list(root = 1L, nodes = ...)`; every node
stores `edge_kind`, `edge_names`, and traversal-canonical integer `edge_nodes`.
An explicit R work stack, not recursive graph calls, preserves shared-node
topology without making independently constructed but equivalent DAGs differ.
Derived COLLECTION/SHADOW callback adapters are omitted because their
child/origin authority is already present, and an active-path repeat errors as
a cycle. The projection never traverses R6/private environments or interprets
capsules itself. There is no parallel native equality routine, so this is one
cold presentation-level implementation rather than an R fallback for a C
result.

## Node graph

All native semantic/admission graph traversal is iterative, uses checked
`R_xlen_t` arithmetic, supports shared child identity, and rejects a node
repeated on the current active path. It does not reject a shared node seen on a
completed sibling path.

Deep cloning is deliberately cold R orchestration over the same capsule graph,
not a second semantic engine. It discovers topology iteratively, memoizes shell
identity, shallow-clones every non-root shell exactly once, and installs cloned
capsules in post-order. Consequently repeated COLLECTION children and a SHADOW
origin that is also reachable by another edge remain one shared node in the
cloned graph. R's serializer preserves the same topology without a custom
serialization format. Opaque ParamUty R6 values retain the established R6 deep
clone behavior: each top-level occurrence is cloned independently, while
opaque nested containers are not recursively interpreted.

This is R6 shell-lifecycle orchestration only. Native capsule validation,
replacement, generation checks, and Shadow signature rebuilding remain the
semantic authority. Cold internal tuning and exact-TuneToken search-space
conversion are the two narrow R *semantic* orchestration families; clone and
detached equality do not own admission, checking, callback selection, or
mutation rules.

### BASE

A BASE node owns one immutable parameter schema snapshot plus its current
values, tags, dependency edges, transformations, extra transformation,
constraint, and supported metadata. Constructing it snapshots and validates
the five closed Domain kinds. A later mutation of the source Domain or a
detached `$domains` result does not affect it.

### COLLECTION

A COLLECTION owns ordered child object references, names, postfix mode, and one
canonical translation table per capsule generation. Construction and `$add()`
install a validated replacement generation; a translation table is never
mutated in place. `$add()` is one registered native transaction. It snapshots
the current graph and the proposed child graph, follows COLLECTION and SHADOW
origin edges, rejects corruption and any existing or proposed cycle before
commit, builds the complete new tables/capsule, rechecks every admitted graph
generation, and swaps only the root collection core. It does not copy dynamic
child values, dependencies, transformations, or constraints into a second
source of truth.
Each operation snapshots the required child capsules and translates their
results in deterministic nested order. Prefix/postfix spelling, named empty
results, named NULL values, and shared-child behavior remain public behavior.

Live `$extra_trafo` and `$constraint` accessors return package-owned thin
closures that enter the collection native evaluator family. Subset and flatten
closures retain exactly translation, callback carriers, and owner indices for
the detached view. An extra-transformation carrier owns the detached BASE shell
needed for the documented `param_set` callback argument; a constraint carrier
does not. A SHADOW over a COLLECTION uses the same engine.
Callback selection comes from the admitted capsule graph, not from a child R6
method override. Translation, collision checks, child-result admission,
merging, and scalar constraint validation therefore have one implementation
for live and detached uses. The merge first copies retained/untransformed
inputs in input order. It drops every callback-owned input, then appends the
changed outputs in callback-plan order and in each callback's result order;
omitted owned names therefore disappear. A changed name colliding with a
retained input is rejected.

### SHADOW

A SHADOW owns exactly one origin object reference plus a construction-time
snapshot of its fixed visible schema in capsule `.params`. `shadowed` is
constructor input rather than retained state; current origin IDs outside the
fixed schema are the hidden complement. Origin values,
dependencies, constraints, individual transformations, and extra
transformation are synchronized at operation entry. Visible value assignment
writes through while preserving every hidden value. A dependency crossing the
fixed boundary is checked both at construction and at every live dependency
snapshot.

Direct origins are BASE or COLLECTION nodes. A SHADOW does not wrap another
SHADOW directly; the caller expresses the combined hidden-ID partition against
the ultimate origin, avoiding layered refresh authorities.

The origin edge in `.sets[[1L]]` is the sole origin authority; there is no
parallel private `.origin` field to synchronize. Public `$origin`, live-value
adapters, constraint plans, and graph cloning all derive from that edge. A
BASE-origin constraint closure does not retain the origin: refresh reduces the
admitted edge to the exact callback/hidden-values plan described below. Deep
clone derives replacement adapters from the memoized cloned origin rather than
retaining the old Shadow's private environment.

For a BASE origin, the constraint closure retains one exact two-field plan:
the admitted callback and hidden values. Its native evaluator snapshots both,
builds an ordinary hidden-first/visible-second list manually, preserves opaque
leaf identity, calls the callback once, and admits only one non-missing logical
answer. It never uses `c()` and therefore cannot dispatch on an S3-classed
visible-list container. A COLLECTION-origin adapter continues through the
shared collection evaluator family, with the same native Shadow merge boundary
where hidden values must be restored.

The R6 shell likewise stores no parallel visible-ID or hidden-ID fields.
Construction admits and materializes the visible schema once in C; later
operations derive that fixed schema from capsule `.params` and the hidden
complement from the authoritative origin edge. An origin that no longer
supplies a compatible visible ID is corrupt/unsupported and errors; a newly
added collection ID remains outside the fixed view.

Central native admission refreshes a SHADOW once when traversed through any
operation, including inside a COLLECTION, and validates the complete path
before reentry. R wrappers must not perform a duplicate synchronization.
Shadow-to-origin is a real graph edge for cycle detection, even where a
flattened outward result treats the shadow as a semantic leaf.

## Closed kinds and callbacks

Domain operations switch over exactly ParamDbl, ParamInt, ParamFct, ParamLgl,
and ParamUty. Dependency operations switch over exactly CondEqual and
CondAnyOf. Exported compatibility functions are closed ordinary functions, not
`UseMethod()` extension points. Adding a future package-owned kind requires one
reviewed central kind entry and implementation in every affected operation.

The standalone `condition_test()` wrapper enters the registered built-in
Condition operation directly. It shares exact closed admission and comparison
semantics with dependency evaluation while using an optimized native vector
kernel for direct input. A built-in RHS is an attribute-free logical, integer,
double, or character vector without missing values; `CondEqual` requires one
element and `CondAnyOf` a non-empty unique vector. Public admission roots the
selected RHS and copies each element once, so compact or stateful ALTREP never
enters the strict capsule representation. The compared `x` may be `NULL` or a
plain vector of the same four kinds carrying at most names; stable ALTREP `x`
is likewise materialized once. Classed and otherwise attributed operands
reject rather than dispatching through `Ops` or `%in%`. Condition shells and
structural class/name metadata are ordinary non-ALTREP/non-S4. Atomic RHS and
direct operands may be admitted stable ALTREP, but explicitly reject S4.
`condition_as_string()` is deliberately cold R formatting over an already
closed built-in shape; it is not a competing semantic evaluator.

`p_dbl()` and `p_int()` pass their raw bounds, numeric source kind, and
`logscale` flag to the row constructor once. That registered operation owns
type/range admission, materialization, logscale-bound normalization, the fixed
mapping callback, and canonical row construction. There is no preliminary
native bounds probe and no independently callable partial numeric admission.
The exact package-owned zero-row `empty_domain` shape is likewise admitted by
the native Domain kernels; R wrappers do not short-circuit empty checks,
properties, quantiles, or sanitization.

`paradox_admit_builtin_domain_row()` is the sole canonical semantic owner for a
built-in Domain row. The constructor's final-state assertion, ParamSet
construction, and ObjectTuneToken Domain admission all call it. Boundary code
may validate an outward table/class envelope and extract its one row, but cargo,
kind/storage, grouping, bounds/tolerance, levels, special values, default, tags,
transformation, requirements, and initialization are not restated there. The
owner returns the admitted kind/field view used by operation-specific target
compatibility or construction.

The shared owner rejects ALTREP or S4 structural Domain objects/metadata. For
Dbl, Int, Fct, Lgl, and Uty the outer special-values list, names, and metadata
must be ordinary non-ALTREP/non-S4. For Dbl, Int, Fct, and Lgl it rejects an
ALTREP special-value leaf before observing it. An admitted typed S4 special is
opaque and matches only by pointer identity;
an S4 default/init passes only when it is that same special leaf. ParamUty
leaves remain opaque, including S4 objects, but special membership preserves
Paradox-1 base `identical()` semantics as the sole narrow observation and does
not dispatch. None of these leaf rules relaxes surrounding Domain structure.

Static ParamSet properties (`nlevels`, `is_number`, `is_categ`, and
`is_bounded`) use the same closed native kind switch and return their named
vector directly. There is no parallel “known kind” mask, NULL decline sentinel,
or grouped S3 property replay. Factor levels are ordinary materialized capsule
storage before this operation; encountering a semantic ALTREP or unknown kind
inside a current capsule is corruption, not an alternate dispatch request.

Quantile and grid operations admit the same canonical plain parameter table
through the shared validator; they do not recognize an internal data.table
shape or depend on R-4.6-only attribute APIs. Integer range warnings and typed
zero-row and zero-dimensional grid results are produced by the native engine
itself, never by an R retry.
A zero-level `ParamFct` is canonical. Zero-row quantile, grid, and uniform
sampling results retain a `character(0)` column; a positive-row quantile or
uniform-sampling request errors before level indexing or RNG entry.

Supported callbacks remain first-class state: ParamUty custom checks,
individual transformations, extra transformations, constraints, aggregation,
and internal tuning. Native code snapshots callback objects before execution,
evaluates them in documented order, protects all arguments/results, and
propagates each warning or error once. It does not infer compatibility from a
callback's closure body.

The unified transformation engine requires ordinary non-ALTREP/non-S4 result
and non-table input list shells and snapshots each callback result once. A
documented data-frame input may use the suffix-classified top-level ALTREP table boundary,
which is materialized once before the same validation; admitted atomic leaves
or columns may be stable ALTREP. A BASE extra transformation accepts
either an unnamed
list or a completely and uniquely named list, retaining that outward shape. A
COLLECTION child result requires complete unique names before the same engine
translates and merges it into the parent namespace. The distinction is a
node-kind validation rule in one C path, not a wrapper that repairs or
re-executes callback output.

Fixed R closure factories exist only to return callable public callbacks. Their
environments contain the minimum exact native evaluation plan and do not
traverse child shells, select callbacks, or reproduce collection semantics.
The wrapper installed by `to_tune(ParamSet)` similarly calls its documented
user transformation once, checks the single list result, and assigns its public
name directly instead of paying checkmate/mlr3misc dispatch on every call.

`ParamSet$set_values()` captures `...` in R, validates the scalar `.insert`
language argument without checkmate, and hands both value sources to one native
merge. The C entry point owns name uniqueness/disjointness and merge ordering;
there is no preliminary R scan followed by the same native scan, and malformed
direct calls raise instead of returning a replay sentinel.

Direct checked and unchecked `$values <-` both reject an outer ALTREP shell
before observing its length, names, or elements. The Paradox-1 clear-values
spellings—`NULL`, an ordinary attribute-free zero-length atomic/expression
vector, or an accepted empty base/S3-representation list—are canonicalized to
a named native `list()` by the store operation. `set_values(.values=)` is the
sole general-list ALTREP exception and
owns its one native shell snapshot; it does not weaken direct assignment.

ParamSet-bearing `ObjectTuneToken`s follow the same atomic boundary. Native
checking builds and validates one rooted exact BASE candidate, requires at least
one bounded dimension, and never runs candidate callbacks.
Before that graph work, the engine admits one exact closed token snapshot. The
token is exactly `{content, call}` with only names/class attributes and one of
the five package class vectors for Full, Range, Object, Internal-Full, or
Internal-Range. Full/Range content has its exact built-in fields; scalar names
are normalized away; Object content is an admitted bounded, value-producing
built-in Domain or an exact `c("ParamSet", "R6")` shell with ordinary
self/private linkage to a canonical BASE core. Because a ParamUty Domain is
unbounded, it is not valid in the Domain form. A zero-level ParamFct remains
canonical for typed empty operations but cannot be Object-token Domain content
because it produces no tuning value. Other bounded typed Domains may still
carry admitted opaque leaves,
and the BASE-ParamSet form may construct an opaque target value. COLLECTION,
SHADOW, and additive subclasses reject. A subclass,
extra/reordered field/class/attribute, S4 structure, malformed
call/content, or recursive metadata rejects at the fixed root without arbitrary
traversal. Exact creator provenance is intentionally not inferred from mutable
R6 method bodies or registries: a shell alias retaining the exact genuine BASE
private/core linkage may pass, safely, because native code never calls its
methods.

Checking records a rooted `{shell, private, core}` receipt for each admitted
live candidate. Receipt reauthentication follows callback-capable validation;
checked assignment retains receipts through replacement construction and ends
with a non-forcing, allocation-free identity scan immediately before commit.
Candidate mutation therefore wins and the outer write errors without a partial
commit.

Explicit `$search_space(values=)` enters the same native snapshot boundary. Its
outer input is an ordinary named list or a names/class-only S3 named list; C
discards the class and selects tokens without S3 subsetting. S4/list-like or
otherwise attributed containers reject. Each live BASE candidate is replaced
in the admitted token copy by a sealed, single-use BASE subset capability before
R or a candidate callback runs. R constructs the detached candidate from that
capability and never rereads or invokes the original shell.

The deterministic `$search_space()` conversion exclusively owns transformation
execution plus one-dimensional/output compatibility. It is one cold R
orchestration over the stable native snapshot and uses closed built-in
switching, not `UseMethod()` or third-party token/Domain methods. This separates
structural admission from callback-dependent plausibility without a second
engine or a reentrant R preflight before value commit.

Malformed exact-token or Domain structure raises at the public boundary,
including from `$check()`. Only an ordinary structurally admitted value that is
infeasible for its target follows the returned character-diagnostic protocol.

`ParamSet$check_dependencies()` is a separate strict dependency-only boundary,
not a wrapper around an R data.table traversal. It requires an ordinary,
uniquely named base list, builds the same graph snapshot and point mapping as
`$check()`, and invokes the same dependency kernel. Unknown IDs are diagnosed
even when the graph has zero dependency rows; TuneToken endpoints skip that
edge. The operation stops at and returns the first deterministic diagnostic
rather than constructing and collapsing an R list of every dependency error.

`ParamSet$test_constraint()` and `$test_constraint_dt()` are registered native
boundaries over the same graph plan, point initializer, and constraint kernel.
The table operation enters the shared public-table classifier/materializer,
then requires the data.table terminal suffix; any valid leading additive
classes remain representation-only and are ignored during semantic work.
Names, class, and cache carriers are strict ordinary structure; row names use
the shared count-only rule. Its
admitted semantic atomic columns may be stable ALTREP. When value assertion is enabled,
it admits every row before executing any constraint callback; a
ParamUty custom check may run as part of that preceding Domain-value admission.
Only then does it evaluate the snapshotted constraints once per row in order.
This preserves all-or-no-constraint-callback validation and operation-entry
constraint selection without an R row loop or a second constraint engine.

The tag, dependency, and BASE callback mutators are collected in
`src/paramset_mutate.c`. Tag get/set and dependency snapshot/get/set/add own
their canonical detached/replacement objects. Dependency snapshot and bulk
replacement share one callback-free structural admission routine. It validates
the table, child IDs, self-edges, and exact closed Conditions, but preserves RHS
predicates even when a narrowed parent Domain makes some or all of them
infeasible. Dependency append is the separate authoring operation: it invokes
the shared check kernel for RHS feasibility and then verifies that callback
reentry did not replace the target generation. SHADOW dependency append first
proves both IDs remain visible and routes to the origin through that strict
native entry. Constraint and extra-transformation setters admit their callback
shape and atomically replace the selected BASE field. R wrappers do not plan
these mutations.

Dependency presence is deliberately not implemented as `nrow(self$deps)`. The
registered scalar reader in `src/paramset_collection_deps.c` validates the
selected canonical BASE dependency table, performs authoritative SHADOW
refresh before validating its table, or admits the complete COLLECTION graph
and reads the root `subtree_dependencies` count. It emits no detached columns
or data.table facade. The COLLECTION branch is the existing graph admission,
not a cached or reduced-integrity reader mode.

## Operation transaction

Every operation follows the same lifecycle:

1. the R wrapper captures language-level inputs and outward representation
   metadata that C cannot capture directly; this is not semantic admission;
2. public arguments are forced left-to-right; interpreted outer shells reject
   ALTREP/S4 before semantic observation except at the documented public-table and
   `set_values(.values=)` one-snapshot boundaries, while supported ALTREP
   atomic semantic vectors are materialized once at native admission; that
   native materialized state, not captured representation text, is semantic
   authority;
3. the complete required capsule graph and callback set is structurally
   validated and rooted;
4. a bounded native plan is built from that snapshot;
5. the native kernel executes, calling only documented user callbacks;
6. a mutating operation checks every generation on which its write depends and
   swaps replacement capsules in an allocation/callback-free commit section.

A nested callback mutation is visible to later operations. A read-only outer
operation may finish from its snapshot. A mutating outer operation whose
dependency generation changed errors rather than overwriting or retrying the
nested mutation.

Checked value assignment plans the complete BASE/COLLECTION/SHADOW graph
through the ultimate BASE targets before invoking ParamUty or constraint
callbacks. Shared targets are deduplicated with deterministic last-owner
semantics. The plan captures every target generation, validates the complete
assignment once, and prebuilds every replacement capsule. It then rechecks all
target generations and swaps every replacement `.core` in one allocation- and
callback-free commit wave. A nested public assignment to any planned target
therefore wins: the outer setter raises before changing any target. Validation,
callback, or allocation failure likewise leaves the complete graph unchanged;
there is no second child-store pass or root-only generation guard.

ParamSet-bearing ObjectTuneTokens add a generation dependency outside the
write-target graph. Candidate receipts are rooted before callbacks and retained
through sanitized-result and replacement allocations. After normal target-core
generation checks, the commit path performs one final allocation-free scan of
every candidate's ordinary self/private/core linkage and pointer identity, then
immediately swaps the replacement target cores. This closes callback and
finalizer validate-then-mutate races without replaying admission or invoking a
candidate method.

ALTREP methods may allocate and reenter R. Hence no kernel sizes from one
ALTREP observation and fills from another, and no raw pointer survives an
accessor. Stable ALTREP implementations, including base compact sequences such
as `1:n`, are supported in admitted semantic-vector positions. This does not
turn interpreted structure into a materialization surface: configuration/
search-space and transformation list shells, ParamSet `params` lists, Domain/
Condition/TuneToken/capsule shells, Domain cargo/interpreted cargo entries,
internal table/row shells, dimnames, class/name vectors, and list metadata must
be ordinary non-ALTREP/non-S4 objects. The six documented public-table
ingresses use the single classifier and ownership sequence above: a
suffix-classified top-shell ALTREP is copied once, additive presentation
classes are canonicalized in that snapshot without dispatch, while ordinary
shell prefixes are ignored without copying; names/classes are owned before callback-capable
observation, data.table cache carriers are checked then ignored, and row names
use the narrow count-only integer/character rule. Admitted atomic columns may
be stable ALTREP. Direct
checked/unchecked `$values <-` rejects an outer ALTREP before observation and
canonicalizes the Paradox-1 empty spellings (`NULL`, an ordinary attribute-free
zero-length atomic/expression vector, or an accepted empty list container) to a
named native list. The public
`set_values(.values=)` merge is the one general-list exception and owns one
operation-specific shell snapshot before validation. A hostile custom ALTREP may change after R-side
language/representation capture but before native admission; exact semantics
or matching printed representation are not promised for that boundary. It is
rejected or admitted from one native materialization without an R retry or
Paradox itself causing a crash or memory corruption. An ALTREP implementation
that violates the R C API or crashes inside its own accessor cannot be
sandboxed by Paradox and is outside this guarantee.

## Source organization

- `src/core_state.[ch]`: capsule creation, validation, field replacement,
  ownership, SHADOW refresh, and graph-path safety;
- `src/domain_construct.c`, `src/domain_admission.h`, `src/domain_kernels.c`,
  `src/paramset_domain_common.[ch]`: closed Domain construction, the sole shared
  built-in row-admission owner, and canonical capsule table/kind validation;
- `src/builtin_condition.[ch]`: closed Condition admission/evaluation;
- `src/paramset_construct.c`, `src/paramset_collection_construct.c`: BASE and
  COLLECTION construction and atomic collection add;
- `src/paramset_mutate.c`: tag/dependency projection and atomic mutation plus
  BASE callback replacement;
- operation-specific `src/paramset_*.c`, design, and sampler units: thin graph
  planners and kernels over capsule state;
- `src/r_utils.c` and `src/r_api_compat.c`: small R-API ownership and version
  adapters, never alternate semantics. Raw stored-attribute selection uses
  `R_mapAttrib()` on R >= 4.6 and the established `ATTRIB` traversal on R
  4.3--4.5; older supported R releases also use the documented public `FORMALS`
  backport for newer closure inspection. These adapters never evaluate
  `formals()`, `attributes()`, or another R/data.table helper. The sole
  non-public compatibility exception is also
  centralized here: for R < 4.6, one declared/exported
  `Rf_findVarInFrame` call supplies the non-forcing ordinary-binding lookup and
  rejects `PROMSXP`; R >= 4.6 uses the documented experimental API
  `R_GetBindingType`. The former is required because R 4.3--4.5 has no public
  non-forcing classifier and an
  R-level `substitute()` workaround would make simultaneous generation/receipt
  scans unsound.
  It is ledgered in
  `environment/r-api-exceptions.tsv`, raw-token-audited to one occurrence/path,
  and pinned-header/runtime tested. It is not CRAN-allowlisted for the supported
  pre-4.6 build path: those DSOs require the symbol, while the current-R DSO audit
  requires its absence. No other internal R API is permitted;
- `src/init.c`: fixed-arity registration with dynamic lookup disabled;
- `R/ParamSet*.R`, `R/Domain*.R`, `R/Condition.R`: public shells, language
  capture, fixed native-entry callback factories, and documented cold graph
  orchestration only;
- `R/to_tune.R`: package-owned token construction and the one cold exact-token
  search-space conversion over an already admitted native snapshot;
- `R/all_equal.R`: cold detached-view equality glue, with no capsule/private
  environment interpretation;
- `R/upgrade_paradox_object.R`: explicit nonexecuting conversion of accepted
  legacy graphs.

The first narrow cold R semantic-orchestration family is internal tuning for
`$aggr_internal_tuned_values()`, `$disable_internal_tuning()`, and
`$convert_internal_search_space()`. Each captures its required capsule cargo,
translation, Domain, and owner-value state before the first callback and is the
sole implementation of its documented cargo behavior. Commits use native
value/capsule mutation. After native subset/flatten has returned canonical
detached state, collection flattening may walk the validated translation
snapshot and rebind documented `cargo` callbacks to flattened IDs, then replace
that one column in the detached BASE capsule through the package replacement
primitive. This family neither repeats structural admission nor competes with
native graph, value, checking, or callback-selection semantics.

The second is exact-TuneToken `$search_space()` conversion. Its R code receives
only the one rooted native token/target-Domain snapshot, in which every live
ParamSet content has already been replaced by a sealed, single-use BASE subset
capability. It switches over the five built-in token class vectors and owns
callback-dependent plausibility, one-dimensional compatibility, dependency
reconstruction, and outward ParamSet assembly. It does not admit structure
independently, call a third-party S3/candidate method, reread a live candidate,
or select a competing native/R path. Its deterministic sampling restores the
caller's RNG kind/state on success or failure.

Constructor-owned callbacks capture only the bindings they need. In
particular, categorical value mapping and integer log-scale mapping use
package-owned closure factories whose call frames contain exactly their level
or bound state. They do not invoke `compiler::cmpfun()`/`mlr3misc::crate()` for
every Domain instance: per-instance compilation was measured constructor
overhead, not an isolation or compatibility requirement. The returned closures
remain ordinary serializable R functions and documented user callbacks retain
identity.

Four final measured hot-path changes remove redundant work while retaining the
same validation boundary. A ParamSet constructor with no initial values skips an
empty value-store transaction. Complete collection-value admission retains the
already resolved BASE parameter row and translates that validated offset upward
instead of looking up the ID again. The inherited native Shadow dependency
reader owns refresh, so the R binding does not request a second refresh.
Finally, `$has_deps` reads validated native dependency counts instead of
materializing `$deps`; the retained 64-parameter BASE/COLLECTION/SHADOW probe
moved from 498.575 to 156.065 microseconds (3.195x). Its one-evaluation
`Rprofmem` result was exactly 12,688 bytes in 14 records on both sides, so this
is a measured latency win rather than a claimed allocation reduction. The
exact 50-iteration, three-warmup methodology and package/source fingerprints
are retained in `benchmarks/README.md` and
`.local/benchmarks/has-deps-scalar-ab-final-20260719`. These are
operation-local shortcuts, not persistent validation caches.

A proposed sparse-target search-space projection was also measured and rejected.
Search-space construction is cold, and the maintained end-to-end workload moved
only about 2%; a second target-facade path would cost more design and validation
surface than it saves. A native bulk-dependency constructor transaction is also
rejected for the 2.0.0 release. The isolated 64-parameter/27-requirement estimate
moved from 6.57 ms to 4.11 ms, with requirement-heavy estimates spanning about
1.4--2.5x, but representative xgboost learner construction improved only about
6--7%. That gain does not justify a moderate-risk new native batch transaction
while the maintained release workload lacks dependency-rich constructor
coverage. This is an internal optimization boundary, not a compatibility
decision, so it may be reconsidered later without another API or major-version
break.

SHADOW related-state validation uses an operation-local open-addressed index
over admitted CHARSXP IDs. Pointer identity is the common fast path and an
encoding-aware comparison preserves correct matching when identities differ.
The index is derived scratch state, retains no unrooted pointer across an
allocating boundary, and is never stored in a capsule. This replaces repeated
quadratic ID scans without weakening corrupt-state validation.

The shared string comparator and COLLECTION affixed-ID validator use the same
portable nonallocating boundary: pointer identity first; stored-byte equality
for equal UTF-8, Latin-1, or bytes encodings; and stored-byte equality for
native encoding only when both strings are ASCII. Mixed encodings and
non-ASCII native strings retain the translating UTF-8 path. This removes a
measured translation hot spot while keeping R's encoding semantics. It is not
a graph cache or a weaker reader mode: every collection read still validates
the complete admitted graph, translation table, permanent rows, and dynamic
state.

Uniform random sampling is one closed `src/sampler_unif.c` operation over the
selected BASE/COLLECTION/SHADOW capsule graph. It allocates typed columns,
draws in public parameter/column-major order, and constructs the outward
data.table without executing the per-dimension R6 objects. `SamplerUnif` keeps
its inherited `$samplers` list only as descriptive compatibility metadata;
replacement/reordering is rejected and child mutation has no semantic effect.
Users wanting executable custom child samplers use `SamplerHierarchical`.
`generate_design_random()` and `SamplerUnif` both pass the native table through
the one `Design$new()` boundary for fixed values and dependency masking.

Translation units may be split for readability, but a split must not create a
second semantic engine. Legacy files named around “surface auth”, “builtin
fallback”, or constructor fast/slow pairs are deleted once their operation is
on capsule authority.

## Error and ownership policy

Common public failures retain useful stable Paradox message fragments; exact
checkmate wording, internal implementation frames, and behavior after private
corruption are not contracts. Corrupt current state receives a deterministic
`Corrupt ... capsule/state/graph` error before unsafe access. Direct native
calls with malformed objects are adversarial test inputs and must never crash.
Malformed exact-token or Domain structure is likewise a hard public-boundary
error; the returned check diagnostic is reserved for ordinary structurally
admitted value infeasibility.

Every allocating or callback-capable boundary has explicit protection. Long
loops poll interrupts without holding unrooted objects or raw pointers. Output
sizes, byte counts, recursion replacements, row/column products, and C casts
are checked before allocation or indexing. Portable scalar C17 is the baseline;
architecture-specific code is not required for performance.

## Forbidden architecture regressions

Do not add any of the following:

- complete R/checkmate/data.table/S3 semantic fallbacks;
- `NULL` or other private decline sentinels that restart an operation;
- generated R6 method/body/formal/environment authentication;
- permanent internal data.tables, private data.table execution APIs,
  synthesized indices, or version-specific spare-capacity bridges; the sole
  permitted unexported R lookup is the non-calling presentation identity check
  ledgered above;
- third-party Domain or Condition dispatch as an implicit extension ABI;
- third-party TuneToken subclasses/methods, arbitrary TuneToken metadata, or an
  R token-shape validator parallel to native exact admission;
- generated-surface creator-provenance authentication for an exact BASE
  ObjectTuneToken shell; safe genuine-private/core aliases must remain harmless
  because no alias method is invoked;
- any internal R API beyond the single R < 4.6 `Rf_findVarInFrame` compatibility
  call ledgered in `environment/r-api-exceptions.tsv`;
- permanent semantic ALTREP vectors or repeated observation inside one native
  semantic admission/kernel (prior R-side representation capture is explicitly
  non-semantic and covered by the hostile-custom-ALTREP boundary above);
- materialization or observation of an interpreted ALTREP/S4 shell, except for
  the exact one-snapshot `set_values(.values=)` boundary and the suffix-classified,
  allowed-attribute public-table boundary;
- a namespace-level R implementation of Design transpose or dormant table
  helpers parallel to the registered native engine;
- direct downstream access to `.core` or its protected payload;
- unmanaged state reachable only through an external-pointer address;
- package-byte evidence carried from the superseded candidate.

## Implementation convergence rule

Before the release candidate is frozen, search the complete R/C source for
remaining compatibility-first seams, delete unreachable machinery rather than
silencing it, and add a contract test for every consumer-discovered gap. The
active completion state is maintained only in
[`release-2.0.0.md`](release-2.0.0.md); this architecture describes the final
target and must not be weakened to match a transitional implementation.
