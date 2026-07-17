# Native architecture

## Decision

Paradox 2 keeps its R6 classes as the compatibility shell and replaces the
computation beneath them with registered C routines. Ordinary R objects remain
the canonical, serializable state. In particular, `ParamSet` continues to expose
the established private fields `.params`, `.values`, `.tags`, `.deps`, and
`.trafos` through its R6 enclosure.

This is deliberate. `bbotk` subclasses `ParamSet`; `miesmuschel` both subclasses
it and reaches into these fields; `ParamSetCollection` itself depends on the
same representation. Making an external pointer the sole source of truth would
break subclass initialization, reference semantics, deep cloning, ordinary R
serialization, debugging, and important consumers.

The R6 shell is not the execution engine. Public methods should perform only
argument capture that genuinely requires R semantics, then call one native
entry point for the complete logical operation. C routines operate directly on
the vectors held by canonical state and construct their results without calls
to checkmate or data.table.

`checkmate`, `data.table`, and `R6` remain package imports because they define
the established public objects, diagnostics, extension fallbacks, and legacy
facade behavior. This is a compatibility decision, not a native dependency:
shipping C includes no private header from those packages and calls none of
their C APIs. The one deliberate R-level exception is the authenticated
`data.table::alloc.col()` bridge required by data.table releases before 1.18;
the data-table facade section below records that boundary. Removing these R
imports is not a 2.0 goal: doing so would discard supported fallback and object
behavior for little benefit on the admitted native paths, which do not enter
the imported implementations.

Native admission is deliberately narrower than the public API. Exact
package-generated built-in objects enter C; subclasses, replaced methods,
third-party Domains, callback-capable state, and shapes that cannot be proved
safe retain the established R implementation. A declined native call returns
its private sentinel before mutation, callback execution, or other observable
commit. Once an operation crosses an observable callback or commit boundary it
finishes on that path or raises one deterministic error; it never retries the R
implementation and replays user code. This fail-closed split is the central
compatibility tradeoff behind the individual boundaries below.

## State model

The transitional private tables retain their current column names and types:

- `.params`: one row per parameter, in public parameter order;
- `.values`: a named list in parameter order, omitting unset values;
- `.tags`: long-form `id` / `tag` columns;
- `.deps`: `id` / `on` / `cond` columns;
- `.trafos`: `id` / `trafo` columns.

The following metadata was considered for persistent caches alongside them:

- an aligned integer type-code vector;
- cached aligned property vectors;
- an ID-to-position hash environment;
- per-parameter tag, transformation, and dependency adjacency lists;
- dependency topology and schema/dependency version counters.

The release implementation deliberately does not make such a cache another
source of truth. Important downstream code mutates the established private
tables directly, so reliable invalidation would either break that implicit API
or require enough revalidation to erase the gain. Native operations instead
validate canonical state once per complete operation and keep transient aligned
indices only for that call. A future immutable cache is acceptable only if it
can be discarded without changing behavior and is authenticated against the
ordinary-R state before use.

Every base `ParamSet` and `ParamSetCollection` initializer installs a fresh
empty `.deps` table before exposing the object. R6 class defaults are templates,
not instance-owned storage, and data.table may attach indexes by reference.
Downstream subclasses that replace `initialize()` without calling its parent
therefore remain responsible for initializing any inherited mutable private
state they expose.

## Versioned public C API boundary

All version-specific R access is centralized in `r_api_compat.c`. R 4.5 and
newer provide documented closure and parent-environment accessors; older
supported releases use the equivalent `body()`, `formals()`, `environment()`,
and `parent.env()` primitives from the locked base environment. Attribute
fallbacks copy raw attributes to a neutral protected carrier and inspect them
through base `attributes()`, so tagged pairlists and language objects are
neither evaluated nor mistaken for objects with raw attributes. Namespace
fallbacks likewise resolve only an already loaded namespace through locked
base bindings.

Exact non-forcing binding classification is different: the public API needed
to distinguish direct values from delayed or active bindings was added only in
R 4.6. On R 4.3 through 4.5, every native gate that authenticates private R6
state therefore fails closed and immediately executes its established R
implementation. Pure vector kernels and constructors that do not require this
inspection remain native. This preserves the R 4.3 support contract without
using object-layout internals or forcing a promise merely to decide whether a
fast path is safe; R 4.6 receives the complete optimized path.

## Built-in type engine

The native core uses a small internal type enumeration for `ParamDbl`,
`ParamInt`, `ParamFct`, `ParamLgl`, and `ParamUty`. Each supported operation has
a type-specific implementation: scalar validation, batch validation,
sanitization, quantile mapping, and static properties.

Column-oriented `check_dt()` first validates all supported built-in columns
without forcing its optional promises, retaining the established empty-table
return. That pass also records whether every cell is nonmissing and every
parameter is represented. For nonempty tables it forces `presence` and
`check_strict` in their historical order. A literal `presence = "none"` can
then accept the first pass directly; literal `presence = "all"` additionally
requires both recorded completeness bits. The canonical scalar checker and a
second complete-table scan are not replayed when strict checking is disabled
or there are no dependencies or constraints.

After authenticating the generated R6 caller, the gate inspects the forwarded
optional promises without forcing them again and admits only defaults or
explicit literals. A computed argument retains the row-major R implementation,
where forcing it may change the table, bounds, methods, or other state observed
by later rows. Required-mode presence, tokens, extensions, special values,
dependencies, constraints, diagnostic failures, or a changed method surface
likewise retain that implementation and its diagnostics.

An exact `ParamSetCollection` has a separate scalar-check entry because its
inherited R method otherwise pays the vectorized grouping cost before it can
reach the same built-in kernels. For strict checks, C iteratively authenticates
the complete current-path graph: every node is an exact base `ParamSet` or
`ParamSetCollection`, generated public and private wrappers are canonical,
dependency tables are empty, leaf constraints are `NULL`, and cycles are
rejected. Shared children in sibling branches remain valid. Non-strict checks
skip this feature graph because the public contract already ignores
dependencies and constraints in that mode. The flattened parameter columns
and plain scalar input are copied into narrow ordinary snapshots before the
kernel runs. After it allocates, an allocation-free audit proves that the live
parameter state, input bytes, R6 methods, collection edges, constraints, and
dependencies still match admission. Unsupported values—including arbitrary
`ParamUty` payloads—decline on their type before any vector length or element
access and execute the unchanged R path. This entry is success-only: invalid
values also return the sentinel so checkmate retains its exact diagnostics.

Row-oriented design conversion is also native for ordinary unclassed atomic
and list columns. The kernel returns a sentinel instead of interpreting
classed atomic vectors or dispatch-sensitive scalar list elements; the R6
method then executes the historical generic transpose and S3 filtering path.
ALTREP containers, names, and columns take that same fallback before any
length or element accessor is called, because such an accessor is an arbitrary
reentrant callback and cannot safely be mixed with a later sentinel return.

After conversion, an exact base `ParamSet` with no `extra_trafo` may apply its
individual transformations as one row-oriented batch. The R gate accepts only
the exact `c("ParamSet", "R6")` class and a keyed, canonical `id` / `trafo`
data.table with unique, nonmissing, nonempty character IDs and functions in
every transformation slot. Before reading private state, a callback-free C
gate authenticates the generated public `trafo`, `extra_trafo`, and `has_trafo`
wrappers, their exact formals and forwarding bodies, owned enclosure, namespace
parent, and active-method registry. Replaced, reparented, or delayed bindings
therefore decline without being forced. It snapshots the table once, validates and matches
all row names before invoking user code, and then evaluates `trafo(value)` in
the same three-local `id` / `trafo` / `value` callback frame used by
`ParamSet$trafo()`, in historical row-major and parameter-major order. List-element assignment
preserves `NULL` and multivalue results without changing their nesting. If a
callback mutates the private transformation table or installs an `extra_trafo`,
the current row retains its pre-callback individual snapshot, the newly
installed extra transformation is applied to that row, and only the untouched
tail returns to the public per-row method. The generated surface is
re-authenticated after each row as well, so a callback that replaces the public
`trafo` wrapper takes effect on the next row just as it does in the historical
loop. Callback side effects are therefore never repeated merely to leave the
batch path.
Subclasses, an `extra_trafo`, malformed private state, or malformed rows retain
the complete per-row `ParamSet$trafo()` path. The batch owns its row-list
results and does not alter the stored design table.

Exact package-generated log-scale transformations have a narrower
callback-free lane. After authenticating the same base `ParamSet` surface, C
requires either the canonical `exp` transformation used by `p_dbl()` or the
exact crate closure generated by `p_int()`, including its body, environment,
and bounds. Admission is all-or-nothing. Output shells are allocated before a
final allocation-free audit, then every scalar is transformed without R
dispatch or callback frames. Custom closures and any changed or unsupported
surface retain the callback-preserving batch above.

Unknown Domain subclasses take a slow R/S3 fallback. Third-party extension is
not a primary design constraint, but retaining this fallback costs little and
prevents unnecessary breakage. A future package-owned built-in type does not
require redesigning `ParamSet`, but it is a deliberate cross-cutting change:
maintainers must extend classification and construction, the relevant domain
and ParamSet kernels, properties/checks/quantiles/subsetting, design generators
and samplers where applicable, R fallbacks, differential cases, and native and
R regression tests. There is intentionally no third-party native plug-in ABI.

The scalar Domain checker also treats its table columns as an authenticated
boundary. IDs, classes, grouping, numeric bounds, tolerances, and outer and
nested factor levels must be ordinary attribute-free vectors. Every
`special_vals` row must be an ordinary empty list; a nonempty row declines the
whole grouped check before observing its arbitrary contents. This matters even
when the submitted scalar itself is a valid built-in value: the historical R
prelude can dispatch through classed metadata or inspect every special-value
row before reaching the type method. Unsupported nested factor storage returns
the fallback sentinel rather than inventing a native corruption error, so R
retains its established coercion behavior.

## R object construction

The common `ps()` shorthand captures its complete `...` call before evaluating
an argument. A registered planner accepts only named, syntactic ASCII IDs and
exact unqualified built-in constructor calls whose supplied arguments belong
to a small numeric and character literal grammar. It authenticates the live
constructor bindings and every helper referenced by a literal call before
building anonymous Domain-shaped rows directly. Those rows deliberately omit
print-only representation state that `ParamSet$new()` immediately discards;
the ordinary R6 constructor remains the ownership and extension boundary.
Duplicate IDs, namespace qualification, overrides, active or delayed
bindings, rich constructor features, invalid literals, and arbitrary
expressions return the `NULL` sentinel before user evaluation, after which
`list(...)` runs once in its original frame and order.

The planner, literal decoders, constant preparation, and per-row construction
are separate bounded helpers. This decomposition preserves the same one-shot
admission and fallback transaction, but prevents a static analyzer from
cross-multiplying every constructor kind, optional literal, and allocation
path inside one monolithic function. Before the split, the package constructor
exhausted bcheck's per-function state budget; the bounded 800,000-state
prefreeze review after the split completed 854 functions and 41,293 states
without a package-local state-exhaustion error. Those counts are historical
analyzer evidence, not a runtime-performance claim or a substitute for the
frozen-candidate release gate.

Native table-shaped results are ordinary `VECSXP` objects whose columns are
allocated with the correct storage types and whose names, compact row names,
and class are set to `c("data.table", "data.frame")`. Shipped C does not link
to a data.table C symbol or include a private header. It normally installs
data.table's public object-level
`.internal.selfref` representation through the public R API: the outer pointer
tags the exact names vector and protects an owner pointer for the table. This
is necessary because consumers routinely pass returned tables straight to
`data.table::set()` or `:=`; without a valid self-reference, those operations
warn, copy unexpectedly, or try to assign a modified value back through an R6
active binding.

data.table before 1.18 assumes that every table carrying a valid self-reference
also has at least `ncol()` allocated column-pointer slots. A native `VECSXP`
has zero `TRUELENGTH`, so those releases otherwise try to copy nonempty tables
into a zero-slot shallow shell. Only for such older releases, the common table
finalizer evaluates the exported `data.table::alloc.col()` closure and adopts
its returned shell. Private construction state passes `0L`, allocating exactly
`ncol()` pointer slots and avoiding the usual 1024 spare slots. Publicly
returned facades use data.table's configured spare capacity so direct `set()`
calls can add columns by reference. Setting `datatable.alloccol` to zero
deliberately disables that spare capacity, just as it does for data.table's own
constructor. Both forms keep every canonical unnamed column shared. Named
column vectors receive an owned shallow copy before their names are removed,
because the legacy `alloc.col()` wrapper would otherwise remove those names
through a shared reference. The bridge validates the returned columns,
owner/self-reference, and capacity and fails
closed on a changed contract. data.table 1.18 and newer retain the
allocation-free public-R API path and its native construction performance.

ParamSet construction also avoids asking data.table to sort state that C has
already validated and ordered. During `.onLoad`, official `setindexv()` calls
produce nonidentity, duplicate, identity, and empty secondary-index probes. A
registered configurator accepts only exact reviewed data.table versions and
exact carrier/cache metadata, retaining only a process-local C capability.
Its first invocation seals either the enabled or disabled state until the DSO
is unloaded, so the registered entry point cannot be replayed to change the
load-time decision. Canonical ASCII
parameter and tag tables can then receive equivalent composite and tag indices
directly. Unknown versions, changed layouts, marked/non-ASCII strings, and any
unsupported shape simply omit the native index; the R initializer detects the
missing marker and calls `setindexv()` as before.

Result metadata is owned by the result. In particular, native table names are
copied into a fresh plain character vector instead of attaching an input
matrix's `dimnames` vector. A later by-reference `setnames()` call therefore
cannot mutate the input matrix or the character vector from which its column
names were created. The pre-R-4.6 Domain fallback applies the same rule by
shallowly owning its outer table shell and copying its names before replacing
the self-reference. Existing key and secondary-index metadata receive the same
owned copies as data.table's shallow allocator. Its Domain-list names are
copied by subsetting the ID vector, which owns the outer `STRSXP` while
preserving the encoding of each `CHARSXP`.

The same rule applies to Domain rows, conditions, and other small S3 objects.
R remains responsible for language capture such as `substitute(depends)`,
constructor calls used for printing, and evaluation of arbitrary user
callbacks. A utility Domain's `custom_check(1)` therefore remains an R call in
the established frame. Its ordinary scalar-`TRUE` or scalar-string result is
classified by a tiny native gate; only those success shapes skip the generic
assertion machinery, while ALTREP, invalid, and unusual objects retain the
original checkmate path. On R 4.5 and newer, a second narrow native gate
returns printable IDs for the exact plain calls `p_dbl()`, `p_int()`,
`p_lgl()`, and `p_uty()`, whose `deparse1()` output is fixed. The same encoder
admits compact calls with only known named formals and a deliberately small
literal grammar: plain `NULL`, logical and integer scalars, infinities,
integral double scalars from -9999 through 9999 when `scipen` is exactly zero,
and short native-encoding printable-ASCII character vectors. Namespace
qualification, attributes, objects, unknown heads or formals, and unsupported
argument values retain the historical `deparse1()` path. Output is capped at
80 bytes, so
the encoder never reproduces deparse's line-breaking state machine. It reads
the option before touching the live call, performs an allocation-free
authentication and render, allocates the result, then repeats the option read
and complete render; any changed shape or byte rejects the result. R releases
before 4.5 lack allocation-free public attribute inspection and therefore use
`deparse1()` for the complete classifier.

R 4.6 adds a separate standalone-constructor boundary for exact unqualified
`p_dbl()`, `p_int()`, and `p_lgl()` calls. The wrapper crosses into C before
ordinary validation. C authenticates the exact installed closure and caller
binding, verifies every delayed formal without forcing it, and retains forced
values in the original function frame so a decline cannot evaluate an
expression twice. It constructs the established Domain columns and attributes
directly only after a final allocation-free audit of the call, bindings,
values, and runtime roots. For fractional representation values, the owned
deep duplicate is formatted through R's full-precision numeric coercion; text
is accepted only with all significant digits or an exact complete
`R_strtod()` round trip, and `scipen` plus `OutDec` are audited around every
allocating render. Long, multiline, extended, or mutated calls retain the R
constructor. Standalone `p_fct()` also remains on the R path because
conservative direct admission did not improve its representative common
workload; its grouping collapse is still native.

## Native source boundaries

- `src/init.c`: registration and package initialization;
- `src/paradox.h`: shared types, declarations, and invariants;
- `src/r_api_compat.c` and `src/r_api_compat.h`: the only version-dependent R
  accessor boundary, including fail-closed old-R fallbacks;
- `src/r_utils.c`: checked accessors and R object/table construction;
- `src/r_utils.h`: shared checked-construction helpers;
- `src/builtin_condition.c` and `src/builtin_condition.h`: exact built-in
  dependency-condition classification and comparison;
- `src/domain_construct.c`: conservative construction gate for the five
  built-in Domain types;
- `src/domain_construct_builtin.c`: authenticated standalone `p_dbl()`,
  `p_int()`, and `p_lgl()` construction on R 4.6 and newer;
- `src/domain_kernels.c`: built-in Domain validation, sanitization, and
  quantiles;
- `src/properties.c`: aligned static ParamSet properties;
- `src/ids.c`: ordered class and tag filtering;
- `src/paramset_get_values.c`: exact-base value selection with dependency,
  TuneToken, required-value, and final ID filtering;
- `src/paramset_construct.c`: canonical ParamSet table assembly;
- `src/paramset_collection_construct.c`: conservative exact-collection state
  assembly without evaluating child callbacks;
- `src/paramset_check.c`: scalar and column-oriented batch validation;
- `src/paramset_collection_check.c`: exact-collection graph authentication,
  stable snapshots, and scalar built-in validation;
- `src/paramset_qunif.c`: bulk built-in quantile mapping and typed table
  construction;
- `src/sampler_unif.c`: authenticated whole-operation uniform sampling and
  typed table construction for exact retained samplers;
- `src/paramset_domain_common.c`: shared canonical-state validation and Domain
  table construction;
- `src/paramset_domain_common.h`: shared validated Domain-state declarations;
- `src/paramset_get_domain.c`: single-Domain reconstruction, including guarded
  collection callbacks;
- `src/paramset_domains.c`: one-pass reconstruction of all ordinary Domains;
- `src/paramset_params.c`: one-pass construction of the enriched `$params`
  table and the shared static/dynamic assembly boundary;
- `src/paramset_params_internal.h`: internal authenticated parameter-table
  snapshot contract;
- `src/paramset_collection_params.c`: recursive exact-collection admission and
  enriched `$params` construction around the public dependency/value snapshot;
- `src/paramset_collection_deps.c`: callback-free graph admission and
  postorder dependency aggregation for exact collections;
- `src/paramset_collection_values.c`: callback-free graph admission and
  one-pass assembly of exact-collection `$values`;
- `src/paramset_collection_detach.c`: callback-feature discovery and compact
  detachment plans for collection subsets and flattening;
- `src/paramset_values_store.c`: value-list merging, authenticated exact-base
  validation and storage, and collection child-assignment planning;
- `src/paramset_subset.c`: validated slicing and single-use state transfer;
- `src/paramset_bulk_shell.c`: transactional construction of canonical
  one-dimensional ParamSet R6 shells on R 4.6 and newer;
- `src/paramset_bulk_shell_internal.h`: shared validate/prepare/adopt contract
  used by the ParamSet and sampler graph factories;
- `src/paramset_trafo.c`: callback planning for batched transformations;
- `src/r6_surface_auth.c`: callback-free authentication of the generated R6
  surfaces used by Design transformations, native checks, and random designs;
- `src/design_transpose.c`: ordinary design row conversion;
- `src/design_transpose_trafo.c`: exact built-in log-scale batch
  transformations;
- `src/design_dependencies.c`: dependency planning and built-in masking for an
  entire Design operation;
- `src/ps_construct_builtin.c`: literal built-in `ps()` call planning and
  anonymous Domain-row construction;
- `src/sampler_1d_unif_shell.c`: transactional construction of complete
  `Sampler1DUnif` / `Sampler1D` / `Sampler` R6 graphs around owned ParamSets;
- `src/test_altrep.c`: package-private native fixtures used by lifetime,
  arithmetic-boundary, finalizer, and ALTREP regressions.

Source files follow complete logical operations rather than an R-class mirror;
empty architectural layers are not added merely to match a diagram.

## Value-mutation boundary

Public mutation is split into four narrow native responsibilities. The merge
entry combines the already captured `...`, `.values`, and optional current
value lists; it preserves current order, applies replacements in input order,
deletes `NULL` updates only in insertion mode, and returns a newly owned list
shell. The ordered-store entry filters unknown names, selects the first
duplicate, and stores known values in parameter order while shallow-sharing
opaque leaves. An unnamed empty private list is deliberately not normalized:
it returns the fallback sentinel so the R method preserves that internal
object shape.

The checked-assignment entry is a success-only atomic fast path for an exact
base `ParamSet` with canonical built-in rows, no dependencies, and no
constraint. It authenticates the generated `values`, `deps`, and `constraint`
bindings plus `assert()`, `check()`, `test_constraint()`,
`check_dependencies()`, and private `.store_values()`. Authentication covers
the exact wrapper body and formals, owned enclosure, package-namespace parent,
and explicit absence of local `super` or implementation shadows; active,
delayed, reparented, or replaced wrappers therefore decline without being
forced. Validation and sanitization complete before the single ordered commit,
and no user callback may occur between them. Unsupported values, extensions,
malformed state, or a failed check return literal `NULL` without mutation and
execute the established R validation, diagnostics, and callback path.

The collection entry only builds a distribution plan. R still performs each
planned child assignment through its public `values` binding, with touched
children before cleared children, so subclass behavior such as
`ParamSetShadow`, nested prefix/postfix translation, shared-child last-owner
semantics, and extension errors retain their established dispatch and order.
Explicit `NULL` values, input ordering, and list ownership follow the ordinary
setter rules. All inspected tables, wrapper environments, sanitized values,
and plans remain rooted across allocation and garbage collection; direct
private state is installed only after complete admission and validation.

## ParamSetCollection construction boundary

The exact base collection initializer calls the registered arity-four
`param_set_collection_construct` helper only after R has validated the public
arguments and normalized the child names. The helper admits exact base
`ParamSet` and `ParamSetCollection` children with owned R6 enclosures and
canonical ordinary private construction tables. It inspects only permanent
schema state: constructing a collection does not read values, dependencies, or
callbacks, so those child properties remain live after construction.

On admission, one pass preserves child and row order while affixing IDs,
creates independently owned mutable shells for `.params`, `.tags`, `.trafos`,
and `.translation`, and shallow-shares only the same opaque leaves as the R
implementation. Existing tags precede generated set and parameter tags,
duplicates are retained, and the generated tag and transformation tables use
the historical stable ID order. Child objects themselves remain the identical
references supplied by the caller. The native tables are ordinary R objects;
data.table-shaped tables receive a valid public object-level self-reference
without calling data.table C code.

The literal `NULL` result is a pre-callback fallback sentinel. Subclasses,
custom or malformed storage, active or delayed private bindings, unsupported
string encodings, and translated ID collisions therefore execute the complete
historical R initializer and retain its diagnostics. No partially constructed
native state is installed before admission succeeds. This boundary preserves
extension behavior without making it part of the fast path and uses only the
public R C API available throughout the R 4.3-and-newer support range.

## ParamSetCollection Domains boundary

The arity-two `param_set_domains` ABI continues to serve both ordinary sets and
collections. An exact nonempty `ParamSetCollection` is admitted only after an
iterative, callback-free walk of its complete current-path graph. Every node
must have an exact base class, its owned R6 enclosure, canonical built-in
private tables, and the unmodified generated `values`, `deps`, `ids`,
`.get_values`, and collection prefix wrappers. Collection names and postfix
flags are validated without forcing promises or active private bindings.
Reusing one child in separate branches is a valid DAG; encountering a node
already on the current path raises a deterministic cycle error.

The admission plan roots every object, enclosure, wrapper, and source table it
has inspected. Before evaluating R, the kernel copies all eleven permanent
parameter columns into immutable native-owned snapshots, including an owned ID
vector used for result names. It then evaluates the captured root values
binding exactly once, resolves and evaluates the then-current public root
dependencies active binding exactly once, in that order. Tags and
transformations are read after those callbacks, matching single-Domain
collection reconstruction; all four dynamic stores are validated and copied
into rooted shells before matching or result construction.

`NULL` remains the literal fallback sentinel only before either callback has
started. Subclasses, extension Domains, replaced wrappers, delayed or active
private stores, malformed tables, and inconsistent nested objects therefore
retain the complete R path without duplicated side effects. Once values have
started, malformed callback results, missing or corrupt live metadata, and
duplicate value or transformation owners raise a collection-state error. The
kernel never returns `NULL` after a callback and consequently never replays a
callback through the fallback.

Successful output is the same named list of independently owned, mutable
data.table-shaped Domain facades as repeated `get_domain()` calls. All
dependency rows and their order are retained, explicit named `NULL` values stay
distinguishable from absence, and opaque leaves such as closures and
environments keep their historical shallow sharing. The implementation calls
neither data.table nor checkmate and uses only public R APIs available across
the package's R 4.3-and-newer C17 baseline.

## ParamSetCollection values boundary

Exact base `ParamSetCollection$values` has a dedicated arity-two registered
entry point. It performs a complete callback-free admission before allocating
the result: every current-path node must have its exact base class and owned
private environment, canonical generated public and private getter wrappers
parented by the package namespace, canonical permanent ID/class/storage
columns, direct value bindings, and row-for-row collection translation.
Collection names, postfix
flags, affixed IDs, value order, and supported string encodings are checked as
part of the same preflight. A sibling may refer to the same set more than once;
only identity already present on the active path is a cycle and raises a
deterministic error.

The iterative plan roots every inspected object and state vector through the
entire call. Each leaf occurrence records only its root-row offset and the
ordered subset of parameters that currently have values. After admission, one
fresh list and one fresh names vector are filled from the root collection's
canonical ID order. Opaque leaves, including environments and closures, remain
shallowly shared as before, while mutation of either returned shell cannot
alter stored state or another call's result. Empty output is always a named
list. Once a collection frame itself is authenticated, its immediate child
count reserves the corresponding root-carrier capacity without observing those
children early. Canonical constructions also reuse identical, same-position
parameter and translation `CHARSXP`s in one linear pass; small permuted inputs
retain the direct scan and all other encodings retain R's general match.

Subclasses such as miesmuschel's `ParamSetShadow`, custom Domains, replaced or
reparented R6 wrappers, promises and active private bindings, bytes-encoded
names, and malformed relevant tables return the `NULL` sentinel before any
extension callback can run. The R wrapper then executes the established
delegated path.
The native path calls neither data.table nor checkmate, checks long loops for
interrupts, and uses only the public C API available from R 4.3 onward.

## ParamSetCollection dependencies boundary

Exact base `ParamSetCollection$deps` uses a dedicated registered arity-two
entry point. Before reading any public child member, an iterative preflight
validates every current-path node's exact base class, owned R6 private
environment, generated dependency binding, canonical built-in parameter and
dependency tables, direct collection fields, names, postfix flag, and supported
ASCII encoding. Generated `ids()` wrappers are authenticated only on the edges
where the historical implementation would call them: a named child whose
aggregated dependency result is nonempty. Subclasses, custom Domains, replaced
wrappers, promises, active private state, unsupported encodings, and malformed
tables therefore reach the unchanged R aggregation path before any extension
callback has run.

The protected plan records one occurrence per graph edge, so a shared sibling
is a valid DAG while identity already on the active path raises a deterministic
cycle error. Source tables and their parsed columns remain rooted throughout
construction. Dependency rows are emitted once in depth-first postorder into
fresh columns, with collection-local rows appended after descendants and the
outer local rows last. Prefix and postfix translation is applied independently
at every outward edge; dangling and duplicate strings retain the established
`map_values()` behavior, including coincidental matches at a higher layer.

Every Condition is deeply duplicated, ordinary list aliases are detached, and
opaque leaves such as environments remain shared. The resulting three-column
data.table facade has compact row names, no key or secondary index, and a valid
object-level self-reference without calling data.table. Long loops poll for
interrupts, all row and allocation arithmetic is checked, and the traversal
does not consume the C stack.

## ParamSet get-values boundary

The public `$get_values()` wrapper passes only its owned private environment,
`self`, and the current method frame to the registered arity-three native
entry. Keeping the public filters in that frame is intentional: `type` and
`check_required` are forced and validated first, while `class`, `tags`, and
`any_tags` remain promises until after dependency callbacks, TuneToken
filtering, and required-value diagnostics. The adjacent arity-two `ids()`
entry similarly forces and validates those three filters one at a time before
reading the live parameter and tag tables.

Admission is callback-free and exact-base only. It authenticates the generated
`get_values()`, `ids()`, values/dependency bindings and private getter, owned
R6 enclosure, canonical built-in tables, ordered value names, and, for a
collection, the complete graph through the callback-free native values and
dependency aggregators. A `NULL` sentinel can be returned only before
`remove_dependencies` or a Condition callback is evaluated. Subclasses,
custom Domains, altered wrappers, malformed tables, promises in private state,
and bytes names therefore enter the unchanged R path without replay.

The operation retains the historical split between snapshots and live state.
Values and their original names are rooted independently, and the dependency
table is captured once. The `remove_dependencies && nrow(dependencies)` test
and the later `seq_row(dependencies)` call each make their historical live row-
count observation. The exact imported `seq_row` closure is resolved and
authenticated before invocation; a replacement still produces the live row
vector but cannot enter the planned lane after restoring the import. After both
observations, an exact table made solely from
canonical `CondEqual` and `CondAnyOf` objects can enter a second callback-free
lane. That lane revalidates the live columns and values names, authenticates
the locked generic and methods, rejects classed, ALTREP, or otherwise
dispatch-capable operands, roots every planned right-hand operand independently
of the mutable condition table, and matches all IDs in batches. It then traverses
the already evaluated `seq_row()` result in its actual order, including valid
permutations or duplicates, while evaluating the sequential removal plan in C.
After the final dispatch authentication and before committing to that plan,
each parent scalar is read again, revalidated, and rooted independently. The
comparison loop uses those snapshots rather than rereading a user-visible list
after an allocating operation.
Logical, integer, double, and character scalars retain R numeric coercion,
missing-value, encoding, infinity, signed-zero, and membership behavior. Any
custom class, changed binding, malformed condition, unsupported operand, or
observable extension declines before a value is removed and uses the unchanged
live S3 loop instead. In that loop, correctly shaped column and
`condition_test` replacements made by an earlier callback are observed on the
next row. The
public binding API before R 4.6 cannot distinguish this forced canonical
surface from a new delayed replacement, so those releases conservatively use
the live loop while retaining the same package ABI. The
first dependency-removal assignment shallow-copies the local
values shell and its names even if a callback renamed the target and no element
matches, reproducing R's copy-on-write detachment from
`private$.values`; every non-`with_token` type filter establishes the same
boundary even when all elements survive. Opaque elements remain shallowly
shared. Required IDs are checked against the original names, then final filters
read the live parameter/tag tables and emit in current parameter order.

After the first callback, unsupported mutation raises a deterministic state
error instead of falling back and repeating side effects. The retained roots
cover the original names, current detached shell, callback-visible tables, and
selected IDs across allocation and garbage collection. The implementation
calls neither checkmate nor data.table and compiles against the public strict R
C API from R 4.3 onward.

## ParamSetCollection params boundary

An exact `ParamSetCollection$params` call has a dedicated registered entry
point; it does not broaden the ordinary `ParamSet$params` ABI. Before invoking
R, the collection routine iteratively validates the complete current-path
graph: exact base class vectors and owned private environments, canonical
built-in state and data-frame row names, generated R6 bindings and superclass
proxies, collection name/postfix rules, keyed translation ownership, one-layer
prefix/postfix spelling, and row-for-row snapshot agreement. Reusing one child
in sibling branches is valid. Reaching the same collection again on the active
path is a cycle and raises a deterministic error, because the historical
recursive fallback cannot finish.

Every admitted frame and source table is retained in a protected R root plan.
The shared builder first copies the collection's static `.params`, `.tags`, and
`.trafos` snapshots into an owned 16-column result. Only after that snapshot is
complete does it read public `$deps` and then public `$values`, exactly once and
in their historical order. The first callback result and all admitted sources
remain rooted across the second callback. Dynamic joins use the result's owned
ID vector, so a finalizer or delegated callback that rebinds private state
cannot invalidate or retarget the in-flight table.

The native call returns its `NULL` fallback sentinel only during this
callback-free admission phase. Once public dependency or value evaluation has
begun, dangling dependency IDs are handled like an unmatched data.table update
join and ignored. Any other unsupported callback result raises one deterministic
state-change error instead of returning to the R implementation and executing
the callbacks a second time.

Subclasses, custom Domains, replaced active bindings, malformed or delayed
private bindings, noncanonical translation/index metadata, and inconsistent
nested snapshots return the `NULL` sentinel before public callbacks. The R6
binding then executes the established implementation. Successful results own
the table, column and produced list shells, preserve the private opaque
secondary index and valid data.table self-reference, and continue sharing only
historical opaque leaves such as closures and environments.

## Subset and flatten state-transfer boundary

`ParamSet$subset()` validates its public arguments and observes `$deps` at the
historical R point before calling the registered subset planner. The planner
admits either an exact base `ParamSet` or an exact base
`ParamSetCollection`. Ordinary sets read their private dependency and value
stores directly. Collections independently authenticate their complete graph
through the callback-free native dependency and value aggregators; an external
caller cannot inject a table that later becomes constructor state. Any custom
child, replaced wrapper, malformed translation, unsupported Domain, or cycle
therefore returns the `NULL` fallback sentinel before a delegated value
binding is evaluated.

The planner roots the complete source state, validates requested IDs and all
owner maps, preserves request order and repetition, and allocates fresh
parameter, tag, transformation, dependency, and values shells. A missing
dependency is reported before `keep_constraint` is forced. Otherwise the
result is protected by a process-local external-pointer capability that is
accepted only by a fresh exact `ParamSet` constructor and consumed on first
use. This keeps table ownership independent while retaining the historical
leaf-sharing or duplication rules for opaque values, transformations, and
Conditions.

`ParamSet$subspaces()` uses a separate internal mode of the same capability.
The native planner freezes the complete attribute-free ID vector, validates
and indexes the source once, and first creates one unreachable composite state
in request order. It then splits that rooted state into independently owned
one-row parameter, tag, transformation, values, and historically empty
dependency tables for every requested occurrence, including duplicates. Each
fresh singleton is validated again before its capability is created. The
planner snapshots the source table children and relevant metadata before any
of this work and performs one final source, surface, request, and values audit
after every child and result attribute has been allocated. A failure exposes
no partial token batch. Each token also carries the authenticated direct
`extra_trafo` value. `SamplerUnif` may transfer these fresh states directly
into `Sampler1DUnif`; `Sampler1D` probes and consumes the capability,
constructs the one-row `ParamSet`, replays the public `extra_trafo` setter
validation, and skips the otherwise redundant deep clone.
On a reviewed data.table layout, each singleton payload also carries the exact
composite `id` / `cls` / `grouping` secondary index synthesized by the native
constructor. Adoption skips `setindexv()` only when that marker is present;
unsupported layouts or encodings retain the data.table setter.

Subspace planning is bound to the one public `$values` result captured before
the caller observes the private ID vector. The single validate-once transaction
requires that exact private value pointer at admission and audits it again
after its allocations. A finalizer or other replacement observed before the
final audit therefore declines the complete native plan; a pending finalizer
that R runs only after the single native call cannot alter the already detached
batch. Both public `$subspaces()` and exact-base `SamplerUnif` reuse the already
captured value snapshot in their historical R fallback instead of mixing child
states.

On R 4.6 and newer, public `$subspaces()` hands a complete token batch to
`src/paramset_bulk_shell.c`. Package load preserves one unexposed canonical
empty ParamSet graph and snapshots the live generator's complete sorted
binding inventory, ordinary binding types, locks, parent, attributes, exact
top-level values, and owned copies of every list container. The factory never
invokes the live generator. It authenticates that full snapshot, allocates
fresh public, enclosure, and private environments plus one clone of every
generated closure per child, and installs each active closure at pointer
identity in both the active binding and `.__active__` registry. These three
environments are non-hashed, matching R6's observable `env.profile()` state;
their parents, class, binding inventory and locks match ordinary ParamSet
construction. After installing result attributes, the factory makes one final
generator-name allocation, then reaudits the generator and every token before
the no-allocation adopt-and-lock loop. A generator mutation or invalid token
returns `NULL` without consuming any state, so only the historical R path
invokes altered constructors. R 4.3 through 4.5 always use that R path because
their public API cannot inertly distinguish every generator binding type.
R's debugger can toggle a closure's internal debug bit without replacing that
closure, and the public extension API exposes no inert inspection for that bit.
This diagnostic-only in-place mutation is intentionally unsupported by the
factory: ordinary method replacement is detected, but a breakpoint placed
directly on an unchanged generator wrapper is not reproduced on canonical
children.

`SamplerUnif$new()` has a still narrower combined factory in
`src/sampler_1d_unif_shell.c`. Package load records an unexposed canonical
`Sampler1DUnif -> Sampler1D -> Sampler` prototype, all four participating R6
generators (including `ParamSet`), the R6 generator capsule helpers, and the
exact package-namespace constructor targets. Admission requires their binding
types, pointer identities, parents, attributes, insertion order, lock state,
and owned list snapshots to remain unchanged. One all-or-nothing call prepares
every ParamSet token, builds every ParamSet shell, and then constructs all three
non-hashed sampler slices per child with their shared private environment,
super chain, cloned generated closures, active-binding registry identities,
and historical locks. The last allocation is followed by an allocation-free
reaudit; only then are all single-use tokens adopted. Any altered generator,
capsule, namespace target, source graph, token, or special environment value
declines the whole batch without consuming state and runs the ordinary R6
constructors. This specialized copier is intentionally not a general R6 clone
API and is not an extension boundary.

Every load-time prototype and authentication snapshot held with
`R_PreserveObject()` has a paired, idempotent release routine called from
`R_unload_paradox`. This includes the combined sampler graph, the ParamSet shell
prototype, the standalone built-in Domain constructor state, and the literal
`ps()` constructor state. Release helpers clear their static roots after
`R_ReleaseObject()`. Both initialization and unload are exported on Windows;
reloading the namespace must establish a fresh set of roots rather than reuse
addresses from the prior DSO lifetime.

If a selected fixed/special value is an environment, it retains the ordinary
Sampler initializer so R6 objects receive the same deep clone as before.
Foreign pointers, subclasses, altered surfaces, delayed private state, and
ALTREP requests never enter this owned hand-off.

The collection override still creates detached snapshots of live child
constraints and extra transformations. Its common exact-graph no-feature case
returns the already constructed result immediately, before graph admission or
translation copying. Callback-bearing exact graphs use a fail-closed direct
private traversal. Before inspecting a private feature slot, it authenticates
the instance's generated R6 active binding, closure owner, namespace parent,
formals, and body without invoking that binding. The resulting wrapper closes
over compact named lists containing only the child callback state and the full
four-column translation, not cloned source `ParamSet` objects. Encountering a
replacement, subclass, or malformed/cyclic graph restores the public
active-binding traversal. `flatten()` additionally scans plain cargo first and
rewrites only rows containing internal-tuning callbacks, rather than running a
data.table callback over every parameter when there is nothing to detach.

## Bulk ParamSet quantile boundary

`ParamSet$qunif()` retains its public R validation before native dispatch:
inputs must be numeric matrices or data frames with at least one column, contain
only finite values in `[0, 1]`, and have unique column names drawn from the
ParamSet. Data frames are normalized with the established `as.matrix()` step.
This preserves checkmate diagnostics for every invalid public input.

The native routine accepts only the exact permanent `.params` representation:
the two-class `data.table` / `data.frame` facade, all eleven columns in their
canonical order and storage modes, nonmissing metadata, and unique parameter
IDs. It accepts an unclassed integer or double matrix with valid dimensions and
column names, then resolves requested IDs through an interruptible hash table.
Only requested parameter rows must be supported, so an unselected extension
does not disable a built-in slice.

Selected rows map natively only when they are canonical `ParamDbl`, `ParamInt`,
`ParamFct`, or `ParamLgl` rows with matching storage metadata. Factor choices
must be nonempty, nonmissing character vectors and the logical level vector
must be exactly `c(TRUE, FALSE)`. Selected `ParamUty` or custom Domain rows,
altered metadata, malformed tables, and unsupported matrix objects return the
`NULL` sentinel without mutation. The R method then executes its prior grouped
`domain_qunif()` / S3 implementation, including its diagnostics and extension
callbacks.

The successful path allocates one correctly typed output vector per requested
column and constructs the ordinary data.table-shaped result directly. Input
column order is retained, row names use the standard compact representation,
zero-row inputs retain typed zero-length columns, and neither input values nor
input names are shared mutably with the result.

Numeric quantile mapping deliberately preserves the historical sequence of R
floating-point primitives.  The affine kernel materializes `x - 1`, both
products, and then the subtraction through automatic `volatile double`
intermediates.  These are rounding barriers, not shared-state synchronization,
and must not be removed or replaced by an expression that permits contraction.
Apple Clang otherwise emits a fused multiply-add on ARM64: the ordinary `.499`
probe differs from R by 64 representable doubles, and an adversarial integer
probe crosses a `floor()` boundary and returns `-2L` instead of `-1L`.  The
barriers remain effective under forced contraction with both GCC and Clang.
Pinned AB/BA benchmarks in
`.local/benchmarks/release-fma-affine-20260717`, pooled by library identity
rather than command-position labels, measured a 1.2--2.5% cost for public mixed
`ParamSet$qunif()` workloads.  Allocations were unchanged, and the release
workload remains about 9.8 times faster than the R implementation.  This
bounded cost is part of the compatibility contract.

## Grid-design bulk boundary

After the public wrapper has constructed and validated the named resolution
vector, an exact generated base `ParamSet` may hand all axes to the registered
grid routine at once. The routine reuses the built-in quantile specifications,
but allocates the complete Cartesian table directly instead of constructing a
one-column table per parameter and joining those tables in R. Resolution order
is output-column order and the last axis varies fastest, matching the previous
unsorted cross join. Interior unit values use the same reciprocal multiplication
as `seq.default()` and explicit endpoints, including bit-for-bit floating-point
agreement.

Admission is deliberately narrower than bulk `qunif()`. It requires R 4.6's
allocation-free attribute inspection, all current parameter rows to be
canonical built-ins, a plain named integerish resolution vector containing
each ID exactly once, categorical counts equal to their live level counts, and
a Cartesian product representable by an ordinary data frame. Names and counts
are copied into rooted carriers in one allocation-free snapshot before factor
levels or output columns are allocated. Translated ID operands remain rooted
through hash collision probes.

Any zero resolution declines even though C could cheaply return an empty
table: historically every axis is quantile-mapped before the cross join, so a
different unsupported axis can warn first. Infinite integer bounds, extensions,
replaced generated methods, old R runtimes, altered metadata, overflow, and all
other unsupported shapes likewise return the `NULL` sentinel and execute the
complete R path. `Design$new()` remains the compatibility boundary for values,
dependencies, and duplicate removal; profiling that boundary, rather than
further tuning the grid fill loop, determines the next optimization.

## Design dependency planning boundary

For an exact base `ParamSet`, `Design$new()` asks one registered routine for a
complete dependency plan after fixed values have already been installed. C
reconstructs the historical stable, layered `topo_sort()` order, evaluates
exact `CondEqual` and `CondAnyOf` columns, and returns one typed row mask and
child ID per dependency edge. R then performs the same character-`j`
`data.table::set()` call for every edge, including empty masks. This retains
by-reference aliases outside the table, key/index invalidation, `.Last.updated`,
and the fixed-values / dependencies / duplicate-removal order without calling
data.table from C.

Admission is all-or-nothing and callback-free. The generated R6 surfaces,
imported helpers, the live S3 method table, condition `$` dispatch, private
tables, column types, and string encodings must be exact. The cloned topology
is enabled only for reviewed `mlr3misc` source versions; an unknown future
implementation falls back rather than silently adopting different tie rules.
Columns that share storage with another design column, parameter/dependency
table state, or any exact condition child or class/name vector are rejected,
because an earlier historical `set()` can otherwise change a later lookup or
dispatch.

Every ID, storage type, dependency endpoint, and data name is copied before
workspace allocation and must remain unchanged at the commit boundary. Output
allocation and allocation-capable shape/surface checks finish before a final
allocation-free state, alias, condition, and dispatch validation; only then is
the plan simulated a second time into its exact-sized row vectors. Inactive
state is a sparse-parent bit set capped at 64 MiB, and edge/index-plan caps make
oversized cases decline before an avoidable native allocation. No `NULL`
fallback is possible after R begins the ordered `set()` loop.

## Random design bulk boundary

`generate_design_random()` admits only exact base `ParamSet` and
`ParamSetCollection` R6 objects whose rows are built-in `ParamDbl`, `ParamInt`,
`ParamFct`, or `ParamLgl` Domains. Collection children are checked recursively;
any subclass, custom Domain, altered required R6 shape, or collection cycle
fails closed to the established `SamplerUnif` implementation. Zero-dimensional
spaces also retain that path because their historical result has zero rows even
when a positive sample count was requested.

Admission authenticates the exact generated `clone`, `ids`, `qunif`, and
`subspaces` methods and the `length`, `class`, `is_bounded`, `has_deps`, and
`values` active bindings. Collections additionally authenticate their own
`clone` and `sets` wrappers plus the inherited superclass enclosure. The check
reads binding metadata and promises without evaluation, so an ordinary delayed
replacement declines before its expression or any extension callback runs.

The bulk path preserves the sampler's operation order. It first validates the
parameter support, makes one deep clone, and then validates `n`. It draws one
column-major vector of `n * p` uniforms, gives the resulting named matrix to the
cloned set's bulk `qunif()` method, and constructs one `Design` without duplicate
removal. A single column-major draw consumes exactly the same random stream as
the former ordered sequence of `p` calls to `runif(n)`; `runif(0)` neither
creates nor changes `.Random.seed`.

Differential tests compare the complete data columns, storage types, table
metadata, cloned parameter-set state, and final RNG state against
`SamplerUnif` over mixed, fixed, dependent, collection, zero-row, and multiple
seed cases. Separate regressions cover clone isolation, empty-space behavior,
validation/error priority, subclasses at both the root and nested collection
levels, custom Domains, and spoofed non-R6 objects. The coordinated clean build
results used during development are diagnostic only; the frozen-candidate
workflow in `AGENTS.md` owns the release claim.

## Repeated uniform-sampler boundary

`SamplerUnif` has its own whole-operation kernel because consumers such as
bbotk and mlr3hyperband retain one sampler and invoke `$sample()` inside their
optimizer loops. The exact generated private method enters C immediately. A
successful call allocates one typed column per parameter, draws the same
column-major stream as the former ordered `runif(n)` calls, maps each value
with the shared built-in quantile primitives, and returns a complete
data.table facade. The existing public `Sampler$sample()` method still creates
the sole outer `Design`, applies root dependencies, and retains the sampler's
`param_set` identity.

Admission is intentionally narrower than `generate_design_random()`. The
first retained lane requires a nonempty exact base `ParamSet`, an exact
`SamplerUnif` / `Sampler1DUnif` R6 graph in root parameter order, canonical
built-in one-row child sets, and empty root and child fixed-value stores. Each
child's type, bounds, and levels must still equal the detached root row from
which it was constructed; altered bounds, reordered or duplicate children,
replaced public, private, or active methods, dependencies installed on a child,
subclasses, collections, and serialized malformed state all decline. The
hierarchical implementation then observes those supported mutable surfaces in
its established order.

Package and imported method targets must already be forced canonical closures;
the admission check never forces a delayed lazy-load binding merely to decide
whether C is safe. A sampler's first call may therefore use the hierarchical
path to establish those package bindings, while retained subsequent calls enter
the native lane. Zero-row priming does not create or advance the RNG state.

For positive row counts, the RNG binding must be an ordinary unlocked,
unaliased direct integer seed for one of R's seven built-in uniform generators.
Active, delayed, locked, shared, absent, malformed, and user-supplied RNG
bindings decline without a read, write, force, warning, or consumed variate.
Zero rows do not inspect, create, or change `.Random.seed`. This restriction
lets `PutRNGstate()` reuse the admitted seed and removes callback and allocation
interleaving from the committed draw while retaining the exact
values and final seed for Wichmann-Hill, Marsaglia-Multicarry, Super-Duper,
Mersenne-Twister, both Knuth variants, and L'Ecuyer-CMRG.

The graph is authenticated once to size the output and again after every
output, data.table self-reference, factor-level snapshot, and root carrier has
been allocated. All borrowed R6 objects, tables, columns, closures, and level
copies are retained in that carrier. A final allocation-free audit compares
the live graph and seed with the retained snapshot; only then does C call
`GetRNGstate()`. No fallback, allocation, callback, diagnostic, or interrupt
check remains inside the column-major draw, matching base `runif()`'s committed
loop. After `PutRNGstate()`, C no longer reads any borrowed sampler state.

## Validation evidence

The native bulk-quantile tests cover forced registration, exact endpoint and
storage behavior, randomized agreement with individual built-in Domains,
reordered subsets, integer matrices, normalized data frames, zero rows,
`ParamSetCollection`, selected and unselected custom Domains, `ParamUty`, public
diagnostics, corrupt storage, nonmutation, result-name ownership, and GC torture.
Intermediate focused and full-suite results are useful while editing, but are
invalidated by later source changes.

Development iterations passed strict GCC and Clang builds, focused tests, GCC
`-fanalyzer`, per-translation-unit Clang Static Analyzer reports, exhaustive
cppcheck, forced-registration auditing, and ELF export and hardening checks.
These intermediate results are not release evidence. The frozen release
workflow in `AGENTS.md` and the retained run receipts described in
`design/validation.md` are authoritative for any release claim.

rchk is intentionally treated as a bounded, source-specific model rather than
an oracle that understands every R root carrier. The release policy hashes the
complete report and assigns every ordered diagnostic block to a narrow reviewed
model limitation; maacheck remains exactly empty and fficheck must account for
all 61 registered routines. A changed diagnostic, block count, rationale, or
package-local analyzer error fails closed. This keeps the remaining UP/PB
reports visible without contorting correct, ownership-explicit C merely to fit
bcheck's abstraction.

## C rules

- Portable C17 is the shipping baseline, avoiding dependence on C23 semantic
  changes while retaining a current compiler contract. There are no
  architecture-specific
  intrinsics or `-march=native`; Apple silicon is a first-class target. The
  documented `SystemRequirements: USE_C17` mechanism requires R 4.3 or newer.
- Define `R_NO_REMAP` and `STRICT_R_HEADERS`; use the public R API only.
- Use `R_xlen_t` and `XLENGTH`, not `int` and `LENGTH`, for R vector lengths.
- Register every entry point, disable dynamic lookup, and force native symbols.
- Use no variable-length arrays, unchecked fixed-size formatting buffers, or
  pointers into R vectors across a call that can allocate.
- Protect every newly allocated object across any allocating call. Preserved
  objects have explicit, paired release paths.
- Treat ALTREP accessors as arbitrary R callbacks. Canonical private table
  shells, their names and attributes, and any column used through a retained
  raw pointer must have an ordinary representation. Transient public inputs
  are either copied once into an ordinary, protected snapshot or declined to
  the R compatibility path; `duplicate()` alone is not a materialization
  guarantee.
- A callback-capable input is declined before its first `Length` or `Elt`
  observation whenever the R wrapper can perform the whole operation as the
  sole observer. In particular, native design transposition rejects ALTREP
  containers, column names, and columns, and native Domain lookup rejects an
  ALTREP requested ID. This prevents a callback from invalidating admission or
  being replayed after a later fallback decision.
- Never size an output from live R state and then reread that state while
  filling it. Cache the validated decisions or source positions once, or use a
  checked maximum-capacity output and trim it after a single extraction pass.
  Every writer checks its capacity and verifies the final count even when
  admission currently proves the equality.
- A protected container is not a permanent root for a child that can be
  replaced by reference. Exact columns, attributes, callbacks, and selected
  list elements remain independently rooted until their final use.
- Treat every user callback as a reentrant boundary: retain no transient vector
  pointer or assumed mutable-state snapshot across evaluation.
- Do not call the R API from worker threads. Long loops check for interrupts.
- Preserve the distinction between an absent named element and a present
  element whose value is `NULL`.

## Completed migration record

The implementation proceeded by first freezing structures, serialization,
errors, aliases, callbacks, and consumer assumptions; introducing registered
checked C utilities; moving built-in Domain and ParamSet operations; replacing
row-list validation with column-oriented passes; and finally moving whole
Design, collection, subset, constructor, and sampler operations where measured
consumer paths justified the risk. Unknown Domain classes, subclasses, altered
R6 surfaces, callbacks that cannot be safely snapshotted, and older R runtimes
retain explicit R fallbacks. This sequence is complete for 2.0.0. New native
work now requires a measured release-relevant bottleneck plus differential and
priority-consumer coverage; it is not part of release convergence by default.

The performance freeze is deliberate rather than a claim that every R wrapper
has disappeared. Standalone built-in Domain construction is already dominated
by the unavoidable public-call and compatible-object shell boundary; an
attempted direct `p_fct()` lane did not improve representative factor calls and
was reverted. Replacing arbitrary R6 deep cloning with a general native clone
engine would enlarge the compatibility and lifetime surface well beyond the
measured gain. The release therefore keeps the narrow ParamSet and
`Sampler1DUnif` graph factories that address the observed bulk-construction hot
paths, and leaves a general R6 copier out of scope.
