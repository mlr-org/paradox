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

## State model

The transitional private tables retain their current column names and types:

- `.params`: one row per parameter, in public parameter order;
- `.values`: a named list in parameter order, omitting unset values;
- `.tags`: long-form `id` / `tag` columns;
- `.deps`: `id` / `on` / `cond` columns;
- `.trafos`: `id` / `trafo` columns.

Native metadata may be stored alongside them as ordinary R vectors and
environments:

- an aligned integer type-code vector;
- cached aligned property vectors;
- an ID-to-position hash environment;
- per-parameter tag, transformation, and dependency adjacency lists;
- dependency topology and schema/dependency version counters.

Caches are never allowed to make direct mutation of the established tables
incorrect. Immutable schema properties may be cached permanently. Mutable tags
and dependencies must either invalidate through their active bindings or be
validated against the canonical objects before use.

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

Unknown Domain subclasses take a slow R/S3 fallback. Third-party extension is
not a primary design constraint, but retaining this fallback costs little and
prevents unnecessary breakage. A future package-owned parameter type is added
as one type-table entry rather than by redesigning `ParamSet`.

## R object construction

Native table-shaped results are ordinary `VECSXP` objects whose columns are
allocated with the correct storage types and whose names, compact row names,
and class are set to `c("data.table", "data.frame")`. Shipped C never calls
data.table internals. It does install data.table's public object-level
`.internal.selfref` representation through the public R API: the outer pointer
tags the exact names vector and protects an owner pointer for the table. This
is necessary because consumers routinely pass returned tables straight to
`data.table::set()` or `:=`; without a valid self-reference, those operations
warn, copy unexpectedly, or try to assign a modified value back through an R6
active binding. No data.table C symbol or private header is linked.

Result metadata is owned by the result. In particular, native table names are
copied into a fresh plain character vector instead of attaching an input
matrix's `dimnames` vector. A later by-reference `setnames()` call therefore
cannot mutate the input matrix or the character vector from which its column
names were created.

The same rule applies to Domain rows, conditions, and other small S3 objects.
R remains responsible for language capture such as `substitute(depends)`,
constructor calls used for printing, and evaluation of arbitrary user
callbacks.

## Native source boundaries

- `src/init.c`: registration and package initialization;
- `src/paradox.h`: shared types, declarations, and invariants;
- `src/r_utils.c`: checked accessors and R object/table construction;
- `src/domain_construct.c`: conservative construction gate for the five
  built-in Domain types;
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
- `src/paramset_qunif.c`: bulk built-in quantile mapping and typed table
  construction;
- `src/paramset_domain_common.c`: shared canonical-state validation and Domain
  table construction;
- `src/paramset_get_domain.c`: single-Domain reconstruction, including guarded
  collection callbacks;
- `src/paramset_domains.c`: one-pass reconstruction of all ordinary Domains;
- `src/paramset_params.c`: one-pass construction of the enriched `$params`
  table and the shared static/dynamic assembly boundary;
- `src/paramset_collection_params.c`: recursive exact-collection admission and
  enriched `$params` construction around the public dependency/value snapshot;
- `src/paramset_collection_deps.c`: callback-free graph admission and
  postorder dependency aggregation for exact collections;
- `src/paramset_collection_values.c`: callback-free graph admission and
  one-pass assembly of exact-collection `$values`;
- `src/paramset_values_store.c`: value-list merging, authenticated exact-base
  validation and storage, and collection child-assignment planning;
- `src/paramset_subset.c`: validated slicing and single-use state transfer;
- `src/paramset_trafo.c`: callback planning for batched transformations;
- `src/r6_surface_auth.c`: callback-free authentication of the generated R6
  surfaces used by Design transformations, native checks, and random designs;
- `src/design_transpose.c`: ordinary design row conversion.

Future dependency and collection kernels receive their own files only when
their complete logical operation moves to C; empty architectural layers are
not added merely to match a diagram.

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
list.

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
count observation; correctly shaped column and `condition_test` replacements
made by an earlier callback are then observed on the next row. The first
dependency-removal assignment shallow-copies the local
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

The collection override still creates detached snapshots of live child
constraints and extra transformations. Its common exact-graph no-feature case
uses a fail-closed direct private traversal and returns before copying the
translation table. Before inspecting a private feature slot, it authenticates
the instance's generated R6 active binding, closure owner, namespace parent,
formals, and body without invoking that binding. Encountering a replacement,
subclass, or malformed/cyclic graph restores the public active-binding
traversal. `flatten()` additionally scans plain cargo first and rewrites only
rows containing internal-tuning callbacks, rather than running a data.table
callback over every parameter when there is nothing to detach.

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
passed both the focused native/compatibility selection and the full package
suite under the isolated R 4.6.1 toolchain.

## Validation evidence

The native bulk-quantile tests cover forced registration, exact endpoint and
storage behavior, randomized agreement with individual built-in Domains,
reordered subsets, integer matrices, normalized data frames, zero rows,
`ParamSetCollection`, selected and unselected custom Domains, `ParamUty`, public
diagnostics, corrupt storage, nonmutation, result-name ownership, and GC torture.
The bulk slice passed both the focused characterization/native/regression
selection and the full package suite under the isolated R 4.6.1 toolchain. The
subsequent result-name ownership hardening and its new regression were then
rebuilt and rerun through the dedicated and complete focused selections.

Development iterations passed strict GCC and Clang builds, focused tests, GCC
`-fanalyzer`, per-translation-unit Clang Static Analyzer reports, exhaustive
cppcheck, forced-registration auditing, and ELF export and hardening checks.
These intermediate results are not release evidence. The frozen release
workflow in `AGENTS.md` and the retained run receipts described in
`design/validation.md` are authoritative for any release claim.

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

## Migration sequence

1. Freeze structures, serialization, errors, aliases, callbacks, and consumer
   assumptions with characterization and differential tests.
2. Introduce registration, checked R utilities, native type metadata, ID
   filtering, and cached static properties.
3. Move built-in Domain checks, sanitization, and quantile mapping, retaining
   S3 fallback for unknown classes.
4. Move ParamSet construction, values, scalar checks, and dependency checks.
5. Replace row-list `check_dt()` with one column-oriented native pass.
6. Move design generation/masking and union/collection translation after their
   aliasing semantics are frozen.
7. Optimize TuneToken and callback-heavy paths only when consumer benchmarks
   identify them as material.

Each phase must pass package, differential, and priority reverse-dependency
tests before the next representation change.
