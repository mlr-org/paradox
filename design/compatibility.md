# Paradox 2 compatibility contract and exceptions

## Preserved surface

Paradox 2 treats the following as compatibility requirements even where older
documentation called the representation internal:

- exported functions, S3 methods, R6 generators, inheritance, class vectors,
  active bindings, public method names, and `$new()` constructor forms;
- shallow/deep clone and live reference behavior;
- `ParamSet$new()`, `ps()`, `psc()`, `c.ParamSet()`, `ps_union()`, and
  `ps_replicate()`, including ordering and duplicate-name diagnostics;
- named output types, names, order, and attributes of all parameter property
  bindings;
- mutable values, tags, dependencies, constraints, and transformations;
- the `$values$x <- value` idiom, unset-via-`NULL`, sanitization, required tags,
  TuneTokens, dependencies, and presence modes;
- `$get_values()` forcing and error priority: type/check validation, one values
  and dependency snapshot, sequential Condition dispatch, TuneToken filtering,
  required diagnostics against original names, then sequential class/tag
  filters against live parameter metadata;
- public data.table-shaped output schemas and list-column types;
- immediate `data.table::set()` and `:=` updates on temporary `$params` views,
  while the read-only active binding and underlying ParamSet remain unchanged;
- `ParamSetCollection` live delegation to children, with detached `flatten()`
  and union results;
- collection value getters preserve prefix/postfix and nested order, sibling
  reuse, explicit named `NULL`, named empty results, and fresh mutable output
  shells while retaining opaque leaf references;
- saveRDS/readRDS and serialize/unserialize behavior;
- the established readable shapes of `.params`, `.values`, `.tags`, `.deps`,
  `.trafos`, and collection `.sets` for priority consumers.

Exact historical error text is characterized where consumers test it. New
native-only argument corruption may use a clear package error instead of
attempting to reproduce undefined R-level behavior.

## Observed constructor surface

A source-level scan of the retained maintained consumer corpus found 8,481
short-form constructor calls in 968 parsed R files, with no parse failures:
2,336 `p_dbl()`, 2,061 `p_int()`, 1,688 `p_lgl()`, 1,312 `p_uty()`, and 1,084
`p_fct()` calls. This is the empirical compatibility surface for constructor
representation and ID behavior, rather than only the examples in paradox.

Tags occur in 77.4% of those calls. Literal character levels occur in 97.7% of
the factor calls, and two-to-four-level literals alone account for 78.0% of
them. Zero-argument calls and calls made only from safe scalar or short
character-vector values cover roughly 55% of the complete corpus. Reaching
about 90% would require rendering lists and function literals, while 14.4% of
calls contain a dynamic expression, function, or list somewhere in the call.

The native printable-ID encoder therefore optimizes the common unqualified,
single-line atomic grammar and fails closed to `deparse1()` for dynamic
language, lists, functions, dependencies, transformations, marked strings, or
unsupported option-sensitive numbers. The R 4.6 standalone numeric constructor
has an additional exact round-trip gate for common fractional doubles, while
long or multiline calls still fall back. Standalone `p_fct()` remains on its R
constructor because conservative native admission did not improve its
representative common workload. These boundaries capture material real
workloads without creating a second general R deparser whose subtle differences
would be more damaging than the remaining R cost.

## Allowed breakage

- Undocumented writes that replace private tables with malformed schemas are
  unsupported. Read access and the known, well-formed mutations used by current
  priority consumers remain covered.
- A third-party Condition method called by native `$get_values()` receives the
  same `cond` and `x` values, S3 dispatch, row order, and propagation and
  priority of callback errors and ordinary side effects, but the former R
  loop's callback language is not emulated. In
  particular, `substitute()`, `sys.call()`, and `parent.frame()` observe a
  generic call with literal operands from the paradox namespace instead of the
  expressions `cond` and `values[[p2id]]` in the R method frame. Extensions
  that inspect or mutate that caller frame must use the retained R path through
  a ParamSet subclass; built-in Conditions and value-oriented third-party
  methods are unaffected.
- New third-party Domain subclasses use the retained S3 fallback and are not
  promised native performance.
- Detached collection constraint and transformation wrappers preserve callback
  values, names, calls, ordering, and errors, but their private closure cargo
  now contains compact named child-state carriers rather than cloned
  `ParamSet` objects. Code that introspects or mutates those undocumented
  closure internals is outside the compatibility contract.
- Registering methods for a new Domain class remains supported, but replacing
  paradox's own `domain_check`, `domain_sanitize`, or `domain_qunif` methods
  for the built-in `ParamDbl`, `ParamInt`, `ParamFct`, and `ParamLgl` classes
  is outside the extension contract. Canonical built-in rows may use the
  native implementation without consulting a replacement S3 registration.
- Unlocking, replacing, and relocking paradox's hidden `.__*` namespace
  helpers after package load is unsupported. Generated public R6 wrappers,
  their formals, owning enclosures, and subclass overrides are authenticated
  extension surfaces; mutation of locked implementation-only namespace
  bindings is not. No downloaded consumer relies on such mutation.
- Accidental behavior that is demonstrably erroneous can change with a NEWS
  entry and regression test.
- The process-local tag and protected payload inside data.table's
  `.internal.selfref` are not comparison targets. Returned data.table objects
  do, however, carry a valid self-reference: downstream code performs row
  operations and compares null transformations with `identical()`, and a
  class-only facade is observably repaired on first use. Public class, column,
  row-name, key/index where documented, attribute order, and interoperability
  behavior are covered.

## Intentional bug fixes identified before the native migration

- Empty settings must still honor `presence = "all"` and `"required"`.
- `domain_qunif()` must reject input lengths that cannot be distributed over
  the Domain rows.
- A ParamSetCollection child transformation runs exactly once and receives the
  correct callback signature.
- ParamSetCollection value assignment reaches a child subclass's public value
  binding instead of writing an inherited private slot that the subclass may
  not use. In particular, assignments to miesmuschel `ParamSetShadow` children
  update their shared origin and are visible through the collection.
- A ParamSetCollection child constraint receives the child's unprefixed values,
  not the full prefixed collection values.
- Strict ParamSetCollection checks consult live child constraints instead of an
  unused inherited private field.
- TuneToken validation no longer consumes caller RNG state or changes its
  result with the caller's RNG algorithm and seed.
- `ParamSet$ids(tags = character())` returns a typed `character(0)` instead of
  `NULL`, and overlapping `any_tags` matches are deduplicated in original
  parameter order.
- Grouped double sanitization clamps each value against that parameter's own
  bounds instead of recycling another row's bounds.
- One-sided and fixed infinite double Domains expand their accepted interval
  without producing `NaN`, and defined infinite quantile endpoints remain
  representable.
- `ParamSet$subset()` can replicate an ID arbitrarily often. The former
  data.table assembly failed with an `allow.cartesian` error once repeated IDs
  with multiple tags crossed data.table's join-size heuristic, even though the
  requested subset was well defined.
- Empty dependency tables are per-instance mutable state. Attaching a
  data.table secondary index to one parameter set no longer contaminates an
  unrelated later instance through R6's shared class-level default.

Each fix has an isolated regression test. TuneToken validation retains the
ten-point plausibility check with a package-owned deterministic stream; it
restores the exact incoming RNG kind and state on success and failure.

## Release gates

Priority 0 and 1 consumers in `compat/reverse-dependencies.tsv`, plus the active
mlr3 book and website examples, are hard gates unless a failure is proven
unrelated to paradox and recorded. Priority 2 packages probe broad external
usage. Priority 3 Suggests-only packages are best-effort where their unrelated
system stacks are unavailable.

Every reverse-dependency failure that reveals a reusable assumption first gains
a package-level regression test. This keeps future native work from depending
on repeatedly running the full ecosystem to rediscover the same contract.

Release convergence freezes discretionary performance changes. The internal
split of the literal `ps()` native constructor into bounded planning and row
construction helpers is not a compatibility exception: it preserves the same
admission, evaluation, fallback, and object-ownership boundaries while allowing
the bounded static analysis to complete. Final unit, differential, consumer,
runtime, memory, documentation, and benchmark gates all consume one frozen Git
candidate; a behavior-affecting source change reopens those gates rather than
borrowing evidence from the earlier candidate.
