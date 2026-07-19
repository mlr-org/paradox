# Paradox 2 validation policy

Validation maximizes confidence per wall time. It must be source-bound and
adversarial without rebuilding the world or replaying complete suites after
each small fix. This policy replaces the superseded candidate's fixed
file/count matrix.

## Evidence classes

### Development diagnostics

Parse checks, strict compilation of changed translation units, focused tests,
profiles, and local downstream experiments guide implementation. They may use a
dirty worktree and disposable libraries. Record the source state and command,
but do not call them release evidence.

### Candidate evidence

Release claims consume one clean immutable full Git ref. Every retained stage
binds its source archive, ref, commit, tree, toolchain/dependency identities,
commands, logs, results, and artifact manifests. Evidence for older package
bytes is never relabeled or carried forward.

Toolchains, package downloads, dependency libraries, reference sources,
headers, analyzer runtimes, and content-addressed consumer installations may be
reused when their authenticated byte-affecting keys match. Each distinct
R/compiler/instrumentation stage gives the frozen candidate one clean full
source build and shares that immutable installation across all tests or rows in
the stage. Gates using the same ordinary runtime/profile may share the exact
authenticated candidate installation; a different runtime or sanitizer
profile receives one new clean build. Development object/DSO component reuse
never substitutes for that build, and semantic results are rerun only for the
gates affected by changed bytes or inputs.

## Inner-loop sequence

For a coherent edit batch:

1. run `git diff --check`, parse changed R/test files, and audit registrations;
2. compile only changed C files with the release C17 warning set;
3. copy the package source to a stable disposable stage, excluding `.o`/`.so`;
4. install that stage once into an absent library;
5. run every directly affected test file in one batch and collect the complete
   failure set;
6. fix the shared cause and rerun affected files;
7. once affected files converge, run the complete Paradox unit suite once.

Stop at the cheapest failing layer. Do not start a full check, consumer corpus,
documentation build, memory analyzer, multi-R matrix, or benchmark while a
focused contract test is red. Do not rerun a full suite merely to discover the
next single failure; retain the reporter output and diagnose all related
failures first.

Never build from a live source directory concurrently modified by another
worker. Stable copied stages prevent mixed object/source bytes and allow all
tests in a batch to share one candidate installation.

## Parallel resource policy

Use `scripts/environment/resource-jobs` immediately before a parallel wave and
retain its report. Parallelize independent outer tasks—test files, runtime
stages, or consumer rows. Within each admitted worker set make/CMake/Cargo,
testthat, `parallel`/`future`, BLAS, and OpenMP pools to one unless a reviewed
test specifically verifies a bounded worker contract.

The outer ceiling is lowering-only and memory-aware. Leave the resource
helper's reserve untouched so the controlling process and OS are not OOM-killed.
Wait for all siblings, retain each exit status/log, and let one parent aggregate
and seal results. A wave failure does not discard successful row artifacts.

## Required package tests

The discovered suite, rather than a stale numeric count, must cover these
families.

### Capsule and graph

- exact v1 tag/schema/field validation for every node kind;
- malformed types, lengths, names, attributes, indices, arithmetic, and graph
  edges error without crash;
- BASE, empty/nested/shared COLLECTION, and live SHADOW behavior;
- path-cycle rejection, including collection-to-shadow-to-origin cycles;
- native collection add rejects pre-existing/proposed cycles and corrupt child
  graphs before commit, preserves the old core on every failure, and detects a
  generation change anywhere in either admitted graph;
- shallow/deep clone and serialize/unserialize topology;
- detached equality of every semantic field, BASE callbacks, COLLECTION
  children, SHADOW origins, independently built equivalent DAGs, and
  shared-versus-duplicated topology, plus active-path cycle rejection;
- atomic replacement, detached old snapshots, and reentrant generation conflict.

### Closed semantics

- all five Domain kinds and every operation on each supported kind; constructor,
  ParamSet, and ObjectTuneToken tests demonstrate that the sole canonical
  built-in Domain-row owner admits kind/storage, cargo, grouping, bounds,
  levels, defaults, tags, requirements, initialization, and
  special-value/transformation combinations. Object-token Domain coverage
  admits only bounded value-producing built-in Domains, rejects unbounded
  `ParamUty` and zero-level `ParamFct` tuning ranges, and tests
  opaque-leaf identity through a bounded typed Domain. Structural ALTREP/S4
  rejects include every outer `special_vals` list; typed special leaves reject
  ALTREP, typed S4 special/default/init
  matching is pointer-only, and ParamUty opaque S4 leaves retain identity with
  base-`identical()` special membership as the sole no-dispatch observation;
- CondEqual/CondAnyOf admission, mutation/detachment, evaluation, formatting,
  and unknown-kind rejection; standalone evaluation covers `NULL`, all four
  supported atomic families, names, stable ALTREP operands, separate operand
  snapshots under reentry, and deterministic rejection of class, dimensions,
  other attributes, S4 structure, incompatible families, and malformed shells;
- additive ParamSet-family subclasses work, while ParamSet core overrides and
  private replacement are rejection/no-crash cases rather than
  fallback-success cases; documented Sampler subclasses remain executable;
- all five exact TuneToken class/content shapes, ordinary scalar-name
  normalization, current serialization, and explicit `$search_space(values=)`
  enter one native exact snapshot boundary; subclasses, extra/reordered fields,
  classes or attributes, S4 structure, malformed calls/content, and deep/cyclic
  metadata reject before arbitrary traversal, and cold conversion switches only
  over admitted built-in kinds. Exact BASE ParamSet content is accepted while
  COLLECTION, SHADOW, and additive subclasses reject; a genuine-private/core
  shell alias is safe without method dispatch and no literal creator-provenance
  claim is tested.

### Values and callbacks

- named assignment, scalar assignment, unset, explicit named NULL, ordering,
  filtering, tags, required values, TuneTokens, presence, and sanitization;
- the public `assert_values` shell policy selects the checked/unchecked native
  store and remains stable across clone, serialization, and equality;
- dependencies, constraints, individual/extra transformations, aggregation,
  internal tuning, and ParamUty custom checks;
- native tag and dependency projection/replacement/append own detached state,
  snapshot Conditions, validate feasible RHS values, reject malformed tables,
  route visible Shadow dependency append, and detect mutation during callbacks;
  native BASE callback setters retain established formal admission and atomic
  replacement;
- aggregation, disabling, and conversion internal-tuning operations capture
  cargo/translation/Domain/owner-value state before callbacks and commit only
  through native mutation; flattened `cargo` closures are rebound to detached
  IDs after native flattening, preserve documented lexical behavior, and
  cannot act as an alternate capsule/check/callback-selection engine;
- exact-TuneToken search-space conversion consumes one rooted native
  token/Domain snapshot, replaces live ParamSet candidates with sealed single-
  use BASE subset capabilities before callbacks, uses closed built-in switching,
  restores RNG state around callback-dependent plausibility sampling, and has no
  R/native or S3 fallback path; explicit values cover ordinary non-ALTREP and
  names/class-only S3 named-list containers without dispatch and reject ALTREP,
  S4, and other attributes;
- ParamSet Object-token checks retain rooted `{shell, private, core}` receipts
  across callback/allocation/finalizer pressure; public checking reauthenticates
  after callbacks and checked assignment performs the final nonallocating scan
  immediately before an all-or-none commit;
- malformed exact-token/Domain structure raises rather than entering the
  ordinary character check-diagnostic protocol for infeasible values;
- the strict `check_dependencies()` named-list boundary, unknown IDs with and
  without dependency rows, first-diagnostic ordering, TuneToken edge skipping,
  and BASE/COLLECTION/SHADOW graph traversal without child-method dispatch;
- direct checked/unchecked `$values <-` rejects an outer ALTREP before length,
  names, or element observation and natively canonicalizes the Paradox-1 empty
  spellings (`NULL`, an ordinary attribute-free zero-length atomic/expression
  vector, or an accepted empty list container); only `set_values(.values=)`
  exercises the one-snapshot shell exception;
- transformation inputs/results require ordinary non-ALTREP/non-S4 list shells,
  including unnamed BASE output and named collection output cases; documented
  ordinary data-frame inputs and stable semantic ALTREP leaves/columns remain
  admitted;
- scalar and data.table constraint-only calls reuse the native graph/point/
  constraint kernels; table tests cover all-rows-before-constraint-callback
  validation (including ParamUty checks during Domain admission), once-per-row
  order, stable input admission, one immutable constraint snapshot,
  reentry/mutation isolation, BASE/COLLECTION/SHADOW graphs, and scalar
  non-missing logical callback admission;
- callback order, values, warning/error propagation, reentry, nested mutation,
  operation snapshots, and exactly-once execution;
- collection and shadow live semantics, including hidden-value preservation and
  unprefixed child callback values;
- exact BASE-Shadow `{callback, hidden_values}` constraint plans, hidden-first
  manual merge, classed visible-list admission without S3 dispatch, opaque leaf
  identity, callback-once behavior, and scalar non-missing logical admission;
- live collection callbacks plus detached subset/flatten and Shadow-origin
  adapters all enter the same native evaluator family, use capsule-selected
  callbacks, reject malformed child output/constraint cardinality, and do not
  honor a core-method override as another execution path; their merge tests
  cover retained/unknown input order, callback-plan/result order, omission, and
  collisions.

### R object boundaries

- one-pass native materialization of stable/base ALTREP semantic vectors under allocation,
  finalizers, interrupt, and reentry; hostile state-changing custom ALTREP
  across prior R-side representation capture may reject or yield its one native
  snapshot; Paradox must neither retry nor treat captured printed
  representation as semantic authority, and its own code must not crash or
  corrupt memory; typed Domain ALTREP special leaves reject before observation,
  typed S4 specials match only by pointer identity, and ParamUty opaque leaves
  are not materialized except for base-`identical()` special membership;
  structural configuration/search/trafo and ParamSet-`params` lists, table/row/
  Domain/Condition/token/capsule shells, Domain cargo/interpreted cargo entries,
  dimnames, class/name vectors, and list metadata reject ALTREP/S4, except for
  the explicit one-snapshot `set_values(.values=)` merge boundary;
- detached public data.table facades with valid self-reference and no capsule
  aliasing; no internal data.table state; documented ordinary data.frame/
  data.table inputs accept stable admitted semantic ALTREP columns while their
  shells and structural metadata remain ordinary;
- current serialization and explicit upgrade of CRAN Paradox 1.0.1,
  shared/nested graphs, callbacks, both pinned `mbo_config` fixtures, and
  rejected legacy extensions;
- constructor representation, Design, sampler, subset/flatten/union, and
  ordinary edge diagnostics.

Every consumer-discovered failure adds the smallest internal regression that
would have caught it before the downstream fix is accepted.

Complete unit evidence sets `PARADOX_MBO_CONFIG_ROOT` to the authenticated
pinned checkout's `common/` directory, where `mixed_search_space.rds` and
`numeric_search_space.rds` reside. Leaving it unset is a development-only skip;
pointing it at the checkout root is a harness error, not a missing-fixture
allowance. The native release driver verifies the clean checkout against the
source snapshot's exact repository revision and retains both fixture hashes
before exporting this path to isolated test workers.

## Native build and API gates

The frozen candidate must pass:

- strict GCC and Clang C17 builds with the repository's highest warning set and
  warnings as errors;
- fixed-arity registered-routine inventory, dynamic lookup disabled, direct
  probe for every entry, and no unregistered native symbol use;
- ASan and UBSan direct hazard/probe coverage;
- pinned R-header compilation against every supported source version;
- no forbidden private data.table API or unledgered/unsupported R API symbol;
- exact authentication of `environment/r-api-exceptions.tsv`: the sole
  exception is one declared/exported `Rf_findVarInFrame` occurrence/path in
  `src/r_api_compat.c` for R < 4.6, with `PROMSXP` rejected; R >= 4.6 must use
  the documented experimental API `R_GetBindingType`. Raw-token counts and
  pinned-header compilation are verified; the R 4.3--4.5 runtime DSOs must
  contain the legacy symbol, while
  the current-R DSO audit must prove it absent for R >= 4.6. The symbol is not
  CRAN-allowlisted for the supported pre-4.6 build path. No public non-forcing
  classifier exists on R 4.3--4.5, and a forcing R-level `substitute()`
  workaround is not accepted as an alternative.

Primary drivers are `scripts/native-check` and
`scripts/check-r-api-compatibility`. Use their current `--help`; their retained
inventories must be generated from the candidate rather than copied historical
counts.

## Real R runtime matrix

`scripts/test-runtime-matrix` runs the exact candidate on repository-local R
4.3.3 and R 4.5.2; development validation also uses local R 4.6.1. Each stage
has a fresh candidate library, builds/installs Paradox once, runs the complete
supported test inventory, audits DSO symbols, records package/compiler/session
identity, and seals the source/build/library/log tree.

R 4.3.3 receives only the SHA-256-authenticated cached data.table 1.18.4 source
overlay before Paradox is built. This is not a reason to skip tests or accept
1.17 behavior. R 4.5.2 and development R resolve 1.18.4 directly. The two
runtime stages may run concurrently when the resource report admits two outer
workers; nested work stays at one.

No fixed “57 of 79 files” or expected skip count is a contract. Exclusions must
be narrow, behavior-based, documented, and validated against the discovered
current inventory. A test removed because its behavior is intentionally no
longer supported is deleted/replaced, not indefinitely excluded by filename.

## Differential validation

Compare the exact candidate against the pinned upstream Paradox-1 baseline for
ordinary documented inputs. Normalize implementation-only frames and messages
only where the compatibility document permits it. Every difference is one of:

- a reviewed major-version contract change;
- an intentional bug fix with package regression and NEWS entry;
- a defect to repair before release.

Do not maintain a general whitelist based on hashes from the old candidate.
The differential inventory must include constructors, domains, checks,
values/dependencies/transformations, collections, Design, samplers,
serialization/upgrades, exact package-built and forged/subclassed TuneTokens,
and common consumer call patterns.

## Downstream validation

Run in increasing cost:

1. focused bbotk additive-subclass/public-sets bridge;
2. focused miesmuschel official-Shadow bridge;
3. mlr3mbo and other priority-zero consumer tests;
4. priority-one CRAN/Bioconductor reverse dependencies and maintained mlr-org
   repositories;
5. active book/gallery/website/cheatsheets and serialized configurations.

Use the exact local bridge commits intended for PRs and one authenticated
candidate installation. Content-addressed consumer package installations are
reused; independent rows run in admitted outer waves. Mine all failures before
changing source. Very old repositories that neither import nor call current
Paradox are recorded but not made blockers.

Remote write access is unavailable to agents. Successful local branches are
handed to the user with manual push/PR commands; CI is accepted only after the
user publishes the exact reviewed commits.

## Memory and adversarial validation

After package and focused consumers are green, run:

- deterministic GCT/gctorture direct routine and allocation-hazard probes;
- Valgrind under the dedicated unoptimized/instrumented local R and pinned
  package closure;
- bounded rchk/bcheck plus maacheck/fficheck inventory;
- ASan/UBSan builds;
- corrupt capsule, malformed graph, callback reentry, long-vector arithmetic,
  semantic ALTREP allocation/finalizer, structural ALTREP/S4 rejection,
  pointer/opaque special membership, interrupt, and serialization fuzz-style
  tests.

`scripts/memory-check` consumes the exact source/archive from a passed native
run and does not rebuild examples, vignettes, or the full functional corpus in
each analyzer mode. Analyzer-specific probes cover every registered routine and
reviewed hazard. Valgrind/rchk are serial memory-heavy stages and run only after
the resource helper admits one process and retains its exact report. Valgrind
requires a 16-GiB working-set allowance plus at least a 16-GiB host reserve;
rchk requires its enforced 20-GiB analyzer address-space allowance plus at
least the same host reserve. Valgrind is not constrained with `RLIMIT_AS`,
because its shadow mappings make virtual address space a misleading OOM proxy.
Both reports are sealed as mode evidence and independently replayed.

The retained verifier rescans semantic analyzer output, source/DSO identity,
commands, limits, and manifests; the presence of a log file or zero process
status alone is insufficient.

## Package, documentation, and portability gates

For the exact candidate run clean package checks with Suggested packages and a
depends-only configuration, examples, vignettes, manuals, and migration docs.
Then exercise the active pkgdown/book/gallery/website/cheatsheet workloads and
both serialized `mbo_config` upgrades.

Windows release x86-64 and real macOS Apple-silicon ARM64 CI must check the
exact candidate source. Each workflow step and its final completion check must
propagate R errors and nonzero status; a green wrapper around a failed R command
is a harness defect. Retain job/run/source identities and artifacts.

Portability requirements are detailed in [`portability-ci.md`](portability-ci.md).

## Benchmark gate

Benchmark only after correctness freezes. Use paired baseline/candidate runs on
an otherwise idle host with raw distributions, warmup, stable CPU/memory
conditions, and representative downstream workloads. Include constructors,
`check`/`check_dt`/`check_dependencies`, values, params/domains/dependencies,
subset, collections, live Shadow constraints and read/write paths, Design, and
samplers.

Profile first. Optimize measured R-boundary, repeated validation/snapshot,
lookup, allocation, or per-row overhead while keeping portable C17 and readable
ownership. After each optimization run affected correctness tests; after the
performance source freezes rerun the final memory/portability evidence once.

## Acceptance and replay

Each release stage records a unique run ID and exact ref/commit/tree, and seals
its complete required artifacts. Verifiers are read-only and may reuse
authenticated inputs, but cannot transform evidence for one DSO/source into
evidence for another. A later source change reopens only the gates it can
affect; before the first contract-first candidate, the complete matrix is
necessarily fresh.

The active accepted run IDs and candidate hashes belong in
[`release-2.0.0.md`](release-2.0.0.md). Until that ledger says `accepted`, no
collection of partial green diagnostics is a release authorization.
