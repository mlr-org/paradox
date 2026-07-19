# Memory-analysis evidence and replay

Memory safety is a release contract, not a best-effort diagnostic. This file
defines evidence for the contract-first candidate. Retained analyzer runs for
the superseded compatibility-first source remain historical and may not be
relabeled.

## Source binding

`scripts/memory-check` consumes the exact immutable source/archive identity and
eligible native-run inputs from a passed candidate run. A mode may reuse an
existing candidate installation only when its complete R/compiler/
instrumentation profile is identical. An instrumented R, sanitizer, or other
distinct profile receives one clean source build and one immutable installation
shared by every compatible probe in that mode. The driver verifies the source
ref, commit, tree, archive, applicable DSO, routine inventory, harness inputs,
completion seal, and toolchain/dependency receipts before execution and again
before sealing.

Analyzer modes do not rebuild or rerun examples, documentation, or the complete
functional corpus. Those belong to other gates. They run the complete direct
registered-routine inventory, reviewed allocation/callback hazard probes, and a
bounded analyzer-sensitive test subset against one exact DSO.

## Modes

### GCT

Run deterministic `gctorture2` probes over every registered routine and hazard
family: capsule construction/replacement, graph traversal, Domain/Condition
kernels, values/checks, callbacks/reentry, stable/base ALTREP materialization,
hostile custom-ALTREP rejection/admission safety, public table construction,
ordinary non-ALTREP/non-S4 structural rejection for configuration/search/trafo
and ParamSet-`params` lists, Domain/Condition/token/capsule/internal-table
shells, cargo, dimnames, and list metadata, the shared six-ingress public-table
classifier, cache-carrier disposal, row-name form/count admission (including
one-Length/no-Elt stable ALTREP), one-shot normalization of base R's stable
public-table lazy wrapper, ownership of names before hostile Elt reentry,
zero-column data.frame row preservation, direct checked/unchecked assignment
rejection before shell observation, the sole `set_values(.values=)` shell
snapshot,
subset/collection/shadow, live and detached collection callback plans and
their native evaluator, standalone Condition operand materialization and
reentry, dependency-only checking, scalar/two-phase table constraint-only
checking, tag/dependency/callback replacement and generation conflicts, atomic
collection add with cycle/corruption paths, BASE-Shadow constraint plans and
merge callbacks, exact TuneToken snapshot admission and cold search-space
conversion (including rooted candidate receipts, final nonallocating
reauthentication, and sealed single-use BASE capabilities), shared built-in
Domain-row admission, Design, sampler,
serialization, and upgrader validation.
Rooting regressions are direct assertions, not inferred from the absence of a
crash in unrelated tests.

### Valgrind

Use the repository-local unoptimized, level-2-instrumented R and exact pinned
package closure prepared before the release run. Install the candidate once for
that R, run the same DSO-bound probe inventory and analyzer-sensitive tests,
and parse every retained log for errors, invalid reads/writes, uninitialized
use, leaks, suppression counts, and terminal summaries.

Valgrind is a serial, memory-heavy stage. Before taking the shared Valgrind
state lock or scanning its multi-gigabyte prerequisites, the resource helper
must admit exactly one process with a 16-GiB conservative working-set allowance
while retaining at least 16 GiB (and otherwise one quarter of currently
available memory) for the host and controlling process. This is intentionally
not an `RLIMIT_AS`: Valgrind's shadow-memory mappings make virtual address space
a poor proxy for resident-memory pressure. The exact admission report is mode
evidence and replay rejects a missing, malformed, parallel, under-budget, or
under-reserved decision. Valgrind never installs prerequisites or mutates shared
libraries implicitly.

### rchk

Use the authenticated bounded bcheck build/container and candidate source
directly. Retain complete bcheck, maacheck, and fficheck output. The verifier
requires the current registered routine count and reviewed bounded-state policy;
historical routine counts or block exceptions are invalid after this rewrite.

### Sanitizers and adversarial execution

ASan/UBSan builds and ordinary-R adversarial tests complement the three modes.
They cover corrupt/forged capsules, malformed table shapes, graph cycles/depth,
long-vector and size arithmetic, callback errors/reentry/nested mutation,
ALTREP allocation/finalizers, hostile state changes across R-side
representation capture and native admission, interrupts, external-pointer
leaves, exact TuneToken subclasses/extra/deep/cyclic metadata, malformed
TuneToken Domains, bounded Object-token Domain admission with unbounded
ParamUty rejection and opaque leaves exercised through a bounded typed Domain,
structural ALTREP/S4 rejection (including every outer `special_vals` list),
typed-special ALTREP rejection, typed S4 special
pointer identity, opaque ParamUty S4 leaves and base-`identical()` special
membership without dispatch in supported Domain/value operations, documented
data.frame/data.table inputs with exact-class top-level ALTREP shells, ordinary
discarded cache carriers, hostile shared-name mutation, malformed/mismatched
and stable ALTREP row names, zero-column table counts, and stable semantic
ALTREP columns, exact
BASE-only ParamSet
content, safe genuine-core aliases, candidate mutation during receipt lifetime,
sealed search capabilities, serialization, clone, and explicit legacy upgrades.
Hostile
custom ALTREP is a safety target: no replay or Paradox-caused crash/corruption.
It is not an exact representation-equivalence target.

## Replay verifier

The read-only verifier accepts only a completed run directly below
`.local/checks`, checks its exact schema and completion seal, rehashes the
retained source/mode trees once (in at most a bounded two-worker wave), and
semantically rescans analyzer output. It validates:

- exact source/archive/DSO/routine/harness identity, including the native-test
  coordinator, worker-group process boundary, individual worker, and ledger
  verifier;
- compiler, R, analyzer, package, container, and resource-admission identity;
- ordered mode status and all required probe/test ledgers;
- zero-error/zero-unreviewed-suppression Valgrind summaries;
- current bounded-rchk policy, empty required reports, and full FFI inventory;
- absence of missing, duplicate, symbolic, mutated, or unsealed artifacts.

The existence of a log, a zero process exit, or an old completion file is not
evidence. A verifier update may reinterpret the same immutable bytes only when
it does not claim new execution; it records both old and new verifier identity.

## Information economy

Run memory modes only after strict builds, the complete unit suite, supported R
runtimes, and focused downstream bridges are green. A consumer-rejected
candidate does not merit Valgrind/rchk time. Cache the instrumented R,
dependency closure, analyzer binaries/images, and reference sources by exact
authenticated inputs; never cache a candidate DSO across source changes.

Within a mode, install the candidate once and cover all hazards in one bounded
run. When a mode fails, inspect the complete report, add a focused ordinary/GCT
regression, fix the coherent cause, rerun the affected cheap probe first, and
then rerun that full analyzer mode once.

## Acceptance

Memory safety is accepted only when GCT, Valgrind, bounded rchk, sanitizers, and
adversarial tests all name the frozen candidate recorded in
[`release-2.0.0.md`](release-2.0.0.md) and their independent verifiers pass.
Any source change affecting native code, registration, state layout, callbacks,
ALTREP, graph traversal, or analyzer harness reopens the relevant evidence.
