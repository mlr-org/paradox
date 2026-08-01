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

The source-run validator is deliberately relocatable. Every helper it resolves
beside itself is part of one exact retained bundle: `scripts/memory-check`
copies and hashes it, and the independent memory-run validator requires the
same inventory. Root-dependent fixture/Git helpers are instead authenticated
against the active repository as explicitly specified by the source-run
validator; the two trust classes must not be conflated.

Source manifests bind exact regular-file modes as well as bytes. Snapshot
creation and snapshot replay restore modes with `use_umask = FALSE`, then
verify copied modes and source stability. A passed functional native run whose
manifest disagrees with its copied-tree receipt is not a replayable source
donor and cannot be repaired in place.

### 2026-08-01 bounded-state discovery

The active immutable candidate is
`refs/paradox-release/candidate-20260801T092108Z`, commit
`4e549f3a8994f513cee1d88d71e037c733a51531`, tree
`4f17819b92f7dfb255c8aafe501ff3e684027d67`. Its replayable release-core
native donor is `release-candidate-4e549f3-r1-native-release-a001`.
Source-manifest, source-tree, modes-tree, and completion SHA-256 values are
`2362acd1d5748792c1e7b02040c02b11da0309aef325c611c798fa62ff049e88`,
`5eb7c1e4961bb46b632d227792414c533103648a98d6cf9cc3b20956dbf06352`,
`18697cb2b06f0ebf43cebec847fc375c9833438d623951d1ad9eb7ac7c25a1a7`,
and
`8e9ad5dfa1c22d84629669833b6aa2c8cdacb71df22f30dcfe7e703412ee15b5`.

Discovery `release-candidate-4e549f3-rchk-discovery-r1` completed every
analyzer and failed only at the intended comparison with the superseded
policy. It analyzed 1,288 functions and 202,140 states. Its 115 blocks contain
396 UP and 30 PB diagnostics; fficheck reports the exact 110 routines. Raw
bcheck and ordering-insensitive semantic SHA-256 values are
`cda7598e591bcfa7b3866acdc09530d24dc1643de17fff144056b46cde76e78f`
and
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`.
Maacheck is byte-empty, SHA-256
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`;
fficheck SHA-256 is
`26bfc1718868d77f8f1d477c0d60312cfc707d1cebc8afd433079bf6f51918a0`.

Three disjoint, independently performed source-review partitions covered all
115 observed blocks and found no C defect. Their TSV SHA-256 values are
`aea265b2c31338d9b8a5fc7823238cc1b1bf5b3999b175f9d05b1f16f1bc83ae`,
`12f7b37cb6628ac9095e12ba835d0832c03f8138693428442926b38e040edfa4`,
and
`dc2aa8c138f6cfec843d6b8eabae7b13160337858e500b26c72dc6007eab2a01`;
the ordered rationale-assignment input SHA-256 is
`e30d249f9484e9b5529ac6c74916fee85a46158ed8900b8195462ec643b358cd`.
Independent generation and validation produced exact policy, block-table, and
unchanged-rationale-catalog SHA-256 values
`e2e7ccc9cc225240986bcb99fc6bd64f8b828924d4aaf0d5765a8a847c346087`,
`88363b5a2c173b378ebcdf2f6e3f05f8b7234240b9f04fc63b343871b1d21067`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
That exact policy is now checked in. Combined memory still requires a fresh
post-policy donor because the donor snapshot must contain the selected policy;
the successful discovery donor cannot seed the final all-mode run.

### 2026-07-27 provenance incident

`release-candidate-dbbdcc1-native-release-a001` completed its package work, but
its snapshot manifest recorded original worktree modes while the copied source
tree recorded umask-filtered modes. The first memory attempt,
`release-candidate-dbbdcc1-memory`, also omitted the relocated trusted
`create-offline-check-repository.R` helper and therefore stopped in source
preflight before loading Paradox or starting any analyzer. Both directories
are retained only for the bounded conclusions they actually establish.

The repair spans the two snapshot-copy helpers, the memory driver and
independent memory-run validator, and their two adversarial tests. It preserves
exact modes under a restrictive umask, retains/hashes the missing helper,
rejects its substitution or removal, and derives the complete relocated-
sibling set generically. The candidate package results remain informative, but
memory acceptance requires a fresh full native source run and a fresh combined
memory run from that replayable proof.

The first requirement is complete. Coordinator
`release-candidate-dbbdcc1-native-replay-r2` passed all four selected rows and
its full-native child
`release-candidate-dbbdcc1-native-replay-r2-native-release-a001` passes the
independent relocated source-run validator, including exact source and harness
modes. Its source-manifest, source-tree, and modes-tree SHA-256 values are
`d10b24eee410164ca28b3f456ddebf4a151d9f884ce9e648adde3f7efbdb2cee`,
`821e28ab6db88a0125e4dbea167730235096bc2f699cb33a94d0c85cc2594a3a`,
and
`6550f985a6bfc766618a95719ec638ec02988021e118c7b7f296a71df0c95b95`.
It supplies immutable source input for policy discovery. Because the resulting
policy changes the source-bound memory snapshot, final memory acceptance uses
one new static/focused native donor from the policy/harness commit.

An initial isolated rchk discovery from this donor,
`release-candidate-dbbdcc1-rchk-discovery-r1`, failed closed before analyzer
start because live capacity was 536 MiB below the reviewed 20-GiB analyzer
allowance plus 16-GiB host reserve. Retain it as capacity evidence only; do not
reduce either bound or reuse the failed run ID.

The admitted discovery
`release-candidate-dbbdcc1-rchk-discovery-r2` completed every analyzer and
retained stable pre/post source proofs. It is deliberately a failed run: the
old policy rejects the changed report after successful report-shape, status,
maacheck, fficheck, error-inventory, and semantic extraction checks. The raw
bcheck report SHA-256 is
`ddbad6140d74e96422bc4f77942f9303af9e51706005f9f95d36b3d677b011d8`;
its semantic SHA-256 is
`9135cbe02ddbd69019d7901d09bd5a7286fd24b74260e1864be0c92aeceef1d9`.
It analyzed 951 functions and 50,395 states with 83 blocks, 239 UP, and 17 PB.
Maacheck is byte-empty. Fficheck reports 82 functions and one registration
call, SHA-256
`448c7d8dae05fab9b43570ed169a4472de8ca6ca40836061642c4203c3391882`.

All changed blocks were audited against source. The four additional PB
diagnostics belong only to `build_dependent_grid()` and arise when bcheck loses
its conditional VECSXP missing-sentinel protection depth; every branch has an
exact balance. The existing five-rationale catalog is sufficient. Independent
generation and validation produced policy/block/rationale SHA-256 values
`eb1e2e9d89a27b9a43a7be87606c22b71fef636284c5c570c5f795aa2345833f`,
`d0ba3cbc6d2b536b157939fc3c6cbe2d9fcf5b2ed497a088234ee345dddf2fad`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.

## Modes

### GCT

Run deterministic `gctorture2` probes over every registered routine and hazard
family: capsule construction/replacement, graph traversal, Domain/Condition
kernels, values/checks, callbacks/reentry, stable/base ALTREP materialization,
hostile custom-ALTREP rejection/admission safety, public table construction,
ordinary non-ALTREP/non-S4 structural rejection for configuration/search/trafo
and ParamSet-`params` lists, Domain/Condition/token/capsule/internal-table
shells, cargo, dimnames, and list metadata, the shared six-ingress public-table
classifier, no prefix-induced ordinary-shell copy, ALTREP-snapshot
canonicalization, and malformed class-vector
rejection, cache-carrier disposal, row-name form/count admission (including
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
Domain-row admission, Design, fixed and dependency-aware grid construction
(including a list-valued fixed special value and inactive row), sampler,
serialization, and upgrader validation.
Rooting regressions are direct assertions, not inferred from the absence of a
crash in unrelated tests.

### Valgrind

Use the repository-local unoptimized, level-2-instrumented R and exact pinned
package closure prepared before the release run. Install the candidate once for
that R, run the same DSO-bound probe inventory and analyzer-sensitive tests,
and parse every retained log for errors, invalid reads/writes, uninitialized
use, leaks, suppression counts, and terminal summaries.

The analyzer-only testthat process sets cli's supported `CLI_NO_THREAD=1`
switch. Loading testthat otherwise starts cli's detached presentation timer,
whose asynchronous cancellation at process shutdown can nondeterministically
leave glibc's 336-byte thread-local allocation classified as possibly lost.
The timer is unrelated to test execution and Paradox, and disabling its
creation is stronger than suppressing that report: default suppressions remain
off, the suppression count must remain zero, and definite, indirect, and
possible leaks from every thread that actually exists still fail the gate.
The direct native-probe process does not load cli and retains its unchanged
environment.

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
The bcheck executable has independently authenticated finite per-function caps
of 3,000,000 states for its main and allocator-discovery analyses, both exposed
in analyzer identity and scope evidence. The 20-GiB address-space limit,
serial execution, and protected host reserve remain unchanged. Any
package-local state exhaustion is fatal; it is never admitted as a reviewed
diagnostic, and recurrence requires source simplification rather than an
automatic cap increase.

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
data.frame/data.table inputs with suffix-classified top-level ALTREP shells,
ordinary additive presentation classes, malformed class vectors, ordinary
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
