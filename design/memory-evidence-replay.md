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

### 2026-08-03 active-candidate acceptance

The active immutable candidate is
`refs/paradox-release/candidate-20260803T131049Z`, commit
`a0a9ff3e05b535068392e0c20442ad9794f3b824`, tree
`49079e12a816542fe8d8d6a0a2290a757a558b41`. Replayable release-core donor
`release-candidate-a0a9ff3-r1-native-release-a001` passes independent source
validation. Its source-manifest, source-tree, modes-tree, completion, result,
and source-archive hashes are
`f9bc153e50a594902ecd67f240f2be274c08b4f9bde19ca79f9a19277b892dc3`,
`3b6246c007039a4a07a2132d521c64971155ee3feeddcbcdface25d4f309b1aa`,
`d7231b895a8e41ddce9d7f679b58f96c1e4c062484b97a02fb234cbbb16eccf1`,
`2a45e572c4f83b1e800927642f554db1d7a5e80aece9c6ca8ed2d7e57ca0101c`,
`1a2b43a43a03c8b87f9483d5a2b178056c486d678a92027fe84837735e328551`,
and
`0ed1d9bdb8750cd064971c25a0c72e1106eee976b62893ac2c2947f9a9ab9b62`.

Combined run `release-candidate-a0a9ff3-memory-r1` passed GCT, Valgrind,
bounded rchk, and independent replay. Completion, result, memory-source-tree,
modes-tree, validator, and archive hashes are
`6e38ed22a4633d9145b5a5176a12f22c097947e3ea56fa8e9990c2e4b9036708`,
`80b582512c3b0b28c60cd5c0745bacf9545e32da31562b1865565b085f6af370`,
`577d57005e12ab21ad33cf06ae193790ff73620de81a21cff2f458ace6a74471`,
`a1bc3685a8456f7e42467c5c92e6ed2db2c18eefeb41f3dd7c9ddf87b7309363`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`0ed1d9bdb8750cd064971c25a0c72e1106eee976b62893ac2c2947f9a9ab9b62`.
GCT covers all 111 routines and four hazards. Valgrind has zero errors,
definite/indirect/possible losses, or suppressions; eight files and 141 blocks
produce 1,028 expectations, with 1,018 passes and ten reviewed skips. Bcheck
analyzes 1,305 functions and 201,585 states and exactly matches 116 blocks,
397 UP, and 30 PB. Raw bcheck, semantic, empty maacheck, and fficheck hashes are
`3afe281aa9aeb59a3ba468b60119b767ece80b0bd56ec38068b2d890bfc068cd`,
`c5a7396c584257e309d4738bac5dae13934764801c17612a683779834719a5fa`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`742b60254990e82b45b4e33ead0b911946ade3f1751fb99f3401d90959bb6c88`.
Policy, blocks, and rationales hash to
`3ca2416f9d0920850431381d35fc0ce00f8d2fe5b4e08fe98d85f86e47dfd806`,
`5ae729f8d3b07bd471050a81f793a862fda59e1a4457483fb66618e27acc250a`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.

### Historical 2026-08-02 replacement-candidate discovery

The superseded immutable candidate was
`refs/paradox-release/candidate-20260802T183338Z`, commit
`f27776ee1eca5d964945aa53d14d0ec7947dccbf`, tree
`95012f6ae771b04fe91afcf30f8df3b78daa701a`. Its replayable release-core
native donor is `release-candidate-f27776e-r1-native-release-a001`; the source
manifest, source tree, modes tree, completion, and result hashes are recorded
in the active release ledger.

Discovery `release-candidate-f27776e-rchk-discovery-r1` completed every
analyzer and failed only at the intended stale-policy comparison. Bcheck
analyzed 1,305 functions and 201,585 states; 116 blocks contain 397 UP and 30
PB diagnostics. Maacheck is byte-empty and fficheck records all 111 routines
and one registration call. Three independent reviews found no defect. After
normalizing source-line and compiler suffix movement, the only addition is
`scan_unchecked_value_leaves()` losing the address-taken `classes`
out-parameter. Its protected transaction snapshot owns the complete leaf and
metadata, and the alias is not used after the bounded scan, so the existing
`ADDRESS_TAKEN_MODEL` rationale applies.

Raw bcheck, semantic bcheck, maacheck, and fficheck SHA-256 values are
`35d5ad41f7fe4bcbe62d8848759dd694e00be29bf06c2109852d608d4ae304f7`,
`c5a7396c584257e309d4738bac5dae13934764801c17612a683779834719a5fa`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`742b60254990e82b45b4e33ead0b911946ade3f1751fb99f3401d90959bb6c88`.
The exact reviewed-assignment input SHA-256 is
`a217e24c7dc5d179aa9e7353af7e0d940c12c726f6d40a1dbab995c065acc698`.
Current and retained generators produced byte-identical policy directories,
and each validator accepted the other's output. Policy, blocks, and rationale
catalog SHA-256 values are
`3ca2416f9d0920850431381d35fc0ce00f8d2fe5b4e08fe98d85f86e47dfd806`,
`5ae729f8d3b07bd471050a81f793a862fda59e1a4457483fb66618e27acc250a`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
Attempt `release-candidate-f27776e-memory-r1` passed GCT and stopped in
Valgrind structure preflight before Valgrind/rchk execution. Its fresh
toolchain receipt differed from the sealed receipt only at the top-level
`lib` directory mode, `0777` versus `0775`. The culprit was a self-test
directory symlink to that live tree combined with R's forced recursive cleanup,
which applies `chmod` before unlink and follows the link. The fixture now uses
an explicit library search path for its copied scratch Git and creates no
external link; its focused regression confirms the live mode stays unchanged.
The exact sealed mode has been restored. R1 is diagnostic only.
The repair is frozen at package-facing-source-identical ref
`refs/paradox-release/validation-tooling-20260802T203920Z`, commit
`c20c1a3e7bb459757d57038c6eaa89daa6d9082c`. Donor
`release-candidate-f27776e-native-policy-r2` passed all static/focused modes
and independent validation. Combined run
`release-candidate-f27776e-memory-r2` then passed GCT, Valgrind, bounded rchk,
and independent replay. Its completion, result, memory-source-tree,
modes-tree, independent-validator, and source-archive SHA-256 values are
`62e81f915f39b9500a5a964f99cc2d384765f96d6571185584fe1fe0660b44dd`,
`b23c6bb8a0d521ea4366d3914eb24529454fea5b2d00a265ab3e36373d499f1d`,
`18969ef1367133958dc4271c8dc48468f100b2853458e0af07be04751d515877`,
`98943dfffcb27025d3d098052ddf87c0cadc0cff5b06866aea1fc580612d7344`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`0da3db695f7182fbf6ce436d52f12f3de7f29cb109b582682054484b3b7e5177`.
GCT covers all 111 routines and four hazards. Valgrind reports zero errors,
losses, or suppressions; eight files, 141 blocks, and 1,028 expectations
resolve to 1,018 passes and ten reviewed skips. Rchk matches the source-bound
116-block, 397-UP/30-PB policy over 1,305 functions and 201,585 states.
Discovery alone is not acceptance; r2 owns the active combined-memory result.

### Historical 2026-08-01 bounded-state discovery

The superseded immutable candidate was
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
That exact policy was checked in at package-facing-source-identical
validation-tooling ref
`refs/paradox-release/validation-tooling-20260801T112017Z`, commit
`0507daff5cbd208781172b0f35ad405975c342f9`, tree
`d36e26e5881ba443e09de79c5990f0fcb6c34f84`. Donor
`release-candidate-4e549f3-native-policy-r1` passed all six static/focused
modes and the independent source-run validator. Combined run
`release-candidate-4e549f3-memory-r1` executed GCT, Valgrind, and bounded rchk
successfully; its independently replayed executable evidence is clean.

A separate evidence audit nevertheless found a false literal in its Valgrind
scope receipt. The selected analyzer corpus contains eight test files but ten
reviewed `skip_on_cran()` scopes. The trusted ledger correctly records
`files=8`, `passed=852`, and `not_cran_scopes=10`; the scope receipt incorrectly
said “eight ... scopes.” This does not indicate a package, memory, or analyzer
failure, but an internally false receipt cannot own release acceptance.
`scripts/memory-check` now omits the brittle count and explicitly binds source
and count to the trusted ledger; the independent validator requires that exact
corrected line. This package-excluded harness change invalidates current-
harness replay of r1 by design. R1 remains semantically clean diagnostic
evidence, but cannot own release acceptance.

The correction is frozen at package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260801T124036Z`, commit
`3902d5bf0bedcef5a39f266978f371a8cc5640b7`, tree
`ce0b22d8276a8524efe47451d7d76a824d30031e`. Fresh donor
`release-candidate-4e549f3-native-policy-r2` passed all six static/focused
modes and the independent source-run validator. Its source-manifest,
source-tree, modes-tree, completion, and result SHA-256 values are
`adccc238e79a04ff83d3e1394fbb4fdce9e3817b270839dc1bb0a87a79bb8698`,
`f5b0f7cf3a1230938b0dd59a8910e893f2d506a6b5f0177692ab97850c0970d5`,
`c10d5aa4a68a35365175836949dab466fbe9ceb4ddf5602d5b0a05bbb102b0ea`,
`a4c812fddffc1dbd66f26ba37241b2d7d994daad10359454e10796b605a3a747`,
and
`40d934a672ac66bb221db3fca43e45889972a9e5e964774c31ed3753baaf9bb4`.

Corrected combined run `release-candidate-4e549f3-memory-r2` passed GCT,
Valgrind, bounded rchk, and independent validation. Its completion, result,
memory-source-tree, modes-tree, independent-validator, and source-archive
SHA-256 values are
`5bdeda5338c840d73301a6b08f142692517c933476a5f1e77edab877bda7bb62`,
`508dadf355230d03ce8678c0e666b61cc2022b4892c503c0b7815beace6c0cda`,
`964d6133d7e0ac2595d8df60693bfd0062b3706d384128a174b5f49f3436a228`,
`754665b8f1383fab02b7c38ae3b357d0799ed6576e5f7cf69205ac8dbcc63c70`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`1c137df1ccc07e83b7b11a71b6e564baf2d53d9f3907a72f74fc6b08e704387d`.
GCT covers all 110 registered routines and four reviewed hazards. Valgrind's
baseline, probes, and analyzer have zero errors, losses, or suppressions; the
trusted ledger binds eight files and 128 blocks to 862 expectations, comprising
852 passes and ten exact skips. Rchk analyzed 1,288 functions and 202,140
states and matched all 115 blocks with 396 UP and 30 PB diagnostics. Raw
bcheck, semantic, byte-empty maacheck, and fficheck SHA-256 values are
`d0e55ca0b0b46a53e5f551ebe1d84786a235d74199cbd1c4093709d7bb79aac1`,
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`26bfc1718868d77f8f1d477c0d60312cfc707d1cebc8afd433079bf6f51918a0`.
This corrected combined run, not discovery or r1, owned memory acceptance for
the superseded `4e549f3` package-facing source. It does not accept `f27776e`.

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
construction-time materialization of stable atomic non-S4 typed-special ALTREP,
operation-time rejection of typed ALTREP special leaves introduced into live
Domain tables, typed S4 special
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
