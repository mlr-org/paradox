# Paradox 2 validation policy

Validation maximizes confidence per wall time. It must be source-bound and
adversarial without rebuilding the world or replaying complete suites after
each small fix. This policy replaces the superseded candidate's fixed
file/count matrix.

## Current validation status

The active immutable package-facing ref is
`refs/paradox-release/candidate-20260801T092108Z`, commit
`4e549f3a8994f513cee1d88d71e037c733a51531`, tree
`4f17819b92f7dfb255c8aafe501ff3e684027d67`. Its exact coordinator at
`.local/verify/runs/release-candidate-4e549f3-r1` passed all nine
`release-core` rows: all four harness gates, differential, API headers,
dual-compiler C23, native release on R 4.6.1, and `runtime-supported`. The
latter ran the complete suite at R 3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3,
and 4.5.2 with 67,109 passing assertions and 145 exact reviewed capability
skips. Together, these lanes exercise every minor series from R 3.6 through
current. Completion/JSON-summary/TSV-summary SHA-256 values are
`c492eeb65a578cbcc868e09d7c222788a524f6795328c6d0a600e84730c16406`,
`c1e5bb86dfbfb9f56d7ce64667f47d65dbd03cf72a2a33d3c27652bae68c030c`,
and
`59d95159bf1f6bdbb50f07cf1087ed0c2f0dc604d1102a81c53c5364f845f855`.

Native child `release-candidate-4e549f3-r1-native-release-a001` is replayable
and passes independent source-run validation. Its source-manifest,
copied-source-tree, copied-modes-tree, and completion-content SHA-256 values
are respectively
`2362acd1d5748792c1e7b02040c02b11da0309aef325c611c798fa62ff049e88`,
`5eb7c1e4961bb46b632d227792414c533103648a98d6cf9cc3b20956dbf06352`,
`18697cb2b06f0ebf43cebec847fc375c9833438d623951d1ad9eb7ac7c25a1a7`,
and
`8e9ad5dfa1c22d84629669833b6aa2c8cdacb71df22f30dcfe7e703412ee15b5`.

Discovery `release-candidate-4e549f3-rchk-discovery-r1` completed all three
analyzers and failed only at the intended stale-policy comparison. It analyzed
1,288 functions and 202,140 states; 115 function blocks contain 396 UP and 30
PB diagnostics, and fficheck records 110 routines. Raw bcheck and semantic
SHA-256 values are
`cda7598e591bcfa7b3866acdc09530d24dc1643de17fff144056b46cde76e78f`
and
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`.
Maacheck is byte-empty, SHA-256
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`;
fficheck SHA-256 is
`26bfc1718868d77f8f1d477c0d60312cfc707d1cebc8afd433079bf6f51918a0`.
Three disjoint, independently performed source-review partitions covered every
observed block and found no C defect.

The independently generated and validated exact policy, block table, and
unchanged rationale catalog are now checked in. Their SHA-256 values are
`e2e7ccc9cc225240986bcb99fc6bd64f8b828924d4aaf0d5765a8a847c346087`,
`88363b5a2c173b378ebcdf2f6e3f05f8b7234240b9f04fc63b343871b1d21067`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
They and the pre-run ledger used by the first memory attempt are frozen in the
package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260801T112017Z`, commit
`0507daff5cbd208781172b0f35ad405975c342f9`, tree
`d36e26e5881ba443e09de79c5990f0fcb6c34f84`. Post-policy donor
`release-candidate-4e549f3-native-policy-r1` passed all six static/focused
modes and independent source-run validation. Its completion, source-manifest,
source-tree, modes-tree, and result SHA-256 values are
`19bffadbcff762dedea96d4468a7f1481dd07b3be512d4a1fa1b24dfb81c56d4`,
`3fdf8708a0dc94e5835238611ec179e1e6dc6b7e9d9c7329043250ca321c2f02`,
`02ba4e8d2c0732d6a6e4413ea855c4257efeb6d11ed88f0acca471e3b34724ae`,
`9baec8705159a41d00e7224b379822553f6709bf64efaa9711eb7d45621b06fc`,
and
`5c116f81689bf905b4ceafbe0cbda1c1d41c4b64488f5f00e3dbea1ed5c3192e`.

All-mode run `.local/checks/release-candidate-4e549f3-memory-r1` executed
GCT, Valgrind, and bounded rchk successfully and initially passed independent
replay. Valgrind reports zero errors, lost blocks, and suppressions; its
analyzer ledger records eight files, 852 passes, and ten reviewed CRAN skips.
Rchk matches all 115 reviewed blocks and the exact 396-UP/30-PB policy.

An independent audit then found that the separate scope receipt incorrectly
said “eight ... scopes,” confusing those eight selected files with the ten
ledger-bound scopes. The execution and ledger are correct, but the false
receipt makes r1 diagnostic evidence. The package-excluded correction removes
the hard-coded count and changes the independent validator's exact expected
line. R1 remains semantically clean diagnostic evidence, but it does not own
combined-memory acceptance.

The corrected harness is frozen at package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260801T124036Z`, commit
`3902d5bf0bedcef5a39f266978f371a8cc5640b7`, tree
`ce0b22d8276a8524efe47451d7d76a824d30031e`. Fresh donor
`release-candidate-4e549f3-native-policy-r2` passed all six static/focused
modes and independent source-run validation. Its source-manifest, source-tree,
modes-tree, completion, and result SHA-256 values are
`adccc238e79a04ff83d3e1394fbb4fdce9e3817b270839dc1bb0a87a79bb8698`,
`f5b0f7cf3a1230938b0dd59a8910e893f2d506a6b5f0177692ab97850c0970d5`,
`c10d5aa4a68a35365175836949dab466fbe9ceb4ddf5602d5b0a05bbb102b0ea`,
`a4c812fddffc1dbd66f26ba37241b2d7d994daad10359454e10796b605a3a747`,
and
`40d934a672ac66bb221db3fca43e45889972a9e5e964774c31ed3753baaf9bb4`.

Corrected all-mode run `.local/checks/release-candidate-4e549f3-memory-r2`
passed GCT, Valgrind, bounded rchk, and independent validation. Its completion,
result, memory-source-tree, modes-tree, independent-validator, and
source-archive SHA-256 values are
`5bdeda5338c840d73301a6b08f142692517c933476a5f1e77edab877bda7bb62`,
`508dadf355230d03ce8678c0e666b61cc2022b4892c503c0b7815beace6c0cda`,
`964d6133d7e0ac2595d8df60693bfd0062b3706d384128a174b5f49f3436a228`,
`754665b8f1383fab02b7c38ae3b357d0799ed6576e5f7cf69205ac8dbcc63c70`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`1c137df1ccc07e83b7b11a71b6e564baf2d53d9f3907a72f74fc6b08e704387d`.
GCT covers all 110 registered routines plus four reviewed hazards. Valgrind
reports zero errors, losses, or suppressions; its eight selected files and 128
blocks produce 862 expectations: 852 passes and ten exact reviewed skips.
Rchk analyzes 1,288 functions and 202,140 states and matches the exact
115-block, 396-UP/30-PB policy. Raw bcheck, semantic, byte-empty maacheck, and
fficheck SHA-256 values are
`d0e55ca0b0b46a53e5f551ebe1d84786a235d74199cbd1c4093709d7bb79aac1`,
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`26bfc1718868d77f8f1d477c0d60312cfc707d1cebc8afd433079bf6f51918a0`.
Combined-memory acceptance is complete.

The preceding immutable ref is rejected diagnostic candidate
`refs/paradox-release/candidate-20260801T034415Z` (`fb2a37f`), commit
`fb2a37fc7d9b29d1998a8639a01e18130eaa4919`, tree
`86283a233c6de7d16c3d3bd20e682fa0ed5409c1`. Its exact
`release-candidate-fb2a37f` run passed all eight non-runtime rows, including
all harness, differential, API-header, dual-compiler C23, and native-release
gates. `runtime-supported` ran the full suite on all seven minor-series
runtimes with clean assertions. R 3.6.3 through 4.2.3 then failed result
reconciliation because one list-ALTREP capability skip lacked its reviewed
ledger row; R 4.3.3 through 4.5.2 passed the stage. The
completion/JSON-summary/TSV-summary SHA-256 values are
`9f84f7900b83671042f8ba7dc78016be1dd59f29c169e5abffb7a837a7cae8a8`,
`9ceb023f9d572f1273b1b456951b50bb07d0c8b8782098d588ed53902bcd7d6f`,
and
`9aadf3a9227779e1d4b04b5099362205b46e4091fbc5a36fcd2edf7bed1a8697`.

The omitted derived result was `materialized and rejected inputs remain safe
under forced collection`. The four affected stages stopped before the old-R
stress slice, final R-3.6 package check, and cross-version serialization
handoff. Reopened source adds a leading reviewed capability guard and exactly
four derived ledger rows, one for each R 3.6.3--4.2.3 runtime. The hidden
fixture helper now errors instead of dynamically skipping on an unsupported
runtime, leaving reviewed leading guards as the sole version-capability skip
authority. These repairs are not accepted results. Reject `fb2a37f`, name no
replacement until source converges, and rerun every applicable source-bound
gate.

The preceding immutable ref is rejected diagnostic candidate
`refs/paradox-release/candidate-20260801T014150Z` (`6f28dae`), tree
`1b283f19be4534ed30b86017e75eaa9426ec31e2`. It is
package-facing-source identical to `de1752f`; its only change was the reviewed
recursive test-support staging repair and package-excluded documentation. Its
`release-candidate-6f28dae` run passed all eight non-runtime rows, including
all harness, differential, API-header, dual-compiler C23, and native-release
gates. `runtime-supported` then ran all seven minor-series suites and verified
every recursive support-tree receipt before failing on bounded suite results.
The completion/JSON-summary/TSV-summary SHA-256 values are
`9c1b797ae67324bf448f5c07227ee1a2264f406b61bc040dd783e54e5df3be5a`,
`521bf58a476b816920132b3e8432b4b8dfb79a080429bd567281baeb26a0cfe6`,
and
`e25224aeca5225f28c5277bb9d00f2be2674f5be43ecfa2b0dff6563d61d97da`.

The runtime results are bounded to invalid-native-byte diagnostic rendering on
old R and three test-fixture/policy families: portable no-`DATAPTR` ALTREP
construction, version-dependent terminal callback counting, and the intended
R 4.5 promise-inspection fail-closed boundary. Focused repairs exist for all
four families, including one package-facing C change, so source is reopened.
They were not accepted results. Reject `6f28dae`; the later `fb2a37f` run
exercised those repairs but is separately rejected for the reviewed skip-row
omission above.

The preceding diagnostic ref
`refs/paradox-release/candidate-20260801T000111Z` (`de1752f`) passed every
`release-core` task except `runtime-supported`: all seven runtime stages built,
installed, loaded, probed, and symbol-audited the exact package before one
common harness defect rejected the tracked recursive test-support directories,
so none reached testthat. Its completion/JSON-summary/TSV-summary SHA-256
values are
`69634511a79c0d8ab2b6b8dd92aa3b35012f58d420e1333cc4c1ba821cbc44e9`,
`6e6fa7f39e63a74890ecb9b2e062822b9add56a99b24ddc89986c057192a431c`,
and
`76d60a19e82fc0249e5f4bf8b3f8b81ff240181c04a8eec9f6e34676f1306e94`.
This is diagnostic evidence, not complete acceptance.

The repair recursively stages every ordinary support leaf while preserving
testthat's top-level `test*.R`/`test*.r`
discovery boundary, brackets execution with a deterministic tree receipt, and
is independently reconstructed during evidence validation. Because that new
trusted helper was absent from `de1752f`, `6f28dae` carried and successfully
exercised it. The full native lane executes current R 4.6.1; the runtime matrix
executes 3.6.3 and one terminal release from every R 4.0--4.5 minor, so the
combined release gate covers every minor series from R 3.6 through current
without a duplicate R 4.6 suite. The `fb2a37f` run reached all seven suites
but did not complete their post-suite phases; the complete combined gate
remains pending for the next converged immutable source.

The rejected diagnostic ref
`refs/paradox-release/candidate-20260731T150816Z` (`a153fae`) passed its
corrected bounded Domain comparison, but `release-candidate-a153fae` finished
five tasks passing, three failing, and one dependency-blocked. All four harness
rows and the API-header matrix passed. C23 stopped before compilation on a
stale micromamba-output assumption; differential completed 33 cases with zero
unexpected changes but seven stale candidate hashes; strict GCC found one
stale expected diagnostic plus formal-S4 and authentic populated missing-
`row.names` migration defects. The every-minor runtime task never started.
Completion/JSON-summary/TSV-summary SHA-256 values are
`83140b9737809c8b8db6457160938bdc5a39b6db4531fef42962508b61e3e165`,
`e73d372b7533782ba66f922948c7acb4764aa115cafe02c9a64973a531b5269f`,
and
`f912757fe598e4e5fa269e6511f93969b20ebf7aee9d22c9fbd3881626755737`.
It is not an acceptance run or memory donor.

### Historical `dbbdcc1` evidence

Package-facing feature work previously converged at the frozen ref
`refs/paradox-release/candidate-20260727T152133Z`, commit `dbbdcc1`. Its
`release-candidate-dbbdcc1` run passed all eight `release-core` tasks, including
the complete native lane and supported-runtime matrix. The package executions
remain informative historical evidence, but the native child run is not
replayable: its manifest records original worktree modes while its copied
source-tree receipt records
umask-filtered modes. It therefore cannot supply the source proof for the
reopened source's combined memory gate.

The first memory attempt failed closed in preflight before package load because
the relocated source-run validator did not receive its trusted offline-
repository helper. The six-file harness repair retained and authenticated that
helper and preserved exact modes across both snapshot copies. For that payload,
this was a validation-provenance repair, not a package failure or a reason to
rerun the already green API/runtime branches. The required replacement native
run completed: `release-candidate-dbbdcc1-native-replay-r2` passed all four
selected rows, its full native child passed, and direct independent source
validation accepted the exact copied bytes and modes. The retained child
`release-candidate-dbbdcc1-native-replay-r2-native-release-a001` supplied the
isolated rchk policy discovery; the superseded
`release-candidate-dbbdcc1-native-release-a001` was never used as a donor.

The final harness repair itself passed shell syntax checks for the four changed
shell drivers/tests, R parsing for both snapshot helpers, and the complete
activated `scripts/environment/test-validation-hardening` adversarial suite
(`validation hardening helper tests passed`, about 226 seconds). That suite
includes the independent memory-run validator, restrictive-`0077` initial and
replay copies preserving exact `0664`/`0775` modes, relocated-helper
removal/substitution rejection, and equality of the 21-row copied, hashed, and
independently validated harness inventories. Focused probes also confirmed
`Sys.chmod(..., use_umask = FALSE)` restores exact `0664` under both actual R
3.6.3 and the local current R. These are exact repair tests. At that point the
combined-memory execution remained pending; it subsequently passed as recorded
in the historical release ledger.

The first isolated rchk discovery from the new donor,
`release-candidate-dbbdcc1-rchk-discovery-r1`, stopped before analyzer start
because live admission found 36,328 MiB against the reviewed 36,864-MiB
analyzer-plus-host-reserve requirement. The replacement retained the 20-GiB
analyzer allowance and 16-GiB host reserve. This was neither an analyzer nor
package failure.

Discovery `release-candidate-dbbdcc1-rchk-discovery-r2` subsequently admitted
40,004 MiB and completed bcheck, maacheck, and fficheck with zero tool failures.
Its retained run fails only at the expected stale-policy comparison. The
reviewed report has 951 functions, 50,395 states, 83 blocks, 239 UP, and 17 PB;
maacheck is empty and fficheck matches the exact 82-function/one-call
registration inventory. Independent policy generation and complete evidence
validation accept only the three established base-R abstraction ceilings and
six exact objdump infrastructure warnings, with an empty residual diagnostic
file.

No C defect was found in the source review. Four new PB diagnostics are
bcheck's loss of the conditional list-column protection depth in
`build_dependent_grid()`; every normal and error branch balances exactly. A
small harness-only native probe exercised that dependent list-valued
inactive-row branch under the final GCT run. The historical combined-memory
stage then used one fresh `--mode static --tests focused` native donor without
repeating full package, API, or runtime gates.

## Unattended orchestration

`scripts/verify` and the reviewed DAG in `verification/tasks.json` are the
top-level development coordinator. The existing native, memory, runtime,
differential, and compatibility drivers remain the semantic/evidence
authorities; the coordinator must not duplicate their validators. Use
`scripts/verify plan` to expose selection, exact cache status, priority,
dependencies, and resource claims before a long run. The design and operator
contract are in `verification/README.md`.
Every currently shipped local task inherits the mounted toolchain's Linux
x86-64 constraint. Other platforms fail at planning and use the hosted matrix
until a reviewed task supplies a self-contained platform toolchain image.

Generic exact-key success caching is development acceleration only. A release
profile runs source-bound semantic gates fresh and relies only on the existing
gate-specific authenticated receipts. Changed-file impacts may select and
prioritize development tasks, but never narrow a release inventory.

The default `adaptive` failure policy completes independent peers in the
current phase and then blocks descendants and later expensive phases.
Provenance, cache, source, containment, or protected host-reserve failures abort
the complete run. `keep-going` and `fail-fast` are explicit alternatives; no
driver should implement an accidental mixture by depending on shell
`set -e` behavior.

A task that cannot ever fit its reviewed static CPU/RAM/PID/disk or declared
platform/architecture contract is a scheduler-origin capacity block, not an
executed semantic failure. Its descendants remain blocked, but unrelated
branches remain informative under every policy. Momentary free RAM/disk/PID
pressure is compared only with live admission budgets and receives bounded
backoff; it must not become a false permanent-capacity result. Static task fit
applies the configured RAM fraction to physical/parent-cgroup capacity, while
live admission recomputes it from current availability. Starting under
pressure therefore cannot freeze a low future ceiling, and a busy large
machine does not withhold a capacity-sized live reserve.

Planning content-hashes every declared input and reauthenticates the same
digest before execution, before publishing a success, and at completion.
Semantic task identity retains reviewed OS/architecture, toolchain content,
hard-backend/cgroup generation, and immutable worker-image content. Freshly
proved kernel, engine-version/path/storage, live-capacity, and image-alias
observations live in the invocation receipt instead, so incidental machine
differences do not destroy otherwise reusable plans. The explicitly
non-release best-effort fallback remains keyed to its exact host platform and
controller Python.
The activated repository R-library tree is always semantic: disabling generic
release-cache publication does not disable successful-row reuse during
coordinator resume.
Release runtime refs must resolve to the exact captured clean HEAD commit/tree;
the offline differential baseline is resolved to a commit before cache keying.
Retries use attempt-specific child run IDs. Compatibility drivers with a
candidate-wide public stage use that attempt ID for private work and
authenticate an already-published stage on resume. Interrupted private
evidence therefore remains immutable without blocking or being mistaken for a
successful retry.
Every execution attempt has an immutable result beside its immutable log.
`result.json` is only the latest view. Each coordinator invocation, including
a resume on another machine, retains and hashes its current host/engine
receipt. Resume accepts a success only when that latest view is byte-identical
to the numbered attempt receipt and authenticates the exact log and original
invocation. A task marked for revalidation also invalidates all of its
descendants; even a previously completed coordinator run enters this path
rather than returning from its old summary. Host capacity remains outside
semantic plan identity. Scheduler,
dependency, and policy blocks retain an explicit origin, but only real
execution failures affect the next exact-key priority.

The prepared release-compatibility inventory is explicit rather than hidden
behind synthetic fixtures. `prepared-downstream` remains axis-neutral;
`prepared-reverse` and `prepared-documentation` are Paradox-2-only; and
`prepared-release-compat` combines their real gates with maintained downstream
checks under `keep-going`. A broken downstream overlay blocks its consumers
but does not cancel an independent CRAN/Bioconductor reverse branch. The real
reverse task first runs the required fresh miesmuschel plan-only preflight,
then uses one stable child ID so accepted reverse rows and the authenticated
install cache resume after interruption. Attempt one accepts only an empty
controller reservation. Later attempts require a run/candidate/options/
harness-bound reservation marker and treat a durable initialized marker as
the sole semantic-resume boundary; authenticated pre-boundary interruption is
reset transactionally. A completed reverse result is revalidated on
coordinator resume. Documentation has no row-level resume and therefore uses
a fresh attempt-specific child ID; all essential rows across repositories
precede every advisory full row.

Prepared consumer tasks source the verified opt-in compatibility-system layer
inside the worker. Native, API, runtime, and differential tasks source only
ordinary activation. Candidate context keeps ordinary consumer extra
libraries separate from documentation-only libraries; the documentation argv
is bridge, consumer extras, then documentation extras, while repository checks
never see the final role. Reverse-only selection validates only the base
candidate tuple, so missing unrelated bridge/documentation overlays cannot
block it. The detached source uses its exact commit-owned snapshot path and
bridge libraries are candidate-run-owned. The two real gates receive exact pre-reserved output
directories, verify their filesystem identity, and cannot write sibling
historical evidence. Their current authenticated TinyTeX/toolchain/system
contracts make them intentionally Linux x86-64 gates, not substitutes for the
hosted macOS and Windows matrix.

Worker self-tests must not assume the host's procps-ng option parsing or locale
inventory. Negative process-group operands passed to external `kill` are
always separated from options with `--`; otherwise some procps-ng releases
interpret the operand as a signal option and target process group zero. The
source-package fixture deliberately declares UTF-8 and exercises `Authors@R`
expansion under the same explicit `C.UTF-8` locale as production `env -i`
rows. A minimal worker need not install `en_US.UTF-8`. This environment
contract does not relax the check-status rule: every real ERROR or WARNING
remains fatal.

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
2. compile only changed C files with the release C99 warning set;
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

At the top level, use `scripts/verify`: it consumes
`scripts/environment/resource-jobs` host/cgroup discovery and continuously
admits heterogeneous tasks by CPU, hard RAM ceiling, PID ceiling, and scratch
reservation. Direct low-level drivers still use `resource-jobs` immediately
before their own inner wave and retain its report. Parallelize independent
outer tasks—test files, runtime stages, or consumer rows. Within an admitted
worker, Make may use only the CPUs reserved for that coarse task; testthat,
`parallel`/`future`, BLAS, and OpenMP stay at one unless a reviewed test
specifically verifies a bounded worker contract.

Every admission is capped by both static machine/cgroup capacity and current
resource availability. Leave the resource helper's reserve untouched so the
controlling process and OS are not OOM-killed.
Each scalable coarse task declares a reviewed minimum and desired ceiling.
Admission first preserves every selected peer's minima, then assigns spare
CPU/RAM to higher-information peers up to their ceilings. Live parent-cgroup
memory and PID availability and free disk are refreshed before new waves and
at a bounded cadence; temporary outside pressure waits with bounded backoff.
Wait for all siblings, retain each exit status/log, and let one parent aggregate
and seal results. A wave failure does not discard successful row artifacts.

There are two acceptable hard-containment modes. Per-worker Podman/Docker
limits must pass an active inner-cgroup and sacrificial-OOM proof. Aggregate
containment must put the complete controller/local-rootless-Podman tree in one
dedicated root-created system service and prove its exact v1/v2 cgroup leaf,
finite memory/CPU/PID ceilings, no-swap contract, systemd kill/accounting
properties, and real worker inheritance. Both active probes run with the
eventual worker UID and prove a read-only checkout bind with a nested writable
leaf. SELinux label handling is explicit (`label=disable`) rather than an
untested host default.

Rootless Podman on cgroup v1 cannot enforce the per-worker resource flags.
Inside a proved aggregate service it instead runs with
`--cgroups=disabled --cgroupns=host`; individual resource declarations become
admission reservations within the hard aggregate ceiling. Docker and remote
Podman cannot use this mode because their daemon may escape the controller
cgroup. The root-owned launcher may set systemd properties as root, but
mutable repository code must execute only after systemd changes to the
configured unprivileged UID/GID.

Reservations in this aggregate mode must not be confused with per-worker hard
limits or hypothetical worst-case address spaces. In particular, the prepared
release-compatibility phase uses one consumer-sized 8-GiB minimum for each of
the reverse and broad-repository branches and 4-GiB minima for the serial
documentation and focused branches. Their 24-GiB/6,144-PID/8-GiB-scratch
combined admission fits inside the installed aggregate budgets and permits all
four independent branches to overlap. The two broad branches may grow to 16
GiB when capacity is otherwise idle. There are no retained per-task peak
measurements for the old 26--51-GiB weights, so those values are not
evidence-based safety limits. Fail-closed aggregate OOM/PID events, protected
disk-pressure checks, immutable failed attempts, and a raised reservation on a
replacement run are the deliberate feedback mechanism. This does not relax the
conservative uncontained-direct `resource-jobs` policy.

Nested drivers must treat `PARADOX_VERIFY_ASSIGNED_CPUS` and
`PARADOX_VERIFY_ASSIGNED_MEMORY_MIB` as upper bounds on their worker counts.
Those values are already inside the outer safe budget. An authenticated hard
worker uses measured cooperative weights inside its exact task cgroup and a
1-GiB intra-worker reserve floor; the aggregate service uses the same weights
with a 4-GiB shared floor. A worker marker is accepted only when the assigned
envelope and raw v1/v2 CPU quota/period and memory limit prove that exact
boundary. Uncontained direct work retains the conservative host policy. The
retained inner live-resource check may still lower any result. A finite
`operator_max_jobs` is valid evidence only when it lowers the independently
recomputed CPU/memory/profile ceiling. The schema-2 report retains and replays
that arithmetic decision; it is not a standalone cgroup attestation.
Generation-time limit authentication and the source-bound controller/worker
receipts own the containment conclusion.

Without either proof, normal runs fail closed. `--best-effort` is an explicit
serial development fallback using `RLIMIT_AS`, an RSS watchdog, and the host
reserve; it cannot produce release evidence. Aggregate memory admission is
approximately the minimum of cgroup headroom and global `MemAvailable` minus
the outside reserve: the reserve must not be subtracted from both sides. Any
aggregate event-counter increase, contract/path/limit change, or protected
host pressure terminates all siblings and invalidates the run as
infrastructure. A per-user machine execution lock prevents two top-level runs
from independently admitting the full budget. Free RAM, disk, PID headroom,
and aggregate state are recomputed before new waves and at a bounded cadence
while work is active.
The source/toolchain mount is read-only; attempt-private runtime/temp
directories and explicit manifest paths are the only writable mounts, and
candidate or dependency libraries nested below an evidence path are remounted
read-only.

The combined `scripts/memory-check --mode all` is not a generic coordinator
task. Its rchk branch launches a nested pinned Podman image and its inner
resource policy retains another 16 GiB, so wrapping the whole driver would be
unrunnable or would misstate containment. Keep it as the direct source-bound
release gate until GCT/Valgrind are separated into a normal worker and the rchk
image itself becomes the outer worker.

## Required package tests

The discovered suite, rather than a stale numeric count, must cover these
families.

### Capsule and graph

- exact v1 tag/schema/field validation for every node kind;
- malformed types, lengths, names, attributes, indices, arithmetic, and graph
  edges error without crash;
- BASE, empty/nested/shared COLLECTION, and live SHADOW behavior;
- path-cycle rejection, including collection-to-shadow-to-origin cycles;
- graph-path validation beyond its 16-frame inline capacity under
  `gctorture()`, and the disposable
  `scripts/environment/test-core-graph-roots` strict build after carrier
  changes. Its compile-time-only barrier detaches each selected core, forces
  collection and pending finalizers, and verifies that the active-path carrier
  owns all 21 exact generations; the ordinary build contains neither barrier
  nor registered counter;
- native collection add rejects pre-existing/proposed cycles and corrupt child
  graphs before commit, preserves the old core on every failure, and detects a
  generation change anywhere in either admitted graph;
- shallow/deep clone and serialize/unserialize topology;
- detached equality of every semantic field, BASE callbacks, COLLECTION
  children, SHADOW origins, independently built equivalent DAGs, and
  shared-versus-duplicated topology, plus active-path cycle rejection;
- atomic replacement, detached old snapshots, and reentrant generation conflict.

### Legacy object-graph migration

- `upgrade_paradox_object()` remains non-mutating, returns a newly built
  canonical ParamSet/Collection graph, preserves shared children/callback
  identity, accepts current objects idempotently, normalizes standalone built-in
  Domain/Condition objects, and rejects malformed or unknown inputs without
  executing legacy methods;
- `upgrade_paradox_object_graph()` returns the exact root invisibly and
  transplants every admitted nested legacy ParamSet-family environment without
  changing its identity. Tests cover aliases from multiple R6/private
  enclosures, ordinary lists/pairlists/calls/expressions, attributes and S4
  slots, closure environments/formals/bodies/bytecode, and legacy shells
  reachable from authenticated current-core protected payloads;
- pointer memoization terminates on cycles in the *containing* graph and visits
  shared objects once, while a semantic cycle in a ParamSet/Collection/Shadow
  dependency graph remains a path-specific rejection;
- environment tests cover nested local/crate-style frames and enclosing
  parents, while `.GlobalEnv`, attached search environments including
  Autoloads, package/namespace/import environments, base, and empty remain hard
  boundaries. A boundary object that itself is the root is left unchanged;
- active-binding functions are traversed but never invoked. Delayed binding
  promises are inspected without forcing and expose only expression/evaluation
  environment; forced binding promises expose stored expression/value.
  Ordinary delayed/forced/missing `...` cells receive the same coverage.
  Realized direct language objects and symbols are distinguished from delayed
  promises carrying language/symbol expressions by the native classifier;
  neither path uses `substitute()` or evaluates the binding.
  R 3.6--4.4 also inspect detached `PROMSXP` structure. R 4.5 fails recursive
  migration closed on any reached promise, proves it remains unforced, and
  requests R 4.0--4.4 or R >= 4.6. R >= 4.6 treats a detached promise outside a binding/dots
  cell as opaque. The DSO inventory excludes `R_getVar` before R 4.6 because
  it could force a delayed cell without `R_GetBindingType`; current source must
  contain the exact three reviewed call sites and every call must follow the
  classifier, while the DSO contains one undefined-symbol row. Tests record
  side effects and cover all three policy branches.
  The same inventory authenticates the independent Domain-rendering policy:
  `Rf_GetOption1` is absent through R 4.4, where the compatibility facade uses
  public `base::getOption()`, and occurs exactly once from R 4.5 onward. The
  old/current semantic test compares the bounded native result with the
  existing `deparse1()` fallback under integer and numeric `scipen` states.
  On R 3.6 the
  active-binding case must fail closed without invocation and direct the user
  to migrate under R >= 4.0; because Paradox-1 ParamSet-family R6 shells use
  active bindings, their practical migration runs only on R >= 4.0. The exact-
  current R 3.6 exception traverses capsule state and ordinary public edges,
  while package active facades remain opaque. A regression records that an
  unsupported in-place facade replacement is neither invoked nor traversed on
  R 3.6, because that runtime cannot distinguish its closure without an
  accessor;
- attributes of generic weak references and external pointers remain normal
  edges, but their internal referents/protected/tag/address slots are opaque.
  Only an authenticated Paradox core contributes its protected payload;
- all discovery, legacy/current authentication, owner inspection, offside
  rebuilding, dependency-plan validation, and shell-shape checks fail before
  mutation. Current-shell cases cover ordinary additive BASE/COLLECTION/SHADOW
  suffix chains, malformed/reserved/duplicate/hybrid class vectors, exact
  `assert_values`, canonical core agreement, and descendants with corrupt
  classes. A stale Shadow preflight must compute authoritative live semantics
  without installing a refresh, retain distinct source/semantic core receipts,
  and detach collection callbacks from the admitted graph. Tests mutate an
  earlier root after its initial preflight and require the initial joint
  all-prepared-roots barrier to abort before the first transplant. A second
  fault-injection mutates an unrelated current root during rebase and requires
  the per-rebase live-root barrier to reject before the corresponding
  transplant. A third injection mutates an unrelated current root immediately
  after a transplant and requires the post-transplant barrier to detect it
  while retaining the completed transplant, documenting the explicit
  no-rollback boundary for hostile external finalizers. The capsule barrier is
  not a complete public-shell receipt: two additional injections replace a
  method on the transplanted shell and on an unrelated already-current shell.
  The successful commit's final native barrier must reject both after its
  allocation-capable graph admission, in the same allocation-free tail that
  checks every exact capsule receipt, class, environment lock, public binding
  value/active kind, and binding lock. A direct probe separately changes only
  a lock bit before changing a binding value. An unlocked environment is
  rejected before receipt construction because its binding-name inventory
  could grow or shrink. A current-only graph is also exercised on R 3.6: with
  no transplant and therefore no binding wave, it must return after joint
  capsule validation without requiring the unavailable active-binding-function
  accessor.
  A simulated interrupted binding wave, including the
  lock-restoration edge, verifies post-order monotonicity:
  a parent is identity-rebased only after each original child has been
  transplanted. The already-current identity roots plus that newly rebased
  prepared root are jointly validated before the parent changes; unrebased
  parents remain offside templates until their turn, and the current identity
  roots are jointly validated again after the original joins them;
  `.__enclos_env__` changes last, already transplanted nodes are independently
  valid, an incomplete shell remains authenticated by its old enclosure, and
  an idempotent retry completes the graph;
- newly serialized shells call versioned targets directly and never consult the
  legacy option. A historical target forwards an authenticated current core
  directly; otherwise it defaults to an informative error, while `"upgrade"`
  silently migrates and resumes the requested operation. Invalid option values,
  retired/unknown target names, old argument-form combinations, and
  pre-release capsule-backed Paradox-2 Shadow stubs fail or forward as
  specified. Gateway tests cover additive family chains, defining-family
  enclosure selection, exact `assert_values`/canonical-core rejection, and
  prove that serialized `private`/`super` promises are ignored and unforced;
- the owner registry admits only an exact direct-owner class from the owner
  package's current namespace, resolves namespace-local hook names rather than
  serialized functions, requires an empty dependency list for additive
  rebuilding and exactly one `origin` plus a current Shadow result for
  replacement rebuilding, and rejects duplicate/overlapping/stale
  registrations, altered owner methods, undeclared fields, R6 finalizers,
  malformed inspection/rebuild results, deeper class vectors, and S3 fallback.
  Authentic bbotk legacy Codomain and miesmuschel legacy Shadow fixtures
  exercise additive and replacement migrations respectively;
  `params_unid`/`set_id` become precise retired-field errors.

### Closed semantics

- all five Domain kinds and every operation on each supported kind. Every
  nonempty and typed-zero public operation proves the exact complete
  sixteen-column structure; the canonical zero-column empty Domain proves its
  dedicated exact structure. The registered interpretation-closure fixture
  covers all 64 masks and idempotence, `domain_check()` requests the complete
  mask, and per-operation tests show that a whole irrelevant rule is skipped
  until the first operation that interprets it. Terminal-generation,
  callback/reentry, outer-metadata mutation, typed-zero, and empty-Domain
  regressions cover the compact indexed-root receipt. Constructor, ParamSet,
  and ObjectTuneToken tests demonstrate that the sole canonical built-in
  Domain-row owner admits kind/storage, cargo, grouping, bounds, levels,
  defaults, tags, requirements, initialization, and special-value/
  transformation combinations. Object-token Domain coverage admits only
  bounded value-producing built-in Domains, rejects unbounded `ParamUty` and
  zero-level `ParamFct` tuning ranges, and tests opaque-leaf identity through a
  bounded typed Domain. Structural ALTREP/S4 rejects include every outer
  `special_vals` list; typed special leaves reject ALTREP, typed S4
  special/default/init matching is pointer-only, and ParamUty opaque S4 leaves
  retain identity with base-`identical()` special membership as the sole
  no-dispatch observation. Genuine formal-S4 leaves are also proved non-token
  through checked/unchecked graph stores, dependency activity, value-type
  filters, fixed designs, and both stored and explicit search-space
  extraction, while an S4-marked ordinary TuneToken remains a structural
  rejection;
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
- native tag and dependency projection/replacement/append own detached state
  and snapshot exact Conditions. Bulk dependency replacement rejects malformed
  tables, invalid children, and self-edges, preserves dangling parents and
  infeasible predicates without callbacks, and covers graph copying after
  parent-Domain narrowing. `$add_dep()` separately validates feasible RHS
  values, routes visible Shadow dependency append, and detects mutation during
  callbacks; native BASE callback setters retain established formal admission
  and atomic replacement;
- `$has_deps` returns exact false/true results for BASE, live SHADOW, and
  COLLECTION nodes through its registered reader; malformed or bytes-encoded
  dependency state and collection cycles fail closed, and the direct-routine
  probe plus forced-gctorture suite cover the allocating graph/refresh paths;
- aggregation, disabling, and conversion internal-tuning operations capture
  cargo/translation/Domain/owner-value state before callbacks and commit only
  through native mutation; flattened `cargo` closures are rebound to detached
  IDs after native flattening, preserve documented lexical behavior, and
  cannot act as an alternate capsule/check/callback-selection engine.
  namespace-sensitive Collection and Shadow flattening selects all IDs and
  route context inside one native graph snapshot, subsets those exact IDs, and
  rejects a source mutation at the terminal receipt instead of mixing derived
  generations. Callback-free flattening selects all current IDs inside the
  cheaper native subset transaction and rejects metadata introduced after its
  mode decision. BASE flattening and omitted-`ids` `$subspaces()` likewise
  select all current IDs inside their native transaction, while explicit
  subspace IDs retain selected-ID behavior;
- LHS and Sobol generation preserve exact caller ParamSet identity while
  sampling from one owned graph; an LHS callback mutation is rejected by the
  terminal complete-graph receipt. The ordinary-environment identity bind
  precedes that allocation-free barrier;
- default `$search_space()` snapshots the current raw value store and target
  Domains in one native generation, while explicit values retain the separate
  caller-snapshot path; focused tests also authenticate that leanified public
  stubs preserve omission without changing the reflected default;
- zero-level categorical Samplers accept `numeric()`, return a typed
  `character(0)` column for zero rows, and reject positive rows without
  advancing the RNG. Hierarchical construction owns one graph before reading
  arbitrary Sampler subclass parameter bindings, rejects duplicate sampler
  IDs, and cannot splice IDs from one source generation onto another;
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
- transformation results and non-table inputs require ordinary
  non-ALTREP/non-S4 list shells, including unnamed BASE output and named
  collection output cases; documented data-frame inputs may use the exact
  top-level ALTREP table boundary, and stable semantic ALTREP leaves/columns
  remain admitted;
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
  structural configuration/search/trafo and ParamSet-`params` lists, internal
  table/row/Domain/Condition/token/capsule shells, Domain cargo/interpreted
  cargo entries, dimnames, class/name vectors, and list metadata reject
  ALTREP/S4, except for the explicit one-snapshot `set_values(.values=)` merge
  boundary and one-shot normalization of a suffix-classified, allowed-attribute
  top-level VECSXP ALTREP at a documented public-table ingress;
- detached public data.table facades with valid self-reference and no capsule
  aliasing; no internal data.table state; documented data.frame/data.table
  inputs use one classifier at all six ingresses. Tests cover canonical and
  additive terminal class suffixes, no prefix-induced ordinary-shell copy,
  ALTREP-snapshot canonicalization without dispatch,
  malformed/reversed/non-suffix/reserved/duplicate class vectors, allowed
  attribute sets, ordinary/discarded data.table cache carriers, absent/S4/
  object/attributed/mismatched row names, compact positive/negative counts,
  stable integer/character ALTREP row names with one Length/no Elt, and shared
  mutable names under top-shell Elt reentry. They also distinguish row-consuming
  count checks from direct trafo/no-edge dependency planning, preserve unnamed
  zero-column data.frames, and prove Design transpose emits one empty
  configuration per declared row. Suffix-classified top-level ALTREP shells are covered by
  a cross-version native fixture, with base R's lazy duplicate covered on
  runtimes that select that optimization; stable admitted semantic columns
  remain covered;
- current serialization, pure single-object conversion, and recursive
  identity-preserving upgrade of CRAN Paradox 1.0.1, shared/nested graphs,
  callbacks, both pinned `mbo_config` fixtures, authentic mlr3/gallery
  containing-object snapshots, registered bbotk/miesmuschel extensions, and
  rejected unknown legacy extensions. Legacy table snapshots admit the
  authentic missing-row-name spelling for empty and populated keyed tables,
  because equal-length independently owned columns are then the sole row-count
  authority. Present metadata remains mandatory ordinary canonical integer
  metadata matching that count. Coverage also rejects inconsistent column
  lengths and proves that a deferred by-reference source-column write cannot
  splice a pre-receipt name with a post-receipt payload;
- constructor representation, Design, sampler, subset/flatten/union, and
  ordinary edge diagnostics. Subset coverage pins the compatible
  `keep_trafo = TRUE` default and verifies independent transformation stripping
  for BASE, COLLECTION, and SHADOW at both the public and registered-native
  boundaries, rejects classed/attributed controls before S3 dispatch, and
  exercises the stripped transaction under forced GC.

Every consumer-discovered failure adds the smallest internal regression that
would have caught it before the downstream fix is accepted.

Complete unit evidence sets `PARADOX_MBO_CONFIG_ROOT` to a retained bundle's
`common/` directory, where `mixed_search_space.rds` and
`numeric_search_space.rds` reside. Leaving it unset is a development-only skip;
pointing it at either a repository root or the mutable checkout's `common/`
directory is not release evidence. The shared fixture helper requires the
candidate's GitHub snapshot and mlr-org review ledgers to agree on one exact
commit/tree, reads the files from those immutable Git objects, publishes them
read-only, and records their complete receipt and hashes. The native driver
seals this bundle under its functional mode. The supported-R coordinator
stages it once before worker admission, all runtime stages consume the same
bytes, and the evidence verifier reauthenticates the ledgers, tree, files, and
receipt.

## Native build and API gates

The frozen candidate must pass:

- strict GCC and Clang C99 builds with the repository's highest warning set and
  warnings as errors;
- a bounded current-R forward-compatibility slice which installs one exact
  archive with explicit `--use-C23` under repository-local GCC >= 15 and recent
  Clang, then loads each DSO and runs the registered-native probe inventory;
- fixed-arity registered-routine inventory, dynamic lookup disabled, direct
  probe for every entry, and no unregistered native symbol use;
- ASan and UBSan direct hazard/probe coverage;
- for an ASan-selected contained run, exact preloaded-R startup plus an XDR
  serialization round-trip before expensive compiler modes, with the singleton
  success log included in the sealed and independently replayed mode tree;
- native parallel-worker interruption cleanup in which both the process-group
  supervisor and detached token watchdog converge on the same bounded
  descendant scan for catchable direct `HUP`/`TERM`/`USR1`, while inherited-
  ignored `SIGINT` is covered by bounded coordinator-death polling;
- pinned R-header compilation beginning with R 3.6.0 and covering every
  supported API branch;
- no forbidden private data.table API or unledgered/unsupported R API symbol;
- exact authentication of `environment/r-api-exceptions.tsv`: every
  raw-attribute, coherent old-R closure-snapshot, non-forcing stored-binding,
  and promise symbol has its
  precise source, count, version range, and rationale. R 3.6--4.5 must contain
  the one centralized `ATTRIB` occurrence; R 3.6--4.4 must additionally ledger
  exactly one occurrence each of `FORMALS`, `R_ClosureExpr`, and `CLOENV`,
  while excluding `R_BytecodeExpr` in favor of its cold public bridge. R 4.5
  must instead
  require the five public closure/bytecode/environment accessors
  `R_ClosureFormals`, `R_ClosureExpr`, `R_BytecodeExpr`, `R_ClosureEnv`, and
  `R_ParentEnv`.
  R 3.6--4.1 must contain the exact
  old-only `R_HasFancyBindings` receipt-scan entry. R 3.6--4.4 must contain the
  one `Rf_findVarInFrame` call plus the ledgered header-declared/exported
  `R_PromiseExpr`, `PRENV`, and `PRVALUE`. R 4.5 must retain only
  `Rf_findVarInFrame` and exclude all three promise accessors; R >= 4.6 must
  exclude all four and use its experimental binding/delayed-binding/dots APIs.
  A reached formal promise retained by an ordinary callback factory is covered
  by the same R 4.5 fail-closed rule; tests must not misclassify such a promise
  as exotic or force it to make migration pass. Recursive migration is
  performed under R 4.0--4.4 or R >= 4.6 when this occurs.
  Raw-token, pinned-header, and DSO inventories verify every branch.
  The real R 4.5.2 stage must also retain a zero-issue result from that
  runtime's own `tools:::check_compiled_code()` on the installed package; its
  evidence verifier rejects absence, extra output, a nonzero issue count, or a
  digest mismatch.
  None of these
  entries is CRAN-allowlisted. An R-level `substitute()` workaround is not
  accepted because its promise expression is not an unambiguous binding-kind
  or generation receipt. The registered plain-binding probe must additionally prove
  that realized and delayed literal values of identical apparent R type,
  including language objects and symbols, are distinguished without evaluation
  on every supported runtime. R 3.6--4.1 additionally exercise cold optional
  absence lookup, locked bindings, fancy-frame fail-closed receipt scans, and
  the allocation-free required ordinary-frame binding path. Current runtimes
  must retain their public fast-path symbol inventory, while adversarial tests
  prove the recognized `UserDefinedDatabase` class is rejected before that
  callback/layout boundary.

The R 3.6 stage exercises atomic ALTREP normally. It records one precise
capability exclusion for the adversarial VECSXP ALTREP fixture, because R did
not expose list ALTREP classes until R 4.3. No production behavior is waived:
list ALTREP objects cannot exist on the excluded runtime and the corresponding
production branch is vacuous.

The old optional-binding Shadow generation-receipt regression has the inverse
capability boundary. It executes on R 3.6--4.1, where absence-tolerant lookup
enters the evaluator, and records one exact source-derived skip on R 4.2--4.5.
Those runtimes expose the public non-evaluating binding-existence operation, so
the old branch cannot be entered; no package behavior is waived.

Primary drivers are `scripts/native-check` and
`scripts/check-r-api-compatibility`. Use their current `--help`; their retained
inventories must be generated from the candidate rather than copied historical
counts.

## Real R runtime matrix

`scripts/test-runtime-matrix` runs the exact candidate on repository-local R
3.6.3, R 4.0.5, R 4.1.3, R 4.2.3, R 4.3.3, R 4.4.3, and R 4.5.2;
current R 4.6.1 complete test execution is owned by the local full native lane
rather than duplicated here.
Each stage
has a fresh candidate library, builds/installs Paradox once, runs the complete
supported test inventory with `NOT_CRAN=true`, audits DSO symbols, records package/compiler/session
identity, and seals the source/build/library/log tree.
The exact result ledger rejects `Reason: On CRAN`; only source-derived runtime
capability skips are admitted. The runner also clears any inherited
characterization-GCT override before testing.

Before the supported-R workers are admitted, the coordinator also stages the two
mandatory historical `mbo_config` objects through the shared authenticated
Git-object helper. The full direct and recursive ParamSet-family migration
assertions execute on R >= 4.0. R 3.6 instead proves the non-invoking
active-binding failure plus
current-object/idempotent and standalone legacy Domain/Condition paths. Exact
current-shell regressions cover unlocked replacement closures as graph edges
and the explicit R-3.6 opacity of relocked methods and package active facades;
its
capability exclusion is version-derived, not a file-wide waiver. An unset
fixture root is an unexpected harness skip, not a reviewed runtime exclusion.
The retained bundle is outside all mutable
stage trees, is read-only, and is joined to top-level and per-stage evidence by
commit, tree, receipt, provenance, and file digests.

R 3.6.3 through R 4.2.3 build their exact SHA-256-authenticated dependency and
test closures into repository-local source libraries described by their
`environment/runtime-r-*-packages.lock` files; none of those stages mutates its
runtime prefix, host R, HOME, or a user library. R 4.3.3 receives only the
SHA-256-authenticated cached data.table 1.18.4 source
overlay before Paradox is built. This is not a reason to skip tests or accept
1.17 behavior. R 4.4.3, R 4.5.2, and current R 4.6.1 resolve 1.18.4 directly.
All selected stages may run concurrently when the aggregate-contained resource
report admits their outer workers; nested work stays at one.

Every selected R 3.6.3 stage also installs the same built candidate archive
against the sealed `declared-floor` library profile: the exact five direct
dependency floors plus `digest` 0.6.39. The bounded smoke authenticates every
installed package identity and dependency namespace origin, plus the candidate
Paradox DLL origin and registration. Representative
constructor/check/dormant/grid paths, logs, the dependency receipt, and the DSO
are retained and replayed. This lane does not rerun the complete suite and its
package closure is cached by exact inputs. Ambient `R_DEFAULT_PACKAGES` is
removed before admission so machine startup configuration cannot preload a
floor dependency.

Only the complete seven-runtime selection may claim the post-stage
cross-serialization result. It must produce the current-v2 fixture under the
exact R 4.0.5 stage package, load/exercise/mutate/reserialize it under the exact
R 3.6.3 stage package, and seal both package/DLL origins, stage receipts,
fixture and round-trip bytes, semantic ledgers, logs, and isolated state.
Partial selections retain no cross-runtime artifacts and explicitly report the
gate as not applicable.

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
serialization/pure and recursive upgrades, first-use gateways, exact
package-built and forged/subclassed TuneTokens, and common consumer call
patterns.

## Downstream validation

Run in increasing cost:

1. focused bbotk additive-subclass/public-sets/native owner-root behavior plus
   exact legacy-Codomain registration, explicit graph upgrade, first-use
   upgrade, and identity preservation;
2. focused miesmuschel official-Shadow bridge plus exact legacy replacement,
   origin dependency migration, retired fields, owner-local historical
   gateways, and identity preservation;
3. focused mlr3mbo public transform-stripping subset bridge and the small
   dual-version diagnostic adaptations;
4. priority-one CRAN/Bioconductor reverse dependencies and maintained mlr-org
   repositories;
5. active book/gallery/website/cheatsheets and serialized configurations.

Use the exact local bridge commits intended for PRs and one authenticated
candidate installation. Content-addressed consumer package installations are
reused; independent rows run in admitted outer waves. Mine all failures before
changing source. Very old repositories that neither import nor call current
Paradox are recorded but not made blockers.

The historical default pre-refresh schema-2 evidence installed seven reviewed
bridge packages once per candidate, in the fixed dependency order bbotk, mlr3,
miesmuschel, mlr3pipelines, mlr3fselect, mlr3mbo, and celecx, by
`compat/install-downstream-bridges --candidate-source "$candidate_source"`.
Construction consumes the authenticated candidate and priority-one dependency
libraries, verifies the organization review ledger once, archives each exact
Git object, and atomically publishes a read-only candidate-specific overlay at
`.local/compat/runs/$PARADOX_CANDIDATE_RUN_ID/library-downstream-bridges` plus
sealed evidence. That evidence binds candidate provenance and content,
dependency content, all bridge/review inputs, archive hashes, installed
versions and content, the installer, repository-evidence verifier, and resource
scheduler. Its schema-2 completion receipt is invalid for another candidate
even when package versions happen to match. One candidate-run owner serializes
construction. The library and evidence use atomic no-clobber directory
publication, and failure cleanup is allowed to remove a published path only
while both owner identity and its recorded filesystem device/inode still
match. A concurrently installed replacement is therefore never repaired or
deleted by a losing process.

Repository-corpus, documentation, and benchmark gates put that exact overlay
first in their extra-library path and call the helper's `--verify` mode before
loading packages or beginning retained work. Verification is read-only and
replays the sealed inputs, its exact seven Git archives, package inventory/content,
and read-only modes; it never rebuilds or repairs the overlay. A caller may
skip duplicate candidate/dependency tree hashing with
`--protected-content-preverified` only after authenticating those exact trees
itself. `scripts/environment/test-downstream-bridge-installer` cheaply pins the
installer order, provenance agreement, entrypoint hooks, and release recipe;
shell syntax and shellcheck accompany it. Candidate evidence still requires
one real construction followed by verification. This seven-package unsuffixed
overlay is historical infrastructure, not the active
`release-refresh-20260720` release overlay.

After a package candidate is frozen, a downstream-only head refresh uses a
named profile in `compat/downstream-evidence-profiles.tsv`; it never edits the
default manifest or overwrites its overlay/evidence. The profile binds its
repository manifest, executable snapshot, complete bridge provenance,
primary-checkout namespace, dependency-input snapshot, and install order. The
dependency receipt is profile-specific but axis-neutral and run-local; it
prepares only the unchanged external dependency closure. Overlay,
repository-test, full-check, lock, and completion identities include both
profile and axis. Non-default profiles authenticate one clean
committed tooling tree, require every profile input to be tracked, and, on the
`paradox2` axis, prove that this commit changes none of the frozen package
source, tests, help, or package-facing documentation. Each axis pins one exact
candidate ref/commit/tree/version tuple, and `paradox1` uses a separate
run-local candidate receipt for released Paradox 1.0.1, so P1 observations
cannot be presented as Paradox-2 candidate evidence.

The `release-refresh-20260720` profile installs its complete nine-package
dependency order. Its final broad repository stage executes the complete
priority-zero/one corpus once with `jobs = 2`; the active retained execution
used 14 admitted waves and completed 20 of 28 exact repositories, with eight
reviewed non-Paradox/environmental exclusions. The additional exact
source-package check stage selects all seven changed PR heads: bbotk,
mlr3tuning, miesmuschel, mlr3pipelines, mlr3mbo, celecx, and mlr3fda. The last
retained exact stage predates this expansion and remains historical evidence
for its five green rows; it does not satisfy the seven-head release gate. The
overlay still authenticates the exact reviewed mlr3 and mlr3fselect support
heads, and their behavior is covered by the broad repository stage. Both axes
retain profile/axis registries and
selected input hashes, reject path, symlink, head, tree, and ancestry
mismatches before work, and publish profile-specific overlays with the same
no-clobber protocol. Use one outer worker for this focused release
confirmation. Run the sealed exact-head `R CMD check` harness in addition to
repository suites so Rd links and other package-level checks are retained
evidence. For each authenticated Git archive, that harness first runs `R CMD
build`, retains and hashes its build log and resulting package tarball, and
checks that tarball rather than the raw source directory. This makes build-time
`Authors@R` expansion part of the authenticated boundary required by R 4.6.
The harness separately binds the check exit/log, classifies the retained final
check status rather than trusting the process exit alone, so a WARNING cannot
be sealed as a pass, and binds every configured extra-library input by ordered
path and content hash.

The final benchmark deliberately does not require validation-tooling
`HEAD` to equal the candidate commit. Such a requirement is circular: the
post-freeze axis row cannot name its own commit hash. Instead,
`benchmarks/release` requires the managed detached candidate source plus an
explicit non-default profile and axis. The candidate source authenticates the
source archive and candidate-owned differential helpers. The current primary
checkout authenticates the benchmark driver and validation helpers as one clean
recorded tooling commit/tree/status. The profile derives, rather than accepts as
an argument, the suffixed bridge library and evidence path.

Ordinarily one final validation-tooling commit is frozen before constructing a
fresh named overlay, and documentation, full checks, and the benchmark reuse it
read-only. The checked-in `paradox2` axis now pins the exact active `4e549f3`
candidate. Validation tooling is frozen at
`refs/paradox-release/validation-tooling-20260801T124036Z`, commit
`3902d5bf0bedcef5a39f266978f371a8cc5640b7`, tree
`ce0b22d8276a8524efe47451d7d76a824d30031e`; its diff from the candidate is
package-facing-source identical. The fresh overlay and prepared gates remain
to be constructed from this corrected tooling. Historical tooling
`fc92edd7f1ab612468066fe06bd3d9fc7afea41c`, tree
`05cc4e5213c5ee73d0bc764c3d102c15e4c57141`, belongs to the historical
`8797f11` documentation, broad-corpus, source-check, and benchmark stages. The
earlier `bf64490` to `9e87556` composition belongs only to the superseded
`10c6a0e` history. Do not cite either as active evidence. There is still no
arbitrary older-tooling replay, default unsuffixed-overlay substitution, or
result relabeling.

The non-default Paradox-2 overlay admits changes only below the explicit
package-excluded validation roots. This includes `verification/`, whose
controller, task manifest, tests, and operator documentation schedule the
existing compatibility drivers but are excluded from the built package by
`.Rbuildignore`. Package-facing paths remain rejected, and the active checkout
must still be one clean recorded tooling commit.

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
  tests;
- migration-crawler cycles, deep graphs without C recursion, non-forcing
  promises, active-binding non-invocation, search/package boundaries, malformed
  attributes/pairlists, generic external pointers/weak references, current-core
  protected payloads, owner-hook failures, and injected post-order partial
  commits followed by retry.

`scripts/memory-check` consumes the exact source/archive from a passed native
run and does not rebuild examples, vignettes, or the full functional corpus in
each analyzer mode. Analyzer-specific probes cover every registered routine and
reviewed hazard. Its retained source-run validator authenticates root-dependent
fixture/Git helpers against the active repository copies rather than resolving
them from the validator's relocated evidence directory. Its package-local
sibling helpers are a different trust class: the memory harness retains and
hashes the complete relocated bundle, and the independent memory-run validator
requires the same exact inventory. Snapshot creation and replay restore and
verify manifest modes independently of the caller's umask; a content-correct
but mode-inconsistent source tree is not replayable. Valgrind/rchk are
serial memory-heavy stages and run only after
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
both serialized `mbo_config` upgrades, including the recursive containing-object
path and each exact owner bridge.

The local current-R package checks run without network access against a
per-run, SHA-authenticated repository index generated from the exact locked
source closure plus the candidate. CRAN is populated; the three standard BioC
names share a valid empty local index so R cannot silently fall back to live
URLs. Seal and replay `PACKAGES`, `PACKAGES.gz`, `PACKAGES.rds`, and the empty
BioC `PACKAGES` before accepting exact `Status: OK`. This retains local
dependency-cycle evidence; hosted CI owns claims about live repository
freshness, remote orphan metadata, and external clock verification.

Windows release x86-64, exact Windows x86-64 R 3.6.3/Rtools35, and real macOS
Apple-silicon ARM64 CI must check the exact candidate source. The old-Windows
job is a separate source-build/link/load/smoke lane over the authenticated
seven-package runtime closure; the local real R 3.6.3 stage retains ownership
of complete old-R behavior. Its R 3.6 dependency check may retain only the
single exact unavailable-Suggests NOTE and final `Status: 1 NOTE`; current
platform checks retain exact `Status: OK`. The independent local R 3.6.3
source-package check has the analogous exact four-package missing-Suggests
contract defined in `portability-ci.md`; neither lane may relabel its bounded
NOTE as clean. Each workflow step and its final completion check
must propagate R errors and nonzero status; a green wrapper around a failed R
command is a harness defect. Retain job/run/source identities and all three
platform artifacts.

Portability requirements are detailed in [`portability-ci.md`](portability-ci.md).

## Benchmark gate

Benchmark only after package bytes and behavior freeze. It may execute while a
slow independent exact-memory or remote portability stage is still running,
but its release conclusion is accepted only after those correctness gates pass.
Use paired baseline/candidate runs on an otherwise idle host with raw
distributions, warmup, stable CPU/memory conditions, and representative
downstream workloads. Include constructors,
`check`/`check_dt`/`check_dependencies`, values, params/domains/dependencies,
subset, collections, live Shadow constraints and read/write paths, Design, and
samplers.

Profile first. Optimize measured R-boundary, repeated validation/snapshot,
lookup, allocation, or per-row overhead while keeping portable C99 and readable
ownership. After each optimization run affected correctness tests; after the
performance source freezes rerun the final memory/portability evidence once.

The upstream-v1 comparison distinguishes ordinary hot paths from integrity work
introduced by the 2.0.0 contract. Exactly seven rows use the wider integrity
tiers. `shadow_values_live` uses `integrity-shadow-read` (median 3.25/q75 3.50).
The three synthetic `collection_values_{plain,rich,nested}` rows and the three
real `$values` rows for `mies_mutator_maybe`, `mies_optimizer`, and
`mlr3pipelines_graph` use `integrity-collection-read` (2.75/3.00). They validate
an origin generation/signature or the complete capsule DAG before returning a
detached value. The ceilings are finite same-host ratios, preserve marginal
review, and still reject retained pre-optimization evidence. The exception is
timing-only; allocation retains the `hot` 1.25 ratio and 16-KiB minimum.
Consumer `$params`, `get_values_unchecked`, filtered getters, and all other real
consumer operations remain `hot`; domains, dependencies, constraints, and
mutation do not inherit the exception. Reassess and normally remove these
contract-reset tiers when Paradox 2 replaces v1 as the authenticated baseline.

## Acceptance and replay

Each release stage records a unique run ID and exact ref/commit/tree, and seals
its complete required artifacts. Verifiers are read-only and may reuse
authenticated inputs, but cannot transform evidence for one semantic package
payload or DSO into evidence for another. A later source change reopens only the
gates it can affect; before the first contract-first candidate, the complete
matrix is necessarily fresh.

A ref-only or release-tooling change is different from a package change. A
completed package-facing gate may be reused only when a retained, sealed, and
independently replayed equivalence stage proves that every changed Git path is
excluded by the exact candidate `.Rbuildignore`, clean builds have the same
complete payload inventory, and every built payload byte agrees after removing
only R's generated `Packaged:` record. The release ledger must identify both
commits and trees, the normalized manifest, and the affected gate families.
Source-tree tooling, analyzer policy, documentation/profile inputs, benchmark
drivers, and portability harnesses remain independently bound and are rerun
whenever their own bytes change. Likewise, after a downstream head changes only
tests, rerun that package's affected rows and transfer other consumers only
after proving their heads and production bytes unchanged. This narrow identity
rule avoids expensive zero-information rebuilds without accepting behavioral
similarity as release evidence.

The accepted run IDs and candidate hashes belong in
[`release-2.0.0.md`](release-2.0.0.md). Until that ledger says `accepted`, no
collection of partial green diagnostics is a release authorization. The
historical `dbbdcc1` candidate's original `release-core` native child had a
non-replayable source snapshot; the repaired harness subsequently produced and
independently validated
`release-candidate-dbbdcc1-native-replay-r2-native-release-a001`, which supplied
the reviewed rchk discovery and historical combined-memory conclusion. None of
those results accepts reopened package-facing source. After a new candidate is
frozen, complete every applicable local and hosted gate against that exact ref,
followed by user-performed remote publication.
