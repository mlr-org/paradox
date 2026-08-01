# Paradox 2 C rewrite: maintainer and agent notes

## Authority

The first public Paradox 2 release is a contract reset, not a continuation of
the compatibility-first native candidate frozen in July 2026. The normative
behavioral and architectural contract is
[`design/contract-first-2.0.0.md`](design/contract-first-2.0.0.md). The shorter
implementation map is [`design/architecture.md`](design/architecture.md), the
intentional compatibility boundary is
[`design/compatibility.md`](design/compatibility.md), and the active release
ledger is [`design/release-2.0.0.md`](design/release-2.0.0.md).

Old candidate commits, refs, logs, and artifacts are historical unless a
retained sealed proof transfers an explicitly bounded conclusion. Git history
retains the former long design narratives; do not copy their R6-surface
authentication, S3 fallback, sentinel replay, or pre-data.table-1.18 decisions
back into current source. The independently replayed `a4617ca` to `10c6a0e`
package-payload proof is one historical transfer for that superseded payload;
it establishes nothing about the active implementation.

The active immutable package-facing candidate is
`refs/paradox-release/candidate-20260801T051235Z`, commit
`bf0b68fa3bef496dcc09b31658c9e5453cba5276`, tree
`79873126ae3034962edaaad30745914115240be5`. Its exact
`release-candidate-bf0b68f` `release-core` run passed all nine tasks. This
includes the four harness rows, all 33 differential cases, the complete API-
header matrix, explicit C23 installations and native probes under GCC 15.2 and
Clang 22, the complete native-release lane on R 4.6.1, and
`runtime-supported`. The latter passed the full suite at R 3.6.3, 4.0.5,
4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2: 64,848 assertions passed and all 145
skips were exact reviewed capability results. Together, the runtime and native
lanes exercise every minor series from R 3.6 through current at the eight
versions just named. The old-R stress slices, exact R-3.6 declared-floor smoke
and package check, and R-4.0.5-to-R-3.6.3 serialization handoff also passed.
The coordinator completion, JSON-summary, and TSV-summary SHA-256 values are
respectively
`1f24c42c7fe7be59629e25bc6fd910916524b13aea371d5665a4e14c4b55a276`,
`22a4bb223c413214b14b218797f3ad61cca51d691e20147a6ed232a0f37e9fdf`,
and
`98e77c2c4f05d51b2728eb3aa410e636d4349cb63de8bd511963692bfd778558`.

Native child `release-candidate-bf0b68f-native-release-a001` is a replayable
source donor and passes independent source-run validation. Its source
manifest, copied source tree, copied modes tree, and completion-content
SHA-256 values are respectively
`12c9b6a07426d3036ebbad4e82f48d92a1fd8b154e1f0e8cfeeaff2fa7b30b65`,
`b6973336386e691cd220d1d00ea557db3e1b425e383ae89070962af0bcca2ba0`,
`0e6ab98dd3c8ba7e5a8c6036fd9064ac8fc4647f732a6681996778508bf9e951`,
and
`7c0bd80e9b439fe6ee531fda3fa060e3f5776c58042912c2db5e9eaf4be3d963`.

The active candidate has not yet passed combined memory acceptance. Attempt
`release-candidate-bf0b68f-memory-r1` completed its GCT probe and then failed
closed in Valgrind prerequisite preflight because the live
`.local/toolchain/lib` directory mode had drifted from the sealed `0775` to
`0777`; the other 30,916 receipt rows matched. Restoring `0775` made a fresh
complete toolchain receipt byte-identical to the sealed receipt, SHA-256
`c4ff86d8334edbda0bc91a6748817e78fe39e700910e353f5098c7fa9fd7876d`.
This is environment-integrity diagnostic evidence, not package or analyzer
evidence.

Fresh attempt `release-candidate-bf0b68f-memory-r2` passed GCT and the direct
Valgrind probes, then failed the exact no-leak policy in the analyzer-only
testthat process. Loading testthat started cli's detached timer thread; glibc
reported its asynchronous-shutdown TLS allocation as 336 bytes in one
possibly-lost block through `cli__start_thread`, with zero definitely or
indirectly lost bytes and zero suppressions. rchk did not start. The current
narrow package-facing-source-identical harness repair sets cli's supported
`CLI_NO_THREAD=1` only for that analyzer process, leaving direct probes
unchanged and retaining zero suppressions and exact definite, indirect, and
possible-leak rejection. The complete validation-hardening, memory-validator,
and verification-economy self-tests pass; an exact instrumented-R/Valgrind
probe with the switch also reports zero possible loss and zero errors. The
checked-in rchk policy still describes the earlier `dbbdcc1` source, however,
while the active candidate has materially different native source. Run a
fresh source-bound rchk discovery, review every changed block, commit its exact
policy, create the required new static/focused source donor, and only then run
fresh immutable combined-memory `r3`. Do not claim memory acceptance from
`r1` or `r2`.

The preceding immutable candidate is the rejected diagnostic ref
`refs/paradox-release/candidate-20260801T034415Z`, commit
`fb2a37fc7d9b29d1998a8639a01e18130eaa4919`, tree
`86283a233c6de7d16c3d3bd20e682fa0ed5409c1`. Its exact
`release-candidate-fb2a37f` `release-core` run passed every one of the eight
non-runtime rows: the four harness rows, all 33 differential cases, the
complete API-header matrix, explicit C23 installations under GCC 15.2 and
Clang 22, and the complete native-release lane. `runtime-supported` built,
installed, and ran the full test suite on all seven supported-minor runtimes.
All test assertions were clean; R 3.6.3, 4.0.5, 4.1.3, and 4.2.3 failed only
when result reconciliation found one list-ALTREP capability skip without its
reviewed ledger row. R 4.3.3, 4.4.3, and 4.5.2 passed that stage. Completion,
JSON-summary, and TSV-summary SHA-256 values are respectively
`9f84f7900b83671042f8ba7dc78016be1dd59f29c169e5abffb7a837a7cae8a8`,
`9ceb023f9d572f1273b1b456951b50bb07d0c8b8782098d588ed53902bcd7d6f`,
and
`9aadf3a9227779e1d4b04b5099362205b46e4091fbc5a36fcd2edf7bed1a8697`.

The missing row was the derived result for `materialized and rejected inputs
remain safe under forced collection`. Reconciliation stops the affected stage
before its post-suite phases, so the old-R stress slice, final R-3.6 package
check, and cross-version serialization handoff did not run. Reopened source
puts the reviewed list-ALTREP capability guard at the start of that test and
adds exactly the four derived rows for R 3.6.3 through 4.2.3. The hidden
fixture helper now errors if called on an unsupported runtime instead of
dynamically skipping; reviewed leading guards are the sole authority for
version-capability skips. This repair is development state, not acceptance.
Candidate `fb2a37f` is rejected, package-facing source is reopened, no
replacement candidate has been named, and every applicable source-bound gate
must run again after convergence.

The preceding immutable candidate is the rejected diagnostic ref
`refs/paradox-release/candidate-20260801T014150Z`, commit
`6f28daed7596413d5c1227134b2bfa28a2ef7721`, tree
`1b283f19be4534ed30b86017e75eaa9426ec31e2`. It is
package-facing-source identical to `de1752f` and adds only the reviewed
recursive runtime-test staging repair and release documentation. Its exact
`release-candidate-6f28dae` `release-core` run passed eight rows: the four
harness rows, all 33 differential cases, the complete API-header matrix,
explicit C23 installation under GCC 15.2 and Clang 22, and the full
native-release lane. `runtime-supported` alone failed, this time after all
seven supported-minor suites actually ran and every recursive support-tree
receipt passed. Completion, JSON-summary, and TSV-summary SHA-256 values are
respectively
`9c1b797ae67324bf448f5c07227ee1a2264f406b61bc040dd783e54e5df3be5a`,
`521bf58a476b816920132b3e8432b4b8dfb79a080429bd567281baeb26a0cfe6`,
and
`e25224aeca5225f28c5277bb9d00f2be2674f5be43ecfa2b0dff6563d61d97da`.

The seven runtime failures are bounded to four families: one real old-R
diagnostic-encoding difference, plus portable no-`DATAPTR` ALTREP fixture
construction, versioned terminal-callback counting, and the documented R 4.5
promise-inspection fail-closed expectation. The encoding path and all three
test-fixture/policy families have focused repairs in the reopened source, but
those repairs were development state at that point, not accepted evidence.
Candidate `6f28dae` is rejected; the later `fb2a37f` run exercised those
repairs but is separately rejected for the reviewed-skip-ledger omission
above.

The preceding frozen package-facing candidate was
`refs/paradox-release/candidate-20260801T000111Z`, commit
`de1752fe2085dc021b2e6936bcd3d5c0c809d9d4`, tree
`b91022dfcae1f1daeb886533620a234228f08803`. Its exact
`release-candidate-de1752f` `release-core` run passed the four harness rows,
all 33 differential cases, the complete API-header matrix, explicit C23
installation under GCC 15.2 and Clang 22, and the full native-release lane.
The native lane completed strict GCC/Clang builds, both analyzers, exhaustive
cppcheck, symbol policy, full tests, a clean `R CMD check --as-cran`, ASan, and
UBSan. The supported-runtime task alone failed. Completion, JSON-summary, and
TSV-summary SHA-256 values are respectively
`69634511a79c0d8ab2b6b8dd92aa3b35012f58d420e1333cc4c1ba821cbc44e9`,
`6e6fa7f39e63a74890ecb9b2e062822b9add56a99b24ddc89986c057192a431c`,
and
`76d60a19e82fc0249e5f4bf8b3f8b81ff240181c04a8eec9f6e34676f1306e94`.
This is not complete candidate acceptance and is now historical diagnostic
evidence.

The runtime failure was a harness staging defect, not a package or supported-R
failure. Every R 3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2 stage
successfully built, installed, loaded, probed, and symbol-audited Paradox
before the common runner rejected the tracked `tests/testthat/_problems` and
`tests/testthat/fixtures` directories as if every top-level support entry had
to be a regular file. No stage reached testthat; the old-R stress, final R-3.6
check, and closing receipts therefore did not run. At that point only
package-facing-source-identical validation tooling and release documentation
were reopened for the recursive support-tree repair. The later `6f28dae` run
proved that repair and exposed the bounded runtime failures above; the
subsequent production diagnostic repair reopened package-facing source.

The repaired contract keeps testthat's actual discovery boundary: only
top-level `test*.R`/`test*.r` files are executable tests. Every other
authenticated regular leaf, including nested test-shaped diagnostic fragments,
is support and is copied without flattening at its exact relative path. One
shared R-3.6-compatible projector rejects symlinks, special members, ambiguous
paths, mode drift, byte drift, and destination overlap. A deterministic
SHA-256 tree receipt is created before testthat and verified after it; the
coordinator and independent evidence verifier replay the receipt and
independently reconstruct the recursive source projection. The old-R stress
runner uses the same projector and differs only by its separately regenerated
top-level filter. Focused self-tests cover a `0777` umask, empty files, empty
and nested directories, a nonempty read-only directory, duplicate basenames,
nested test-shaped support, file/directory/dangling symlinks, FIFO,
control-character paths, non-empty or overlapping destinations, and receipt
tampering. The runtime contract remains all minor series: the exact
runtime matrix owns R 3.6 through 4.5, and the complete native lane owns current
R 4.6.1; do not add a duplicate current-R runtime stage.

The preceding frozen package-facing ref is the rejected diagnostic candidate
`refs/paradox-release/candidate-20260731T225009Z`, commit
`1720d60c3bb8eefbc02d9b0455a9ee537b95a97a`, tree
`044a7fe8c58098f6739e5cef6e715c4bf30981d6`. Its exact
`release-candidate-1720d60` coordinator passed all seven independent foundation
tasks: the four harness rows, differential, the complete API-header matrix, and
explicit C23 builds under GCC 15.2 and Clang 22. `native-release` then passed
the strict native full tests, a clean `R CMD check --as-cran`, and the complete
GCC analyzer. The Clang analyzer reported two uninitialized-`kind` paths in
`builtin_condition.c`; the every-minor runtime task was correctly blocked.
Both reports are analyzer false positives, not package defects. Admission
returns `R_NilValue` on every path that may leave `kind` unset and a non-NULL
snapshot only after exact built-in Condition admission, but Clang treated the
externally declared `R_NilValue` binding as potentially mutable across the
caller's intervening `PROTECT()` and therefore lost that sentinel/out-parameter
implication. Completion, JSON-summary, and TSV-summary SHA-256 values are
respectively
`847829b78e6d5c58f360b10f0fef36c310514b3d2a42cb4ee5bc436a4ae65540`,
`63cdfae9d5b266ccb74963a07425669642d0546c2519fee868c418f3f132c135`,
and
`9f5abf153eb7d5f6f95d0fb34d98ca385d907fce39750a83f453a7e5aa29c841`.
This is diagnostic evidence only. Source was reopened solely to test the
admission result before `PROTECT()` at all three consumers. No allocation,
callback, or R transition occurs between return and that protection, so the
selected non-NULL snapshot remains safe; the accepted path retains exactly one
protection, adds no default enum store, and has identical or better generated
code. The first follow-up Clang pass then exposed three dead `R_NilValue`
initializers for `outward_class`, `outward_selfref`, and `outward_repr` in
`domain_row_admission.c`. Every path to the loop's sole `break` assigns all
three from the selected metadata generation, while the only earlier
`continue` restarts and overwrites them; removing the initial stores changes no
behavior or optimized code. The complete follow-up Clang analyzer preflight
passes at
`.local/checks/release-condition-admit-clang-20260731-r2`; its completion,
source-manifest, and analyzer-report-list SHA-256 values are respectively
`d8fa00a349db9561f1e0e014e653f963afb474ab9136d1a0636359f2eb496811`,
`f2e055c67b833e380067d0eef6bec0429cb0b8ab5f42e631c8b9f9f6a2b2406c`,
and
`00ed8c7e5176591f023ceb7928cab3917949cb50fb2872e446561003f3cff595`.
The first complete static/focused preflight,
`release-static-focused-20260801-r1`, passed the strict GCC and Clang installs,
focused tests, GCC analyzer, and complete Clang analyzer before cppcheck
reported five harness-model diagnostics. Cppcheck's `unix64` model knew the
size of `uintptr_t` but omitted `UINTPTR_MAX`, and its compiler-neutral C99
model erased R's real `NORET` declaration; the package's unsupported-width
`#error` and four impossible post-`Rf_error()` NULL paths were therefore
selected. No package defect was found. The cppcheck invocation now completes
those two exact model facts with `UINTPTR_MAX=UINT64_MAX` and R's
`__attribute__((noreturn))`; it does not suppress a diagnostic, change package
source, or make a broader compiler-model claim. The complete 39-translation-
unit harness replay `release-cppcheck-model-20260801-r1` passes. Its completion,
source-manifest, and cppcheck-log SHA-256 values are respectively
`719792c414b3a51d0a7994aaf54a7352cc603dc299dac813b519ed40c8636fc2`,
`8e0329bca4243941d42d8d10fafaa727033907b52a02f8cd0c79a65212ed01e2`,
and
`c88c365634d0813b982fc0a97c550a5feb0a8e7a833e423d39712d2a96faaad7`.
The exact package/harness-source replacement preflight
`release-static-focused-20260801-r2` now passes all six modes: strict GCC with
the focused package suite, strict Clang probes, GCC analyzer, complete Clang
analyzer, exhaustive cppcheck, and the symbol/registration audit. Its
completion, source-manifest, GCC-analyzer, Clang-report-list, cppcheck-log, and
registration-log SHA-256 values are respectively
`f45c566a308340c45475dd25682ba31085c5d4e9a2c986e894d4a288c39a2415`,
`222b0df99e5bec279d95bb7fab56e36533d2692767364be1be62084d688a618e`,
`c4ecb5d84e31537baf2fab335b445b2311ec23cb2a68df3bc232505691ecac33`,
`210b2dd82ad36618afd565499b1b780921e538787d3c13f320d9105e7508ac22`,
`779667b845361e3b31367871745c6c74af7026e0dafda8aa04a5b1de6c2b9391`,
and
`bebd79a1f7b3333b0db3a671cf518e8391a5f89c1d68cd69689639e676753e68`.
This is focused development evidence, not candidate acceptance. Freeze and
fully execute another immutable candidate.

The preceding rejected diagnostic candidate was
`refs/paradox-release/candidate-20260731T221636Z`, commit
`d892d94b11109fd2817b3f78db2127781cb35542`, tree
`6d93885457f42f57026a57d5a278bcd05dfde6ce`. Its exact
`release-candidate-d892d94` coordinator passed all four harness rows,
differential, the complete API-header matrix, and explicit C23 builds under GCC
15.2 and Clang 22. The strict native full tests and clean
`R CMD check --as-cran` passed, then GCC 14 `-fanalyzer` exposed one relational
false positive in the optional Domain receipt workspace: it skipped a
computed-nonzero workspace-size branch and later independently treated the
same bounds bit as true. The every-minor runtime task was correctly blocked.
Completion, JSON-summary, and TSV-summary SHA-256 values are respectively
`5f40ae2ac9c61992430f6fb3336427a6406476b269ae01264740252daad55a6f`,
`0f8bb02da2eb087f6044c4ebdad82cb227ca0b2a63e03b620dacb6f60d0d5221`,
and
`2e4c6f61b82f7b67c2897ab4eba45e911bdc722bef5343ea0dd5beb252327849`.
This is diagnostic evidence only. Source is reopened solely to branch the
single existing workspace allocation directly on the cached bounds/special
booleans and reuse those exact booleans at every access. The rewrite preserves
the allocation count and exact bytes and adds no guard or hot-path work.
The complete GCC-analyzer development preflight
`.local/checks/release-domain-workspace-analyzer-20260731-r2` passes; its
completion, source-manifest, and analyzer-install-log SHA-256 values are
`5231952a3868b4a58d28084ba8b36aa4f35e99de925da4ac86b13761f94fb723`,
`320e7a7a320e844b1267b73492fe672650a7d49d537a6188a89501fb20d31f13`,
and
`36a72e83ef84dc4f152536290c231133c030a813c5d1712fbd19bb168144b3eb`.
The directly affected installed-package selection also passes 1,591
assertions with no skips. This is focused development evidence, not candidate
acceptance. Freeze and fully execute another immutable candidate.

The preceding rejected diagnostic candidate was
`refs/paradox-release/candidate-20260731T215429Z`, commit
`41dc51dc4f6de4d92999a34cbd2cab6f17364db7`, tree
`85f2bf5e84a9df32143e1b9d3e404c74be831de9`. Its
`release-candidate-41dc51d` coordinator passed all four harness rows,
differential, the complete API-header matrix, and explicit C23 builds under GCC
15.2 and Clang 22. The full native test executions and every substantive
`R CMD check --as-cran` stage passed; the check ended with one code-analysis
NOTE because the cold categorical Paradox-1 sampler gateway left the
syntactically present `sample_truncated` symbol bound to `NULL`. The
every-minor runtime task was correctly blocked. Completion, JSON-summary, and
TSV-summary SHA-256 values are respectively
`c46f32dad37d5d6b2fb8dab1287433eb4bfc30ecbe911c449259052d957f167f`,
`23b1a0374a849d26fc96b79b4dc2b36ef96372bfed822270da4cc62d520d889a`,
and
`cf9bcef5fe4784fb85fc5297f2ee43e83776279abd51ede3181e6c04c119f96e`.
This is diagnostic evidence only. Source is reopened for the narrow cold
gateway binding repair and its focused regression; freeze and fully execute a
new immutable candidate afterward.

The earlier rejected diagnostic candidate was
`refs/paradox-release/candidate-20260731T150816Z`, commit
`a153faeeed8735c4aeeba512a70fb71b11235469`, tree
`2fe241b90b0e86381a6f884fd29d3a80baab6b7b`. Its corrected source-bound Domain
comparison under
`.local/perf/domain-indexed-root-final-threeway-candidate-a153fae-repaired-r5/results`
passed all eight timing, allocation, instruction, and changed-code gates; its
completion, gate, round, and summary SHA-256 values are respectively
`379d4c1839c2d05f8c7c8c9d867f492d68355ef3fc22eae46fdf36a389a417d7`,
`e87450b0ab084a0f43867daeb6aee7473eea888b952af9a306c894fa7f89dce2`,
`0cfb24b08727eb930ee34f42384242abaece108181e70e66bbc1bddea7c60d8c`,
and
`ed1d3d6efc8f311d80e8371e0d055aebcdc87f3f5c19d6d1950ab3386672cd43`.
That bounded performance obligation is closed and must not be rerun for
unrelated cleanup.

The subsequent `release-candidate-a153fae` coordinator passed five tasks,
failed three, and dependency-blocked the every-minor runtime task before it
started. The four harness rows and API-header matrix passed. C23 stopped before
compilation because the validator expected an obsolete micromamba inventory
header; differential completed all 33 cases with zero unexpected differences
but found seven stale candidate hashes; strict GCC completed the package suite
and exposed six errors—one stale expected diagnostic, one genuine formal-S4
leaf misclassification, and four authentic populated Paradox-1 transformation
tables whose keyed spelling omitted `row.names`. The completion, JSON summary,
and TSV summary SHA-256 values are respectively
`83140b9737809c8b8db6457160938bdc5a39b6db4531fef42962508b61e3e165`,
`e73d372b7533782ba66f922948c7acb4764aa115cafe02c9a64973a531b5269f`,
and
`f912757fe598e4e5fa269e6511f93969b20ebf7aee9d22c9fbd3881626755737`.
This run is diagnostic only and is not a memory donor. Source is reopened for
the reviewed C23-inventory repair, refreshed differential ledger, one shared
formal-S4 semantic-leaf classifier, and coherent independently owned legacy
table snapshots. Freeze and fully execute a replacement candidate after these
changes converge.

The earlier frozen package-facing candidate was
`refs/paradox-release/candidate-20260727T152133Z`, commit
`dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea`, tree
`b60b75e3923cdcb49f1ef2fb0f9b386d5cac291d`. It includes the managed
active-path carrier and its ordinary and deterministic instrumented
regressions. The exact `release-candidate-dbbdcc1` `release-core` run completed
all eight tasks, including strict native/sanitizer/package checks and the R
3.6.3, 4.0.5, 4.3.3, and 4.5.2 supported-runtime matrix. Those successful
package executions remain informative historical evidence, but the retained
native child run cannot seed the memory gate: its source manifest records the
original worktree modes while the copied source tree reflects the worker
umask. Package-facing source has since reopened; no result for this ref accepts
the current source or substitutes for a fresh immutable candidate.

The first combined-memory attempt,
`release-candidate-dbbdcc1-memory`, stopped in source-proof preflight before
loading Paradox or starting an analyzer because its relocated source-run
validator lacked the trusted `create-offline-check-repository.R` sibling. The
failed attempt is harness diagnostic evidence only, not a package or memory
failure. The six-file harness repair retains, hashes, and independently
validates that helper; preserves and verifies exact regular-file modes in both
the initial and replayed snapshots regardless of caller umask; and tests the
complete relocated-helper inventory. Do not mutate or rehabilitate
`release-candidate-dbbdcc1-native-release-a001`. A fresh full native source run
under the repaired, package-facing-source-identical harness has now created the
replayable source proof:
`release-candidate-dbbdcc1-native-replay-r2` passed all four selected
controller/harness/native rows, and child
`release-candidate-dbbdcc1-native-replay-r2-native-release-a001` passed the
full native lane. Independent source-run validation reproduces its exact
21-file harness inventory and source bytes and modes. The coordinator
completion, JSON summary, and TSV summary SHA-256 values are respectively
`2b4781b276583903f929f3b659037ce1ec228dbbe3755ca7ca2c29a2d776ba01`,
`9a279062fa8f2d99ff97561b6530dd3944614d04fc133e953b772aee64f5cb80`,
and
`92eb669197c8cd82a43f2424143efe7f3967213a3a1da2006bcdc691ce519bd2`.
That child was the immutable donor for the isolated rchk policy discovery. Once
the source-bound policy changed, the final combined-memory run used a new
static/focused native donor from the converged policy/harness commit; it did
not require another full package acceptance run.

The first isolated bounded-rchk discovery attempt from the new donor,
`release-candidate-dbbdcc1-rchk-discovery-r1`, failed closed in resource
preflight before starting the analyzer: 36,328 MiB was available against the
reviewed 20-GiB analyzer allowance plus 16-GiB protected host reserve
(36,864 MiB total). It is capacity evidence only and was not reused. Both
limits were retained; after sufficient nonessential memory was freed, the
replacement used a fresh discovery run ID.

The admitted replacement discovery,
`release-candidate-dbbdcc1-rchk-discovery-r2`, ran all three analyzers
successfully and failed only at the intended old-policy comparison. It analyzed
951 functions and 50,395 states: 83 Function blocks contain 239 UP and 17 PB
diagnostics. Raw bcheck and ordering-insensitive semantic SHA-256 values are
`ddbad6140d74e96422bc4f77942f9303af9e51706005f9f95d36b3d677b011d8`
and
`9135cbe02ddbd69019d7901d09bd5a7286fd24b74260e1864be0c92aeceef1d9`.
Maacheck is byte-empty; fficheck reports that source's exact 82 functions and one
registration call, SHA-256
`448c7d8dae05fab9b43570ed169a4472de8ca6ca40836061642c4203c3391882`.
The independently generated and validated policy, block table, and unchanged
rationale catalog SHA-256 values are respectively
`eb1e2e9d89a27b9a43a7be87606c22b71fef636284c5c570c5f795aa2345833f`,
`d0ba3cbc6d2b536b157939fc3c6cbe2d9fcf5b2ed497a088234ee345dddf2fad`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.

Every new or changed block was source-reviewed. No C defect was found. The four
additional PB diagnostics all come from bcheck losing the conditional
list-column protection depth in `build_dependent_grid()`; branch-by-branch
accounting is exact. The native GCT probe now also exercises a dependent,
list-valued fixed special value and its inactive row, rather than relying on
separate atomic GCT and ordinary list-column tests.

The provisional R 3.6 ref `refs/paradox-release/r36-39855c9`, commit
`39855c919beec323fd5940e4c46286e8df1be8ff`, completed all eight
`release-core` tasks, including the four supported runtimes. A subsequent
independent audit found that its capsule-graph validator stored active
`SEXP` pointers only in unscanned `R_alloc()` memory; an old-R optional binding
lookup could run a finalizer that detached such a generation. That run is
therefore historical development evidence, not candidate acceptance.
Post-freeze release-policy, validation, and ledger commits for the active
candidate must prove package-facing paths unchanged; call that
relationship *package-facing-source identical*, not package-identical, unless a
separate sealed complete-payload byte proof exists.

The original source-bound bcheck discovery under
`.local/checks/serialized-migration-release-8797f11-memory-20260724/modes/rchk`
analyzed 870 functions and 30,245 states and produced raw report SHA-256
`02b08085ea0fadc906fb8e8fdd3f5211a6eb2a7eb08e922205e69f4d25361072`.
The final combined memory run under validation tooling
`a05cd51a5570c4a674b6c80d6cd38c7898223635` reproduced the same reviewed
semantics—80 blocks, 238 UP diagnostics, and 13 PB diagnostics—with final raw
report SHA-256
`0226275247eb16736ab317dfb3e1c7f836ee006fb59683276998632aa120cd9d`
and ordering-insensitive semantic SHA-256
`f3dc5caccc4508f9f9263d8d912455820dfba428cb00f7d0454e1734adf6da18`.
The two raw reports are not byte-identical. Maacheck is byte-empty
(`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`);
fficheck records 79 functions and one registration call
(`565392164712e15df4bbdd0b34fb852b35eeab380f612c0983f2ccab69c31370`).
The final policy, block table, and rationale table SHA-256 values are
`e6010f58c58dfb8e952ee0e515a1a2352decd143e01bda50af7c800b4aa0470d`,
`50445fd2be3da7cbeb05f689377802deac7597ee2eeed8acc34f683989e3a71d`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The `a05cd51` tooling tree
`c58b6bea97d66e23b542a34864479e28c5e5e02f` is package-facing-source
identical to the candidate. Its retained GCT/Valgrind/rchk completion is
`a07df9509c78976d6d8a31a9b4f3415787f512e73bf85a2311d6ced672e9b226`;
that combined completion, not discovery alone, owns the memory conclusion.

The discovery exposed a real protection imbalance in `schedule_vector()`: an
ALTREP `Duplicate` method may legally return its input, but pointer equality was
incorrectly used to decide whether to release the second protection. One
indexed root plus `REPROTECT` fixes the imbalance, and the candidate includes a
self-returning ALTREP-list regression that captures R's direct stack-imbalance
diagnostic. `schedule_vector` is absent from the final reviewed report.

Post-freeze validation uses the named `release-refresh-20260720` profile and
explicit `paradox2`/`paradox1` axes. Candidate, tooling, overlay, dependency,
repository, documentation, and benchmark identities remain separate and must
never be relabeled. The last historical `8797f11` candidate's final
benchmark/documentation tooling was
`fc92edd7f1ab612468066fe06bd3d9fc7afea41c`, tree
`05cc4e5213c5ee73d0bc764c3d102c15e4c57141`; its diff from that candidate
contains no package-facing path. Its sealed 77-row release benchmark passes
with 73 pass, four bounded marginal reviews, and zero failures; completion,
manifest, and seal SHA-256 values are
`5d615637bd6448bb95b8ba798cc8f7e23d40a78e1a63a4e3988eb436914109a0`,
`a16d403c20ed8ab80233a877e81ec146b4a5c128fdf0a765be440993eb325499`,
and `d7a4bbefdba3e1d0a800b2e6bdae6e10ddde22b612cf05bebc5d019eba198fc7`.
Its policy has exactly seven integrity rows: `shadow_values_live`, the three
synthetic `collection_values_{plain,rich,nested}` rows, and the three real
miesmuschel/mlr3pipelines consumer `$values` rows. Consumer `$params`,
`get_values_unchecked`, and all other consumer operations retain their ordinary
`hot` tier.

The broad repository corpus used `jobs = 2` in 14 admitted waves and has 20 of
28 exact repositories green; the eight retained non-green rows are reviewed
non-Paradox upstream, optional-runtime, environmental-dependency, or bounded-
timeout exclusions. The final exact Paradox-2 source-package check independently
builds and checks bbotk, miesmuschel, mlr3mbo, celecx, and mlr3fda; all five
finish with `Status: OK`. All mandatory documentation rows pass; advisory
`mlr3book` full-render and legacy `mlr3gallery` dependency rows are excluded and
do not weaken the focused Paradox documentation conclusion. All local release
gates were complete for that historical payload; none is a completion claim
for the current package-facing source.

The direct-child portability companion is
`refs/paradox-release/portability-harness-5305ead`, commit
`5305eaddbc9c2159fe194e6be10388c17b4c506a`, tree
`e1fe00ddad08af89566e3df12460bc47d3e98292`. It changes only
`.github/workflows/r-cmd-check.yml` (SHA-256
`14c4c8d1cc8d8e07aea1829d1203f6217464ae9c6b94efc1050bf192638196e3`)
and is package-facing-source identical to that candidate. The only remaining
release gates for that historical candidate were retained hosted Windows
x86-64/macOS ARM64 results and the user-performed downstream branch/PR
publication handoff. That historical replacement candidate later passed
`release-core`, a fresh replayable memory-source proof, and its combined-memory
gate. Package-facing source has since reopened, so those conclusions are
historical and every applicable source-bound gate requires a new candidate.
Agents must not perform any remote write.

The structural boundary below is an intentional Paradox-2 break made in this
release, not a migration shim to relax later. Supporting exotic structural
ALTREP/S4 shells or parallel R/native admission would preserve no known
maintained use while retaining duplicate authority, dispatch, and
multi-observation hazards. Ordinary documented containers and stable ALTREP
semantic vectors remain supported at their stated positions.

The pre-release dependency contract is also intentionally settled here:
Domain-valid values may be stored while dependency-inactive, defaults
participate in recursive activity for absent parents, and constraints observe
only active entries. This is not a policy toggle or a deferred compatibility
shim. Explicit check-family calls remain strict point validators and are
store-blind, so a legal raw dormant store is not itself promised to pass
`$check()`.

The final pre-release performance batch is governed by
[`design/final-performance-implementation-plan.md`](design/final-performance-implementation-plan.md).
That plan was recorded at package commit `b2e1649` before implementation and
locks both the measured targets and their compatibility/integrity proof
obligations. P5 landed at `81cbccf`; P1--P4, the profile-led Condition/RHS
follow-ups, direct native probes, and focused tests landed at `387c1cd`.
Balanced per-slice evidence is complete and recorded in the plan, including
the measured stop boundary at the remaining single exact integrity pass. Do
not rerun those slice A/B experiments during minor cleanup. Do not run the
full compatibility, memory, portability, or release matrices until source
converges and the final release gate begins. Operation-local indexes and
one-use package-private ownership handoffs are allowed optimizations;
trusted caller metadata, skipped graph checks, and weakened generation
reauthentication remain prohibited. The capsule verification stamp is not an
exception and must not become one: it records that no capsule was installed
anywhere since a node was proven consistent with its children, which is
exactly the condition under which revalidation can only repeat its previous
answer. It never records that a caller may be trusted, and it never survives
a change it did not observe.

The later Domain admission carrier compaction is a distinct bounded follow-up
governed by
[`design/domain-admission-receipt-compaction-plan.md`](design/domain-admission-receipt-compaction-plan.md).
It does not reopen the P1--P5 slice experiments above. Its indexed-root and
focused correctness work and the subsequent unified-capture recovery are
complete, and candidate `a153fae` passed its corrected source-bound three-way
comparison. That bounded obligation is closed; do not rerun it for unrelated
cleanup. Its optional bounds/special receipt workspace has one structural
owner: allocation and every later access use the same cached interpretation
booleans. Do not recover a computed workspace-size branch followed by repeated
raw mask tests; besides obscuring the invariant, GCC's analyzer can lose that
relational fact and invent an impossible NULL path.

The exact focused adversarial review closure is retained at
`.local/checks/integrated-selectors-r9-20260731T122010Z`; its source-manifest
SHA-256 is
`0a000dfb692332aed5eacd80119b9ae3fdf037e21a31fa7bf1cf60956584db0a`.
The final review closed the Condition post-`Length` bounded re-admission,
initialized every interpreted Domain row's empty-special names-presence byte,
and rejected a provisional broad exception for structural ALTREP names in
favor of materializing only names produced by Paradox itself. The five
directly relevant package-source SHA-256 values are
`f9b81c1d3aca0f99bae5bfc3625ba252eb513eb177c2e1b6dbf8d760110104fb`
(`src/r_utils.c`),
`68233a976348f6b09ff42f5157fc780a3451fb52c797a91c85cbf82cc71e38c5`
(`src/domain_row_admission.c`),
`621ce8498691a7ff60ba39e8cff617024664738c505a295cfa310cfd854aceff`
(`src/builtin_condition.c`),
`4ee3b9179ff78880969acedcaafefb2f0eb8356c87d8a35b3d5ee590e16a3d53`
(`R/ParamFct.R`), and
`e676006d826fc97089d56ff5852fa369b79de4f9250b44cb7bfba7b3a66d940d`
(`R/ParamSet.R`). The current-R `Condition`, `native-design-transpose`,
`native-domain-construction`, and `native-public-accessor-ownership` logs and
the actual-R-3.6.3 accessor-ownership log all ended `DONE` with exit zero;
their SHA-256 values are, in order,
`7e494791778fad9946d60883cf0d514b403dfd11b312c80af2bd3c550fd2a27d`,
`eb1940f1604ef0174515ed11db7ad6584f7b78ecb1c3e2389178da29221e91e0`,
`033caeaef2f31c11d05f2bcaefb584d47edc60bcfe3cf2737d61a9a980d24df3`,
`9c97379a6cb472533ee656ceb683e83f850c7c15f9ced6e507bb38f086e4d3d6`,
and
`c95f6a004903c3bd1d4d0bdc70847254fcd2a8410438632ee770c5feec791b2f`.
The r9 C/R implementation is byte-identical to r8 manifest
`6ed37e6c07ca17d8f487a949cfc2e4cf510eaded448e89fc1b11c7750a114634`,
which owns the strict GCC/Clang GNU99 and current/R-3.6 package-install proof.
All of this is focused development evidence only. The first immutable timing
candidate,
`refs/paradox-release/candidate-20260731T124039Z` at
`024e28f770f8dd7a8802b70a4a8e866ac2cb344f`, failed the targeted direct
one-row performance policy and is diagnostic evidence, not the active release
candidate. `domain_check_dbl_one` and `domain_sanitize_uty_noop` were
respectively 1.213x and 1.141x the exact `e923c1a` baseline despite zero
allocation growth; bulk Domain gates passed. Callgrind localized about 5,700
avoidable instructions per call to repeated names/row-names recovery around
the three exact outer-metadata generations that actually are required.
Section 11 of the Domain compaction plan freezes the bounded unified-capture
recovery and the corrected timing-harness requirements before implementation.
That recovery now uses one exact five-cell outer-metadata capture per required
generation, capture-local column and row-count receipts, package-load-interned
metadata symbols, pointer-first built-in kind/class resolution, and an ID-based
shape count that never observes callback-capable row names. The three required
shape, admission, and terminal generations remain distinct.

Review fixed an early ALTREP row-name observation that could have repaired a
malformed column, duplicate/poorly ordered typed-class resolution, and a raw
`XLENGTH(environment)` failure for malformed factor levels. A proposed genuine
cyclic-attribute-spine fixture would itself require forbidden `SET_ATTRIB`, so
there is no runtime-policy exception: that public-API-unconstructible case
remains at the hard-bounded static-proof boundary while constructible metadata
hazards have direct regressions.

The exact package-source/test diff against plan commit `aa0bff7` has SHA-256
`8413a97b358ba9a13a45e1137559dd1a4d4d7477a5562d5180f026d82701ac98`.
Focused current-R tests, the 230-record/zero-failure native probe inventory,
and the public-API audit have hashes
`30125069780d5c650055436a9a168092cbaf4f4bd8428db41d1705716861e78b`,
`63c1bcf383aefec6ae729cee086ef775651e22865f5c6f2d654f1c4e7b5f1366`,
and
`2a68d9c8376847945c88837caa2342ac184fb8f0b180f64d1e05f53f57a7febc`.
The corrected actual-R-3.6.3 final install/test log hashes are
`9b905c9f57f46009f289be8dc5d8cd6f402c0e11c4874283635fe52cae14e3b9`
and
`11a3146ba597a593faf31845e1e59314d962e0423db7ebea34e5414851f46589`;
the only three skips are the expected pre-R-4.3 list-ALTREP fixture skips.
Strict GNU C99 GCC/Clang install logs have hashes
`584609789585a6f8d49141b742938f3bd12cc7d7203f23b9a26f87b8f60c5e3a`
and
`9b81d1accc20458fdaa079d714abb8a2ea60c5865aefacda9ec2ca174955bf01`.

Callgrind now measures 15,520.245 instructions per direct double check and
11,355.074 per no-op utility sanitize, respectively 0.706x/0.604x the rejected
candidate and 0.981x/0.870x the exact `e923c1a` baseline. The final bulk total
is 0.99154x exact `aa0bff7`, with a 0.98914x C-entry ratio. The direct-check,
direct-sanitize, final-bulk, and comparison-bulk reports have SHA-256 values
`8f8e6a68bc16a1c70469f6ea944acb04dda124fcdd7e799a98b7540b62e98898`,
`54b31a72e4deb17b399c2a18f2892b235cf57126ff7427983d8fe819937fe4d6`,
`a4a710ee103597f5b0bb3279c71c281e546707683c9826039402379950bb9766`,
and
`5292ddb5661cab24d7357d2c90bb38869a94d5b0155044b2b8c66a154391781c`.
The focused implementation evidence remains development evidence, while the
corrected immutable three-way timing is complete under the sealed `a153fae`
results recorded near the top of this file. Source remains reopened because
the subsequent `release-core` run exposed unrelated C23-harness and native
correctness defects; every complete release gate remains pending.

The final compatibility batch is governed by
[`design/r-3.6-compatibility-implementation-plan.md`](design/r-3.6-compatibility-implementation-plan.md).
Paradox 2 supports R >= 3.6 and uses portable C99. Normal current-R source
installations select a dialect no later than C17 through
`SystemRequirements: USE_C17`; this is an installation ceiling, not permission
to weaken the strict GNU C99 lanes or use C11/C17-only source. The separate
bounded forward-compatibility gate installs the exact package archive with
explicit `--use-C23` under repository-local GCC >= 15 and recent Clang, loads
both DSOs, and runs the native probe inventory. It is not crossed with old R or
the downstream matrix. The detailed boundary is
[`design/c17-c23-release-plan.md`](design/c17-c23-release-plan.md).
Old-runtime adaptation stays
inside the small R API facade, except for the graph crawler's narrow
capability gates where an old runtime cannot expose an edge at all; it is never
a second semantic engine. The real supported-runtime matrix executes the
complete package suite on every minor line from R 3.6 through R 4.5:
3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2. The independent full
native lane owns the current R 4.6.1 execution, so no second 4.6 runtime row
duplicates that expensive suite. The header matrix includes the 3.6.0 minimum
plus the 4.0.0 and 4.2.0 API transition releases before the existing later
axes. Historical candidate evidence that started at R 4.3 remains historical
and cannot prove the active package-facing source.
The R 3.6 stage has two separate dependency proofs: its complete-test closure
and a cached, sealed six-package closure at the exact five direct
`DESCRIPTION` floors plus `digest` 0.6.39. The latter installs the same
candidate tarball into a fresh one-package library and runs only a bounded
dependency-origin/import smoke; it is not a duplicate test suite. A complete
seven-runtime run also serializes a representative current-v2 nested graph on
R 4.0.5 and loads, exercises, mutates, and reserializes those exact bytes on
R 3.6.3. Every selected R 3.6 stage owns the floor proof; only a complete
runtime selection may claim the cross-version handoff.
Hosted portability also has one separate exact Windows x86-64 R 3.6.3/Rtools35
job. It installs the reviewed seven-package runtime closure from the existing
SHA-256 source lock, builds and loads the candidate DLL, runs focused semantic
smoke probes, and performs a bounded runtime-import-only check. It complements
the complete local R 3.6 behavior stage; it is the compiler/linker/loader and
old-Windows ABI proof and is mandatory in the release companion.
Do not authenticate that lane from paths or self-reported banners alone:
`design/portability-ci.md` records the official Rtools35 GCC, G++, `objdump`,
and Make byte digests, and the workflow, source installer, retained
`rtools35.tsv`, and offline verifier must agree on all four. Exact R 3.6.3
Windows already defines default `CXX` as the authenticated G++ plus one
`-std=gnu++11`. Locked `digest` 0.6.39 is the sentinel proving that ordinary
path with four distinct raw compile commands; no dependency override is
needed. One shared helper clears the reviewed hostile R/compiler/make inputs,
binds six empty read-only startup/Makevars files, and creates fresh
phase-specific home, temporary, and library state before all three execution
phases. It also binds `R_BUILD_ENVIRON`, `R_CHECK_ENVIRON`, and
`R_INSTALL_ENVIRON` to the authenticated empty environment file.
`R_PKG_CXX_STD` therefore enters the installer absent. The three exact
policy receipts are mandatory artifact evidence. An earlier raw-G++ audit
accidentally used the R 3.6.0 header source rather than the hosted R 3.6.3
configuration; do not reintroduce its discarded override.

The focused old-R binding benchmark is recorded in the final-results ledger of
[`design/r-3.6-compatibility-implementation-plan.md`](design/r-3.6-compatibility-implementation-plan.md).
On actual R 3.6.3, replacing absence-tolerant lookup at already-required
ownership/core/collection reads produced old/required median ratios from
1.08x on BASE `$params` through 1.24--1.40x on collection reads and 1.50x on
BASE `$values`; paired current-R controls show no regression.  The retained
development-evidence hash list is
`cccced57e09e66a656f89d50cff144602ef1dc3d3df7d6a49207411f3806eb7b`.
This is not release evidence and need not be rerun during cleanup.  Keep the
compile-time-specialized required and optional shell readers: remaining
optional sites are candidate/graph admission, fresh-destination detection, or
absence-sensitive generation reauthentication, not trusted hot-path reads.
Do not recover the old-R gain through caller-trust metadata or a second
semantic path.

The final native gate must use a worker userland compatible with the mounted
project-local R and Clang sanitizer runtime. The former immutable worker
`608af79d0330e7cb2f70ed8f0cf540b0550e07b33f842cb06e728a2b00875be4`
(glibc 2.42) deterministically crashed in libR's XDR lazy-load path under
ASan before Paradox loaded; the failed `r36-release-076eb44` ASan lane is
invalid worker evidence, not a package failure. Relaxing Podman isolation and
resource settings did not help. The reviewed local replacement is
`localhost/paradox-verification-worker@sha256:9af0174219c3b0467cec8a4f0db8b3b664077ff797c88fd38ad03402e94bab4c`,
built from exact Debian Bullseye base
`docker.io/library/debian@sha256:cba95a21c96c1f5fc2470081829363eed57706634f7dc26e8c6712934303d57a`.
Bullseye has an unmerged `/usr`, so the worker recipe must retain its explicit
plain, non-symbolic compatibility copies for the complete reviewed command
subset used by archive extraction, configuration, receipt cleanup, and
structural tests. In particular this includes `/usr/bin/bash`,
`/usr/bin/mktemp`, `/usr/bin/uname`, the gzip/bzip2 front ends, and
`/usr/bin/rmdir`; missing any one can fail a later clean-`PATH` phase after
the worker's initial self-tests have passed.
The R-header tool inventory must also retain its bounded `/bin` terminal-root
allowance: Bullseye's `/usr/bin/pager` traverses `/etc/alternatives` to the
regular `/bin/more` executable. The complete chain and terminal executable
remain authenticated and `/bin` is not added to configure `PATH`.
The container entry point must continue to pin `C.UTF-8`, UTC, and an empty
`LANGUAGE`; Bullseye otherwise defaults to C and changes R's serialized AST
metadata even with the identical mounted R executable.
The native-test token watchdog must run `terminate_tokens` for catchable
`HUP`, `TERM`, and `USR1`. Processx can signal it directly during coordinator
teardown; a direct signal exit races the worker supervisor and can strand a
nested processx session. Do not claim a direct `SIGINT` trap: an asynchronous
POSIX-shell child inherits INT/QUIT ignored, while coordinator INT is covered
by the watchdog's bounded parent-death poll.
An ASan-selected native run now fails closed on exact preloaded-R startup and
an XDR round-trip before any expensive compiler mode. Never add a direct-host
fallback or weaken container isolation; provision and pin a compatible worker.
The checked-in systemd example names the replacement digest, but changing the
installed root-owned `/etc/paradox-verify-systemd.conf` remains a manual
operator action.

## Non-negotiable design decisions

- `ParamSet`, `ParamSetCollection`, and `ParamSetShadow` are serializable R6
  shells over one package-owned `.core` capsule. Public R6 names and ordinary
  behavior remain; private layout is not an API.
- `.core` is an external pointer with no allocation or finalizer. Its tag is
  `paradox.core.base.v1`, `paradox.core.collection.v1`, or
  `paradox.core.shadow.v1`; its protected slot is the complete ordinary-R
  capsule/model state; its address slot holds only the session-local
  verification stamp described below, never a pointer, so the capsule still
  owns no native resource and R restores the slot as `NULL` on unserialize.
  The sole stateful R-shell policy outside it is the
  documented public `assert_values` flag, which selects checked versus
  unchecked native value assignment. It is serialized/cloned with the R6 shell
  and compared by `all.equal()`, but is not graph/schema/value authority and
  does not change the eleven-field ABI. Direct assignment and `set_values()`
  select its exact non-missing logical value inside the same native
  read/merge/write transaction and terminally authenticate it with the
  selected capsule graph; malformed policy shapes reject and a nested commit
  during the allocating merge wins.
- The v1 payload is the fixed eleven-field list `.params`, `.values`, `.tags`,
  `.deps`, `.trafos`, `.extra_trafo`, `.constraint`, `.sets`, `.translation`,
  `.postfix`, and `.edges`. It is one internal schema shared by all three node
  kinds. `.edges` is the derivation record of a node whose schema is derived
  rather than owned: `NULL` for BASE; for COLLECTION the exact child
  generation each flattened edge was built from plus that edge's `tag_sets`
  and `tag_params` flags, which no flat table can recover; for SHADOW the
  origin schema slice the visible tables were projected from plus the retained
  hidden ID set. It is a cache, so a mismatched or absent record makes the
  node stale rather than corrupt.
- A COLLECTION's flattened schema and a SHADOW's projection are derived state
  and are kept current lazily, never by parent back-references. Every semantic
  entry point passes one gate that walks the graph below it in post-order,
  re-flattens a COLLECTION whose child's `.params`/`.tags`/`.trafos` slice has
  moved, and refreshes a SHADOW; the result is what a node freshly constructed
  from its current children would be. Each installed generation is
  individually consistent, so an interrupt or a deferred name collision leaves
  a partially healed graph whose healed nodes are correct and whose remainder
  heals at the next entry. A refresh is not a semantic change: it installs the
  state the node already denoted.
  Two session-global epochs make "nothing changed anywhere" a single
  comparison: capsule installation that changes derived-schema inputs advances
  the schema epoch, any other semantic installation advances the state epoch,
  and a cache refresh advances neither. A COLLECTION records an epoch only
  after every direct child has itself proved that its complete subtree is
  SHADOW-free, mixed bijectively with its own address at `uintptr_t` width so a
  duplicated capsule is unverified, in the address slot. SHADOW and
  Shadow-bearing COLLECTION graphs deliberately remain unstamped: no finite
  address-word fingerprint can exactly authenticate arbitrary same-pointer
  rewrites of the ordinary Shadow signature list, so those graphs
  reauthenticate it against authoritative origin identities on every entry.
  This is a proof that nothing has been installed, not a trusted-caller or
  trusted-node cache, and it authorizes skipping only work whose inputs
  provably did not change.
- A SHADOW `.core` has exactly one package-private derived-cache attribute,
  `.paradox.shadow.snapshot.v1`. Its value is an ordinary, attribute-free list
  alternating every origin-graph shell with the exact capsule generation used
  to build the protected payload. It is only a derived refresh signature:
  origin authority remains `.sets[[1L]]`, callbacks come from the locked
  Paradox namespace on a rebuild, and neither origin nor callback factories
  are duplicated in the attribute. Deep clone rebuilds the signature against
  the memoized cloned graph; serialization may preserve its graph-relative
  identities. Every read validates the exact attribute/signature shape, and a
  missing, extra, or malformed attribute is corrupt state, never a fallback.
- Capsule tables are canonical plain base `data.frame`s. They have no
  data.table key, index, spare capacity, or self-reference. Public table
  accessors return valid data.table facades whose mutable shells and columns
  are newly owned or detached from capsule state. A native producer that owns
  a genuinely fresh shell and metadata may complete that facade directly;
  caller-owned/public-ingress tables still require defensive materialization
  and finalization. Attribute values, classes, names, and row names are
  contractual by attribute name; incidental pairlist order inherited from an
  R/data.table version is not. Native grid facades use one fixed
  `row.names`/`class`/`names` construction order on every supported runtime.
  Ownership extends through public `$params`, `$domains`, `$data`, static
  properties, raw `$values`, and `$get_values()`: mutable list carriers and
  built-in typed atomic leaves, including their complete supported ordinary,
  acyclic, bounded nested attribute metadata, are detached. A closure
  recursively embedded as general presentation metadata is unsupported and
  rejects cleanly. The sole package-defined exception is a Domain's printable
  `repr` attribute: its top-level carrier must itself be ordinary non-ALTREP
  and non-S4, but the complete value is an opaque exact identity after one
  bounded top-level attribute-generation receipt. This preserves arbitrary
  semantic factor-level representations, including function-valued levels,
  without asking the general metadata owner to duplicate a closure shell.
  Semantic function-valued leaves and callbacks are likewise not metadata and
  retain their separately documented identity behavior. ParamUty leaves,
  callbacks, environments, external pointers, typed S4 identity tokens, and
  ParamSet children remain exact opaque identities. `$sets` therefore owns its
  named outer carrier but not its child shells. This cost belongs only to
  outward construction; internal planning and mutation use retained capsules
  directly.
  No caller-owned attribute spine in these ownership paths is passed to R's
  shallow or recursive duplicator. The package-owned copier hard-bounds each
  attribute spine and recursive path at 64 edges/frames and the complete graph
  at 65,536 nodes, selects children into scanned R carriers before allocating,
  and installs attributes with public setters in the fixed
  `dim`/middle/`dimnames`/`class` order. A setter-normalized raw spelling,
  post-selection mutation, cycle, overbound graph, closure, or `DOTSXP`
  presentation node rejects at the terminal receipt, apart from the exact
  Domain-`repr` exception above. Qunif metadata and typed list-leaf
  compatibility deliberately retain shallow nested metadata identity,
  but use the same bounded top-level tag/value selection and public setters;
  their ordinary attr-free path allocates no metadata carrier. On R >= 4.6 the
  bounded facade stops `R_mapAttrib()` with a guaranteed non-NULL global
  symbol; older R uses the one reviewed `ATTRIB` loop with the same edge bound.
  Interpret `"NoDefault"` as the package marker only at a schema default/init
  position, and only for its exact ordinary, non-S4 zero-length marker shape;
  a formal S4 class named `NoDefault` and an identically classed general
  ParamUty stored value are opaque and retain exact identity.
- Mutations build and validate replacement capsules and swap `.core`
  atomically. Value assignment plans the complete BASE/COLLECTION/SHADOW
  graph through ultimate BASE targets, deduplicates shared targets with
  deterministic last-owner semantics in exact depth-first child order (never
  touched-before-untouched order), and commits every replacement in one
  allocation- and callback-free wave. A callback mutation of any planned
  target or routed graph node wins and makes the outer assignment error before
  any target is changed. The planning epoch makes the allocating multi-node
  selection coherent; an exact graph/Shadow-signature receipt spans validation
  and replacement construction. Native readers retain the capsule chosen at
  operation entry.
  Checked assignment validates every supplied entry's Domain, special-value,
  custom-check, sanitization, TuneToken, and structural contract, but does not
  require its dependencies to be satisfied. A valid dependency-inactive entry
  is stored as a dormant value in raw `$values`; the default
  `$get_values(remove_dependencies = TRUE)` view omits it and makes it visible
  again when a later state makes it active. Assignment computes activity only
  when a constraint exists, solely to pass that callback the active subset.
  Unchecked assignment keeps its existing structural-only boundary.
- `BASE` owns a schema and mutable values. Each `COLLECTION` capsule generation
  owns ordered child references and an immutable translation snapshot; `$add()`
  is one native transaction that validates both complete graphs, rejects an
  existing or proposed cycle (including a SHADOW origin path), checks every
  admitted generation again, and only then installs a replacement generation.
  Corruption, name collision, reentry, or allocation leaves the old collection
  unchanged. `SHADOW` owns one origin edge and a retained hidden ID set, and
  computes its visible schema as "origin minus hidden" while reading/writing
  dynamic origin semantics live.
  Shared DAG nodes are valid; a repeated node on one active path is a cycle.
  The graph-path validator's raw native frames are scratch, never GC roots.
  One indexed VECSXP carrier owns both every active shell and the exact selected
  capsule generation until that frame is popped; grow the carrier before any
  allocation that could invalidate a raw frame or child pointer. Do not replace
  this with `SEXP` fields held only in `R_alloc()` storage.
  Three same-process paired runs measured a 2--3% cost when the native Shadow
  constructor was isolated and 0--1% on the public R6 constructor; that bounded
  cost is accepted for exact-generation safety and is not grounds for weakening
  the carrier.
  Deep clone memoizes the complete graph and clones each shell once; never
  restore independent per-edge `$clone(deep = TRUE)` recursion.
- `ParamSetShadow` is supplied by Paradox. Visible writes preserve hidden
  origin values; dependencies, constraints, and transformations are live;
  dependencies crossing the visible/hidden boundary are errors. Its capsule
  origin edge is the only origin authority; do not add a parallel private
  origin field. A direct SHADOW-to-SHADOW origin is rejected; construct the
  combined view over the ultimate BASE or COLLECTION origin instead. The R6
  private environment must not cache a second `.visible`/`.shadowed` schema.
  The hidden ID set retained in `.edges` plus the origin's current parameters
  define the visible/hidden partition; the visible tables themselves are a
  projection and are recomputed whenever the origin's schema slice moves. A
  projection that did not change keeps its exact field objects, so value churn
  behind a Shadow never looks like a schema change to a collection above it.
- A BASE-origin Shadow constraint adapter has one exact two-field
  `{callback, hidden_values}` plan and a thin native evaluator. The evaluator
  validates and snapshots both inputs, merges the already activity-filtered
  hidden values before the already filtered visible values without `c()`/S3
  dispatch, preserves opaque leaf identity, executes the callback once, and
  requires one non-missing logical result. The authoritative Shadow graph
  operation computes activity before constructing/invoking this exact plan;
  the two-field adapter has no schema and must not grow a duplicate activity
  evaluator.
- Third-party inheritance from the ParamSet family is additive only. Core
  ParamSet method/active-binding replacement, generated-wrapper mutation,
  reparenting, delayed bindings, and capsule/private-table writes are
  unsupported. `bbotk::Codomain` is the maintained additive-subclass case.
  This restriction does not remove the documented `Sampler` subclass API.
- Domain dispatch is closed over `ParamDbl`, `ParamInt`, `ParamFct`, `ParamLgl`,
  and `ParamUty`. Condition dispatch is closed over `CondEqual` and `CondAnyOf`.
  The existing exported names and built-in outward shapes remain; third-party
  S3 methods are not an extension contract. Standalone `condition_test()` uses
  the same exact built-in admission and comparison semantics as dependency
  execution, with an optimized native vector kernel for direct input.
  It accepts `NULL` or a plain logical, integer, double, or character vector
  with at most names, materializes a stable ALTREP operand once, and rejects
  classed or otherwise attributed vectors rather than invoking `Ops`/`%in%`
  dispatch. A callback-capable ALTREP `Length` is followed by one terminal
  hard-bounded re-admission of type, ALTREP/object/S4 state, and the complete
  allowed attribute spine before raw names selection. `condition_as_string()`
  is cold presentation glue, not a second evaluator. `p_uty(custom_check=)`
  remains the general callback escape hatch.
  A built-in Condition RHS is likewise an attribute-free logical, integer,
  double, or character vector without missing values: `CondEqual` has one
  element and `CondAnyOf` is non-empty and unique. Native dependency or direct
  comparison admission roots that selected RHS, materializes every element
  once, and validates the ordinary snapshot. The strict capsule validator does
  not accept ALTREP; it validates only the already admitted snapshot. An
  operation may retain pointers to the exact admitted RHS values in temporary
  workspace while their dependency/Condition owner graph remains rooted. It
  must not run the same exact Condition admission a second time merely to
  recover those operands.
  Numeric bounds/logscale admission and row construction are one registered
  operation; exact empty Domain operations and zero-dimensional grids also
  enter C and have no R special-case engine. Canonical semantic admission of
  one built-in Domain row has exactly one native owner, shared by constructor
  final-state validation, ParamSet construction, and ObjectTuneToken Domain
  admission. Boundary code may validate the outward table/class container, but
  it must not restate cargo, kind/storage, grouping, bounds, levels, default,
  tags, requirements, initialization, or special-value/transformation rules.
  After admission it receives the closed kind and validated fields and handles
  only its operation-specific work.
  Every public operation on a typed built-in Domain structurally admits the
  exact complete sixteen-column outward shell for the generation it selects,
  including typed zero-row and empty-value exits. The canonical zero-column
  empty Domain instead passes its dedicated exact empty-Domain validator. The
  identity spine is always semantically admitted. Each operation declares
  which of bounds, levels, special values, cargo, tags, and transformation it
  interprets; `paradox_domain_interpretation_closure()` is the sole owner of
  rule dependencies, and `domain_check()` declares the complete mask. A rule
  outside the closed mask is skipped whole rather than reimplemented, so
  malformed semantic state outside one operation's mask is diagnosed by the
  first operation that interprets it. The four constructor-owned columns whose
  contents are irrelevant to public kernels remain semantically opaque, but
  their canonical presence, uniqueness, storage type, and row count are still
  mandatory. An operation with a callback-capable ALTREP `Length`, including
  `domain_check()` and `domain_qunif()`, performs a bounded shape probe first
  and one complete masked admission afterward; Length is observed exactly
  once. An operation such as `domain_sanitize()` that completes and roots
  admission before later value-side callbacks deliberately retains that
  coherent selected snapshot; callbacks do not replace already captured table
  fields.
  A canonical `ParamFct` may have zero levels. Quantile/grid/uniform-sampling
  operations preserve its typed `character(0)` result when zero rows are
  requested; any positive-row quantile or sampling request errors before RNG
  entry or indexing. `Sampler1DCateg` accepts the exact empty probability
  vector and follows the same typed-empty and pre-RNG-error rule. Empty levels
  are semantic emptiness, not corrupt schema.
- Interpreted structure must be ordinary non-ALTREP and non-S4. This includes
  outer general-list/internal-table/Domain/Condition/TuneToken/capsule shells,
  ParamSet constructor `params` lists, non-table transformation inputs and all
  transformation result list shells, Domain cargo containers and interpreted
  cargo entries, class/name vectors, row containers, dimnames and other list
  metadata. A narrow documented public-table ingress may first materialize a
  suffix-classified, allowed-attribute top-level VECSXP ALTREP shell once; base R's
  lazy attribute-only duplicate is the common motivating case. Public-table
  names/classes and ignored data.table cache carriers remain strict ordinary
  structure, while row names have the count-only integer/character exception
  specified below. Admitted semantic atomic leaves and columns may be stable
  ALTREP and are materialized once.
  In particular, the outer `special_vals` list, its names, and list metadata
  are structural for every Domain kind and must be ordinary non-ALTREP/non-S4.
  Do not add a general exception for deferred-string or other ALTREP names:
  package-generated categorical names are materialized once at construction,
  and package-generated data-frame facades emit compact ordinary row metadata
  directly.
  `ParamDbl`, `ParamInt`, `ParamFct`, and `ParamLgl` reject an ALTREP
  `special_vals` leaf before observing it. An S4 special leaf for those kinds
  is an opaque identity token: it matches only the pointer-identical object,
  and an S4 `default` or `init` is accepted only when pointer-identical to an
  already admitted special leaf. `ParamUty` values, defaults, initial values,
  and special leaves are opaque and may be S4; its retained Paradox-1 special
  membership is exactly base `identical()` over the admitted leaves, including
  S4, and is the sole narrow native observation of those objects. Neither rule
  invokes S3/S4 dispatch. One shared allocation-free semantic-leaf classifier
  treats genuine formal-S4 class metadata as opaque and therefore never as
  TuneToken syntax; an S4-marked ordinary object with an inspectable
  `"TuneToken"` class still claims token syntax and is rejected by exact
  non-S4 token admission. Assignment, checking, dependency activity, type
  filtering, fixed designs, and stored/explicit search-space extraction all
  use this same distinction. Condition structure is never opaque.
- Every capsule operation has one native semantic implementation. Thin R
  wrappers may capture R language constructs and call documented callbacks,
  but there is no complete R/checkmate/data.table/S3 fallback, no `NULL`
  sentinel replay, and no second implementation used to authenticate the first.
  Ordinary built-in value admission uses one package-owned C classifier shared
  by standalone Domain checks and ParamSet scalar/table checks. It distinguishes
  missingness, type/shape, integerish, bounds, and factor-level failures on the
  native hot path. Accepted values do not pay for message construction; one
  native failure-only formatter produces informative, checkmate-style
  diagnostics. Do not call or link to checkmate from that native validation
  engine, rebuild its checks in R, or add a second formatter. Compatibility
  gives scalar missingness priority over an otherwise irrelevant storage-mode
  mismatch, as the useful checkmate diagnostics did; that extra classification
  is entered only on the already-invalid type path, not on accepted typed
  values. Compatibility targets the established useful categories and message
  fragments, not
  byte-identical reproduction of every checkmate quirk, `conditionCall()`,
  implementation frame, or exotic classed/ALTREP object behavior outside the
  structural contract.
  Unknown-ID suggestions belong to the same failure-only native diagnostic
  boundary. Rank the actual unknown name—not its position—by case-insensitive
  partial edit distance, retain the 20% threshold, stable top-three order, and
  cheap collection affixes. Exact ID hits must not allocate, calculate a
  distance, or construct suggestion text.
  Dependency activity likewise has one native list-basis kernel shared by the
  check family, `check_dependencies()`, stored-value filtering, and every
  authoritative constraint check/assignment site. Activity is transitive and
  conjunctive. After cycle validation, a TuneToken child skips its incoming
  edges. Otherwise an inactive parent never satisfies its child, even if it
  carries a value or default. For an active parent, an explicit basis value
  wins; a TuneToken parent skips that edge; otherwise an admitted recorded
  default is used, while `NoDefault` leaves the edge unsatisfied. Explicit
  point checks are store-blind:
  they use only the candidate point plus defaults, never stored `$values`.
  Stored-value reads use the raw store plus defaults. The complete-row
  Design/sampler dependency masker remains its specialized vector kernel
  because every parent value is already present; it must stay semantically
  equivalent where the two domains overlap and must not become a second
  list/point activity implementation. BASE dependency mutation admission is
  unchanged and can currently admit a cycle; every activity consumer tracks
  the active dependency path and raises a deterministic cycle error rather than
  recursing forever or reading partial state.
  There are exactly two narrow cold R semantic-orchestration families. The
  first is internal tuning.
  `$aggr_internal_tuned_values()`, `$disable_internal_tuning()`, and
  `$convert_internal_search_space()` are single R implementations over one
  captured capsule/cargo/translation/value snapshot and execute only their
  documented cargo callbacks; commits still use native value/capsule mutation.
  After native flatten semantics are complete, R may also rebind cargo closures
  whose lexical environments must change. Its only canonical write is a
  package-owned replacement of the detached result's rewritten `cargo` column.
  When Collection or Shadow flattening has namespace-sensitive internal-tuning
  cargo, it requests every root ID inside that same native graph snapshot,
  subsets exactly the returned IDs, and authenticates its complete receipt
  before rebinding. The callback-free path uses the subset engine's dedicated
  all-current-ID transaction and never carries a preliminary R-side ID vector
  into subset. Its post-subset cargo scan fails closed if namespace-sensitive
  metadata appeared after the mode decision. Do not restore an R-side
  `.params$id` handoff: a derived schema can move between that read and subset
  and silently splice generations.
  Ordinary BASE `$flatten()` and every omitted-`ids` `$subspaces()` call also
  select all current IDs inside their native capsule/graph transaction. They
  never evaluate an R default `$ids()` vector and carry it into a later
  generation; explicit `$subspaces(ids)` retains its documented selected-ID
  transaction.
  This family supplies no alternate structural admission, checking,
  callback-selection, or graph engine and is never a fallback. The second is
  exact-TuneToken `$search_space()` conversion. It consumes one natively
  admitted and rooted exact token/target-Domain snapshot, with live BASE
  candidates already replaced by sealed single-use capabilities, switches only
  over the package's built-in token kinds, and solely owns callback-dependent
  one-dimensional output compatibility and construction of the outward search
  space. When `values` is omitted, the raw current store and every target
  Domain are selected from the same capsule/graph generation; the lean R6 stub
  preserves the reflected `values = self$values` formal without evaluating
  that default before native entry. An explicit `values` container remains an
  independent caller snapshot. This family performs no independent
  token/Domain/graph admission, exposes no S3
  extension seam, and has no competing native or R conversion path.
- Deep clone is cold R6 shell-lifecycle orchestration, not another semantic
  exception. It uses an explicit work stack only to preserve shell identity and
  graph topology while shallow-cloning shells; native capsule validation,
  replacement, generation checks, and Shadow signature rebuilding remain the
  authority. Its one allocation-free terminal receipt authenticates every
  shell's exact enclosure/private linkage, selected private capsule, exact
  Shadow signature, and corresponding public `assert_values` policy. Opaque
  ParamUty clone callbacks then consume only those selected generations; every
  child shell receives the selected policy rather than a later live value.
  Thus the two families above are the
  complete cold R *semantic*
  orchestration boundary, while clone and detached equality are outward
  shell/presentation glue and must not acquire independent admission or
  mutation rules. The one-way legacy upgrader is outside that current-operation
  count: its R code authenticates a retired private schema, invokes current
  constructors/native validators, and transplants R6 lifecycle state. It is
  never a second implementation or fallback for an operation on a current
  capsule.
- Collection callbacks are not an exception. Live `$extra_trafo` and
  `$constraint` access, detached subset/flatten callbacks, and the corresponding
  adapters used when a SHADOW wraps a COLLECTION all enter one registered
  native evaluator family with shared semantic helpers. Package-owned closures
  retain exactly three plan fields: translation, callback carriers, and owner
  indices. An extra-transformation carrier additionally contains the detached
  BASE shell required for its documented `param_set` argument; a constraint
  carrier does not. Callback selection, translation, merging, result admission,
  and constraint scalar validation are not reimplemented in R and never
  dispatch through an overridden child ParamSet method. Collection
  check/test/assignment sites compute activity over the complete translated
  collection configuration, including collection-level cross-child
  dependencies, and pass each child callback only its active child-scope
  entries with prefixes removed. Detached BASE checking likewise filters
  before it invokes a detached carrier. Activity is derived at the
  authoritative graph site and is never another carrier field or a duplicate
  evaluator inside the schema-free carrier. Collection extra-transformation
  merging keeps retained/untransformed inputs in input order, then appends all
  changed child outputs in callback-plan order (and in each callback's result
  order). Child-owned inputs omitted by their callback disappear, and a changed
  name that collides with retained input is an error.
  Every transformation result and non-table input outer list is ordinary
  non-ALTREP and non-S4. A documented data-frame input may use the exact
  public-table ALTREP boundary above. Semantic atomic leaves and admitted
  columns may be stable ALTREP and enter the same native admission.
  Do not restore the removed namespace-level R `transpose()` implementation or
  the unused table helpers: `Design$transpose()` has one registered native
  semantic engine.
- Direct public `$values <-` assignment accepts an ordinary named base list or
  an ordinary S3-classed named list container. Checked and unchecked assignment
  both reject an outer ALTREP shell before observing its length, names, or
  elements. Both preserve the Paradox-1 clear-values spellings: `NULL`, an
  ordinary attribute-free zero-length atomic/expression vector, or an accepted
  empty list container is canonicalized to a named native `list()`. The outer
  S3 class is representation-only and is discarded before
  validation/storage; it never selects dispatch or another value engine.
  S4/list-like objects and semantic attributes other than `names` and `class`
  remain unsupported. This preserves ordinary configuration objects such as
  bbotk's `local_search_control` without reopening S3 extension seams.
  `$values` is the raw store and therefore includes dormant entries.
  `$get_values(remove_dependencies = TRUE)` (the default) applies the shared
  activity kernel, and `check_required` is evaluated against that filtered
  view. Dependency filtering occurs at the node whose dependency rows own the
  rule: a collection-level cross-child edge filters the collection read, not a
  direct read from either child. The getter builds one operation-local
  open-addressed index over admitted CHARSXP IDs and reuses it for stored-value
  names, tags, and both dependency endpoints. Pointer identity is the common
  path and the encoding-aware comparator is the fallback. A raw read with no
  dependency filtering and no required parameters skips activity evaluation,
  but never skips parameter/dependency/Condition admission. With no ID/tag
  filter it may reuse the rooted admitted schema IDs internally; the outward
  values list and names remain fresh and detached.
- Explicit `$search_space(values=)` input has the same outer-container
  representation boundary: an ordinary named list or an S3-classed named list
  carrying only `names` and `class`. Native code discards the class and selects
  tokens without `[`/S3 dispatch. S4/list-like containers and other semantic
  attributes reject.
- TuneToken admission is closed over exactly five format class vectors:
  `c("FullTuneToken", "TuneToken")`,
  `c("RangeTuneToken", "TuneToken")`,
  `c("ObjectTuneToken", "TuneToken")`, and the two corresponding Full/Range
  vectors prefixed by `"InternalTuneToken"`. The token is an ordinary named
  list with exactly `{content, call}` and no attributes other than exact names
  and class. `call` is an attribute-free, non-missing `character(1)`.
  Full content is exactly `{logscale}`; Range content is exactly
  `{lower, upper, logscale}`; Internal Full/Range content may append one `aggr`
  function and requires false `logscale`; Object content is one admitted
  bounded, value-producing built-in Domain or an exact
  `c("ParamSet", "R6")` shell linked through
  ordinary `self`/`private` bindings to a canonical BASE core. A `ParamUty`
  Domain is unbounded and therefore invalid in the Domain form. A canonical
  zero-level `ParamFct` remains valid for typed empty operations but is not a
  value-producing tuning range. Opaque leaves may still be retained through a
  bounded typed Domain, and opaque target
  results may be constructed through the BASE-ParamSet form. COLLECTION, SHADOW, and
  additive-subclass content is rejected. Names on public scalar
  bounds/flags are representation-only and are discarded from the native
  snapshot. Every interpreted token shell/container/class/name/scalar is
  non-S4. Token internals are not an API: subclasses, extra or
  reordered fields/classes/attributes, malformed calls/content, and recursive
  metadata are rejected before traversal. Opaque documented leaves retain
  identity; Paradox never recursively interprets arbitrary token metadata.
- Exact creator provenance is deliberately not authenticated by generated-R6
  surface inspection. A shell alias that retains the exact genuine BASE
  `self`/`private`/core linkage may therefore be indistinguishable and pass.
  This is safe, but not an extension API: C never calls a candidate/alias method
  and admits only the selected capsule generation. Do not claim that every
  manually assembled look-alike is detected merely because it was not returned
  by a package constructor.
- An `ObjectTuneToken` containing a ParamSet is admitted by `$check()` and
  checked value assignment only after the exact token snapshot above and native
  validation of its nonempty, bounded BASE capsule. Admission executes no
  candidate callback. A rooted private receipt records the exact
  `{shell, private, core}` generation before validation callbacks; all receipts
  are reauthenticated after callback-capable work, and checked assignment ends
  with one allocation-free scan immediately before its atomic commit. A changed
  candidate wins and the outer operation errors without committing.
  Explicit `$search_space(values=)` input enters the same structural boundary,
  then replaces every live ParamSet candidate with a sealed, single-use BASE
  subset capability before any R callback. The cold converter constructs its
  detached search-space ParamSet from that capability and never invokes or
  rereads the original shell.
  The one deterministic `$search_space()` conversion remains the sole boundary
  that evaluates the candidate transformation, one-dimensional result, and
  compatibility with the target Domain. Consequently a structurally valid but
  output-incompatible candidate stores successfully and errors when its search
  space is requested. This intentional Paradox-2 timing change avoids an
  unsnapshotted R callback preflight racing an atomic native commit; malformed
  or corrupt candidate state still fails before storage without mutation.
  Malformed exact-token or Domain structure is a hard boundary error, including
  when encountered by `$check()`; ordinary target-value infeasibility remains a
  returned check diagnostic. Do not turn structural forgery into an ordinary
  value diagnostic or add a recovery path.
- `ParamSet$check_dependencies()` is deliberately narrower than value
  assignment: it accepts one ordinary, uniquely named base list with only its
  names attribute. It reuses the native `$check()` graph snapshot, point
  initializer, and dependency kernel; validates unknown IDs even when there are
  no dependency rows; skips a dependency whose child or parent value is a
  TuneToken; consults recorded defaults only for parents absent from that
  candidate point; and returns `TRUE` or the first diagnostic. It is
  store-blind and does not consult the object's stored `$values`. Do not restore
  the R data.table/pmap traversal or newline-collapsed multi-error result.
- `ParamSet$test_constraint()` and `$test_constraint_dt()` reuse the native
  check graph, point admission, and constraint kernel; there is no scalar or
  per-row R constraint engine. Each callback receives only the default-aware
  active subset of its candidate point. With `assert_value = TRUE`, the table
  method validates every row before running any constraint callback, then calls
  the snapshotted callback set once per row in order. Reentrant callback
  mutation is visible only to the next public operation. The table boundary
  continues to require a data.table.
- Public tag get/set, dependency snapshot/get/set/add, and BASE constraint/
  extra-transformation callback replacement enter registered native mutators.
  The public `$has_deps` flag is a separate registered scalar reader: BASE and
  SHADOW validate the selected canonical dependency table, SHADOW refreshes
  from its live origin once, and COLLECTION performs the same complete graph
  admission as `$deps` before reading the root subtree count. It must never
  construct a detached dependency/data.table facade merely to answer the flag,
  cache graph validity, or use a weaker collection admission mode.
  Dependency projection and bulk `$deps <-` build one owned canonical structural
  snapshot: exact built-in Conditions, valid child IDs, no self-edges, and the
  established allowance for dangling parents. Bulk assignment deliberately
  preserves partially or wholly infeasible predicates and runs no Domain/custom
  check callback. This is required when a consumer copies a dependency graph
  after narrowing a parent Domain: a predicate that became impossible simply
  makes its child permanently inactive. `$add_dep()` is the authoring boundary;
  it additionally checks RHS feasibility in the shared check kernel, detects
  callback reentry by capsule generation, and swaps only after validation.
  SHADOW `$add_dep()` routes through that same strict native append and rejects
  any edge that leaves its visible schema. Do not merge bulk assignment and
  append back into one feasibility mode, or restore R/checkmate/data.table
  mutation planners for these fields.
- Names attached by ordinary R subsetting/arithmetic to scalar Domain
  constructor arguments are representation-only and are discarded from the
  owned native snapshot. Classes and other attributes remain fail-closed.
  Named scalar bounds are common R behavior, not a third-party Domain kind.
- Source references on package-interpreted callbacks are representation-only
  admission metadata. Recursively remove `srcref`, `srcfile`, and
  `wholeSrcref` from stored `custom_check`, individual/extra transformations,
  constraints, aggregation, and internal-tuning callbacks, and from printable
  Domain representation language. Normalize package-generated
  Shadow/Collection callback adapters and flattened internal-tuning namespace
  adapters under the same rule. When normalizing `Domain` representation
  components, reject `NULL` before `is.pairlist()`—R classifies `NULL` as a
  pairlist and `reprargs[[index]] <- NULL` would delete the named component
  during fixed-index traversal. At ordinary current-object admission, return
  a clean callback unchanged; a stripped copy must retain the exact enclosing
  environment. Read
  `options(paradox.strip_srcrefs = FALSE)` only at admission as a debugging
  opt-out. The recursive source-reference walk never enters arbitrary callback
  environments or anything reached through `$values`, defaults, specials, or
  initialization payloads. Exact legacy-crate authentication may snapshot only
  its fixed known binding set. The independently specified legacy graph crawler
  may still discover a legacy ParamSet shell stored in an opaque value. Apply
  normalization during legacy preparation, before any graph transplant.
  Authenticate only the exact known Paradox-1 package-generated crate shapes:
  categorical mapping, collection-flattened `in_tune_fn`, tuning-ParamSet
  transformation, and detached collection transformation/constraint adapters.
  Their matcher templates intentionally contain the same free carrier symbols
  as the serialized wrappers. Keep those symbols in the narrow
  `utils::globalVariables()` declaration in `R/zzz.R`; that declaration tells
  codetools about environment-supplied names and does not create namespace
  fallbacks. Do not “fix” the templates with dummy locals, because changing
  their bodies would reject the exact legacy shapes they authenticate.
  Rebuild those wrappers without mutating the serialized input. An authenticated
  detached collection wrapper always receives a fresh closure environment so
  carriers can be rebound safely; with stripping disabled, preserve its source
  metadata but do not promise wrapper pointer/environment identity. Treat
  ParamSet carriers captured by an authenticated detached collection adapter as
  explicit migration dependencies, preserving aliases and rebasing them during
  graph transplant; this is not permission to traverse arbitrary callback
  environments. Before R observes such a carrier list,
  `C_upgrade_carrier_list_snapshot` rejects ALTREP/S4/object shells and takes
  one shallow ordinary-list snapshot; never replace that boundary with
  `length()`/`seq_along()` on untrusted legacy state. Legacy canonical table
  shells use the analogous cold native table snapshot: allocate every outward
  carrier first, pair ordinary names with exact columns in one allocation-free
  pass, obtain any stable first-column length, and then receipt that generation
  together with the allowed class/cache/`repr` attributes without allocating.
  Stored row names must be ordinary canonical integer metadata—empty, compact
  `c(NA, +/-n)`, or exact `1:n`—matching that selected column length. The one
  authentic Paradox-1/data.table spelling built from a classed list followed by
  `setkeyv()` has no `row.names` attribute even when populated. It is accepted
  because absence declares no competing row count: the exact selected columns
  define it and their materialized lengths must agree. Before the terminal
  source-shell/attribute receipt, the cold native snapshot independently owns
  every top-level atomic payload and list-column carrier. Generic tables own
  non-S4 atomic leaves; an exact dependency/Domain schema instead invokes the
  existing closed C owners for Conditions, requirements, cargo, levels,
  special-value carriers, and default/init. Typed leaves are detached,
  ParamUty leaves retain exact identity, and `NoDefault` remains interpreted
  schema. The named legacy value store likewise has one native kind-aware
  snapshot: it owns the carrier and typed leaves but preserves every ParamUty
  value identity, including atomic/classed values. It retains a private shallow
  identity receipt for each source list column while the outward carrier holds
  the owned leaves. After all allocations, every stable top-level or nested
  ALTREP finishes its final Length observation; one callback-free pass then
  authenticates exact source-cell identity plus every ordinary payload and
  complete atomic-leaf metadata generation. A finalizer after the native
  return therefore cannot splice detached old names or representation metadata
  with a newly mutated source column or nested atomic leaf. Opaque and S4 list
  leaves retain their documented identity semantics. R validates only
  surrounding legacy parameter references, rebuilds the outward/internal table
  facades, and normalizes callbacks on these private carriers. It must not
  re-admit or reconstruct the built-in Conditions/requirements already owned by
  C, or restore a second leaf-copy authority. Never restore separate R
  observations of table names, columns, row metadata, and optional attributes;
  a pending by-reference finalizer must precede one selected generation, follow
  an already-independent result harmlessly, or make migration fail closed.
  Every selected callback-capable nested ALTREP leaf is rooted directly across
  its `Length` dispatch. A requirement additionally roots its selected `on` and
  sibling `cond` fields, plus both detached counterparts, as one row before the
  first such dispatch: an `on` callback may replace `cond` and collect, but it
  cannot invalidate the exact Condition still needed by the terminal receipt.
  The regression proves this with a liveness sentinel reachable only through
  that detached Condition. A structurally canonical legacy `.values` carrier
  whose name is empty, missing, or not owned by the selected parameter schema
  is an invalid-name error before leaf ownership, not a generic malformed-list
  error. These checks remain confined to cold migration.
- `$subset(..., keep_trafo = FALSE)` is the public way to derive an
  untransformed search space. The final additive argument defaults to `TRUE`
  for BASE, COLLECTION, and SHADOW. `FALSE` makes the single native subset
  transaction omit every selected per-parameter transformation and the
  `extra_trafo` callback while independently preserving the constraint. Do not
  restore downstream mutation of Domain `.trafo`/ParamSet `.trafos`, permissive
  admission of malformed Domains, or a second R reconstruction path for this
  operation. Its three control flags are exact attribute-free, non-missing
  logical scalars. COLLECTION callback detachment follows the callbacks retained
  in the admitted BASE result and must never reinterpret the original flags in
  R or invoke `!`/S3 dispatch. mlr3mbo's Paradox-2 bridge must use this public
  boundary.
- `all.equal()` on the ParamSet family compares a detached semantic view.
  Never delegate equality to `all.equal.environment()`: evaluating inherited
  R6 active bindings can select the wrong parent reader for a COLLECTION, and
  private capsule environments are not the equality contract. This S3 method
  may use base R equality over state projected by the native readers; it has no
  competing C/R equality path and does not validate or interpret capsule state
  independently. The projection contains node class and `assert_values`,
  detached params, values, tags, and dependencies, BASE callbacks, COLLECTION
  children, and the complete SHADOW origin state. It is one flat ordinary list
  with `root`, traversal-ordered `nodes`, and per-node `edge_kind`, `edge_names`,
  and canonical `edge_nodes` IDs. Build it with an explicit work stack: two
  independently constructed equivalent DAGs compare equal while shared and
  duplicated topology compare different; an active-path cycle errors. Derived
  COLLECTION/SHADOW adapter closures are omitted because their authoritative
  child/origin state is already compared.
- A base `ParamSet$extra_trafo` may return an unnamed list; the native engine
  retains it because `to_tune(ParamSet)` and maintained callers use unnamed
  one-dimensional results. If names are supplied they must be complete and
  unique. A child `extra_trafo` in a `ParamSetCollection` must return complete,
  unique names because translating child output into the collection namespace
  is semantically required. This distinction is one native result-admission
  branch, not an R fallback or a second transformation engine.
- `SamplerUnif` and `generate_design_random()` share one capsule-driven native
  uniform engine. `SamplerUnif$samplers` remains descriptive compatibility
  metadata: replacing/reordering the list is an error and child mutation never
  selects another engine. `SamplerUnif` may move each freshly created,
  unexposed singleton subspace through its package-private process-local
  single-use ownership carrier, avoiding a redundant child clone. Reused,
  serialized, malformed, generic, or fabricated carriers reject. Ordinary
  public `Sampler`/`Sampler1DUnif` construction retains defensive cloning, and
  child order/classes/IDs plus serialized public topology are unchanged. Use
  `SamplerHierarchical` for custom 1-D samplers.
  `SamplerHierarchical` owns and validates one deep-cloned graph before reading
  arbitrary Sampler subclass `$param` bindings; those bindings cannot splice
  pre-mutation IDs onto a later caller generation, and duplicate sampler IDs
  reject.
  Random, Sobol, LHS, hierarchical, and directly constructed designs retain
  fixed-value overwrite and dependency masking at the ordinary `Design$new()`
  boundary. `generate_design_grid()` is the one deliberate prepared-design
  exception: its registered native operation snapshots the complete
  BASE/COLLECTION/SHADOW graph, realizes and deduplicates axes, collapses fixed
  axes, enumerates dependency-valid branches, restores first-nominal-occurrence
  row order, and returns the final table. It enters `Design$new()` with the
  namespace-owned prepared-grid token and must not pay for or risk divergence
  from a second normalization pass. The grid and Design vector masker share
  the same dependency graph planner and built-in Condition comparator; neither
  may grow a parallel R/data.table evaluator.
  A global grid resolution retains canonical ParamSet axis order and named
  overrides change only counts. Without a global resolution, explicit numeric
  control order comes first and remaining categorical axes follow in schema
  order; nominally empty typed grids use canonical schema order.
  Native Design planning and grid generation carry one compact, flat
  operation-local generation receipt across their short R handoffs. The
  allocation-free terminal scanner directly compares every already-admitted
  live `.core` binding and exact Shadow carrier/content, without replaying full
  immutable-capsule validation. Ordinary Design scans after data.table
  patching/deduplication; grid scans after native bundle allocation and again
  after R6-shell construction. This receipt is not persistent authority,
  caller trust, or a reason to skip initial graph admission.
  LHS and Sobol preserve the exact caller-owned ParamSet reference. They
  generate against one owned deep clone, bind that caller reference directly
  into the completed Design, and then perform an allocation-free receipt scan
  over the complete caller graph as the terminal operation. The bind precedes
  the scan because R's ordinary `$<-` preparation allocates even for an
  environment; no callback-capable or allocating work may follow the scan.
  Random generation remains deliberately clone-owning.
  A nominal zero-resolution or zero-level axis still makes the complete grid
  empty before fixed collapse or quantile warnings, but dependency topology is
  admitted first so an empty result cannot hide a cycle. `upper_limit` applies
  to the exact final realized row count. Ordinary same-storage fixed scalars
  retain atomic columns; a valid cross-storage, `NULL`, or S4 special fixed
  leaf is one identity-preserving list-column cell, and fixed TuneTokens receive
  a direct informative grid error.
- Stable ALTREP inputs, including base compact sequences such as `1:n`, are
  supported in documented semantic-vector positions. Structural containers
  remain deliberately ordinary non-ALTREP and non-S4: configuration/search-
  space and transformation list shells, ParamSet constructor `params` lists,
  Domain/Condition/TuneToken/capsule shells, Domain cargo/interpreted cargo
  entries, internal table shells, rows, dimnames, class/name vectors, and other
  list metadata. One shared native classifier governs the six public-table
  ingresses: `check_dt`, `test_constraint_dt`, `qunif`, data-frame `trafo`
  input, `Design$transpose()`, and Design dependency planning. A well-formed
  class vector ends in `"data.frame"` or in
  `c("data.table", "data.frame")`; ordinary leading additive classes are
  representation-only. Native admission never dispatches on those leading
  classes. The classifier does not copy or materialize an ordinary shell merely
  to remove the prefix, and semantic readers ignore it; an ALTREP shell's
  already-required materialized snapshot drops it and installs the canonical
  suffix. Every class
  label is non-missing, non-empty, non-bytes, and unique. The reserved labels
  `"data.table"` and `"data.frame"` may occur only in the recognized terminal
  suffix, so reversed, duplicated, or non-suffix uses reject. The remaining
  attributes must be those allowed for the recognized table kind. Names and
  classes are ordinary, attribute-free character vectors; the historical zero-column
  `structure(list(), class = "data.frame", row.names = ...)` spelling may omit
  names. A data.table's optional `.internal.selfref` is an attribute-free,
  non-S4, nonobject external pointer; `sorted` is an attribute-free ordinary
  character vector; and `index` is an ordinary non-S4, nonobject integer(0)
  carrier. Attributes
  below that `index` carrier are uninspected cache payload. All three carriers
  are discarded; Paradox never consumes their cache contents.

  The raw `row.names` value is an integer or character vector that is non-S4,
  non-object, and attribute-free. Ordinary compact `c(NA_integer_, -n)` and
  `c(NA_integer_, n)` encodings are decoded. A stable integer or character
  ALTREP row-name vector is observed with one Length and no Elt calls because
  labels are irrelevant to all six operations. Row-consuming paths compare
  that count with their admitted columns. Direct `trafo` and dependency
  planning with no dependency rows validate row-name structure but do not add
  column observations solely to compare an otherwise unused dimension. A
  zero-column data.frame retains its row count: `Design$transpose()` returns
  one empty configuration for each row, while an unclassed empty list still
  represents zero rows.

  An admitted suffix-classified top-level VECSXP ALTREP shell is materialized
  once. That owned snapshot installs only the canonical recognized class
  suffix; ordinary admitted shells are not copied merely to remove an inert
  prefix. Native
  admission owns names and classes before any callback-capable row-name Length
  or top-shell Length/Elt observation, calls top-shell Length once and Elt once
  per column, preserves column identities, canonicalizes captured row names by
  count, and drops ignored data.table caches. Base R's lazy attribute-only
  duplicate is the common motivating case; the contract does not depend on its
  current internal width threshold. Admitted semantic atomic columns may be
  stable ALTREP. Package-owned capsule tables and returned facades keep
  canonical ordinary metadata; this input exception does not admit ALTREP
  capsule, Domain, Condition, TuneToken, row, callback-result, or general list
  shells. Raw attribute selection uses `R_mapAttrib()` on R >= 4.6 and one
  exact, ledgered `ATTRIB` compatibility exception on R 3.6--4.5. Neither path
  invokes R or data.table fallback logic. Direct checked or unchecked `$values <-`
  assignment rejects an outer
  ALTREP before observation and canonicalizes the accepted Paradox-1 empty
  spellings (`NULL`, an ordinary attribute-free zero-length atomic/expression
  vector, or an accepted empty list container) to a named list in C.
  `set_values(.values=)` is the sole general-list exception: its merge boundary
  snapshots the supplied shell once before validation. This is
  an explicit operation contract, not a general list-ALTREP fallback. The
  semantic materialize-once guarantee begins at native
  admission, after a thin R wrapper may have captured documented language or
  printable representation metadata. Native admission materializes each
  semantic vector once into rooted ordinary storage before the operation
  snapshot, and that native snapshot is the sole semantic authority. A hostile
  custom ALTREP whose observation changes between R-side capture and native
  admission is unsupported: its printed representation need not agree with the
  admitted value, but it must be rejected or handled without replay or Paradox
  itself causing a crash or memory corruption. Capsules never permanently
  store semantic ALTREP vectors; canonical compact data-frame row names are the
  representation-only exception. This general support does not override the
  typed-Domain rule above: an ALTREP special-value leaf for Dbl/Int/Fct/Lgl is
  rejected rather than observed; an admitted S4 special matches only by pointer
  identity. ParamUty opaque leaves are not materialized, except that special
  membership uses base `identical()` without S3/S4 dispatch.
- data.table >= 1.18.4 is an outward interoperability dependency only. Do not
  restore the old `alloc.col()` capacity bridge or call data.table internals.
  The sole cold presentation exception is the identity lookup in
  `R/ParamSet.R` for data.table's unexported `.reassign_extracted_table` and
  exported `set` R functions. It calls neither function: it only recognizes
  the active data.table stack so detached `$params` facades retain historical
  `:=`/`set()` reassignment behavior. This lookup supplies no semantic engine,
  private C API, capacity/version bridge, or permission for another unexported
  API.
- `upgrade_paradox_object()` remains the pure, non-mutating converter for one
  directly supplied Paradox object. It returns a new current ParamSet-family
  graph for a canonical built-in legacy ParamSet/Collection, returns an
  authenticated current object unchanged, and also owns standalone built-in
  Domain/Condition normalization. On R 3.6 its attempt to authenticate a
  legacy ParamSet-family R6 shell fails closed because active-binding functions
  cannot be inspected; run that conversion under R >= 4.0. It never becomes an
  in-place crawler.
- `upgrade_paradox_object_graph(x)` is the identity-preserving migration
  boundary for a containing object graph. It returns `x` invisibly and
  transplants every admitted legacy ParamSet-family R6 shell in place, including
  shells below a current `.core` payload. Every replacement preserves the
  shell's public `assert_values` policy; this is the sole serialized shell
  state outside the capsule and is not a constructor default. Its native
  iterative discovery uses
  pointer identity, an explicit work stack, and periodic interrupt checks. It
  follows ordinary lists, pairlists/language/expressions, attributes and S4
  slots, environment bindings and parents, active-binding *functions*, closure
  environments/formals/bodies and bytecode expressions, and promise-binding
  expression/environment or forced-value state—including `...` cells—where a
  policy-compliant API exposes them, without forcing an unforced promise.
  R 3.6--4.4 can inspect a reached promise through their compatibility
  accessors. R 4.5 classifies those accessors as non-API and has no replacement,
  so recursive migration fails closed on a reached promise and directs the
  caller to R 4.0--4.4 or R >= 4.6. This can arise without an explicit
  `delayedAssign()`: ordinary factory-created callback closures may retain
  forced or unreferenced formal promises in their lexical call frame. It is a
  limitation of the one-way recursive migration operation on exactly R 4.5,
  not of ordinary ParamSet operations. Never force, silently skip, or inspect
  those cells through the three symbols that R 4.5's compiled-code policy
  rejects merely to make migration appear successful. R >= 4.6 uses the
  public binding/dots API, while a
  detached `PROMSXP` outside such a cell remains opaque. `R_getVar` is excluded
  before R 4.6 because it could force a delayed binding without the new
  classifier. Current source has three reviewed retrieval call sites—one
  direct-value facade plus graph direct/forced-value branches—and every site
  follows `R_GetBindingType`; the DSO has one undefined-symbol inventory row.
  It never invokes an active binding or a serialized method.
  R 3.6 exposes no accessor for an active binding's function; encountering an
  arbitrary one during recursive migration therefore fails closed with an
  instruction to perform that migration under R >= 4.0. The one narrow
  exception is an exact built-in current Paradox-2 shell: native topology,
  class, policy, and capsule receipts admit its capsule as the graph authority,
  locked methods are skipped, and unlocked replacement closures remain graph
  edges. Package active facades are necessarily opaque on R 3.6. Replacing a
  core method or active binding is unsupported; an in-place active-binding
  replacement, or a method replacement that is relocked, preserves every
  receipt available on that runtime and cannot be distinguished from generated
  code. Anything reachable only from such a closure is not traversed (and an
  active binding is never invoked). Additive shells and
  modifications that prevent exact authentication fail closed instead of
  receiving this exception. Paradox-1 ParamSet-family R6 shells contain active
  bindings, so their practical object/graph migration requires R >= 4.0.
  Ordinary current-object operations, exact built-in current-object graph
  traversal, idempotent conversion of current objects, standalone legacy
  Domain/Condition conversion, and graphs without arbitrary active bindings
  remain supported on R 3.6.
  Direct environment bindings are classified natively rather than inferred
  from an R expression: a realized language object or symbol remains a realized
  value, while a delayed promise with the same apparent expression is identified
  as a promise and never evaluated.
  Discovery must schedule only rooted edges from one coherent observable node
  generation. Structural list/expression ALTREP is rejected before attributes
  or an ALTREP provider are observed. For ordinary vectors, pairlists, and
  closures with direct field access through the reviewed version facade, every
  destination carrier is allocated before the allocation-free
  attribute/primary-edge capture. Old-R bytecode alone retains an allocating
  public bridge and therefore requires two independently rooted complete
  observations to be exactly equal. Every environment on every runtime
  likewise requires two equal full snapshots: exact stored
  attributes, parent, sorted binding-name inventory, binding kind and selected
  promise/value/function edges, per-binding lock bits, and environment
  lock/object/S4 state. A mismatch fails closed; neither observation invokes
  an active binding or forces a promise. Package/import/user-database boundary
  policy is reapplied from that selected snapshot, so an old non-boundary
  decision never authorizes traversing a new boundary generation. Do not
  restore separate attribute and primary-edge passes, retry-until-equal loops,
  or unrooted native edge arrays.
  `.GlobalEnv`, namespaces, package/import environments, attached search-path
  infrastructure, Autoloads, base, and the empty environment are hard
  boundaries. Discovery records those process-local identities before the
  allocating walk and owns every one in an indexed ordinary `VECSXP` carrier
  until the operation returns; its parallel native array is lookup scratch
  only. This remains necessary if a finalizer detaches a search-path
  environment during discovery: `R_alloc()` storage is not scanned and must
  never be the only owner of a boundary identity. Grow and root the carrier
  before publishing a new raw-array entry. An `imports:` display name alone is
  not authentication: a
  genuine imports boundary must have an ordinary scalar raw `name` attribute
  with that prefix and the exact base namespace as its direct parent. A user
  environment with a spoofed prefix remains traversable. Generic
  external-pointer internals and weak references are
  opaque; the protected ordinary-R payload of an authenticated Paradox `.core`
  is the sole external-pointer exception.
- Current ParamSet-family shell admission has one native inert classifier.
  The class attribute must be an ordinary, attribute-free, non-ALTREP character
  vector with unique nonempty labels and the terminal suffix
  `c("ParamSet", "R6")`. An immediately preceding
  `"ParamSetCollection"` or `"ParamSetShadow"` selects that family; any earlier
  nonreserved labels are additive R6 subclass layers. With no family marker the
  shell is BASE. The selected family must agree with a canonical package-owned
  core, every Shadow must have exact snapshot metadata, and `assert_values`
  must be an exact attribute-free non-missing `logical(1)`. This current-shell
  classifier is deliberately broader than the exact one-owner legacy registry
  key below; neither invokes S3 or a shell method.
- A graph migration performs complete discovery, authentication, offside
  semantic preparation, replacement construction, and shell-shape auditing.
  It then validates every prepared/current root together in one native
  read-only barrier before the first transplant; all selected generations stay
  rooted until the complete set has passed an allocation-free receipt scan.
  Discovery reports current as well as legacy
  ParamSet-family shells: every current capsule graph is semantically
  validated during that same preflight, even when the current shell itself
  needs no transplant. A corrupt current capsule hidden beside a valid legacy
  shell therefore aborts before the legacy shell changes; a shallow carrier
  check is not a sufficient migration boundary. Current Shadow validation is
  authoritative but read-only: it builds the live semantic generation without
  installing it, retains the selected private `.core` as a separate source
  receipt, and derives collection callback detachment from that same admitted
  graph rather than rereading live shells.
  Commit is post-order, identity-preserving, and monotonic. Once a child has
  been transplanted, its prepared parent's child/origin edge is rebased to the
  identity-preserved original shell. Because rebasing can allocate or execute a
  registered replacement-owner factory, every rebase is followed immediately
  by a joint validation of all already-current identity roots plus that newly
  rebased prepared root, ending in an allocation-free receipt scan. Unrebased
  parents are offside templates rather than live shells after their prepared
  child's enclosure has moved; each re-enters the barrier when its own
  dependencies are rebased. After each transplant, the identity-preserved
  original joins the current-root set and that set is jointly validated again:
  each shell swaps `.__enclos_env__` only after every other binding and
  enclosure link is ready. A completed node is a valid current object; a
  catastrophic allocation failure inside a binding wave leaves the old
  enclosure as its completion marker and authenticates both original and
  already-refreshed package-owned methods (including an interrupted unlocked
  method) for retry. Rerunning the idempotent graph upgrader completes the
  remainder. Ordinary validation or owner-bridge failure occurs during
  preflight and mutates nothing. This atomicity statement does not cover a
  pending finalizer from an unrelated user object that deliberately mutates a
  selected root inside the R-level binding wave: `suspendInterrupts()` does not
  suppress such finalizers. Post-transplant joint capsule barriers detect
  topology, policy, capsule, and Shadow-generation changes. After the last
  allocating barrier in a session which actually transplants a shell, one
  allocation-free terminal batch receipt also authenticates the complete known
  public surface of every transplanted and already-current selected shell:
  exact class, environment lock, binding symbol/value, active kind, and binding
  lock. An all-current graph has no binding wave; it returns after joint capsule
  validation, preserving the documented R-3.6 no-op despite that runtime's lack
  of a public active-binding-function accessor. A mismatch errors, but a
  completed transplant is not rolled back and the externally corrupted graph
  is not promised to be retryable. Migration requires every selected public
  R6 shell environment to be locked; that is what makes the selected binding
  inventory complete while still allowing the supported per-binding
  replacement wave.
- The cold transplant resolves base `unlockBinding` with an explicit
  `get(..., baseenv())` call. R's package-tampering checker otherwise reports
  every syntactic `unlockBinding(name, owner)` whose environment is not the
  literal Paradox namespace, even though `owner` here is a fully preflighted R6
  shell/enclosure. Do not replace this with namespace mutation or remove the
  authentication/lock restoration around it.
- Current Paradox R6 stubs call versioned `.__paradox2_*` namespace targets
  directly. Historical unversioned `.__ParamSet*`,
  `.__ParamSetCollection*`, and pre-release `.__ParamSetShadow*` names are cold
  first-use gateways only. One native context snapshot applies the same
  ordinary suffix classifier, requires the exact public `assert_values` policy
  and a canonical matching core, follows the authenticated additive superclass
  chain to the enclosure that defines the requested BASE/COLLECTION/SHADOW
  target, and roots the resulting enclosure/private/super/core receipt. It does
  not evaluate the serialized stub's `private` or `super` promises, replay a
  guessed top enclosure slice, or reread the shell in R after authentication.
  Historical lean stubs also erase omission by forwarding every formal
  explicitly. The cold `$subspaces()` and `$search_space()` gateways recover
  `missing(ids)` / `missing(values)` from the still-active stub call frame
  without forcing the promise, and omit only that argument when it was
  genuinely omitted; explicit arguments remain lazy and are never dropped.
  This preserves the one-generation native default transactions for both
  first-use migration and current-core payloads that retain an old stub.
  Borrowing another current object's enclosure is not authentication. In
  particular, the actual pre-release Paradox-2 Shadow needs no owner registry.
  A shell without an authenticated current context defaults to a precise error
  directing the caller to `upgrade_paradox_object_graph()`. Setting
  `options(paradox.legacy_object_action = "upgrade")` opts into silent
  identity-preserving first-use migration and then resumes the requested
  operation. Invalid option values fail closed. Do not put a gateway on a
  current hot path or restore direct `mlr3misc::leanify_package()` use.
  Historical `Design$transpose()` and `Sampler$sample()` are the two
  non-ParamSet operations that can hand an embedded legacy ParamSet to current
  native code before invoking any ParamSet-family method. Their unversioned
  targets use one cold embedded-graph gateway with the same default/opt-in
  policy; current versioned Design and Sampler stubs remain direct. Paradox 1
  also serialized narrower private stubs for
  `Sampler1D$as_dt_col(x)` and
  `Sampler1DRfun$sample_truncated(n, rfun)`. Cold unversioned
  `Sampler1DRfun$.sample` (including `Sampler1DNormal`) and
  `Sampler1DCateg$.sample` targets must call the versioned lower-level targets
  directly after the embedded-graph gate; never send their additional
  Domain-derived arguments through the historical stubs or add this bridge to
  a current versioned sampling path. Both generated gateway environments bind
  the exact callable versioned `sample_truncated` target because their shared
  syntactic body contains that call; the exact categorical branch never
  invokes it. Do not replace that binding with `NULL`, which makes the
  serialized closure incomplete and produces a package code-analysis NOTE.
- `register_paradox_object_upgrader()` is the sole narrow owner-package
  extension for serialized ParamSet subclasses. It exact-matches one full
  `c(<owner class>, "ParamSet", "R6")` vector and records only an authenticated
  owner namespace plus namespace-local inspector/rebuilder names, migration
  kind (`"additive"` or `"replacement"`), and declared retired bindings. It
  has no S3/superclass dispatch and never stores or calls a function recovered
  from serialized bytes. An additive inspector returns an empty named
  dependency list because its authenticated BASE is the sole inherited
  dependency. A replacement inspector returns exactly one dependency named
  `origin`; its rebuilder must return the exact registered class backed by a
  current `ParamSetShadow` capsule. Every rebuilder must return a fresh R6
  shell: before Paradox changes the off-side shell, complete-session preflight
  rejects any shared public, private, or enclosure environment with an
  original/current migration node or any other prepared result. Shared
  dependency nodes and identity reuse by already-current graph nodes remain
  valid. Public/private R6 finalizers are rejected:
  transplanting their registration between environment identities risks
  premature or double cleanup. bbotk's legacy `Codomain` is the maintained
  additive case. miesmuschel's legacy Shadow is the maintained replacement,
  with its old-only `params_unid` and `set_id` bindings retired explicitly.
  Unknown classes and undeclared owner fields fail closed. Registration cannot
  intercept serialized owner-local
  lean targets: every owner package that emitted them must reserve the exact
  historical names as cold default-error/opt-in-upgrade gateways and replay
  from the transplanted enclosure. In particular, bbotk must cover all
  `.__Codomain__*` targets, including `$clone()`, and miesmuschel must cover
  all historical Shadow overrides.
- Built-in legacy method enclosures must have the exact currently loaded
  Paradox namespace as their parent. Registered owner enclosures must likewise
  have the exact namespace incarnation retained by the registry, followed by
  the exact current Paradox namespace. `isNamespace()` plus
  `environmentName()` is descriptive metadata and is not authentication: an
  ordinary environment can spoof both.
- Shipped C is portable C99 and supports R >= 3.6. `USE_C17` keeps ordinary
  current-R installation at C17 or earlier, while the explicit GCC >= 15 and
  recent-Clang `--use-C23` slice proves forward compatibility. The strict,
  analyzer, sanitizer, old-runtime, and pinned-header lanes remain GNU C99;
  C23 compatibility is additive evidence, never a replacement source
  contract. API spelling selection is
  centralized in `src/r_api_compat.c`; the graph walker has only the narrow
  capability gates required to select which inert edges a runtime can expose,
  never a duplicated old-R engine. Current runtimes retain their public,
  allocation-free ordinary-frame fast paths. The compatibility boundary keeps
  one conservative rooting proof across all supported branches because hostile
  class metadata can allocate during facade admission, while recognized
  callback-backed user databases are rejected before binding APIs.
  Recent R public headers themselves use a fixed-base enum on some builds, and
  R >= 4.3 uses an anonymous `Rcomplex` struct while package C is compiled as
  C99. The diagnostic push/pop in `src/paradox.h` treats only those
  official-header declarations as compiler extensions and explicitly includes
  the public `R_ext/Complex.h` used by Paradox; the same pedantic diagnostics
  remain errors throughout Paradox source.
  R 3.6--4.5 use one exact, ledgered `ATTRIB` occurrence for raw attribute
  iteration that cannot be expressed through the earlier API without
  expanding compact `row.names`. R 3.6--4.4 retain exactly three ledgered,
  header-declared/exported closure accessors: `FORMALS`, `R_ClosureExpr`, and
  `CLOENV`. Together they capture one allocation-free closure generation for
  transformation admission and recursive migration; `FORMALS` and `CLOENV`
  are called with their historical macro expansion suppressed. All three
  exceptions compile out at R 4.5, where the public closure accessors replace
  them. Directly reached bytecode alone uses the cold, non-executing public
  `as.function.default()`/`body()` bridge on old R; `R_BytecodeExpr` must not
  compile before it becomes API.
  R 3.6--4.1 use a cold
  `base::exists(..., inherits = FALSE)` query only where absence is an accepted
  result. Candidate-shell and fresh-destination classifiers use that optional
  path, while admitted core/generation reads keep the required native path so
  old R does not evaluate `base::exists()` on every hot operation. Required
  binding snapshots remain allocation-free. Because the old optional query
  enters the evaluator and may run pending finalizers, active capsule-graph
  traversal roots the shell and exact selected core generation independently
  of their mutable bindings on every supported R branch. On R 3.6--4.1 the ownership
  gateway first rejects a non-object `self` by its constant-time object bit,
  preventing a malformed direct native call from entering the required reader
  with an absent R6 enclosure binding. That guard is compiled out beginning
  with R 4.2, whose public existence query already handles absence. Their
  terminal
  optional receipt scan fails closed when `R_HasFancyBindings()` reports a
  locked or active frame, then uses the same stored-cell path; this old-only
  exception avoids either evaluator allocation or invocation of an active
  binding. The exact Shadow generation-receipt regression for this branch
  executes on R 3.6--4.1. R >= 4.2 records one source-derived skip because its
  public non-evaluating binding-existence operation makes the old evaluator
  path unreachable; this is a capability boundary, not a behavior waiver.
  The facade rejects `UserDefinedDatabase` environments through the
  same public inheritance predicate R uses before any binding operation:
  their callback-backed table is unsupported, and old
  `R_HasFancyBindings()` assumes an incompatible ordinary-frame layout.
  The graph walker applies this boundary before namespace/package
  classification because old R implements those predicates through an
  object-table lookup.
  R 3.6--4.5 use the declared/exported `Rf_findVarInFrame` to obtain the stored
  frame cell. Only R 3.6--4.4 inspect a returned `PROMSXP`, through the three
  header-declared/exported accessors `R_PromiseExpr`, `PRENV`, and `PRVALUE`.
  R 4.5's compiled-code policy classifies those accessors as non-API, so its
  crawler fails closed when it reaches a promise and requests migration under
  R 4.0--4.4 or R >= 4.6. Ordinary callback factories can retain such formal
  promises in their call frames. R >= 4.6 uses only the documented
  experimental binding/dots APIs:
  the DSO contains none of the three detached-promise accessors, and a
  structurally reached non-binding `PROMSXP` is opaque. An R-level
  `substitute()` workaround is
  insufficient for receipt scans and recursive graph discovery: although
  non-forcing, it returns a promise expression and cannot distinguish that
  expression from a realized language/symbol value or provide a stable
  binding-generation receipt. Every such symbol/version/source occurrence must be listed
  exactly in `environment/r-api-exceptions.tsv`, raw-token and DSO audited, and
  tested against pinned headers and real runtimes before freeze.
  Although `R_getVar` is public from R 4.5, it may force a delayed binding
  before R 4.6's classifier can distinguish the cell. All pre-4.6 inventories
  therefore forbid it. The raw-token audit requires the exact three current
  call sites and every call follows `R_GetBindingType` after direct or forced
  value classification; the DSO inventory requires one undefined-symbol row.
  Printable simple-Domain IDs retain one native renderer on every supported
  R. R >= 4.5 snapshots `scipen` with the documented allocation-free
  `Rf_GetOption1`; R 3.6--4.4 call public `base::getOption()` through the
  compatibility facade and then stay in the same native renderer. The older
  runtimes must not call their header-declared but then-undocumented
  `Rf_GetOption1`, and they must not fall back unconditionally to R
  `deparse1()`. Runtime DSO inventories forbid that symbol through R 4.4 and
  require it exactly once beginning with R 4.5. A malformed or changing option,
  unsupported representation, or overlong output still takes the existing
  correctness fallback.
  `R_HasFancyBindings`, `Rf_findVarInFrame`, `R_PromiseExpr`, `PRENV`, and
  `PRVALUE` are confined to their exact old-runtime branches and ledgered; the
  last three are absent beginning with R 4.5.
  The exact R 4.5.2 runtime stage also runs that runtime's own
  `tools:::check_compiled_code()` against the installed package and retains an
  authenticated zero-issue receipt. The explicit DSO inventory is a semantic
  branch proof, not a substitute for R's policy checker.
  Pre-4.6 required snapshots authenticate once and then use the centralized
  stored-cell selector; do not reintroduce a duplicate active/class boundary
  into this hot path.
  None is a
  CRAN allowlist or permission for another internal API or semantic path.
  Old-Windows portability does not rely on `%lld`, `%I64`, or `j`/`z`/`t`
  integer-length formats: graph indices use bounded decimal arithmetic and
  diagnostic-only long-vector positions use exact `%.0f`/double formatting
  within R's 2^52 long-vector limit. The strict source/header gate rejects
  those integer format spellings, including decorated variants. Formatted
  ParamSet failures never size with `vsnprintf(NULL, 0, ...)`: Rtools35's
  MSVCRT path returns a negative value for that idiom and for truncated real
  buffers. The single formatter instead uses a real local buffer, `va_copy`,
  bounded growth, and supports both the C99 required-length and old-MSVCRT
  negative-on-truncation conventions. All of its format arguments are package-
  bounded; the 64-KiB corruption/formatter-failure ceiling therefore truncates
  no supported diagnostic. The source gate rejects null-buffer printf sizing.
  GCC-only
  diagnostic pragmas are compiler-
  version gated, and strict/analyzer/sanitizer profiles compile the package as
  GNU C99 rather than proving only that C17 accepts it.
  `src/binding_snapshot.c` exposes the same centralized classifier to the cold
  R migration/gateway code as one registered native call: it returns an exact
  realized ordinary frame value, or a negative result for absent, inherited,
  active, or delayed bindings, without evaluation. Do not recreate a
  `substitute()`-based classifier in R; realized language objects and symbols
  as well as literal promises whose expressions are `TRUE`, `NULL`, a language
  object, a symbol, an environment, a closure, or an external pointer prove why
  expression/type shape cannot distinguish a delayed binding from a realized
  value.
  Treat R API predicates as predicates rather than assuming a stable
  integer typedef: when storing their result, normalize it with an explicit
  comparison such as `predicate(...) != FALSE`. The pinned old-header compiler
  matrix is authoritative for signedness and declaration drift. The params
  reader accepts an already admitted, rooted core directly; collection reads
  do not allocate a temporary environment or depend on `R_NewEnv()`. R 3.6
  lacks list ALTREP, so only the package's adversarial VECSXP ALTREP test
  fixture is unavailable there. Production list-ALTREP branches are vacuous
  below R 4.3, while atomic ALTREP and every ordinary-container contract remain
  tested. Linux, Windows x86-64, and Apple ARM64 remain first-class targets.
  Corrupt/forged state must error and must never cause an out-of-bounds access,
  stale pointer, double evaluation, or segfault.

Do not leave obsolete compatibility code merely unreachable. Before release,
all semantic translation units must be free of generated-closure/body
authentication and sentinel-to-R replay. Temporary migration adapters must be
marked, have no alternate semantics, and be deleted before the candidate ref.
The documented versioned-target/historical-gateway layer and exact owner
registry are the permanent serialized-object migration boundary, not temporary
adapters.

## Repository-local environment

The host R 3.6.3 and host/user libraries are out of scope and must not be
modified. Never use `sudo`, edit shell startup files, or install into HOME,
`/usr`, or a system R library.

Provision once, then activate from the repository root:

```sh
scripts/bootstrap
. scripts/activate
```

Activation selects the pinned local R 4.6.1 toolchain and
`.local/R/library`, clears inherited compiler/library variables, and redirects
temporary and cache state below the repository. Confirm retained work with:

```sh
test "$PARADOX_ACTIVE_ROOT" = "$(pwd -P)"
test "$(command -v R)" = "$PARADOX_ROOT/.local/toolchain/bin/R"
test "$(command -v Rscript)" = "$PARADOX_ROOT/.local/toolchain/bin/Rscript"
test "$(R RHOME)" = "$PARADOX_ROOT/.local/toolchain/lib/R"
```

Core authenticated inputs include:

- `environment/toolchain-linux-64.lock`: local development toolchain;
- `environment/r-packages-linux-64.lock`: exact source-package closure;
- `environment/configspace-current-linux-64.lock` and
  `environment/configspace-old-linux-64.lock`: the exact Python 3.10.20
  environments for the current ConfigSpace 1.2.2 and retained ConfigSpace
  0.5.0 full-test axes;
- `environment/runtime-r-3.6.3-linux-64.lock`,
  `environment/runtime-r-4.0.5-linux-64.lock`,
  `environment/runtime-r-4.1.3-linux-64.lock`,
  `environment/runtime-r-4.2.3-linux-64.lock`,
  `environment/runtime-r-4.3.3-linux-64.lock`,
  `environment/runtime-r-4.4.3-linux-64.lock`, and
  `environment/runtime-r-4.5.2-linux-64.lock`: supported-runtime prefixes;
- `environment/runtime-r-3.6.3-packages.lock` and
  `environment/runtime-r-4.0.5-packages.lock`,
  `environment/runtime-r-4.1.3-packages.lock`, and
  `environment/runtime-r-4.2.3-packages.lock`: exact source-package closures
  for the four pre-R-4.3 runtime axes;
- `environment/runtime-r-3.6.3-declared-floor-packages.lock`: the separate
  exact declared-floor source closure;
- `environment/runtime-r-3.6.3-prefix-repair.lock`: the authenticated repair
  boundary for the managed R 3.6 prefix;
- `environment/runtime-matrix.tsv`: authenticated runtime-axis registry;
- `environment/runtime-matrix-old-r-stress.tsv`: exact bounded old-runtime
  stress targets;
- `environment/r-api-sources.tsv`: local reference R sources/manuals;
- `environment/r-api-exceptions.tsv`: the exact reviewed versioned R C API
  exception ledger;
- `environment/valgrind-r-packages.tsv`: instrumented-R package closure.

`scripts/environment/runtime-matrix-trusted-inputs` is the exhaustive
runtime-matrix input enumerator. It also binds the floor-smoke,
old-runtime-stress, cross-serialization, result-skip, and verification policy
programs into retained evidence; do not maintain a competing hand-written
exhaustive list here.

Bulky state is deliberately ignored below `.local/` and `.cache/`. Bootstrap
is idempotent and checksum-verifying; reuse valid downloads and installations
instead of rebuilding them. A lock mismatch fails closed and requires an
explicit reviewed lock refresh.

`scripts/bootstrap` provisions both ConfigSpace profiles through
`scripts/bootstrap-configspace`; use `scripts/bootstrap-configspace --verify`
for a read-only audit. Each profile comes only from its checked-in SHA-256
`@EXPLICIT` lock with `--always-copy`. The bootstrap then makes prefix
directories and executable files `0555`, other regular files `0444`, and
writes authenticated inventory, content/mode/symlink, and metadata receipts.
Full native tests verify both profiles before and after the run, pass their
exact interpreters through `PARADOX_CONFIGSPACE_CURRENT_PYTHON` and
`PARADOX_CONFIGSPACE_OLD_PYTHON`. The `native-release` worker consumes
`.local/configspace` read-only with no network. Reticulate/uv resolution,
provisioning, and shared dependency-prefix/cache mutation are not part of that
gate; disposable per-mode caches remain outside the sealed fixture.

The full current-R package checks are also genuinely networkless. Before
`--as-cran`, `scripts/native-check` reauthenticates every non-bootstrap source
archive in `environment/r-packages-linux-64.lock`, adds the just-built
candidate, and uses public `tools::write_PACKAGES()` to construct a per-run
CRAN index. A separate valid empty index supplies `BioCsoft`, `BioCann`, and
`BioCexp`; omitting any standard name makes current R fall back to live
repositories. The profile, all three CRAN index encodings, the empty BioC
index, and their exact inventory are source- or content-bound and verified
before and after both checks. `_R_CHECK_CRAN_INCOMING_REMOTE_=false` and
`_R_CHECK_SYSTEM_CLOCK_=false` disable only work that inherently needs the
network. Do not replace this with empty `options("repos")`, an empty CRAN
index, warning suppression, or a whitelist: those approaches skip useful
dependency-cycle evidence. Remote-current repository metadata remains a
hosted-CI/CRAN claim, not a local one.

ConfigSpace bootstrap never removes incomplete state. If only one side of a
profile exists, review it and remove only the exact reported
`.local/configspace/prefixes/<profile>` and
`.local/configspace/receipts/<profile>` paths before reprovisioning; never
generalize that cleanup to a parent directory. Because a sealed prefix's
directories are `0555`, first restore owner write permission on directories
below that one exact prefix
(`find <exact-prefix> -type d -exec chmod u+w '{}' '+'`); only then remove that
exact prefix and receipt directory.

The R 4.3.3 conda prefix contains data.table 1.17.8 because no matching
conda-forge R-4.3 build of 1.18.4 exists. `scripts/test-runtime-matrix` copies
the SHA-256-pinned cached source
`.cache/downloads/r-packages/data.table_1.18.4.tar.gz` into the isolated R-4.3
stage and installs it before Paradox. R 4.4.3, R 4.5.2, and the primary
library already contain 1.18.4. Never weaken `DESCRIPTION` or restore the
capacity bridge for this test-infrastructure detail.

Provision and inspect real older runtimes with:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
. scripts/activate-runtime-matrix 3.6.3   # or any registered 4.0.5--4.5.2 axis
. scripts/activate                        # return to R 4.6.1
```

The source-with-version spelling requires Bash or zsh. dash does not forward
arguments to its dot builtin; repository automation uses Bash even though the
activation body itself remains portable shell.

Micromamba must create every supported-runtime prefix with `--always-copy`.
Never seal a prefix whose regular files are hard-linked to the writable
micromamba package cache or to another prefix: a single in-place cache write
would mutate several supposedly immutable runtimes. The complete prefix
receipt rejects every multiply-linked regular file in addition to checking
content, modes, paths, and internal symlinks. A prefix made by the older
hard-linking bootstrap must be deliberately moved aside or otherwise
reprovisioned from the exact lock; do not weaken or refresh its receipt to
admit shared inodes, and do not auto-delete it.

R 3.6.3, R 4.0.5, R 4.1.3, and R 4.2.3 install their exact complete-test
source locks once into sealed, content- and identity-receipted libraries.
Interactive activation fully
verifies the selected library; a retained runtime worker instead consumes the
coordinator's exact authenticated receipt handoff and does not repeat the full
tree check. A source-library build key is scoped to the selected runtime's
artifact fields, exact locks, authenticated prefix-content manifest, and the
explicit install schema; verifier/helper identity remains refreshable receipt
provenance. Therefore an unrelated registry row or verification-only edit must
not rebuild an unchanged package closure. Any install-algorithm or isolated
build-environment change must bump `SOURCE_LIBRARY_BUILD_SCHEMA`. One
source-derived trusted-input manifest covers every helper,
registry, policy, dependency lock, and prefix-repair lock. The coordinator
checks it immediately before and after the worker wave and binds it into the
top-level and per-stage evidence. The declared-minimum R 3.6 package check uses
`--no-tests`; the separately receipted complete-source stage already runs the
supported test suite once. Its locked local test library contains five of the
nine direct Suggests and deliberately omits exactly `reticulate`, `rmarkdown`,
`mlr3learners`, and `e1071`. The check must therefore exit zero with exactly
that one missing-Suggests dependency NOTE, no other NOTE/WARNING/ERROR/halt,
and one sole final `Status: 1 NOTE`; `Status: OK` is not a truthful contract for
this axis. R 3.6's compiled-code checker ignores the configured `NM` and
searches `PATH` for the literal command `nm`. The stage therefore prepends the
sealed prefix's compiler-family target-tool directory only around this check,
after proving that its `nm` alias has the exact expected relative target and
resolves to the same authenticated inode as the `NM` used by the DSO audit.
Do not install a worker-global binutils, copy a second binary, or broaden that
PATH adjustment to other stage commands.

`scripts/bootstrap-runtime-matrix` alone owns persistent runtime and
dependency-library provisioning. For an R 3.6.3 selection, provision mode owns
both the complete-test and declared-floor closures, while `--verify` checks
both without mutation. `scripts/test-runtime-matrix` may only consume those
verified caches and copy their receipts into a retained run. Its trusted-input
inventory includes `environment/Renviron`, `environment/Rprofile.R`, and
`environment/Makevars`; every retained stage points the corresponding R
variables, including `R_BUILD_ENVIRON`, `R_CHECK_ENVIRON`, and
`R_INSTALL_ENVIRON`, to the detached authenticated copies below its retained
inputs. Stage activation selects those copies before its first R child;
interactive activation alone selects the live developer files. Never restore
coordinator-side cache repair or a live
checkout startup path inside an admitted stage. Successful bootstrap
verification also removes its owned empty scratch directories after closing
their contents; it must not leak one directory per runtime replay.

The R 3.6 stage separately consumes the `declared-floor` profile of the same
sealed library manager. Its exact lock contains backports 1.1.7, checkmate
2.0.0, data.table 1.18.4, mlr3misc 0.10.0, R6 2.6.1, and the sole transitive
dependency digest 0.6.39. The stage rebuilds only Paradox into a fresh
candidate library against those floors, authenticates every installed package
identity and dependency namespace origin, plus the candidate Paradox DLL origin
and registration, and exercises constructor/deparse, checked dormant/dependency
behavior, diagnostics, and grid/data.table/R6 paths. Cache keys and receipt
handoffs keep this bounded lane reusable; ambient `R_DEFAULT_PACKAGES` is
removed so an operator startup choice cannot preload a reviewed dependency.
The stage creates and authenticates its fresh candidate-library directory
while the artifact-root descriptor is still pinned, then closes that
descriptor before the first build or R subprocess. A runtime-only directory
must never be created later through the closed descriptor, and the descriptor
must never leak into a child.

Every supported-runtime stage runs its complete main source suite with
`NOT_CRAN=true`; a `Reason: On CRAN` result is therefore a harness failure, not
an admitted skip. The runner also removes the characterization-GCT override so
an inherited developer setting cannot silently shrink that suite. Only exact
source-derived capability skips remain in
`runtime-matrix-result-skips.tsv`: active-binding inspection on R 3.6,
list-ALTREP fixtures before R 4.3, and the inverse Shadow receipt boundary.
That old optional-binding path is exercised on R 3.6--4.1 and its one exact
regression is skipped on R 4.2--4.5, where the public non-evaluating existence
query makes that path unreachable.

R 3.6.3 and R 4.0.5 additionally replay the bounded
`runtime-matrix-old-r-stress.tsv` slice. Its literal test titles are validated
against the authenticated source, and a deterministically regenerated final
test helper leaves top-level setup intact while declining to force every
unselected `test_that()` body. The retained staged sources, title filter,
per-block ledger, log, and counts must replay exactly. Each old runtime first
proves that an unselected body is unforced and that a selected braced
expression still receives testthat's isolated evaluation environment;
selected targets may neither skip nor warn. This focused replay does not
substitute for the full main suite and must not use a frozen expected target
count or be repeated on newer runtimes.
The old testthat/withr releases key their language setup from `LANG`. Their
main and stress launchers therefore use `LANG=C` while the stage's
`LC_ALL=C.UTF-8` establishes the deterministic UTF-8 process locale. The
main and stress runners clear `LC_ALL` only after R startup so temporary locale
changes are not overridden. This prevents thousands of no-op framework warnings
without changing package semantics or weakening the warning-free contract.
Old R's shell front end also expands backslash escapes while transporting an
inline `Rscript -e` expression. Inline harness expressions must therefore use
R constructors such as `intToUtf8(9L)` for control characters (or a
checked-in script), never a literal `"\t"` that R 3.6/4.0 can split before
parsing. Source fixtures that require character dependency columns must spell
`stringsAsFactors = FALSE`; factor-backed dependency IDs remain rejected by
the native closed ingress rather than gaining version-dependent coercion.

After every selected stage is sealed, a complete `--runtime all` run performs
one R 4.0.5 -> R 3.6.3 serialization handoff. The producer and consumer are
bound to their exact stage package and DSO paths. The fixture covers base,
collection, and shadow capsules; shared topology; an attribute-hidden closure;
dormant values including named `NULL`; callbacks, checks, transformations,
mutation, and an R 3.6 round trip. Its artifacts, isolated state, logs,
per-stage seals, candidate DSOs, and receipts are independently sealed and
replayed. A partial matrix records `not-applicable` and retains no such
artifact.

`scripts/test-runtime-matrix` validates both the deliberately empty pre-R-4.6
exclusion policy and the reviewed result-skip manifest against the exact
extracted candidate's current runtime-capability guards before resource
admission or any runtime build/install worker starts. The preflight still
discovers `skip_on_cran()` titles and ordering, but it rejects every `On CRAN`
ledger row because the main suite admits those blocks. Keep that source-derived
policy preflight shared with the old-runtime test runner. The coordinator also
stages and authenticates the mandatory `mbo_config` upgrade inputs before that
same boundary, so the fixture test must execute and may not become an
unreviewed environment-dependent skip. Malformed or stale input therefore
fails cheaply instead of after the runtime package installations. In
particular, validate the header-only exclusion manifest as zero rows; do not
construct a synthetic file name from its empty `context` column.

Reference R source, Writing R Extensions, R Internals, data.table source, and
the analyzer sources are populated by `scripts/fetch-reference-sources`.
Consult those pinned local sources rather than remembered C-API behavior.

The mandatory legacy-upgrade fixtures originate below the reviewed
`mbo_config` commit's `common/` directory, not at its repository root. Before
an ad-hoc direct complete unit-test run, bind the directory that actually owns
the two RDS files:

```sh
fixture_bundle="$PARADOX_ROOT/.local/tmp/mbo-config-fixtures-development"
if test -d "$fixture_bundle"; then
  scripts/environment/mbo-config-fixtures verify "$PARADOX_ROOT" \
    "$PARADOX_ROOT/.local/compat/github/mbo_config" "$fixture_bundle"
else
  scripts/environment/mbo-config-fixtures stage "$PARADOX_ROOT" \
    "$PARADOX_ROOT/.local/compat/github/mbo_config" "$fixture_bundle"
fi
export PARADOX_MBO_CONFIG_ROOT="$fixture_bundle/common"
test -f "$PARADOX_MBO_CONFIG_ROOT/mixed_search_space.rds"
test -f "$PARADOX_MBO_CONFIG_ROOT/numeric_search_space.rds"
```

An unset value deliberately skips that optional development fixture; every
release run sets it and treats either missing file as a failure. The shared
`scripts/environment/mbo-config-fixtures` helper requires
`compat/github-snapshot.tsv` and `compat/mlr-org-review.tsv` to agree on the
exact reviewed commit/tree, reads the two files from immutable Git objects
rather than the checkout worktree, and publishes a read-only, receipted
bundle. `scripts/native-check` retains that bundle inside the functional
mode's sealed artifact tree. `scripts/test-runtime-matrix` stages one shared
bundle before resource admission and gives both old-R workers its `common/`
directory; the evidence verifier reauthenticates the repository manifests,
Git tree, file bytes, and complete bundle receipt. A dirty or differently
checked-out worktree is irrelevant provided the reviewed Git objects remain
present. The documentation and consumer runners have their own exact-corpus
receipts and do not infer this path from HOME.

Documentation generation is optional development tooling and is deliberately
kept out of the pinned runtime library.  The current generator is roxygen2
8.0.0 in `.local/R/tooling-library`; its cached source archive is
`.cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz` with SHA-256
`75816bf3a25554f5752254b985b7242490b0fabe68a5ada335c340b820ba34e8`.
Recreate it in the separate tooling library:

```sh
. scripts/activate
mkdir -p .local/R/tooling-library .cache/downloads/r-tooling
if [ ! -f .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz ]; then
  curl -fL https://cran.r-project.org/src/contrib/roxygen2_8.0.0.tar.gz \
    -o .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz ||
    curl -fL https://cran.r-project.org/src/contrib/Archive/roxygen2/roxygen2_8.0.0.tar.gz \
      -o .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz
fi
test "$(sha256sum .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz | cut -d' ' -f1)" = \
  75816bf3a25554f5752254b985b7242490b0fabe68a5ada335c340b820ba34e8
R CMD INSTALL --library=.local/R/tooling-library \
  .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz
Rscript -e '.libPaths(c(".local/R/tooling-library", .libPaths())); roxygen2::roxygenise(".")'
rm -f src/*.o src/paradox.so
```

The hard dependencies of that generator are already in the pinned development
library. Roxygen 8 needs the R6 method source references supplied by its normal
pkgload method; its source-only loader exits successfully but writes incomplete
R6 method documentation. Consequently a documentation refresh performs one
disposable debug build. Run it only after the R surface has converged, remove
the live-root build artifacts immediately, and do not treat that build as
candidate evidence. Do not install an older generator closure into the runtime
library.

## Editing and native-code rules

- Preserve unrelated user changes and dirty-worktree state.
- Use `apply_patch` for source edits. Generated documentation may be refreshed
  with its normal generator only when that is the intended mechanical change.
- Register every `.Call` routine with a fixed arity, disable dynamic symbol
  lookup, and keep headers, `src/init.c`, the routine coverage ledger, and
  direct probes synchronized.
- Validate every R type, length, name, attribute, row count, graph edge,
  arithmetic conversion, and index before use. A version tag is not a safety
  proof.
- Never retain an unprotected R object or raw vector pointer across allocation,
  callback evaluation, interrupt polling, ALTREP access, or error construction.
- Use checked `R_xlen_t`/`size_t` arithmetic and iterative graph traversal.
- Snapshot callback-bearing state before execution. A reentrant nested
  operation sees current state; the enclosing operation finishes from its
  snapshot. Mutating commits detect intervening capsule replacement and fail
  without overwriting it.
- Construct public data.table facades at the boundary, finalize their public
  self-reference, and ensure every mutable shell and column is newly owned or
  detached from the capsule. The compact fresh-facade helper is only for a
  package-owned shell with package-owned metadata; caller-owned input uses the
  defensive finalizer, which owns every column spine and its complete mutable
  attribute metadata even when the column is unnamed. List-column payload
  leaves retain exact opaque identity. Never synthesize private indices or
  call private C APIs.
- Prefer a compact readable native operation over layers of helpers that only
  existed to reproduce an R vectorization pattern. Avoid per-row R calls except
  documented callbacks.
- Package-owned mapping closures should be created by small fixed factories
  with only their required bindings. Do not compile a fresh closure through
  `crate()` for every Domain construction; this was a measured hot-path cost
  and provides no additional state isolation.
- A ParamSet constructor with no initial values does not enter an empty native
  value-store transaction. Complete collection-value admission retains the
  already resolved BASE parameter row in its operation-local plan rather than
  searching the same ID again during upward translation. The inherited native
  Shadow dependency reader owns refresh; its R binding must not refresh a
  second time. `$has_deps` reads the validated native dependency count directly
  instead of projecting and wrapping `$deps`; its retained 64-parameter
  BASE/COLLECTION/SHADOW probe moved from 498.575 to 156.065 microseconds
  (3.195x) without a weaker graph path. The one-evaluation allocation profile
  remained exactly 12,688 bytes in 14 records on both sides, so record this as
  a latency improvement, not an allocation improvement. The exact libraries,
  fingerprints, 50-iteration/three-warmup method, and raw evidence path are in
  `benchmarks/README.md`. These measured shortcuts remove redundant work
  without caching graph validity or weakening admission.
- Collection graph readers keep bounded inline scratch for 16 nodes, then grow
  all coordinated node/path/postorder arrays together with checked temporary
  allocation. Admission retains and reuses the first prior-node lookup instead
  of rescanning the prefix. This removes 2,232 bytes of scratch at 16 nodes and
  7,008 bytes at 64/256 nodes; it does not create a trusted-node bit, skip a
  graph/generation check, or change shared-DAG/cycle behavior.
- Keep unavoidable package-owned callback wrappers thin. In particular,
  `to_tune(ParamSet)` may call the supplied transformation and perform its
  one-list-result/name boundary directly; it must not add checkmate or
  mlr3misc layers to every callback execution.
- A sparse-target `$search_space()` facade redesign was measured and rejected:
  search-space conversion is cold and the end-to-end maintained workload moved
  only about 2%, which did not justify another target-projection path. Do not
  restore that experiment without new representative evidence. A native bulk-
  dependency constructor transaction is also a measured no-go for the 2.0.0
  release. A 64-parameter/27-requirement isolated estimate moved from 6.57 ms to
  4.11 ms (with requirement-heavy estimates spanning roughly 1.4--2.5x), but
  representative xgboost learner construction improved only about 6--7%. The
  change requires a moderate-risk new native batch transaction, while the
  maintained release workload currently lacks dependency-rich constructor
  coverage. Under release steering this is not low-hanging. Retain the landed
  low-risk wins; the batch design may be revisited as an internal optimization
  without another compatibility or API break.
- Keep the shared public-table classifier and row-count admission explicit at
  each ingress. The final bounded audit found representative hardened paths
  flat to 2.8% faster than the superseded candidate; deliberately minimal
  keyed/indexed data.table paths paid only 0.6--1.1 microseconds. Repeated
  symbol lookup costs under 79 ns per table and a complete keyed/indexed
  classifier costs under 0.47 microseconds, so cached symbols or plumbing a
  prior classification through six operations is not release-worthy
  low-hanging fruit. The exact DSO identities, allocation result, method, and
  raw evidence paths are recorded in `benchmarks/README.md`.
- CHARSXP equality uses pointer identity first. Equal UTF-8, Latin-1, or bytes
  encodings may compare their stored bytes; native-encoded strings may do so
  only when both are ASCII. Mixed encodings and non-ASCII native strings must
  retain the translating UTF-8 comparison. Collection affixed-ID validation
  follows the same rule. These portable fast paths were measured; do not add a
  cached graph-validation mode, skip corrupt-state checks, or use raw bytes
  outside this boundary merely to accelerate collection reads.

Useful pre-build audits:

```sh
git diff --check
rg -n 'surface_auth|fallback sentinel|R_ClosureExpr|R_BindingIsActive' src R
rg -n 'UseMethod\("(domain_|condition_|tunetoken_|pslike_)|checkmate::|data\.table::' R src
rg -n 'PARADOX_CORE_(BASE|COLLECTION|SHADOW)' src
```

Some words such as “fallback” legitimately describe mathematical/default
choices. Review findings in context; the forbidden case is a second semantic
execution or mutable-surface authentication path.

## Information-efficient development verification

Verification should be trustworthy and proportional. During implementation:

1. parse changed R/tests and run `git diff --check`;
2. compile only changed C translation units with the strict C99 warning set;
3. install one stable source snapshot into one disposable library;
4. run all directly affected test files in one batch and fix a coherent batch
   of failures, not one failure per complete rerun;
5. run the full Paradox unit suite once after affected tests converge.

Do not run `R CMD check`, the entire consumer corpus, Valgrind/rchk, all
runtimes, documentation, and benchmarks in the inner loop. A changing source
invalidates those expensive results, and repeating them has low information
value. Do not build from the live root while another worker is compiling there;
copy package sources (excluding `.o`/`.so`) to a stable stage first.
At the release boundary, do not infer full-check success from process status:
the retained final `Status:` line must contain neither ERROR nor WARNING.
For an exact downstream head, first run repository-local `R CMD build` on its
authenticated Git archive and check the resulting retained package tarball,
never the raw source directory. The evidence must bind the build exit/log and
tarball path/hash separately from the check exit/log; this also preserves R's
required `Authors@R` metadata expansion before R 4.6 package checks.
Receipts for downstream checks must fingerprint every library on the actual
search path, including bridge and `mlr3verse` extra libraries, in order.
Keep diagnostic probes bounded as well: do not use recursive
`.Internal(inspect())` on R6/capsule graphs, because environments and shared
edges can produce unbounded traversal and output. Inspect exact attributes,
classes, payload fields, and identities explicitly instead.
In fail-closed Bash validators that enable `pipefail`, do not pipe a long
captured string from `printf` into `grep -q`: `grep` may exit after its match
and turn the producer's `SIGPIPE` into a nondeterministic false failure. Match
the captured value through a here-string or a regular retained file instead.

Use `scripts/environment/resource-jobs` before parallel work. Parallelize
independent test files, consumers, and runtime stages at the outer level while
keeping nested make/testthat/BLAS/OpenMP pools at one. The one reviewed
exception is the Paradox suite itself: it is validated for parallel testthat,
so a driver or an ad-hoc complete-suite run may set `TESTTHAT_PARALLEL=true`
with `TESTTHAT_CPUS` taken from a `light-test` admission; foreign consumer
suites stay serial per row. Honor the reported
memory-aware ceiling. Never multiply every layer by the CPU count, and retain
enough RAM that the controlling Codex process cannot be OOM-killed.
`PARADOX_API_JOBS` and `PARADOX_BRIDGE_COMPILE_JOBS` are lowering-only release
knobs for the R-API matrix and downstream bridge compilation respectively; the
resource scheduler remains the upper bound. Admission is containment-aware
(schema-2 reports record the mode). A proved hard per-worker container uses
measured cooperative weights — 2-GiB consumer rows capped at eight and 1-GiB
light-test jobs — with a 1-GiB intra-worker reserve floor inside its exact
CPU/RAM cgroup. The aggregate systemd envelope uses those weights with a
4-GiB shared reserve floor and fatal event counters. Direct, uncontained work
keeps the conservative policy: on this 32-CPU, no-swap host follow the
reported direct ceilings (at most 4 consumer rows, 16-GiB reserve) unless a
fresh resource report requires less. The controller-only hard-worker marker
requires exact assigned envelopes and authenticated raw v1/v2 limits; never
admit it from task configuration. Prefer running broad parallel work through
`scripts/verify` so it is contained; `verification/README.md` documents how
to loosen one allotment in a targeted way after a limit event.
`scripts/memory-check` performs this admission itself for its serial heavy
modes: Valgrind receives one 16-GiB working-set allowance while at least 16 GiB
remains reserved for the host, and rchk receives its one 20-GiB analyzer
allowance with the same minimum reserve. The reports are release evidence;
do not bypass or hand-edit them. Do not impose an address-space limit on
Valgrind merely to mirror rchk: Valgrind's shadow mappings make virtual address
space a poor resident-memory/OOM estimate.
The retained `validate-native-source-run` authenticates the source-run copies
of the `mbo_config` and runtime-matrix Git/receipt helpers against their active
repository-root copies before executing them; do not resolve those
root-dependent helpers relative to a relocated validator. Every helper that
the validator intentionally resolves beside itself, including
`create-offline-check-repository.R`, must instead be copied into the relocated
bundle, hashed by the memory harness, and named in the independent memory-run
validator's exact inventory. The validation-hardening test derives this sibling
set generically so a later validator dependency cannot be omitted silently.
The memory harness receipt also hashes both native test workers as well as the
runner and verifier.

Worktree snapshot and replay manifests commit exact regular-file modes as well
as content. Copy helpers must restore those modes with
`Sys.chmod(..., use_umask = FALSE)` and verify both source stability and copied
state; `file.copy(copy.mode = TRUE)` alone is not exact under a restrictive
worker umask. A source run whose retained manifest and copied-tree receipt
disagree on mode is not replayable and must never seed another release gate,
even when its functional package tests passed.

Caching policy:

- toolchains, package downloads, installed dependency libraries, reference
  sources, analyzer runtimes, and content-addressed consumer installs are
  reused when their authenticated inputs match;
- package objects/installations may be reused only for identical source and
  compiler keys. During development only, an already compiled DSO may survive
  an R/docs-only change when every native build input (all `src/` bytes,
  headers, generated registration inputs, `NAMESPACE`, `DESCRIPTION`, compiler,
  flags, and platform) is byte-identical; reinstall the R/help databases and
  verify the loaded DSO hash. Record that identity check with the diagnostic;
- every frozen distributable payload receives a clean full source build in each
  executed profile. DSO component reuse is development evidence only and never
  satisfies a release, check, runtime, memory, downstream, or benchmark row;
  reuse across refs is conclusion transfer from a clean donor execution after
  sealed complete-payload identity, never reuse of development objects;
- each distinct frozen R/compiler/instrumentation profile builds/installs the
  distributable payload once from clean source, then shares that immutable
  installation across its tests and any gates that explicitly authenticate the
  identical profile and package bytes;
- a later ref may reuse a completed package-facing gate only when a sealed,
  independently replayed equivalence stage proves all changed Git paths are
  excluded by the exact `.Rbuildignore`, both clean `R CMD build` payloads have
  the same complete file inventory, and every payload byte is identical after
  removing only R's generated `Packaged:` record. The ledger must name both
  commits/trees, the normalized manifest, and each transferred gate. This does
  not transfer source-tree tooling, policy, documentation, downstream-profile,
  benchmark, memory, or portability conclusions whose own inputs changed;
- a failed broad run is mined for the complete failure set and logs before a
  rerun; rerun affected rows first, then one final broad confirmation.

Candidate downstream bridges are one build-once release artifact, not setup
performed independently by each consumer gate. After the candidate and the
priority-one dependency library have been authenticated, run
`compat/install-downstream-bridges --candidate-source "$candidate_source"
--evidence-profile release-refresh-20260720 --paradox-axis paradox2` once. It
installs the exact reviewed heads, in the fixed order bbotk, mlr3, mlr3tuning,
miesmuschel, mlr3pipelines, mlr3fselect, mlr3mbo, celecx, and mlr3fda, into the
suffixed
`.local/compat/runs/$PARADOX_CANDIDATE_RUN_ID/library-downstream-bridges-release-refresh-20260720-paradox2`
and publishes it only after complete verification. The final overlay is
read-only;
its sealed evidence binds the candidate ref/commit/tree/content and provenance,
dependency-library content, reviewed ledgers and verifier, installer, exact Git
archives, installed versions, and installed package content. Completion schema
3 additionally binds the named profile and axis, validation-tooling commit and
tree, profile/axis registries and resolver, repository-evidence verifier, and
resource scheduler used by the build. Schema 2 and the unsuffixed seven-package
overlay describe only the historical default profile. Construction is
serialized by one candidate-run owner. The library
and its sealed evidence are published as separate atomic, no-clobber directory
renames; failed cleanup may remove only paths still matching both that owner
and the recorded filesystem device/inode. It must never delete or repair a
raced replacement. An existing or partial destination is never rebuilt in
place: use the helper's `--verify` mode, or use a new candidate run after
removing a failed unpublished stage.

The repository, documentation, and release-benchmark entrypoints must require
that exact overlay as their first extra library and invoke the read-only
verifier before loading a bridge package or doing retained work. They must not
silently install, repair, or substitute bridge packages. An entrypoint may use
`--protected-content-preverified` only after it has itself authenticated the
exact candidate and dependency-library content in the same operation.
The release benchmark additionally requires a named profile and the exact
`.local/compat/candidate-snapshots/<commit>` detached worktree. A stage's normal
verifier requires its overlay completion's tooling commit/tree and every
retained input to match that exact stage tooling. Reuse one sealed overlay when
those inputs remain identical. If a later reviewed downstream commit changes
tests only, build a new exact overlay and rerun that package's affected rows;
retain other conclusions only with an explicit production-byte/test-only delta
record. The `bf64490` to `9e87556` composition belongs only to the superseded
`10c6a0e` release history and is not active release evidence. There is no
arbitrary older-tooling replay or migration mode. Post-freeze infrastructure
paths may include `benchmarks/`, but package source, package tests, help, and
package-facing documentation remain forbidden changes.
Whenever this contract, helper, or its hooks change, run `bash -n`, `shellcheck`,
and `scripts/environment/test-downstream-bridge-installer`; the latter is the
cheap structural self-test and is not a substitute for constructing and
verifying the overlay once for the frozen candidate.

## Test contract

The package suite must directly cover, before downstream packages are used:

- exact capsule schema/version validation and corrupt-state no-crash behavior;
- BASE, nested/shared COLLECTION, and live SHADOW graphs, including cycles;
- graph-path validation at depth greater than the 16-frame inline capacity
  under `gctorture()`, plus the disposable
  `scripts/environment/test-core-graph-roots` build after any root-carrier
  change; its test-only barrier detaches every selected `.core`, forces GC and
  pending finalizers, and requires the exact generation to survive;
- collection add rejects existing/proposed cycles and corruption before commit
  and generation-checks both admitted graphs without rejecting shared DAGs;
- derived schema stays live: after a contained set gains a parameter or is
  given tags, every containing collection and every shadow over it re-derives
  on every read surface (ids, params, tags, the raw table actives,
  `as.data.table`, check, values read and write, qunif, subset, design, and
  sampler entry points), through nested, shared, diamond, and post-clone
  topologies, with the per-edge `tag_sets`/`tag_params` flags preserved -- an
  edge added while its child was empty included. A refreshed node equals one
  freshly constructed from the same sets; a name collision created below is
  reported at the next read of the affected node and names both the changed
  set and the colliding ID; a `$tags<-` assignment on a collection or shadow
  is that node's own answer for the IDs it named and survives re-derivation
  while leaving the sets alone, and an ID it did not name stays derived; a
  value commit
  changes no containing flatten's generation; a serialized graph revalidates
  rather than re-flattens on load; a dangling dependency stays a first-class
  row through every re-derivation -- a shadow over an origin that has one is
  constructible and usable, an existing view survives an origin that gains
  one, the row survives subset/flatten/clone/serialization, and it starts
  being enforced with no explicit refresh once the parent arrives, while a
  dependency spanning a view's visible/hidden boundary stays refused in both
  directions and one between hidden parameters stays invisible; and the whole
  battery runs under `gctorture()`;
- additive subclasses and deterministic rejection/non-support of core
  overrides or private replacement;
- all five Domain kinds, two Condition kinds, and unknown-kind rejection; the
  constructor and ObjectTuneToken boundaries exercise the same sole canonical
  built-in Domain-row admission owner. Object-token Domain coverage admits only
  bounded value-producing built-in Domains, rejects unbounded `ParamUty` and
  zero-level `ParamFct` tuning ranges, and demonstrates
  opaque-leaf identity through a bounded typed Domain rather than treating
  `ParamUty` itself as a tuning range;
  standalone Condition comparison also covers names, stable ALTREP operands,
  separate snapshots under reentry, and fail-closed class/attribute/type/S4
  cases; Domain coverage includes ordinary non-ALTREP/non-S4 structural shells,
  typed special-leaf ALTREP rejection, pointer-only typed S4 special/default/
  init matching, and opaque ParamUty S4 leaves whose special membership uses
  base `identical()` without dispatch;
- values, dependencies, transformations, constraints, TuneTokens, special
  values, presence modes, sanitization, required tags, named NULL, and errors;
  checked assignment stores Domain-valid dependency-inactive entries as dormant
  values, continues to validate dormant custom/type/bounds/token inputs, and
  preserves graph-wide atomicity. Raw `$values` exposes dormant entries while
  default `$get_values()` filters them and later parent changes reactivate them.
  Tests cover transitive chains, diamonds/conjunctions, explicit-value
  precedence over defaults, `NoDefault`, TuneToken edge skipping, collection
  cross-child state, Shadow visible/hidden state, serialization/clone/equality,
  and the intentional non-invariant that `$check(ps$values)` may fail while the
  store is legal;
  TuneToken coverage includes all five exact class/content shapes, scalar-name
  normalization, serialization, explicit `$search_space(values=)`, and
  fail-closed S4/subclass/extra-field/attribute/deep-or-cyclic-metadata cases;
  Object content covers bounded value-producing built-in Domain acceptance,
  unbounded `ParamUty` and zero-level `ParamFct` Domain rejection, exact BASE
  acceptance, COLLECTION/SHADOW/additive-
  subclass rejection, safe genuine-core shell aliases without method dispatch,
  rooted generation receipts through allocation/finalizers, final no-allocation
  commit reauthentication, and sealed single-use search capabilities;
  explicit search values cover ordinary non-ALTREP and representation-only S3
  named-list containers without dispatch, plus S4/semantic-attribute rejection;
  direct checked/unchecked `$values <-` rejects an outer ALTREP before
  observation and natively canonicalizes the accepted Paradox-1 empty
  spellings (`NULL`, an ordinary attribute-free zero-length atomic/expression
  vector, or an accepted empty list container), while only
  `set_values(.values=)` exercises the one-snapshot list-ALTREP exception;
  malformed
  exact-token/Domain structure raises while ordinary value mismatch remains a
  check diagnostic;
- `assert_values` shell-policy clone/serialization/equality behavior plus
  native tag/dependency/callback admission, ownership, and reentry conflicts;
- callback order, reentry, mutation snapshots, warning/error propagation, and
  no replay;
- strict native dependency-only checking across the graph, including
  ordinary-list admission, unknown IDs, TuneToken edges, first diagnostics,
  default-aware recursive activity, store-blind point semantics, and safe cycle
  errors for cycles admitted by the unchanged BASE mutation boundary. Check
  and presence modes remain point-strict: every supplied entry must be active,
  and a satisfying default can make an absent required child newly required.
  One dangling parent gets one answer from every consumer of a collection --
  `$deps`, `$check`, `$get_values()`, a child constraint's active slice, and
  designs all resolve it through the same outward walk and the reading root's
  flat schema -- so `co$check(co$get_values())` cannot be false while the same
  subset accepts the same list, and a name no namespace supplies stays
  never-satisfiable in every one of them;
- scalar/table constraint-only checking uses the native graph/point/constraint
  kernels, validates all table rows before callbacks, calls once per row from
  one callback snapshot, passes only the active subset of each point, and
  isolates reentrant mutation to later operations;
- live collection callback bindings and detached subset/flatten/Shadow-origin
  plans select capsule callbacks, enter the shared native evaluator family, and
  preserve the specified retained-then-changed order and omission behavior.
  Authoritative collection graph sites filter once in the translated
  collection namespace and give each child only active unprefixed child
  entries before invoking schema-free carriers;
- BASE-Shadow constraint plans merge hidden/visible values natively without S3
  dispatch after the authoritative Shadow graph site filters the complete
  origin configuration, preserve leaf identity, call once, and validate the
  scalar result;
- materialize-once stable/base ALTREP under allocation/finalizers/reentry, plus
  rejection or admission of hostile state-changing custom ALTREP without
  replay or Paradox-caused crash/memory corruption; interpreted ParamSet
  `params`, non-table trafo input/result, Domain cargo, internal table,
  dimnames, and list metadata shells reject ALTREP/S4 before semantic
  observation, while the six documented public-table ingresses share one
  strict classifier. Coverage includes canonical and additive class suffixes,
  no prefix-induced ordinary-shell copy, ALTREP-snapshot canonicalization
  without S3 dispatch, malformed/reversed/non-suffix/reserved/
  duplicate class rejection, allowed data.table cache-carrier shapes and cache
  disposal, missing/S4/attributed/mismatched row names, compact positive and
  negative counts, stable integer/character ALTREP row names with one Length
  and no Elt, shared mutable names under top-shell Elt reentry, and
  row-consuming versus non-row-consuming dimension checks. The public-table
  top shell is materialized once and admitted atomic leaves and columns remain
  supported;
- detached data.table facades, documented data.frame/data.table input including
  suffix-classified top-level ALTREP shells, additive presentation classes,
  and base R's lazy duplicate, stable
  semantic ALTREP columns, and public mutation isolation;
- detached ParamSet-family equality covers complete state and distinguishes
  shared from duplicated DAG topology without traversing private R6 bindings;
- serialization and both migration APIs against CRAN 1.0.1: pure single-object
  conversion; identity-preserving recursive upgrade through shared/nested/
  cyclic ordinary containers, closures, attributes, environments, active
  binding functions, non-forced promises, and current-core payloads; traversal
  boundaries and opaque weak/external pointers; full-preflight failure; retry
  after monotonic partial commit; default and opt-in first-use gateways; exact
  owner-registry admission/rejection; both pinned `mbo_config` fixtures; and
  authentic bbotk/miesmuschel/container snapshots;
- constructor, accessor, subset/flatten, design, sampler, and hot-path
  equivalence against maintained ordinary behavior.

If a consumer exposes a gap, add the smallest package regression that would
have caught it before fixing the consumer-facing issue.

## Downstream transition worktrees

Remote writes by an agentic process are forbidden. Agents may edit, test, and
commit in local downstream worktrees, but the user must push branches and open
or submit PRs manually.

The heads below are the current local PR handoff, including the final
dormant-value adaptations and current-upstream rebases. bbotk, mlr3tuning,
mlr3mbo, and mlr3fda were rebuilt on their exact current target heads so the
proposed PRs remain small; the earlier tested commits remain preserved by
hash. Dual-version
validation is complete at the intended development scope: complete suites and
source checks for miesmuschel, complete suites for mlr3tuning, and focused
clone/spline contracts for mlr3pipelines. Fresh candidate-owned profile
authentication and compatibility execution are still required before these
heads become release evidence. The user must perform every remote publication
action; live remote inspection found every retained migration branch absent,
so each command uses `push --set-upstream`:

- bbotk `codex/public-paramsetcollection-sets` at `b992512`: public collection
  state and rooted detached native search-space snapshots remain, with the
  authenticated exact-class additive `Codomain` inspector/rebuilder
  registration without restoring private ParamSet state access;
- mlr3tuning `codex/paradox2-dormant-values-current` at `0ec4f40`: removes an
  accidental test-helper default so strict store-blind checks remain useful,
  and version-gates only the TuneToken-child expectation. Complete suites pass
  on both Paradox majors: 189 tests and 4,764/4,761 expectations, with zero
  failures, errors, or warnings and the same 16 Redis-dependent skips;
- miesmuschel `codex/paradox-paramsetshadow-bridge` at `ecd7c69`: extends the
  dual-version official `ParamSetShadow` bridge, public-state tests, and
  dual-major documentation link with the exact-class replacement
  inspector/rebuilder registration for serialized Paradox-1 Shadows and any
  owner-local cold gateways needed by old overrides. The bridge retires
  `params_unid` and `set_id`; deep comparisons remain independent of data.table
  secondary-index caches. Its Shadow dependency diagnostic gate remains
  because it covers the intentionally different official Shadow graph
  boundary, not ordinary built-in value admission. Separately, the legacy
  Paradox-1 Shadow's explicit pre-write assert remains strict, while the
  official Paradox-2 Shadow must test dormant storage and filtered
  reactivation; that regression now passes focused/full tests and source
  checks on both majors. The unavoidable load-time
  namespace rebinding is restricted to the exported generator and eleven
  historical package-owned leanification targets; relocking is registered
  before the first unlock, and this bridge exception is not a public API;
- mlr3mbo `codex/paradox2-transformless-subset` at `85dd8a5`: public
  transformation-free subset construction on Paradox 2 plus current release
  notes; publish it as at least 1.2.2;
- celecx `codex/paradox2-diagnostics` at `3a8291a`: only the independent
  cycle/dependency bridge and the exact `mlr3mbo >= 1.2.1.9000` bridge floor
  remain;
- mlr3pipelines `codex/paradox-diagnostic-compat-current` at `13610d3`: on
  current upstream, only the GraphLearner deep-clone ownership fix,
  mutation-isolation regression, and dual-version dormant spline test remain;
  those focused files and the related PICVPlus diagnostic contracts pass on
  both majors;
- mlr3fda `paradox2-snapshots` at `0df56f5`: the Paradox-1 snapshot stays
  byte-identical while the four Paradox-2 headers name the current versioned
  `.__paradox2_ParamSet__values()` gateway; diagnostic bodies are unchanged;
- mlr3 `codex/paradox2-diagnostics` at `35e30a9` and mlr3fselect
  `codex/paradox2-diagnostics` at `ae8e1d1` are wholly redundant. Close those
  PRs without replacement; there is no cleanup commit to publish.

Merge and release the six CRAN reverse-dependency adaptations—bbotk,
mlr3tuning, miesmuschel, mlr3mbo, mlr3pipelines, and mlr3fda—before Paradox 2,
so CRAN and ordinary installations select compatible revisions. Release
mlr3mbo as at least 1.2.2 before publishing the GitHub-only celecx bridge. The
final exact source-package gate covers all seven retained PR heads; the broad
repository suite is not a substitute for those package build/check rows.

The historical exact Paradox-2 source-package check was
`.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-checks-release-refresh-20260720-paradox2`.
It has five rows, zero failures, and five final `Status: OK` results. Its
completion is independently verified; its completion TSV, results, manifest,
and seal-file SHA-256 values are
`a43064cd48b78fd433b4a4995a2da9cfad422da7b3b66f53391de05fbd289179`,
`f443e9ace27450057e4c8fcb6f9d93da85d595028abfff133272cc32f11a364c`,
`aa6d0ad380f0453db8c87888bdd5b7d18d54bc2698d21b9a4e67ce1040bcd731`,
and `7de4719e3fa0016fb05d804aa240dea34a0e5de76763c7f1d5d885e2e581acf1`.
The pre-dormant Paradox-1 five-package conclusion is an explicit composition.
The
`migration-release-final-p1-cdcc8e6-221c95e-r2` stage passed the exact final
bbotk, mlr3mbo, and celecx heads plus mlr3fda base `8f5a3df`; final mlr3fda
`c1cdad5` differs from that base only in four Paradox-2 snapshot headers, so it
does not alter the selected Paradox-1 tests or runtime source. The final
miesmuschel row passed in
`migration-release-final-p1-cdcc8e6-2771f5d-r3`. The r2 donor stage as a whole
is `completed_with_failures` because it exercised the superseded miesmuschel
head; never describe that whole stage or the mlr3fda final head as an exact
five-head Paradox-1 check. The historical broad Paradox-2 corpus used two
workers over 14 waves: 20 of 28 exact repositories passed, and the remaining
eight were reviewed non-Paradox or environmental exclusions. The replacement
source must rerun the applicable downstream waves rather than inherit that
conclusion.

Before the legacy object-graph migration reopened package and downstream
source, the six affected consumer selections passed the same 2,022
expectations with zero failures, errors, warnings, or skips on both Paradox
1.0.1.9000 and the then-focused Paradox 2 DSO. That historical evidence is
retained under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.
Those historical follow-up commits record only the then-tested cleanup trees.
The then-reopened migration payload has separate focused evidence under
`.local/checks/downstream-upgrader-latest-20260724/`: bbotk's exact Codomain
upgrader tests pass 8/8, miesmuschel's Shadow suite passes 62/62, and authentic
Paradox-1 fixtures pass every historical default/opt-in target plus explicit
migration, clone, and RDS-roundtrip checks. This is affected-bridge evidence,
not a transfer of the historical 2,022-per-axis full consumer conclusion.
Never push, open or close a remote PR, publish a tag, or otherwise alter remote
state yourself; the user performs every remote write. Exact commands and PR
text live in `compat/downstream-pr-handoff.md`.

The maintained priority consumers include bbotk, miesmuschel, mlr3mbo,
ConfigSpace, celecx, mlr3, mlr3tuning, mlr3pipelines, and active mlr-org book,
gallery, website, and serialized configuration workloads. Very old repositories
that do not use current Paradox are evidence inventory, not release blockers.

## Release convergence

The top-level verification coordinator is `scripts/verify`, with reviewed task
and profile data in `verification/tasks.json` and its normative operator/design
contract in `verification/README.md`. It schedules the existing retained gate
drivers; it does not reimplement their semantic verifiers. Run
`scripts/verify doctor` and `scripts/verify plan --profile focused` before a
long development run. `scripts/verify self-test` is the cheap, daemon-free
controller regression.

Hard parallel task execution has two supported forms. Per-worker
Podman/Docker containment requires proof of the requested memory, no-swap,
PID, and CPU cgroup ceilings plus an actual OOM-killed sacrificial worker.
Aggregate containment requires the controller and every local rootless Podman
payload to inherit one dedicated root-created system service with authenticated
finite memory/CPU/PID ceilings, no swap, systemd kill/accounting properties,
and stable cgroup/event state. Both probes execute as the eventual worker UID
and prove the read-only checkout plus nested writable-bind pattern; workers
disable SELinux labels explicitly rather than depending on host relabel
defaults. Never infer containment from accepted flags or an environment
marker.

This host's rootless Podman/cgroup-v1 setup cannot enforce per-worker limits.
After the manually reviewed root-owned launcher from `verification/systemd/`
is installed, ordinary `scripts/verify doctor|plan|run` commands enter its
aggregate service by default and can schedule independent coarse tasks in
parallel. Repository code never runs as root: systemd changes to the configured
UID/GID first. The root surface is deliberately identity and rails only:
the worker-image pin (`.local/verify/worker-image.pin`, exported by
`scripts/verify` as `PARADOX_VERIFY_WORKER_IMAGE`) and the envelope target
(`.local/verify/systemd-tunables.conf`, `memory_target_mib` at most the
root-pinned `memory_max_mib` ceiling) are user-owned files the launcher
honours strictly within its root-approved bounds, so image rebuilds and
capacity tuning never need root. Aggregate Podman workers use `--cgroups=disabled
--cgroupns=host` and omit ineffective individual resource flags. Docker/remote
engines are ineligible because their daemon can escape the service.
`PARADOX_VERIFY_SYSTEMD_DEFAULT=off` is the machine-local escape hatch;
`--containment worker|aggregate` requires a specific mechanism.
`--best-effort` remains an explicit serial development-only
RLIMIT/RSS-watchdog fallback and cannot produce release evidence.
All shipped local tasks currently inherit the repository's Linux x86-64
toolchain constraint. The planner must reject another host explicitly; macOS
ARM64 and Windows x86-64 remain hosted-workflow evidence until a task supplies
a self-contained platform toolchain image and overrides that constraint.

The coordinator preserves at least 12 GiB/25% live memory and 8 GiB free disk
outside its work for Codex, the OS, and unrelated processes, and keeps a
3-GiB engine-overhead-and-margin allowance below the aggregate ceiling so
cooperative estimation error does not touch the fatal limit tripwire. It
continuously
admits ready tasks by summed CPU, memory, PID, and scratch reservations;
tasks start at reviewed CPU/RAM minima and receive spare capacity up to their
reviewed ceilings in information order. Make receives only a task's actual
allocation while BLAS/OpenMP/testthat nested parallelism stays one by
default; the full native check re-enables parallel testthat for the Paradox
suite inside its own allocation through an explicit retained light-test
admission. One
per-user machine execution lock prevents independent controllers—even from
different checkouts—from double-spending that aggregate budget. Cgroup-aware
live memory, disk, and PID availability are refreshed before new waves and at
a bounded cadence; transient outside pressure waits with a bounded timeout.
Static fit reserves the configured fraction of physical/parent-cgroup
capacity, while live admission recomputes that fraction from current
availability. Startup pressure therefore neither becomes a permanent ceiling
nor withholds a capacity-sized reserve on a busy large machine.
Under aggregate containment, task allocations are scheduler reservations
rather than individual cgroup limits. Admission uses the smaller of aggregate
headroom and global available memory outside the protected reserve, without
subtracting that reserve twice. `verify-task-entry` and `resource-jobs` must
cap nested work by the assigned CPU/RAM envelope. Schema-2 resource reports
carry their containment mode; the aggregate policy — 2-GiB consumer rows
capped at eight, 1-GiB light-test jobs, 4-GiB intra-envelope reserve floor —
applies only after the process proves it sits inside the dedicated
`/system.slice/paradox-verify-aggregate-*` service leaf, while the live and
direct gates retain the conservative 8-GiB estimate, 16-GiB floor, and
four-row cap, and every report validator re-derives the policy for the
recorded mode. A finite `operator_max_jobs` is a valid lowering-only external
ceiling and must never exceed the independently recomputed raw ceiling. Any
aggregate memory/PID
event-counter increase or
cgroup/systemd/path/limit change is a fatal infrastructure failure that
terminates every sibling. A real payload proves worker inheritance at startup;
runtime monitoring then authenticates the controller cgroup and systemd unit.
The aggregate `TasksMax` is a host-protection boundary, not a per-worker PID
limit; complete saturation may defer engine cleanup until
`KillMode=control-group` tears down the transient service.
Permanent task incompatibility is decided against separate physical/cgroup
memory, filesystem, PID, CPU, platform, and architecture ceilings—not against
momentary free resources. A scheduler-created capacity/dependency/policy block
makes the profile incomplete but never masquerades as an executed failure or
cancels an otherwise independent branch, even under adaptive/fail-fast policy.
The checkout and toolchain are
read-only in workers; only reviewed task paths are writable, with task-private
temporary/runtime state and protected downstream libraries remounted
read-only. Retry HOME/tmp/runtime state is attempt-private. The default
adaptive policy completes independent peers in the
current phase, blocks failed descendants and later expensive phases, and
aborts globally only for fatal provenance, cache, containment, host pressure,
container-cleanup, or source-integrity failures.

The prepared worker is intentionally a minimal Linux userland, but its
reviewed command contract includes `rg`, GNU `timeout`, util-linux `setsid`,
and procps. Git operations against the read-only checkout must disable
optional locks, lazy fetching, prompts, filesystem monitors, and the untracked
cache; synthetic Git mutations belong in a scratch repository, never the real
`.git`. Runtime-matrix stages redirect their package library, temp, cache, and
runtime state to the attempt-private writable mount only after authenticating
the coordinator's prefix receipt handoff. Pinned source archives remain
read-only and are inspected below process-private temporary state. Runtime
and compiler-tool semantic identities are hashed under an explicit C locale;
coordinators and workers must use the same stream and locale rather than
inheriting machine-specific localized `--version` output. Runtime
prefix receipts force the C locale before enumerating and sorting members, and
the bootstrap's explicit-package lock/installed-inventory comparisons use the
same byte order. Their bytes and authentication result therefore cannot vary
with host or worker collation. Receipt verification writes its comparison only
below the caller's process-private `TMPDIR`, which must be a plain directory
outside the authenticated tree; retained manifests and their read-only parent
directories never need write access. The helper owns a private `077` umask, so
hostile caller settings cannot make its scratch files unreadable or leak their
contents.

The worker also maps ordinary activation's generic state and the runtime
matrix's mutable development library, temporary, cache, and runtime subtrees
to attempt-private directories. Prefixes, prefix/dependency receipts, and
sealed dependency libraries are deliberately not in that list and remain
read-only. This lets synthetic activation checks execute without granting
write access to retained compatibility evidence.

Child output and GNU-timeout diagnostics must use separate descriptors.
Deadline evidence accepts the exact full-path or basename diagnostic emitted
by supported GNU coreutils versions, and classifies status 124/137 as a
timeout only with the matching singleton TERM/KILL evidence; unknown,
duplicate, or impossible supervisor output fails closed. Plans bind and
reauthenticate the timeout, wrapper, shell, and worker bytes before launch and
after each batch. Reverse-worker process-group cleanup treats a group as
quiescent only when `kill -0` finds it and a successful procps snapshot proves
that every remaining member is a zombie. This permits completion under a
non-reaping container PID 1 without weakening cleanup for any executable
descendant.

Exact-key semantic cache hits below `.local/verify` accelerate development.
They are never release-evidence transfers: release profiles disable generic
result reuse/publication and retain the existing source-bound gate receipts.
Task identity includes reviewed OS/architecture, toolchain content,
hard-backend/cgroup generation, and immutable worker-image content. Kernel release, engine
version/path/storage, live capacity, and local aliases for that image belong
only to the per-invocation receipt after containment is freshly proved. The
non-release best-effort fallback remains keyed to its exact host/Python.
The activated repository R-library tree is always semantic, including for
release/prepared profiles that disable generic cache publication, because
coordinator resume can still reuse their completed rows.
Changed-file impact rules affect development selection and priority only.
Downstream runs consume one authenticated candidate-context JSON below
`.local`; never hand-template divergent ref/commit/tree/source/library/content
values across tasks. Every retried coarse gate gets an attempt-specific child
run ID. Execution results are immutable per attempt, every coordinator resume
adds a current host/engine invocation receipt, and a retained success is
accepted only when its latest view, immutable attempt, exact log, and original
invocation receipt authenticate one another. Tasks opting into resume
revalidation invalidate their complete descendant closure as well. Only
genuine execution failures—not capacity/dependency blocks—raise future
scheduling priority. A
compatibility gate whose public stage is candidate-wide uses that ID
for private work and verifies an already-published stage on resume. A release
`source_ref` must resolve to the exact clean HEAD commit/tree used by every
other release-foundation gate.

Prepared compatibility has four explicit profiles. `prepared-downstream`
remains axis-neutral. `prepared-reverse` and `prepared-documentation` run the
real Paradox-2-only gates, while `prepared-release-compat` runs downstream,
priority-zero/one reverse dependencies, and all-scope documentation as one
keep-going DAG. The real gates source the authenticated
`scripts/activate-compat-system` layer inside their workers; ordinary native,
API, runtime, and differential tasks must not inherit it. Reverse execution
uses a stable run ID and an explicit controller-owned attempt number: attempt
one requires a fresh reservation, while a retry resumes only a
candidate/options/harness-bound initialized marker. The completed reverse task
is revalidated rather than blindly trusted on a later coordinator resume.
Documentation uses an
attempt-specific ID because it has no resume mode. Both receive a narrowly
reserved exact writable output directory rather than a writable historical
evidence parent. Candidate contexts keep consumer extras separate from
documentation-only libraries so the latter cannot alter repository dependency
resolution; reverse-only profiles neither require nor authenticate those
unused layers. All documentation-essential rows run before any advisory full
render, and its task reserves one CPU because the retained driver is serial.
Their additional compatibility-system, TinyTeX, and documentation contracts
remain Linux x86-64 as well.

The aggregate-contained phase-30 reservations are intentionally 8--16 GiB for
reverse dependencies, 8--16 GiB for the repository corpus, and 4--8 GiB for
each serial documentation/focused branch, with reverse and corpus CPU ranges
of 4--16 so a branch alone can widen to eight concurrent rows. Their phase
minima total 24 GiB of RAM,
6,144 cooperative PIDs, and 8 GiB of scratch, permitting all four independent
branches to run together beneath the hard systemd aggregate and protected-disk
budgets. These values are admission weights, not measured peaks; no retained
evidence supported the old 26--51-GiB weights. Keep the direct
`resource-jobs consumer` reserve unchanged. The completed `4c4cb53`
diagnostic run empirically peaked at 10,670 MiB and had zero memory/PID events;
its one-row corpus waves exposed the assigned-envelope 8-GiB divisor as a
throughput bottleneck, motivating first the 4-GiB and now the
containment-aware 2-GiB cooperative row weight. Both consumer runners refill
within a wave (up to three planned rows per admitted worker, admitted count
concurrent, freed slots start the next row) and order their queues
heaviest-first from the measured `4c4cb53` durations embedded as scheduling
hints; the documentation gate stays deliberately serial because its retained
labels embed execution order and it is hidden behind the corpus/reverse
branches. Supported runtime-matrix stages parallelize across minor versions
and retain exact serial testthat execution inside every stage, so the one-CPU
stage admissions remain truthful. An aggregate memory/PID event
or
protected-disk pressure invalidates only the verification unit and calls for a
replacement run with the offending reservation raised;
`verification/README.md` enumerates the targeted loosening knobs.

The coordinator self-test is the routine harness-change gate. It must cover
strict container exited-state/exit agreement, fail-closed stale cleanup,
attempt/interrupt cleanup, adaptive minima/ceilings, aggregate PID admission,
cgroup-aware live memory, optional future package inputs, locking, cache
tamper rejection, immutable attempt/invocation receipts, static-versus-live
capacity, aggregate v1/v2 and systemd proof, non-duplicated reserve
accounting, event-counter failure, actual parallel aggregate workers, platform
constraints, reserved-output recovery, and source reauthentication. Harness
work alone does not
authorize launching the multi-hour real R, reverse, documentation, or memory
suites.

`release-core` intentionally excludes the combined memory driver. The current
`scripts/memory-check --mode all` launches pinned rchk Podman from inside the
driver and applies its own 16-GiB reserve; putting it in the generic worker
would require unsupported nested Podman and duplicate the reserve. Run that
existing source-bound gate directly after the release foundation, using the
`native-release` task result's `child_run_id`. A future integration must make
the rchk image the outer worker and must not claim nominal containment before
that split is implemented and tested.

Only freeze a candidate after code, contract tests, docs, downstream bridges,
and profiling converge. The active status and exact evidence IDs belong in
`design/release-2.0.0.md`; never encode stale pass counts in scripts as a proxy
for test discovery.

Run release gates against one clean immutable full ref, broadly in this order:

1. strict GCC/Clang C99, registered-routine/probe audit, ASan/UBSan, complete
   package suite, and clean `R CMD check --as-cran`, plus the separate explicit
   `--use-C23` installation and complete native-probe gate under
   repository-local GCC >= 15 and recent Clang;
2. actual R 3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2, with
   current R 4.6.1 complete execution owned by the full native lane, plus
   compilation
   against all seven pinned R 3.6.0--4.6.1 header axes; the exact
   raw-attribute/old-R-closure-snapshot/stored-binding/promise exception ledger,
   raw-token/version-gated DSO audit, and option-access symbol policy; the
   authenticated R 3.6 complete-test closure and separate exact declared-floor
   smoke with `R_DEFAULT_PACKAGES` isolated; and the full-only sealed
   R 4.0.5-to-R 3.6.3 serialization handoff;
3. upstream differential with reviewed intentional Paradox-2 deltas;
4. all exact reviewed downstream bridge heads, then priority-zero/one reverse
   and GitHub consumers and documentation workloads;
5. GCT, instrumented-R Valgrind, bounded rchk, adversarial corruption, and
   direct coverage of every registered routine/hazard family;
6. examples, vignettes, manuals, pkgdown/book/gallery/website and legacy
   upgrade workloads;
7. current Windows x86-64, exact Windows x86-64 R 3.6.3/Rtools35, and real
   macOS Apple-silicon ARM64 CI for the exact ref;
8. paired release benchmarks on an otherwise idle host.

Primary retained drivers include `scripts/native-check`,
`scripts/check-r-api-compatibility`, `scripts/test-runtime-matrix`,
`scripts/memory-check`, the `compat/` runners/verifiers, and
`benchmarks/release`. Read their `--help` before use; do not copy historical
run IDs or expected counts. Expensive analyzer and consumer gates run only
after cheaper package/bridge gates are green.

Never accept a green GitHub matrix label as portability evidence by itself.
Each platform row must reject a nonzero `rcmdcheck` child status and require one
sole final `Status: OK`, except for the separate R 3.6.3/Rtools35 row. R 3.6's
`_R_CHECK_DEPENDS_ONLY_` does not suppress its dependency-inventory NOTE, so
that runtime-import-only row instead requires child exit zero, exactly one
missing-Suggests NOTE naming the complete expected nine-package set, no other
NOTE/WARNING/ERROR/halt, and one sole final `Status: 1 NOTE`. Current Windows
and macOS retain exact `Status: OK`. An always-run completion job must then
reject the complete matrix aggregate unless it is exactly `success`. The offline verifier
independently requires four successful REST jobs (macOS ARM64, current Windows
x86-64, exact R 3.6.3/Rtools35 Windows x86-64, and completion), all three exact
platform artifacts, their check logs, and frozen candidate provenance. A
release-only direct-child companion changes only the workflow to pin and check
out the immutable candidate and to reduce the ordinary matrix; it must retain
the separate old-Windows job and both completion layers.
Create that workflow with
`scripts/environment/render-portability-release-workflow.R`, never by a
one-job hand edit: the renderer pins both checkouts credential-free, inserts
both exact commit assertions, and runs the release structural validator before
publishing an absent output path.

Profile representative constructor, `check`/`check_dt`/`check_dependencies`,
`has_deps`, values, domains/params/dependencies, subset/collection, live Shadow
constraint and read/write paths, design, and sampler workloads. Optimize only
measured hot paths, retain portable scalar code unless a portable
architecture-neutral improvement is proven, and rerun affected correctness
tests after every optimization. Freeze performance changes before the final
memory/portability matrix.

The Paradox-1 comparison has exactly seven narrowly ledgered integrity-read
rows. `shadow_values_live` uses `integrity-shadow-read` (3.25 median/3.50 q75).
The three synthetic `collection_values_{plain,rich,nested}` rows and the real
`$values` rows for `mies_mutator_maybe`, `mies_optimizer`, and
`mlr3pipelines_graph` use `integrity-collection-read` (2.75/3.00). They perform
generation/signature or complete capsule-DAG admission that the cached legacy
surface did not. The exception is timing-only: both tiers keep the `hot` 1.25
ratio and 16-KiB minimum allocation thresholds. Consumer `$params`,
`get_values_unchecked`, filtered getters, mutation/constraint/domain paths, and
all other real consumer operations keep their ordinary `hot`/`standard` tiers.
Never widen a global tier or add another integrity row without retained profile
evidence and explicit design review. Treat non-pass integrity rows as required
raw-distribution review, and normally retire these contract-reset tiers once
Paradox 2 is the authenticated baseline.

## Historical `dbbdcc1` candidate and gate status

The previous frozen candidate was
`refs/paradox-release/candidate-20260727T152133Z` at
`dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea`, tree
`b60b75e3923cdcb49f1ef2fb0f9b386d5cac291d`. Its
`release-candidate-dbbdcc1` coordinator run passed all eight `release-core`
rows. The coordinator completion, JSON summary, and TSV summary SHA-256 values
are respectively
`511da5ccf2ef4e779db97bc6e79ab458f861148b4841625132e613a2b285d090`,
`fe9b41d7552d73e2d342cfa1f3c666cb672508d4d29aef6d3c7dc063ee380e44`,
and
`507160680c2a861ece452f8a23f12caceb590897afec00a38a5757229709b522`.

The native child
`release-candidate-dbbdcc1-native-release-a001` passed its functional work,
but is not a replayable source donor: for example,
`scripts/environment/create-offline-check-repository.R` is mode `0664` in
`source-manifest.tsv` and `0644` in `source-tree.tsv`. The first memory attempt
then failed closed before package load because the retained validator's
relocated offline-repository helper was absent. Keep both directories
immutable. Their package results are informative, but neither directory
supplies combined-memory acceptance.

The replacement `release-candidate-dbbdcc1-native-replay-r2` coordinator and
its child
`release-candidate-dbbdcc1-native-replay-r2-native-release-a001` passed under
the six-file repair. The child also passes direct independent source-run
validation: its source-manifest, source-tree, modes-tree, completion-content,
and result SHA-256 values are respectively
`d10b24eee410164ca28b3f456ddebf4a151d9f884ce9e648adde3f7efbdb2cee`,
`821e28ab6db88a0125e4dbea167730235096bc2f699cb33a94d0c85cc2594a3a`,
`6550f985a6bfc766618a95719ec638ec02988021e118c7b7f296a71df0c95b95`,
`522490b19cf52e6a18b955207d797463f60b51b6c1a85537423fc6088b21fc36`,
and
`d89ff720ea1cda8be1f82538a1e92a245dd901fd7e31990ed2a37e97c62130b0`.
That child supplied the successful rchk discovery. The regenerated policy and
strengthened GCT probe then required one fresh static/focused native donor; the
subsequent combined-memory run recorded below completed that step. For that
historical source, repeating full native/package acceptance would have added no
information.

The repair changed only `scripts/memory-check` and five
`scripts/environment/` harness files. Its historical transfer required proof
that the repair commit was package-facing-source identical to the candidate.
That path-level proof is complete for `50a8593`; generated package
archives are not claimed byte-identical because R injects nondeterministic
metadata and vignette output. At that point the remaining applicable
downstream, documentation, benchmark, and hosted portability gates kept the
release pending.

The checked-in active `paradox2` row in `compat/paradox-evidence-axes.tsv`
still names this historical `dbbdcc1` ref/commit/tree. Its historical focused
structural profile fixture passed, but the row must be repointed to the new
immutable candidate before constructing a new candidate-run-owned
`release-refresh-20260720` overlay. All retained overlays and consumer results
owned by earlier Paradox-2 candidates remain historical.
Post-freeze tooling admission explicitly includes the package-excluded
`verification/` root so that reviewed coordinator scheduling changes can drive
that overlay; package-facing paths remain forbidden, and the tooling checkout
must be clean at one recorded commit.

Exact combined-memory run `release-candidate-dbbdcc1-memory-r3` passed GCT,
Valgrind, and rchk and passes the independent offline validator. Its
completion-content and result SHA-256 values are
`b903b0ba50f50d68704e947a725b47f0569bf395e5304f6d86143c580c560e44`
and
`d260a45c237654509a21e5579e392eb6278afec0c89c43863f30819cbbd9409e`;
the validator SHA-256 is
`0d922790f73550d6a70bebd83008ce099a86f77fe0af18758f2332e97374b3b6`.

Prepared compatibility attempt
`release-candidate-dbbdcc1-prepared-compat-91ca588-r1` stopped after two cheap
worker-only harness failures and did not start a real compatibility gate.
Bullseye procps-ng parsed an unseparated negative process-group operand as a
signal option and killed group zero; all reverse group operands now use `--`.
The initial workaround removed the UTF-8 declaration from the ASCII-only
fixture. The subsequent real `4c4cb53` source checks proved that diagnosis
incomplete: production `env -i` rows had discarded the worker's `C.UTF-8`
locale, so every ordinary UTF-8 downstream package made R 4.6 request the
unavailable `en_US.UTF-8`. The final correction restores the fixture's common
UTF-8 boundary and pins `LC_ALL`, `LANG`, `LANGUAGE`, and `TZ` in both fixture
and production rows; real downstream WARNING rejection remains unchanged.
Keep the failed coordinator and its already-published overlay immutable. After
committing these package-excluded repairs, prepare a fresh candidate-run-owned
overlay and a new coordinator rather than resuming, overwriting, or relabeling
the old evidence.

The immutable replacement diagnostic coordinator
`release-candidate-dbbdcc1-prepared-compat-4c4cb53-r1` ran all unblocked work
for 13,810.8 seconds. All six harness rows, the downstream overlay, and the
17-workload documentation gate passed. Its focused five-package source checks
all built and returned check status zero but were strictly rejected solely for
the common locale WARNING above. Reverse preflight was rejected before package
execution solely because its valid coordinator-assigned finite
`operator_max_jobs` met an obsolete reverse-only prohibition. The complete
28-repository corpus retained 17 passes and 11 honest failures over 28 one-row
waves: only `mlr3tuning` and `mlr3pipelines` expose new dormant-value
expectation drift; the other nine are the previously scoped optional-system,
upstream-fixture, dependency, or timeout classes. Completion, rows, manifest,
and seal SHA-256 values are respectively
`7e4cb24e4e5141c5ae2dcfa36f67c406972ac75b60a98d5b14d17a7fa5a12fd0`,
`9c7cbcecb8faa7419551a0d84d9f6dd138c2564c719f951ceaba093ebea0cee0`,
`efb8f8a3cfa801c52ce17f9fa78ef2c2a7b462d05f9c091efaf0dd88a89b8d26`,
and
`97cc58b471812e74bd89206004677dd31be05017d478fff7d7592062884a4785`.
The narrow correction passed shell syntax, the direct resource scheduler
fixture (including aggregate two-row and conservative direct one-row cases),
the complete reverse reserved-output/self-test, downstream bridge/profile
fixtures with a real UTF-8 `R CMD build`/`check`, all 66 controller unit tests,
and the verification-economy contract test. No broad compatibility task was
repeated before downstream heads and manifests were refreshed.

The final repair passed shell syntax checks, R parsing of both snapshot
helpers, and the complete activated validation-hardening suite in about 226
seconds. The latter covers the independent memory validator, the 21-row exact
harness inventory, missing/tampered relocated helpers, and restrictive-`0077`
snapshot/replay fixtures preserving `0664` and `0775`. Focused actual R 3.6.3
and current-R probes also restored exact `0664` with
`Sys.chmod(..., use_umask = FALSE)`.

## Last sealed candidate before final cleanup (historical)

The last sealed package candidate is
`refs/paradox-release/candidate-20260724T105215Z`, commit
`8797f1163fe612cb01d1facf517834d3f516a697`, tree
`81e6f901266754b97a0906f88a472bf04795f13c`, candidate content SHA-256
`ced2390bc756b01805e7bdcf32fdb4a6ff2c1bd010dd3d576194d939e491f644`.
It contains the informative native diagnostics, recursive legacy object-graph
migration, authenticated native gateway contexts and generation barriers, the
indexed-root `schedule_vector()` repair, and the self-returning ALTREP duplicate
regression.
`AGENTS.md`, `design/`, and `environment/` are excluded from the package build,
and the exact candidate-to-`fc92edd` diff contained no package-facing source,
so that post-freeze validation and release-ledger work did not alter the sealed
source. The later 2026-07-25 cleanup does alter package-facing source and
invalidates this transfer. This was package-facing-source identity, not an
unproved complete package-payload byte identity. Do not infer it for any other
path: authenticate the exact diff before transferring a source-bound result.

The exact candidate's bcheck/maacheck/fficheck reports and refreshed policy are
recorded in the Authority section above. The complete package suite, CRAN-style
and depends-only checks, strict compilers, analyzers, sanitizers, R API/header
matrix, real R 4.3.3/4.5.2 runtime matrix, retained combined memory run,
normalized differential, both downstream axes, 20-of-28 scoped broad corpus,
mandatory documentation, five-package exact Paradox-2 source check, and sealed
77-row benchmark were green or accepted under their recorded reviewed
exclusion policies for that payload. They are historical development evidence
for the cleanup source, not completed release gates. A replacement candidate
needs the applicable local and hosted gates plus the user-performed downstream
branch/PR, tag, and workflow publication handoff.

## Historical candidates

The rejected compatibility-first candidate ref
`refs/paradox-release/candidate-20260717T083921Z` at
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa` was once green under a different
contract. The superseded contract-first candidate
`refs/paradox-release/candidate-20260719T104709Z` at
`5e40d2ba9b9ce3a75615b90420fb9bc298c19ecf` and its stale portability companion
`refs/paradox-release/portability-harness-268ccff` at
`268ccff27ee68bfea71c6370b0616a9c969a94cf` predate the final subset,
dependency, downstream, and `$has_deps` work. Their evidence remains below
ignored `.local/` paths and in Git history, but none is a baseline for current
source completeness, compatibility policy, routine inventory, test counts, or
release readiness.

The serialized-migration candidate
`refs/paradox-release/candidate-20260724T011004Z` at `348539ad` was superseded
after adversarial review tightened native shell authentication, read-only
Shadow receipts, joint graph validation, and postorder commit barriers.
Candidate `a362365` contained that hardening but failed the strict enum
conversion build; `2bcce2b` fixed the compiler warning but retained a
cppcheck-only conservative null-flow diagnostic. The later `8797f11`
candidate makes the admitted graph guard explicit to the analyzer. None of
these rejected refs, nor the now-historical `8797f11` gates, is release
evidence for the reopened cleanup payload.

The later candidate `refs/paradox-release/candidate-20260719T150831Z` at
`612345ceb403c70a0ea6c1149c367c6782d9870b` passed its native, R-API,
runtime, differential, and memory gates, but the final release benchmark
correctly rejected it before candidate timing: R 4.6 had represented the
benchmark's ordinary wide data.frame as a top-level base `wrap_list` ALTREP,
and `check_dt()` rejected that common representation. Its unsealed benchmark
is defect evidence, not a performance result. The replacement design
materializes admitted public-table ALTREP shells once at the six documented
ingresses and does not weaken general structural admission.

The replacement candidate
`refs/paradox-release/candidate-20260719T175053Z` at
`4f28327f894fe17324410a45cabaf7221e6eca45` passed its exact-byte native,
R-API, runtime, differential, and downstream-bridge gates. Its memory run was
stopped and left unsealed when adversarial review found that the shared table
boundary still admitted missing, S4, or dimension-mismatched `row.names` and
could retain a shared mutable names vector across a hostile top-shell Elt
callback using `data.table::setnames()`. The candidate is superseded. None of
its package-byte evidence transfers to a replacement candidate, including the
completed gates; the partial memory directory is diagnostic evidence only.

The subsequent candidate
`refs/paradox-release/candidate-20260719T194741Z` at
`60704fcc6a899c508f5adffbec35bb61723d3704` reached package installation and
downstream-bridge construction, but its exact R-API gate rejected two direct
assignments of `Rf_isObject()` to `int` under the pinned R 4.3 and 4.4 headers:
those headers expose an `Rboolean` return type and strict Clang correctly
reported the implicit signedness conversion. The in-flight native and runtime
runs were stopped immediately. Their partial directories, the installed
candidate, and the bridge overlay are diagnostic only and transfer no release
conclusion. The replacement normalizes both predicate results explicitly and
must rerun every package-byte-bound gate.

The next candidate
`refs/paradox-release/candidate-20260719T200549Z` at
`70c6d728785464c98ffc8c658f20c1937467a593` passed the exact old-header API
gate and installed the candidate and bridge overlay. Its R 4.3 runtime stage
then exposed a test-fixture portability error: unlike R 4.6, R 4.3 did not
choose a top-level base `wrap_list` ALTREP for the wide data.table after an
attribute-only duplicate, so an internal materializer test incorrectly
expected ALTREP-only cache removal from an unchanged ordinary table. This is
not grounds for copying every ordinary table at a hot ingress. The native and
runtime runs were stopped; none of this candidate's package-byte evidence
transfers. Tests that require a top-level ALTREP must use the registered native
fixture, while the base wrapper remains realistic conditional coverage when a
runtime selects that optimization.

Candidate `refs/paradox-release/candidate-20260719T202524Z`, commit
`e3741ab1d3cb8a5f3e7f357af6b7ab90c6e50fb7`, passed its exact native, R-API,
and runtime gates, then failed the maintained repository sweep in
mlr3fselect. `mlr3::BenchmarkResult$aggregate()` produces the ordinary
additive class vector `c("bmr_aggregate", "data.table", "data.frame")`, which
data.table preserves through a narrow subset before bbotk calls
`ParamSet$assert_dt()`. The exact-class classifier rejected that
non-dispatching representation even though Paradox 1 accepted it. The
replacement contract admits well-formed additive leading classes without
dispatching them, and canonicalizes them only while taking an already-required
ALTREP snapshot. All evidence bound to `e3741ab` is superseded.

Candidate `refs/paradox-release/candidate-20260720T022410Z`, commit
`a4617ca769ff5373a7da16c7ce333e36c68fd9b2`, fixed that public-table boundary
and passed its exact R-API, native, runtime, differential, focused downstream,
and bounded performance gates. Gctorture and all four Valgrind diagnostic
inventories were also clean. Its combined memory run remained unsealed because
the source-bound rchk policy still named the pre-classifier bcheck/fficheck
hashes and 69-routine count. The actual bounded report retained the same 77
reviewed Function blocks, 196 UP diagnostics, and 13 PB diagnostics, while the
classifier's three diagnostic registrations raised the routine count to 72.
The refreshed policy binds that exact 782-function/28,140-state report. The
next candidate incorporates the policy and final validation tooling without
changing any package-facing file; do not promote the partial `a4617ca` memory
directory or its earlier tooling-bound overlays as final evidence.

The replacement is frozen at
`refs/paradox-release/candidate-20260720T053518Z`, commit
`10c6a0e65910206c8face91dac6c3dd1115e0bed`, tree
`a205205194f0bc62114106504853721f678fa340`. The immediate child validation
commit changes only the Paradox evidence-axis registry, its exact fixture, and
this ledger; all candidate execution must continue to authenticate the managed
detached `10c6a0e` source rather than the validation worktree. Evidence bound
to earlier candidate commits or tooling identities remains historical except
for the exact, explicitly scoped byte-identity proof below.

The sealed equivalence stage
`.local/checks/package-equivalence-a461-10c6` is the narrow exception. It
authenticates 486 tracked files per ref, 467 identical blobs, and 19 changed
paths, all excluded by the candidate's exact `.Rbuildignore`. Clean builds have
217 payload files each; the only raw difference is R's generated `Packaged:`
line in `DESCRIPTION`. Their normalized manifest is
`e6e767b8fa3cd1a9273d62039c208d7c3ae1aea0b12d7446560297d4750dcc3e`
and the retained evidence manifest is
`e558864a318a465edf058013f743c6ae386b34bcd7894037a02c66938a986fb3`.
Consequently the exact `a4617ca` R-API, full native/sanitizer/test/check, R 4.3.3
and 4.5.2 runtime, and focused-consumer conclusions apply to the identical
installed/package-facing `10c6a0e` payload. This does not promote the unsealed
`a4617ca` memory directory or any older validation-tooling overlay. Exact
`10c6a0e` source-static, memory, differential, documentation, downstream, and
benchmark stages were retained separately.

The first full-check stage under tooling `3f02899` correctly built source
tarballs before checking them, but incorrectly passed `--no-build-vignettes`
to `R CMD build`. Three changed heads passed; celecx's source contains a
vignette and therefore produced two missing-`inst/doc` warnings even though its
tests and vignette code passed. Full-check subjects now use ordinary serial
`R CMD build --no-manual` so built vignettes are present, followed by
`R CMD check --no-manual --no-build-vignettes` to inspect rather than rebuild
them. The sealed failed stage remains diagnostic. A fresh run and overlay after
this harness-only repair passed all four selected source-package checks; no
earlier result was relabeled.

The first all-scope documentation execution under tooling `4d4b637` completed
all 17 workloads with every mandatory row passing, but its process correctly
failed instead of overwriting `metadata/evidence-manifest.tsv`. The harness had
retained the downstream overlay's own manifest and seal under those reserved
basenames before trying to create the documentation stage's manifest and seal.
Retain those four overlay inputs under explicit `downstream-bridge-*` names;
reserve `evidence-manifest.tsv` and `completion.seal` solely for the enclosing
stage. The completed-but-unsealed directory is diagnostic only. The final
documentation stage under tooling `bf64490` was rebuilt and sealed without
changing candidate bytes; all 17 workloads completed and all mandatory rows
passed.

The first Paradox-1 release-refresh execution found one miesmuschel-only test
failure in both direct tests and `R CMD check`: three deep `expect_equal()`
assertions compared R6 ParamSets through private data.table secondary-index
caches. The public state and all 3,041 other expectations passed, and the same
objects differed only by `index` attributes. Miesmuschel head `d4c7f797` uses
semantic equivalence for those three assertions; focused 216-expectation runs
pass on both Paradox majors. This changes only downstream tests, not
miesmuschel production or Paradox candidate bytes. The named overlays were
rebuilt to bind the final reviewed head, the affected miesmuschel rows were
rerun on both axes, and the sealed non-miesmuschel, documentation, and benchmark
conclusions retain their original identities with this exact test-only scope.

That affected-row confirmation is complete. The final Paradox-2 overlay/test/
check evidence is under
`.local/compat/runs/release-final-20260720T053518Z-10c6a0e-r8`; the final
Paradox-1 evidence is under
`.local/compat/runs/release-final-20260720-v1.0.1-paradox1-r2`. Miesmuschel
`d4c7f79750cd15c8174415fb0ba059c597f4f055` passes its full repository suite
and source-package check on both axes. The three other refreshed heads already
passed on each axis, and the final miesmuschel delta changes only three test
comparison calls, so their sealed conclusions transfer without rerunning them.
The final documentation stage has 17 completed workloads with every mandatory
row green; the release benchmark has 72 passes, five bounded marginal reviews,
and no failure. Gctorture, Valgrind, bounded rchk, exact differential, strict
static source checks, and their independent evidence replays are green for the
frozen candidate.

The final portability candidate tag is `paradox-2.0.0-ci-10c6a0e`. Its
direct-child harness is
`paradox-2.0.0-ci-10c6a0e-harness-cc06c18` at
`cc06c182949af09ce80e335ddbfc63a8078692e6`; it changes only
`.github/workflows/r-cmd-check.yml`, whose SHA-256 is
`e6bf11775a59c783a5cbd164ea729a9877a6752edfc96adc0491723b7bf05bfc`.
The local regression/adversarial fixture, `actionlint`, ancestry/diff checks,
and offline evidence-verifier fixture pass. The user must publish those two
tags and dispatch the workflow; Windows x86-64 and macOS ARM64 remain open until
that remote run and its retained artifacts pass independent verification.
