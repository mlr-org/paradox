# September 2026 release verification

## Candidate and scope

The user requested the complete release matrix after the September performance
and validation-policy work. The active package-facing candidate is
`refs/paradox-release/candidate-20260905T212019Z`, commit
`75f85f5be79a0da2dba268b0a7562839f7f9177c`, tree
`f18cf53d2430278b78ee76d222c71c1040378242`. It includes the changes
reviewed in `graph-lookups-20260905.md` and one test-only portability repair:
compare the extreme normal-sampler scale as a relative error, allowing a few
machine epsilons. Computing that scale and parsing `5e307` can differ by one
ULP on macOS ARM64. The sampler implementation is unchanged.

The first `64b7a389` coordinator is diagnostic: eight gates passed, and its
runtime task was deliberately stopped after 7.2 seconds once the native task
completed, avoiding the older-R sweep on the superseded test payload. It is not
a green complete matrix. The replacement candidate has fresh full results.

Local execution is complete. The release foundation, combined memory gate,
all sixteen exact-head checks across Paradox 1 and 2, mandatory documentation,
and the 82-row benchmark policy pass. The broad corpus and reverse-dependency
coordinator statuses remain non-green, with the exclusions described below.
Fresh hosted portability and downstream conflict resolution still block final
publication; this report does not call the entire release matrix green.

No agent pushed a branch, tag, or PR. Final hosted portability, downstream
integration/release ordering, and publication remain separate manual actions.

## Plan and resource policy

1. Freeze source, then run the entire release-core DAG.
2. Review changed analyzer findings explicitly; update only package-excluded
   verification metadata and record package-payload identity.
3. Prepare fresh Paradox-1 and Paradox-2 installations and ten-package overlays.
   Run all eight exact PR-head source checks on both axes, the full maintained
   repository corpus, priority-zero/one reverse dependencies, and all-scope
   documentation on Paradox 2.
4. Run the final source-bound GCT, instrumented-R Valgrind, and bounded rchk
   gate. Discovery reports do not substitute for this combined gate.
5. Run every release benchmark workload on an otherwise idle host.
6. Prepare the exact hosted companion, validate new hosted evidence when the
   user publishes/runs it, and update the final release/downstream handoff.

Cached runtimes, compilers, packages, and analyzer images are reused. Broad
work uses the existing authenticated systemd aggregate ceiling and the
resource-aware coordinator; independent compatibility failures collect their
own results. The full native suite uses 16 isolated workers, and the runtime
sweep uses seven parallel stages with serial nested tests. Keep the direct
memory driver separate from the broad aggregate coordinator so their memory
budgets cannot be double-spent.

The Paradox-2 compatibility coordinator admitted 30 CPUs under a 43,723-MiB
hard aggregate ceiling, with a 40,610-MiB scheduler budget and an 8-GiB disk
reserve. Exact-head, repository-corpus, reverse-dependency, and documentation
branches ran concurrently. Cached preparations and the same read-only ten-
package overlays were reused; independent failures did not cancel unrelated
coverage. The current exact-head driver checks its eight packages serially,
so that branch remained the compatibility critical path. Benchmarks ran only
after the heavy checks ended. No OOM or aggregate-limit event invalidated a
completed coordinator. The user-owned memory target is restored to 49,152 MiB;
the root-owned reserve policy still determines each run's effective ceiling.

The only disk cleanup was engine-managed eviction of the exact unused
repository-local Valgrind reference image
`ghcr.io/r-hub/containers/valgrind@sha256:223a01d67cc109dac7328d353d57884e2336eed6c1755d240d34f9fc97cce419`.
The active instrumented-R Valgrind installation does not use that image.
About 17 GiB was recovered; source, dependency, toolchain, and evidence trees
were not removed. The rchk image remains retained.

## Release foundation: passed

Coordinator `.local/verify/runs/release-candidate-75f85f5b-r1` passes all nine
tasks in 3,488.8 seconds. This includes all four harness gates, differential
comparison, API-header axes, GCC 15.2 and Clang 22 explicit C23 lanes, all native
modes on R 4.6.1, and complete supported-runtime stages on R 3.6.3, 4.0.5,
4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2, including the declared-floor and
cross-version serialization checks.

Coordinator completion/JSON-summary/TSV-summary SHA-256 values:
`15db201b38089c7f15fc9398ac410b2987692fccec9cb19df68f2f09d588898b`,
`65ae50e3112948169329d8d72aa323c1ec339798f2254d398a35350fe92df9e3`,
`2be8915f579215248715a0d438dc35ab9cdaf07a40d085cc50df93825be16069`.

Native child `release-candidate-75f85f5b-r1-native-release-a001` passes the
independent source-run validator. Strict GCC full tests record 11,247 passing
expectations, one expected old-R-only skip, no failures/errors/warnings,
118 files, and 1,156 blocks. Both the CRAN-style source check and the separate
depends-only check finish `Status: OK`. Strict GCC/Clang C99, GCC/Clang
analysis, cppcheck, native-symbol/probe audit, ASan, and UBSan all pass.
Strict-GCC DSO:
`ca73b4c7ffccae15175ba0c6e2fbc203c0ed5086ba5d2402625187adcb061ff1`.

Native source-manifest/source-tree/modes-tree/completion/result SHA-256 values:
`4953181f88cac4ee0bef37519e66df138d45aed60d9700bf209a5f8a0231f6d2`,
`9c818e0a7194b37803b22df640e69e4caa49bd1d2058ac3352ab2e3faa9fea6f`,
`f09b543c9ee169f24376e9c75bead133a7129907052334e0c110b8155929fe7d`,
`d11b0dbccb618c4c47be18d67b3a9eb7ff02c13ca97b3f8ce79e5085e2027551`,
`9d17eb69082eba985f3ed168ba1892b3f4a8f34196f3339028a353dc0c2637f4`.

## Frozen validation tooling and remaining manual gates

Final memory, downstream, documentation, and benchmark execution used clean,
package-facing-source-identical tooling
`refs/paradox-release/validation-tooling-20260905T222715Z`, commit
`28798768ffc6aec0c1566bf3ddf1e86f33bee9f2`, tree
`072ad62fcce89e140b063e6613fe2bcb84dde36f`. It introduces the reviewed analyzer
policy and the new candidate's compatibility-axis identity. This report's
later ledger and focused-selector repair are package-excluded follow-ups;
they do not relabel the retained helpers or evidence as those newer bytes.
The August acceptance ledger still proves only its original candidate.

Read-only PR metadata at 2026-09-06 02:43 UTC confirms all eight exact heads
still match the overlays and remain open drafts. **bbotk #356, mlr3mbo #284,
and mlr3fda #171 report merge conflicts.** The other five are mergeable, which
does not imply all of their hosted checks are green. Resolve the three
conflicts manually and recheck the resulting heads on both axes before
integration; today's exact-head acceptance does not prove future merge
commits. See [`../compat/downstream-pr-handoff.md`](../compat/downstream-pr-handoff.md).

## Hosted companion: prepared, not published

Exact direct child `4f6adf59475680c55779954d14f4fc20065137c9`, tree
`d57f556f11896752285c144fbb782f780fbf2e08`, is frozen at
`refs/paradox-release/portability-harness-75f85f5b` and tag
`paradox-2.0.0-ci-75f85f5b-harness`. Its sole changed path is the excluded
`.github/workflows/r-cmd-check.yml`, SHA-256
`5a8314316b10d43f2b10a7c7b7a3a34a7e6db0ca0b81c880bf2441f6f94adab2`.
The deterministic renderer, release structural check, renderer/helper-lock
regressions, offline evidence-verifier tests, and actionlint all pass.

Manual commands, from the repository:

```sh
git push origin refs/tags/paradox-2.0.0-ci-75f85f5b-harness
gh workflow run r-cmd-check.yml --repo mlr-org/paradox --ref paradox-2.0.0-ci-75f85f5b-harness
```

This is not hosted acceptance until the new run's exact three platform
artifacts and four REST job conclusions pass independent offline validation.
The final read-only workflow query still has `33991236878` at `64b7a389` as
the newest run. Its macOS floating-point assertion is fixed in this candidate;
restarting that superseded commit cannot test the fix. No run of the new
candidate or companion is available yet. Retain the new artifacts when the
user dispatches it; historical Windows/macOS results are not acceptance here.

## Reviewed analyzer delta

Discovery source is `64b7a389`, whose entire C payload is identical to
replacement candidate `75f85f5b` (the replacement changes one R test only).
Run `release-candidate-64b7a389-rchk-discovery-r1` completed every analyzer and
stopped at the deliberately stale August policy check. It is diagnostic, not
combined-memory acceptance.

Comparison with the retained `a0a9ff3` bcheck report removes source-line and
compiler numeric-suffix differences only for human review. The actual policy
generator continues to authenticate the exact, unnormalized analyzer records.
The old three base-R analysis limits are unchanged; maacheck remains empty.

Reviewed changes:

- `apply_sanitized_values`: 14 to 11 UP diagnostics, with `Rf_match` replacing
  repeated `find_name`. `stable` is directly protected and additionally kept
  in the indexed transaction root chain. Its ordinary copied names remain
  attached throughout. Each target's old values and sources remain retained
  in that same chain even when its target pointer is replaced. `input_names`
  therefore cannot be collected or detached by a user callback during the
  match or output construction. Match output and every newly built target
  receive their own direct protection. Existing PROTECTED_ROOT_CARRIER applies.
- `build_collection_static_state`: removes the address-taken admission-root
  warning. Its two remaining borrowed `set_names` warnings are unchanged;
  `stable_sets` is directly protected, and `set_names` is its owned names.
- `initialize_new_node`: removes the warning at the deleted whole-schema
  reader. No additional borrowed-value call is introduced by that deletion.
- Deleted `collection_child_sources` and `ordered_value_sources` account for
  sixteen removed UP warnings; their source matching is now handled by the
  reviewed source-index propagation and single matching pass.
- `param_set_set_tags_impl` is the former `paradox_param_set_set_tags` body.
  The wrapper protects the materialized public list and returned result; the
  existing callee core/name/list/table and derived-edge protection counts are
  unchanged (four base or ten derived roots). Existing DYNAMIC_PROTECT_DEPTH
  covers the same 23 UP and three PB findings.
- `stabilize_table_list_columns`: one added warning at lazy `plan_spec`.
  Both callers (`check_dt` and table constraint testing) directly PROTECT
  `stable_table = snapshot_table(...)` before this call and retain it for the
  whole operation. `snapshot_table` allocates and attaches ordinary owned
  names and column shells; none is exposed to user code before stabilization.
  Thus the borrowed names and columns cannot become unreachable during a
  lazy spec load (including its error/diagnostic allocations). The plan's
  source metadata is independently retained by its indexed graph root.
  Existing BORROWED_OWNER_ROOT covers this added call; adding runtime roots
  or repeated validation would do no useful work.
- The second `load_snapshot` block and internal helper numeric suffixes changed
  only because of compiler numbering; their normalized diagnostic multisets
  are identical. No new function or rationale category is introduced.

The complete result is 1,301 analyzed functions, 202,516 states, 112 native
registrations, 114 diagnostic blocks, 377 UP and 30 PB findings. The five
existing rationale definitions remain byte-identical. Current and retained
`a0a9ff3` generators independently produce the same three policy files, and
both validators accept the discovery reports against that generated policy.
Policy SHA-256:
`be3c5863c3dca6e88aced2b666a712435f7f6ecc42d7d748581c7612cc1760be`.

This review found no new C defect. The required tooling commit, fresh
source-bound donor containing the new policy, and independently replayed
combined memory run are now complete, as recorded next. The stale-policy
discovery remains diagnostic rather than acceptance.

## Combined memory gate: passed

Eligible source donor `release-candidate-75f85f5b-native-policy-r3` passes all
six release-static modes, the full functional suite, both source-package
checks, and independent source-run validation. Its source-manifest,
source-tree, modes-tree, completion, and result SHA-256 values are:
`6a496c7628a50685d8177c586467559356418368e98210fcdb6902defe33087c`,
`314848464ac8b1f6e0d23afcd2d0af2de54fb769e8e0966c96d2464708a64592`,
`f1af73cbb033de6ac6211b82bc674673ec697d8457de6cee486e5f7b62c5fdfc`,
`53f6e445326f724bfc23cf64858f56edc41fa2e092ffab973f593527c64a202b`,
`e546f2f9ce1511a5103d68f8764ce5ef489a5d3631ef50603a3167788d6a8f62`.

`.local/checks/release-candidate-75f85f5b-memory-r2` passes GCT, instrumented-R
Valgrind, bounded rchk, and independent offline validation, completing at
2026-09-06 00:22:42 UTC. GCT covers all 112 native routines and four hazard
categories. Valgrind reports zero errors, lost bytes, or suppressions in all
processes; its eight functional files and 141 blocks record 1,021 passes and
ten reviewed skips. Rchk reproduces exactly 1,301 functions, 202,516 states,
114 blocks, and the reviewed 377-UP/30-PB policy. Maacheck is empty.

Memory completion, result, source-tree, modes-tree, validator, and source
archive SHA-256 values:
`fbbc3bce973202b1fdf9e419d385c4f28329b7984f5e0a4608aa1032296fa170`,
`fa607054d8ff573a34b5edbf97be6567c839093d100e9fcb1e1149c174666963`,
`a098296e98a18466df041c2075365c9bf3ce99687e102e868dbe673c4b785b51`,
`238640729eb8a3732b375bb7b8de209a318c56a9861e1bf7d6cdbd77d8ad785c`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
`3f4ac0d67a4490029b0c9bf26a3835e67805913fa76468907b3c63b0a39d18a3`.

The accepted raw bcheck, semantic bcheck, empty maacheck, and fficheck hashes
are respectively:
`a7ba4498937cfabb41bac2a8e91bad141012c596f08a033b55074ad607179e34`,
`7520e89ac996e638d7770f436b18005b7f2ee46d01bcf1bad9ca5fabfd89ae0b`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
`dfb0d93ce48b16877f88cd9379925c61fee8f38fa7a642028afd6fb6165779af`.
The block table and unchanged rationale catalog hashes are
`c75711301b4e7d9110ded84dd0d83efd6be6d549de3481775d9d95edf255a2a9`
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.

## Fresh compatibility preparations and exact heads: passed

Both `release-candidate-75f85f5b-p1-r1` and
`release-candidate-75f85f5b-p2-r1` reproduce the exact dependency-library
endpoint `3828cdaf0c767d89fa30ea595845d3eab9695dfdb80bee909a5b3fabcc31919e`.
Both ten-package overlays pass current and retained validation. Paradox 1 is
the frozen `v1.0.1` axis; Paradox 2 is candidate `75f85f5b`. Installed Paradox-2
content hash:
`9b55bf4b3e4cf8aa1ad4f0b3b2f7e481b82a4b796ad306ee3f0c65728f379698`.

All eight exact PR heads—bbotk, mlr3tuning, miesmuschel, mlr3pipelines,
mlr3mbo, celecx, mlr3fda, and mlr3forecast—build and complete source-package
checks on both axes. Fourteen checks finish `Status: OK`; the two mlr3fda
checks have only the reviewed `fdasrvf` Rd cross-reference NOTE. Every build
and check exit status is zero. Current and retained independent verifiers
accept both stages with identical output (2,652 Paradox-1 and 2,650 Paradox-2
evidence members).

Paradox-1 coordinator
`.local/verify/runs/release-candidate-75f85f5b-p1-focused-r2` passes all five
tasks in 4,851.5 seconds. Its completion, JSON-summary, and TSV-summary hashes:
`315edffe39a7cfce0a69feabda850e42144ed7c1ad7179006c4fb3b77268084e`,
`7a2b51e490c25fa0f709831871c371d828514130a2ada6909da3a3d327b9579e`,
`8a75c4d10800caf917295e433050fe62afc24a86106bfb35a2becae2f265f9ce`.
Its exact-head stage is
`.local/compat/runs/release-candidate-75f85f5b-p1-r1/repository-checks-release-refresh-20260720-paradox1`.
Stage completion, results, manifest, and seal hashes:
`3b4fb5ea19dd40f6e7e98d44db5a76e2d2d31cd0e09068ca2cea558f30dde5da`,
`a11de40b996a98c761998894165f33fc64cb51b0a5cb1bbda2c88762a0bd9ef1`,
`31bc98e3f744dc5be014ab8c2d5ea016bc9673f1ea5e3624730d73298312e66f`,
`41c062c24df3fb678ef03af6732e7b5087c8ded98c4ae04264823cb81b41e550`.

Paradox-2 coordinator
`.local/verify/runs/release-candidate-75f85f5b-p2-compat-r1` completes in
4,806.0 seconds with ten passing tasks and two non-green semantic aggregate
tasks (corpus and reverse dependencies). It is **not an overall green
coordinator**. All harnesses, preparation gates, exact-head checks, and
mandatory documentation pass. Completion, JSON-summary, and TSV-summary hashes:
`477c9705665f91ce49db387c0b1a7bfd3690fd50ea1831152cf5f0fb41e230c8`,
`9c63b95ea2954fcd22d6cdb5057ce4b67e506dc4fef3543237e592f476731c95`,
`791268313fa787ef2e091082efc05e431470a15ffe170719d16419e2f72b8858`.
Its passing exact-head stage is
`.local/compat/runs/release-candidate-75f85f5b-p2-r1/repository-checks-release-refresh-20260720-paradox2`.
Stage completion, results, manifest, and seal hashes:
`10065ac8c6657413f2733fadcfaec34d2577a24ab4694f0fcb8e12a41d5a4500`,
`f25ee15bc8d4daeb96e79524dba921a9c41f88ef3e0e6e3aae4c24295e42163c`,
`1fb5d95baa0c86b4105e7be59466c1a0e8903292a642b59fcb77151625aacba6`,
`21dd91c01d0945b5467d665380eb0f66b91c2cabe62dcf2c88dcd05a4a340119`.

## Broad repository corpus: 20 pass, seven fail, one timeout

The fresh 28-row stage is
`.local/compat/runs/release-candidate-75f85f5b-p2-r1/repository-tests-priority-1-release-refresh-20260720-paradox2`.
It completes in 4,239.3 seconds without reusing old rows. All 28 exact source
tuples are unchanged from the preceding accepted corpus. All 20 previously
passing repositories remain passing. Six ordinary failure rows have identical
primary messages: xplainfi lacks `tgen`; mlr3torch lacks a working Lantern /
offline datasets; mlr3cluster lacks Weka XMeans; mlr3filters lacks the `filters`
argument; mlr3oml needs network DNS; mlr3tuningspaces lacks `expect_learner`.

Two termination details changed, so the complete outcome vector is not
identical to the previous candidate:

- mlr3resampling now finishes with the reviewed environmental FutureLaunchError
  instead of timing out. Its workers cannot attach `mlr3resampling`; the
  terminal report contains 16 errors and zero failed expectations.
- mlr3extralearners now reaches the 3,600-second limit in `RWeka_classif_smo`,
  after passing all three prioritylasso groups (19/14/14 expectations) that
  previously motivated a focused follow-up. It also records the unrelated
  missing `.__LearnerClassifMLP__.train` binding for the RSNNS learner. This is
  partial optional learner coverage, not a completed full learner suite. The
  hour-long run was not repeated merely to advance its optional tail.

Current and retained verifiers accept the 565-member stage identically.
Completion, row table, manifest, and seal hashes:
`a9353e1ebd68ccebbbc9f87c7bf0a60a9a3dd8128ef2139459a1a6f1f2eb8288`,
`2dcdc472b516737788ebddffd291eb0305ede51070aecc11ba14691233e5668b`,
`10f3b6af6a7c442a56f3a1b1a687bf4b18d0ee3eb1dc25799c76722837a26eee`,
`9c769c0399677dd17b7fa9f3efe2c37fb816b3cfb63d8645c48f10ceed2a0bbc`.

## Reverse dependencies: ten pass, twelve reviewed failures

`.local/compat/reverse-runs/release-candidate-75f85f5b-p2-compat-r1` completes
all 22 rows in 3,573.0 seconds, without timeout, OOM, interruption, missing
worker, or transient failure. Every package/version/archive/status/
classification/timeout/exit/check-status tuple matches the preceding candidate.
All 221 reported testthat failure headings and primary messages match too.

Seven non-green rows are the intentionally unadapted releases of bbotk,
mlr3tuning, miesmuschel, mlr3pipelines, mlr3mbo, mlr3fda, and mlr3forecast;
their exact prepared heads pass above. The other five retain optional/external
dependency, upstream, or offline-network failures: mlr3spatiotempcv, mlr3cluster,
mlr3filters, mlr3oml, and mlrintermbo. The unadapted miesmuschel uses tinytest,
not the testthat ledger: some tinytests pass, while its examples and tests
retain legacy ParamSetShadow `cannot add bindings to a locked environment`
errors. It is incorrect to describe that row as never reaching tests.

All test-count tuples match except passing mlr3inferr (1,380 rather than
1,381 passes, still zero failures/warnings/skips). Its tests include a
data-dependent extra symmetry assertion on sampled confidence intervals;
the small count difference alone is not a package defect, nor proof of its
precise cause. Current and retained independent verifiers pass identically.
Completion, results, accepted-row table, waves, manifest, and seal hashes:
`41b0d6cbda8042b9548eed7d97c9f874b40edaf28f5c0c892ebf022961ac1116`,
`5fea9a70e1fb06cf6b374319658cc4bc1977143b4a953d5572246d46408d288e`,
`65f5bdecdc76bf528f087ca0907da306155f799c448a76821f45b0189ba46fb3`,
`8adbce520e683d0be6445229aa26308c3eefcdff50ab768c801d185304310746`,
`8dc881046eaa75c0bf347e41ed91fc501333b98673fc1f135a6b51db37baf77f`,
`98bb4f2a67e9bce30aa4528e05c620a00cb13d38136f844b2c978e9535c4f9e4`.

## Documentation: all seven mandatory conclusions pass

Stage
`.local/compat/runs/release-candidate-75f85f5b-p2-compat-r1-documentation-compatibility-a001/documentation`
completes in 1,010.8 seconds. Fourteen of seventeen rows pass, including all
seven mandatory conclusions. All repository/workload/required/status/
classification tuples match the previous run. The three advisory exclusions
are two legacy-gallery rows lacking distill, and the full-book render's
mlr3fairness measure lacking `base_measure` in `as.data.table(msr())`.
Current and retained verifiers accept all 2,023 evidence members identically.
Results, manifest, and seal hashes:
`371e266aec2b2c1319b50d42c0f5b12bb2211aa51be306f04f32c5921425405f`,
`77308e582428c133cd3a562c8669213b001f228257a8a49c7df1a89fe095e816`,
`a7af676763aaacde2872e29211dc1ee69d1e6bdf1d80eb3271bb27ea42e182c3`.

## Release benchmarks: all 82 policy rows accepted

`.local/benchmarks/release-candidate-75f85f5b-final-benchmark-r1` passes on its
first attempt, from 2026-09-06 03:05:37 to 03:22:32 UTC. It runs all 73 paired
workloads with 100 samples and five warmups, plus nine exact prepared-consumer
operations, on the otherwise idle host. Baseline provenance comes from the
fresh 33-case passing differential (`main` at
`06091b5b64a78807d332ec95c5cdc1aaac5899b9`, version 1.0.1.9000).

The unchanged policy accepts 81 ordinary passes, one bounded allocation
margin, and zero failures. The sole margin is `design_transpose_plain`:
51,648 additional allocated bytes, allocation ratio 1.3521, within the reviewed
absolute budget; its median-time ratio is 0.009184. The largest median-time
ratio is `shadow_values_live` at 1.2412, also within its existing policy. This
is acceptance under the documented tradeoffs, not a claim that every operation
beats Paradox 1 or that every operation allocates less. No policy relaxation,
selective retry, package change, or benchmark exclusion was needed.

Current and retained independent verifiers accept all 72 evidence members
identically. Completion, manifest, seal, and decision-table hashes:
`16b181817bcfb54b73cba50b1e636444f84f5e5560398dcf491225203e491982`,
`2cbb863b98e3ba9282e6da7830523d3bb67dc6a506a5d578dc6dacfadc8efd56`,
`9b009ef1eba5c958dd5a2b076fe7407e5b2637c180f0a95e4737a85b26cf2676`,
`a437f52897f22931162a12f21df948052eeb2659632d341075bd2c701b9c9abd`.

## Focused-selector repair and retained unsuccessful attempts

The new `test-operation-validation.R` was selected by the native runner, but
the independent ledger verifier's focused expression had not been updated.
That verifier now includes `operation-validation`; the lightweight fixture
uses this filename for its ninth file and includes it in the synthetic worker
selector. The fixture still has twelve focused files and the same first eight
analyzer files. It now catches this selector disagreement rather than testing
only names matching `native`. No test is removed and no package code changes.

The repaired verifier accepts the actual focused ledger (95 files, 923 blocks,
9,578 passes), full control (118 files, 1,156 blocks, 11,247 passes), and analyzer
control (eight files, 141 blocks, 1,021 passes). Logs are retained below
`.local/release-20260905/` as `focused-selector-tracked-{focused,full,analyzer}.log`.
The complete `test-validation-hardening` follow-up passes, recorded separately
in `focused-selector-hardening.log`. The downstream-profile fixture,
documentation-economy tests, shell syntax, and whitespace checks pass too.
The final path/ancestor check against the unchanged `.Rbuildignore` confirms
that every changed path since `75f85f5b` is package-excluded, with no untracked
package input. Its log is `final-package-payload-check.log`. The full release
gates above used the unaffected full/analyzer selections under frozen tooling
`28798768`; their evidence is not relabelled to this repaired checker.

Retain these attempts with their actual status:

- `release-candidate-75f85f5b-native-policy-r1`: all six static/probe modes pass,
  but probe-only functional coverage is not an eligible combined-memory donor.
- `release-candidate-75f85f5b-memory-r1`: fails at that donor admission before
  GCT, Valgrind, or rchk. The contributor guide now states the focused/full
  donor requirement explicitly.
- `release-candidate-75f85f5b-native-policy-r2`: strict GCC runs 9,578 passing
  focused expectations and one expected skip, then the stale selector rejects
  the inventory. Its later modes never run. The successful ledger-only replay
  does not turn this incomplete source run into a completed donor. Fresh r3
  with full selection, followed by memory r2, owns memory acceptance.
- `release-candidate-75f85f5b-p1-focused-r1`: a trial 16-GiB aggregate ceiling
  cannot pass the resource self-test's deliberate direct-mode admission of
  an 8-GiB job plus its 16-GiB host reserve, although the enclosing harness task
  is budgeted at 4 GiB. No consumer starts. This is a capacity-preflight
  limitation, not a candidate failure. The normal target was restored; fresh
  P1 r2 passes after the other heavy work ends. No safety gate was weakened.

Detailed invocation plans, diagnostic comparisons, raw PR metadata, independent
replay logs, and the prepared hosted worktree are retained in
`.local/release-20260905/`. No C implementation fix was indicated by this
release matrix. The only package-facing change this turn is the macOS
floating-point test assertion described at the beginning.
