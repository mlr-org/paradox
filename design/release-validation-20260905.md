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

## Remaining gates at tooling freeze

Final memory, fresh dual-axis downstream/documentation, release benchmarks,
and new hosted portability remain pending. The August acceptance ledger
proves only its original candidate. The newly selected Paradox-2 axis does not
transfer that old acceptance to this source.

The eight remote PR heads still match the reviewed overlay inputs. A fresh
read reports merge conflicts for mlr3mbo #284 and mlr3fda #171. They must be
resolved and the resulting heads rechecked before integration; a passing
check of today's exact heads does not prove their eventual merge commits.

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

This review found no new C defect. Adoption requires a package-excluded
tooling commit, a fresh source-bound native donor containing the new policy,
and a fresh combined GCT/Valgrind/rchk run with independent replay. Neither
the stale-policy discovery nor this note is final memory acceptance.
