# C17 installation ceiling and C23 compatibility gate

## Purpose

The Paradox 2.0.0 package remains written in portable C99 and continues to
prove that contract in the strict GNU C99 compiler, analyzer, sanitizer, old-R,
and pinned-header lanes. Normal installations should nevertheless avoid a
compiler's newer default dialect changing underneath the package. The
`DESCRIPTION` therefore declares `SystemRequirements: USE_C17`, asking current
R installations to select a language standard no later than C17.

This is an installation-policy ceiling, not a new source-language requirement:

- R 3.6 remains supported and may ignore a `USE_C17` marker it predates;
- the shipped source remains portable C99;
- the existing strict profiles continue to invoke GCC and Clang explicitly as
  GNU C99 even though R routes the package through its `CC17` variables;
- ordinary current-R installations select their configured C17-or-earlier
  compiler rather than inheriting a C23 compiler default.

## C23 forward-compatibility slice

C23 compatibility is a separate bounded release gate. It must not replace or
weaken any C99 proof. The gate:

1. authenticates one repository-local GCC 15.2 compiler overlay and the
   existing repository-local Clang 22 compiler, including an exact live
   package-inventory comparison for the main R/Clang toolchain;
2. builds one clean source package;
3. verifies that the built archive records exactly
   `SystemRequirements: USE_C17`, then reauthenticates its bytes at every
   install/probe boundary;
4. installs that exact archive twice with an explicit
   `R CMD INSTALL --use-C23`, once per compiler;
5. verifies from retained command output and compiler feature probes that each
   compilation really used a C23 dialect;
6. loads each installed package and runs the complete registered-native probe
   inventory against its exact DSO; and
7. retains compiler identities, source/archive/DSO hashes, logs, native probe
   ledgers, and a sealed result.

Compiler width is bounded to four by the shared `resource-jobs compile`
admission. In a proved hard worker it uses the measured 1-GiB compiler weight,
a 1-GiB intra-worker reserve floor, the live cgroup CPU/RAM limits, and the
coordinator-assigned envelope. The task therefore has an exact 4-GiB minimum
allocation, enough to retain that reserve and admit at least one compiler
after the short-lived source-build process has exited. A standalone invocation
uses the same helper's deliberately conservative direct policy, including its
12-GiB host reserve. The retained schema-2 admission report is independently
replayed for arithmetic consistency; the controller/worker receipts and the
helper's generation-time live cgroup authentication, rather than the report
alone, prove hard containment.

One package-excluded validator owns the exact source-archive hash, built DCF,
live toolchain-inventory, and install-log checks. Its cheap adversarial fixture
is part of `harness-validation`. In particular, every expected C source must
appear in exactly one retained compile command, the DSO must have exactly one
link command, and every such command must contain exactly one standard token:
`-std=gnu23`. A leading GNU23 token followed by any competing dialect is a
failure, not C23 evidence. Both helper and fixture are authenticated active
inputs of the real gate.

The C23 slice runs only on current local R. It is deliberately not crossed
with old R, every sanitizer, every supported runtime, or downstream packages:
those gates establish different contracts and continue to use their existing
compiler selections. Hosted current Windows and Apple-silicon checks remain
ordinary installation/portability evidence rather than another C23 matrix.

## Source-freeze and release sequence

Before expensive release work:

1. apply the declaration-order correction required by current R headers;
2. add and exercise the two C23 builds;
3. run focused native and package tests for the final adversarial fixes; and
4. review the complete diff and freeze one clean immutable candidate ref.

Against that one candidate, run the `release-core` profile, which includes the
C23 gate, followed by combined memory analysis, the prepared downstream and
documentation gates, release benchmarks, and the hosted portability companion.
No package-facing source change may be made after evidence is collected without
reopening the affected gates. Agents prepare downstream branches, patches,
commands, and PR text locally; the maintainer performs every remote write.

## Acceptance

- Both declarations in `src/r_utils.h` use `NORET attribute_hidden void`.
- The built package records exactly `SystemRequirements: USE_C17`.
- All existing strict GNU C99 modes remain visibly `-std=gnu99`.
- GCC is at least 15 and Clang is a recent pinned compiler in the C23 slice.
- The retained command ledger proves both explicit `--use-C23` invocations,
  and each install log proves its exact C23 compile/link inventory.
- Both installed DSOs load and pass the native probe inventory.
- The full source-bound release gates complete against one immutable candidate,
  or any non-package environmental exclusion is recorded precisely without
  being relabeled as package success.
