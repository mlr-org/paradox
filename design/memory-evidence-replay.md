# Memory evidence replay

Completed `memory-check` schema-2 runs have an independent, read-only evidence
verifier:

```sh
. scripts/activate
scripts/environment/validate-memory-run "$PARADOX_ROOT" RUN_ID
```

The verifier accepts only a run directly below `.local/checks`, requires the
exact schema-2 `run.txt`, `result.txt`, `completion.txt`, and completion seal,
and compares every retained harness input with the trusted current repository
copy. It verifies the completion bindings, the copied native-origin evidence,
the source-manifest/tree relationship, the complete retained source tree, and
the complete retained modes tree. The source and modes walks run concurrently,
with an explicit maximum of two ordinary-R processes. Neither tree is walked a
second time.

Mode replay is semantic rather than log-presence-only:

- GCT and Valgrind probe ledgers are bound to the exact retained DSO and parsed
  by the trusted native-probe verifier. The GCT policy additionally requires
  one round and the exact `gctorture2(10)` transition.
- The Valgrind analyzer ledger is bound to that DSO and parsed against the
  exact six-file source inventory and reviewed baseline. All four Valgrind log
  families are rescanned in parallel by the trusted evidence helper for exact
  terminal zero-error, zero-suppression, heap, and leak summaries. Retained
  schema-2 prerequisite receipts, wrapper/debug receipts, build
  instrumentation, and the exact three-stage metadata ledger are checked
  without traversing the live multi-gigabyte R or package trees.
- rchk's three reports and tool-status receipt are parsed against the frozen
  source-bound policy by the trusted rchk evidence checker. Replay also
  authenticates the sealed bounded-bcheck cache and every byte-producing input,
  the serial resource-admission report, the exact 20-GiB soft and hard process
  limits, and the analyzed DSO and extracted bitcode. The mounted DESCRIPTION,
  pinned image row, digest evidence, and source/image scope are checked without
  starting a container.

The ordinary protected-input ledger must contain exactly `start`,
`pre-source`, each selected mode's `pre-*`/`post-*` pair, and `final`, in that
order. Every stage hash is verified and all retained fingerprints must be
identical. Replay verifies the retained full-content receipts and their seal,
but intentionally does not hash the live ordinary toolchain, ordinary package
library, instrumented R, or instrumented package library. Those expensive
live-tree checks belong to the original gate's two content boundaries.

The replay never loads a candidate DSO, invokes the instrumented R, reruns an R
test, starts Valgrind, or starts a container. Structured ledgers are size
bounded before parsing, and all scratch output is confined below `.local/tmp`.
Every parallel reader or log scanner owns a separately tracked process group;
failure and HUP/INT/TERM cleanup terminate, reap, and, after a bounded grace
period, forcefully stop every still-active group before removing scratch state.
It currently supports exactly schema 2 and the `gct`, `valgrind`, and `rchk`
mode inventory written by the current trusted harness; a future evidence
schema requires an explicit verifier update.

Cheap adversarial coverage is available with:

```sh
scripts/environment/test-memory-run-validator
```

The synthetic test uses a deliberately non-loadable DSO. It proves a valid GCT
replay, interrupts and reaps both concurrent tree-reader groups, then rejects
a self-consistently resealed missing GCT transition, a
duplicated result field, a substituted retained verifier, and changed protected
metadata. On the development host on 2026-07-15, the valid replay took about
1.89 seconds; the complete interruption/four-negative self-test took 15.01
seconds with a maximum resident set size of about 79 MiB. Valgrind and rchk's underlying
semantic parsers have separate adversarial fixtures in
`test-validation-hardening`. A release claim requires the verifier to replay a
real completed schema-2 all-mode run independently; the 2.0.0 release ledger
records that retained replay rather than treating the synthetic fixture as
memory-safety evidence.
