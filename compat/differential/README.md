# Differential compatibility harness

This harness installs two paradox source trees into separate libraries and
runs the same deterministic observations in two separate R processes. The
default baseline is a fresh mirror of `mlr-org/paradox`'s `main` branch; the
candidate is a source archive built from the current worktree. Neither install
loads package code from the repository. The runner exports committed `HEAD`,
applies tracked changes, and overlays non-ignored untracked files into a
run-local candidate source snapshot, so no build step writes into the working
package directory.

All mirrors, archives, libraries, logs, captures, and reports live below
`.local/compat/differential/`. The runner refuses to use the host R: bootstrap
once and activate the repository-local toolchain first.

```sh
. scripts/activate
compat/differential/run --baseline-ref 06091b5 --case shape
```

Use an immutable commit for a reproducible compatibility gate. With no
`--case`, the runner executes every case in `cases.R`. Repeat `--case` to run a
focused group:

```sh
compat/differential/run \
  --baseline-ref 06091b5 \
  --case validation \
  --case dependencies \
  --case transformations
```

The persistent bare mirror is refreshed by default. `--offline` reuses it
without network access and fails if the requested ref is not already present.
`--baseline-url` can point at another upstream repository. Run
`compat/differential/run --help` for all options.

## Results and exit status

Each invocation creates an immutable run directory such as
`.local/compat/differential/runs/20260713T210000Z-1234/` containing:

- clean source export and source archives;
- an immutable `harness/` snapshot and `harness-sha256.tsv` receipt;
- `library-baseline/` and `library-candidate/`;
- build, install, and capture logs;
- `results/baseline.rds` and `results/candidate.rds`;
- a machine-readable `results/report.rds` and concise `results/report.txt`.

A successful comparison records the resolved baseline and candidate provenance
in `metadata/completion.tsv`. When the candidate was clean and stayed at the
same commit/tree/status for the entire run, it also copies the candidate's
evidence helper and seals the complete run. Dirty candidates are useful
diagnostics, but are intentionally never sealed and cannot be release or
benchmark evidence. Verify a clean stage before consuming it:

```sh
Rscript --vanilla compat/verify-repository-evidence.R RUN_DIRECTORY
```

The exit status is zero only when every selected normalized observation is
identical or exactly matches the checked `expected-differences.tsv` manifest.
Each expected row pins separate fingerprints of the complete normalized
baseline and candidate result. The gate fails if either side changes, if an
unlisted difference appears, or if a selected expected case becomes equal.
Thus a new regression inside an otherwise allowlisted case is not hidden.
The checked manifest names every case that characterizes historical behavior
for a bug this rewrite intentionally fixes, including boundary/presence,
collection callback, ID filtering, grouped sanitization, infinite-bound, and
repeated-ID subset behavior. Review those reports alongside the documented
exceptions in `design/compatibility.md`.

Use `--strict` to disable the manifest and make any difference fail. An
alternative manifest can be supplied with `--expected-differences FILE`. The
full pinned release gate is therefore:

```sh
compat/differential/run --baseline-ref 06091b5
```

Package versions, revisions, install paths, R versions, the case-file SHA-256,
the complete harness receipt, and the pre/post content fingerprint of the exact
repository-local shared R library are retained as metadata. The default
cases, normalizer, self-test, capture, comparator, runner, and expected-difference
manifest are copied from the frozen candidate source snapshot; explicit cases
or manifest overrides are copied once and marked `external-override`. `--strict`
uses a receipted header-only manifest marked `strict-empty`. Every capture and
the comparator re-hash all harness files and require the exact same manifest.
Paths and package versions are expected to differ, so the comparator audits but
does not compare them. Dirty candidates are labeled `<HEAD>+worktree`; their
exact exported source and source archive remain in the unsealed run directory.

### Reviewing an intentional fingerprint update

Fingerprints are MD5 hashes of the normalized case object serialized with R's
version-3, XDR representation. They are reproducibility identifiers rather
than a security boundary; the complete objects remain in `baseline.rds` and
`candidate.rds`.

Never update a fingerprint merely to make the gate green. Run the pinned gate
with `--strict`, inspect the case's full `all.equal()` report and both capture
objects, and confirm that every changed observation is covered by an intended
compatibility decision and regression test. The proposed values can then be
read from the failed run without recomputing them by hand:

```sh
Rscript --vanilla -e '
  report = readRDS(".local/compat/differential/runs/RUN/results/report.rds")
  print(report$case_fingerprints[
    report$case_fingerprints$case %in% c("presence", "boundary_matrix"),
  ], row.names = FALSE)
'
```

Update both manifest fingerprints and its reason in the same reviewed change,
then rerun the default pinned gate. A baseline fingerprint change normally
means the baseline ref, case definition, normalization format, or R version
changed and deserves the same scrutiny as a candidate change.

## What normalization does

The captures are base-R canonical structures, not text snapshots. They retain
atomic storage modes, exact values, names, class vectors, attribute ordering,
list-column structure, calls, condition classes/messages, warnings, messages,
stdout, and the RNG state after each case. A data.table's
`.internal.selfref` pointer address is process-local, so it is represented by a
canonical record containing its type and `data.table:::selfrefok()` result. Its
presence and attribute position therefore remain observable, and a missing or
invalid self-reference is a comparison failure. The only generic removals are
closure source-location attributes (`srcref`, `srcfile`, `wholeSrcref`), because
the two installed source paths necessarily differ.

Closures are not replaced with a generic placeholder. Their formals, body,
attributes, enclosing-environment kind, parent kind, and every safely inspectable
lexically referenced binding are fingerprinted. Inactive, non-promise
custom/global bindings are normalized recursively, so changing an ordinary
captured value is a comparison failure. The pinned `rlang` binding inspector is
used to distinguish promises without forcing them. Active bindings, promises,
dynamic environment lookup or runtime evaluation (`get()`, `eval()`,
`do.call()`, namespace-qualified variants, and related constructs), primitive
functions, opaque environments, R6 objects, weak references, and external
pointers fail closed with an actionable projection error. The only external
pointer exception is the narrowly canonicalized data.table self-reference
above. These restrictions ensure normalization cannot execute a getter, force
a delayed computation, recursively chase generated closures, or silently treat
different dynamic environments as equal.

## Adding a case

`cases.R` defines the uniquely named list `paradox_differential_cases`. Add a
`diff_case()` with a fixed seed and a zero-argument `run` function. Return only
observables: public property vectors, ordinary tables/lists, formatted
conditions, callback outputs, or a closure whose lexical capture matters. Do
not return a ParamSet/Design/R6 instance directly; project the relevant fields.

The runner also accepts `--cases path/to/file.R`. A custom file must define the
same named-list contract; it can source or reproduce the tiny `diff_case()`
helper. The override is copied once into the immutable harness, marked as
external in the receipt, and both package processes source that exact copy.
Comparison rejects any SHA-256 or provenance mismatch. Use `--strict` with a
custom case inventory unless a matching custom expected-difference manifest is
supplied.
