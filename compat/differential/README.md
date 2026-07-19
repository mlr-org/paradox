# Differential compatibility harness

This harness installs two paradox source trees into separate libraries and
runs the same deterministic observations in two separate R processes. The
default baseline is a fresh mirror of `mlr-org/paradox`'s `main` branch; the
candidate is a source archive built from the current worktree. Neither install
loads package code from the repository. The runner exports committed `HEAD`,
applies tracked changes, and overlays non-ignored untracked files into a
run-local candidate source snapshot, so no build step writes into the working
package directory.

`cases.R` has been refreshed for the contract-first Paradox-2 boundary. The
checked-in `expected-differences.tsv` pins the complete normalized Paradox-1
and Paradox-2 fingerprints reviewed for the contract-first candidate. The
manifest was populated only after a strict run and full capture review; no
fingerprint from the superseded compatibility-first candidate was retained.

Use `--strict` while diagnosing the changing source so the fail-closed intent
is explicit in the run receipt.

## Reviewed contract-first inventory

The public-API-first inventory now covers:

- all five built-in Domain and both built-in Condition kinds, plus deliberate
  observations of the now-closed third-party subclass seams;
- named scalar Domain inputs, S3-classed value/check containers, and the
  deliberately stricter ordinary transformation-shell boundary;
- additive ParamSet inheritance, values, checks, dependencies,
  transformations, constraints, serialization, designs, and random sampling;
- BASE and COLLECTION behavior, repeated-ID subset behavior, detached
  subset/flatten callbacks, and detached semantic equality;
- the official live `ParamSetShadow` when available, with a public detached
  projection serving as the legacy-side reference; and
- the recovered-TuneToken search-space boundary plus exact package-built token
  admission and deliberate rejection of subclassed or extra-metadata tokens.

The differential intentionally does not inspect private R6 fields or capsule
layout and does not duplicate legacy-upgrade fixtures, deep/cyclic token
forgery, adversarial corruption, or the full downstream corpus. Those belong to
their dedicated package, upgrade, memory, and consumer gates. Its TuneToken case
uses only small fixed root-shape violations so both package processes can report
the intentional contract delta safely.

Each reviewed manifest row contains both complete normalized fingerprints and
a reason naming an intentional compatibility change or retained bug fix.
Equality, an unlisted delta, an old fingerprint, or a row without a current
direct regression fails. The exact clean candidate must still rerun the default
gate and seal its evidence; a dirty review run is not release evidence.

There is deliberately no fingerprint auto-accept generator. The runner's
`compare.R` writes the proposed values to
`RUN/results/report.rds` in `case_fingerprints` and prints the relevant values
in `report.txt`. The reviewed workflow is:

```sh
. scripts/activate
compat/differential/run \
  --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9 \
  --strict

Rscript --vanilla -e '
  report = readRDS(".local/compat/differential/runs/RUN/results/report.rds")
  print(report$case_fingerprints, row.names = FALSE)
'

# After reviewing cases, full normalized results, NEWS, and direct tests:
compat/differential/run \
  --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9 \
  --expected-differences /absolute/path/to/proposed-contract-first.tsv
```

After changing the reviewed `cases.R` or `expected-differences.tsv`, rerun the
default command from the exact clean frozen source, then independently verify
its seal:

```sh
Rscript --vanilla compat/verify-repository-evidence.R \
  .local/compat/differential/runs/RUN
```

`compat/differential/run --help` is the runner interface. The runner executes
the authenticated `test-normalize.R` self-test, captures baseline and candidate
in separate processes, invokes `compare.R`, and seals only a clean candidate
whose comparison passes. The generic verifier checks the retained manifest and
seal without regenerating or reclassifying results.

The normalizer policy also has a cheap standalone self-test:

```sh
Rscript --vanilla compat/differential/test-normalize.R \
  compat/differential/normalize.R
```

All mirrors, archives, libraries, logs, captures, and reports live below
`.local/compat/differential/`. The runner refuses to use the host R: bootstrap
once and activate the repository-local toolchain first.

```sh
. scripts/activate
compat/differential/run \
  --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9 \
  --case shape
```

Use an immutable commit for a reproducible compatibility gate. With no
`--case`, the runner executes every case in `cases.R`. Repeat `--case` to run a
focused group:

```sh
compat/differential/run \
  --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9 \
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
Thus a new regression inside an otherwise allowlisted case is not hidden. The
checked manifest was reviewed against the documented exceptions in
[`design/compatibility.md`](../../design/compatibility.md), NEWS, direct package
regressions, and both complete normalized captures.

Use `--strict` to disable the manifest and make any difference fail. An
alternative manifest can be supplied with `--expected-differences FILE`. The
full pinned release gate is:

```sh
compat/differential/run \
  --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9
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
read from the failed run without recomputing them by hand; this extraction is
not an acceptance generator:

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
An uncaught error from the case-level `run` function is a harness failure on
either side, and the comparator also rejects retained captures with a non-value
top-level outcome. Record errors that are themselves intentional observations
with `observe_call()` inside the case result instead.

The runner also accepts `--cases path/to/file.R`. A custom file must define the
same named-list contract; it can source or reproduce the tiny `diff_case()`
helper. The override is copied once into the immutable harness, marked as
external in the receipt, and both package processes source that exact copy.
Comparison rejects any SHA-256 or provenance mismatch. Use `--strict` with a
custom case inventory unless a matching custom expected-difference manifest is
supplied.
