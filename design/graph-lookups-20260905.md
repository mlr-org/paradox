# Adversarial review and hashed graph lookups, September 2026

## Scope and policy

A fresh review of the complete native source at `67b1feaa` ("Apply
operation-specific validation and streamline native readers") under the
September 2026 validation policy recorded in
[`validation-policy-20260905.md`](validation-policy-20260905.md): native code
must stay memory-safe for everything ordinary R code can do to a Paradox object,
including edits of private members; private state has no semantic guarantee
after such edits; package-owned values are trusted internally; and no
defensive work may cost cycles without a benefit. The review read every C
translation unit, the R wrappers that hand user objects to `.Call`, and the
tests and benchmarks that landed with `67b1feaa`.

## Findings

1. **Design planning indexed an unadmitted column.** `design_dependencies.c`
   read the parameter table with an ID-only mask but indexed the
   `storage_type` column by parameter row in fixed-value classification,
   dependency evaluation, and typed missing patches. A private table whose
   `storage_type` column was shortened or retyped -- reachable with ordinary R
   through the exported `Design$new()` and the internal capsule replacement
   used by the package's own tests -- read out of bounds on R < 4.6 (R 4.6
   converts the read into a bounds error). Every other consumer's mask was
   audited against its positional reads; this was the only gap.
2. **Public list boundaries rejected base R's wrapper ALTREP.** `names<-` and
   every other attribute setter on a referenced list of 64 or more elements
   return a wrapper ALTREP, so `ParamSet$new(setNames(domains, ids))`,
   `set$values = setNames(values, ids)`, `set$check(setNames(...))`,
   `set$trafo()`, `set$tags <-`, `set$search_space(values=)`,
   `set$test_constraint()`, and `ParamSetCollection$new(setNames(sets, ids))`
   failed once a list reached 64 elements. `$test()` returned `FALSE` and
   `$check_dependencies()` returned a message rather than erroring. Paradox 1
   accepted all of these; the wrapper follows R's ALTREP rules, so it is
   inside the supported input model.
3. **Quadratic name resolution in the check engine and value store.** With
   500 parameters: `$values <-` cost 9.3 ms, a collection `$check()` 10.6 ms,
   a collection `$values <-` 23 ms, and `set_values()` on a stored collection
   13.8 ms, all products of two 500-element scans. The sources were
   `validate_node_schema()` (replayed the private tag/value/trafo/dependency
   semantic scan on every ordinary check, with a linear parameter lookup per
   row), `child_root_ids()` (a linear translation-table scan per child row
   plus a linear parent lookup; the translation table is keyed by exposed ID
   and therefore not in child order), `ordered_value_sources()` and
   `apply_sanitized_values()` (linear name searches per value),
   `collection_child_sources()` (re-derived by string search what the store
   plan already knew), the internal-tuning snapshot (linear translation and
   stored-value scans per parameter), `split_values()` in the Shadow reader,
   `validate_related_ids()` in collection construction, `exact_sets()`, and
   the eager loading of every parameter spec on each check plan.

No memory-safety defect beyond finding 1 was identified. PROTECT balance,
finalizer/ALTREP reentry windows, generation receipts, and index arithmetic
were re-audited in every changed function.

## Changes

- `design_dependencies.c` reads `storage_type` through its column mask.
- `r_utils.c` gains `paradox_materialize_public_list_shell()`: a top-level
  ALTREP plain list wearing at most a `names` attribute is copied once (one
  Length, one Elt per element, names copied element-wise) into an ordinary
  rooted carrier; S4, classed, and otherwise attributed shells return unchanged
  so each operation's gate keeps its diagnostic. Every public list boundary
  materializes before its ordinary-list gate: `ParamSet$new()`, the value
  transaction entry behind `$values <-`/`.store_values()`/checked assignment,
  `snapshot_named_list()` behind `$check()`/`$test()`/`$assert()`/
  `$check_dependencies()`/`$test_constraint()`, list `$trafo()`,
  `$tags <-`, the search-space value carrier, and `ParamSetCollection$new()`.
  A callback run by that materialization is ordinary supported reentry: the
  operation proceeds on the post-callback generation with the captured
  elements, the contract public tables already had.
- Check engine: each node owns lazily built hashed indexes over its parameter
  IDs, stored value names, and translation exposed IDs (`node_lazy_index_t`);
  `local_param_row()` and the internal-tuning walk use them.
  `validate_node_schema()` runs only for the read-only migration preflight and
  ObjectTuneToken candidate admission (`admit_schema`), where a serialized or
  argument-supplied graph is certified whole; ordinary operations interpret the
  rows they consume. `child_root_ids()` indexes the owner's original IDs once
  above a small size. Parameter specs load on first use (`plan_spec()`), so a
  one-value check no longer classifies every other parameter. Required-presence
  and internal-tuning tag lookups mark parameters in one pass over the tag
  rows. `exact_sets()` hashes affixing set names above 32 children.
- Value store: `ordered_values()` carries each value's root source along with
  its match; the collection store plan carries per-child sources, so
  `collection_child_sources()`/`collection_source_row()` are gone;
  `apply_sanitized_values()` uses one `Rf_match()` per target.
- Shadow reader: `split_values()` uses two `Rf_match()` passes.
- Collection construction: `validate_related_ids()` hashes the parameter IDs
  above a small product of table sizes.

Every hashed replacement keeps the linear form below a small size threshold
so five-parameter sets pay nothing new.

## Tests

- `test-operation-validation.R`: the Design plan rejects a shortened,
  retyped, or removed private `storage_type` column before indexing it.
- `test-regression-wrapper-altrep-inputs.R`: every public list boundary
  accepts `setNames()` wrappers at 63/64/200 elements with results identical to
  the ordinary spelling, a rejected point is still diagnosed, the copy is
  independent of the caller's list, and (R >= 4.3) a stateful ALTREP list is
  observed exactly once per element with the later provider generation never
  becoming visible. The old-R skip is registered in
  `environment/runtime-matrix-result-skips.tsv`.
- `test-native-altrep-lifetimes.R` and `test-native-paramset-value-mutation.R`
  now pin the materialize-once contract (callback count, post-callback
  generation, captured leaves) where they previously pinned rejection.

## Verification

Strict GCC (`environment/Makevars-check-strict-gcc`) installation is
warning-free. The complete Paradox unit suite (118 files, `NOT_CRAN=true`,
eight parallel workers) passes with the same six capability skips as the
baseline; see the ledger below for the exact counts. Focused probes confirm
the Design plan rejects every retyped/shortened `storage_type` edit with a
"Corrupt ParamSet" diagnostic and that every wrapper-list boundary accepts
64-element inputs.

## Measurements

`benchmarks/graph-lookups-20260905.R` compares the installed baseline
(`67b1feaa`) and candidate libraries in fresh baseline/candidate/candidate/
baseline processes on one CPU with one data.table thread, ten-second warmups,
three blocks of 200 samples per case, and result-key agreement checked by
`summarize-getters-20260905.R`. Development measurements, not release evidence.

| Workload | Baseline (microseconds) | Candidate (microseconds) | Speedup | Both orders |
| --- | ---: | ---: | ---: | --- |
| `base/5/check_full` | 18.5 | 18.2 | 1.02x | faster |
| `base/64/check_full` | 42.0 | 43.2 | 0.97x | slower |
| `base/500/check_full` | 214.0 | 210.2 | 1.02x | mixed |
| `base/500/check_one` | 85.3 | 40.2 | 2.12x | faster |
| `base/64/values_set_full` | 225.8 | 91.5 | 2.47x | faster |
| `base/500/values_set_full` | 8,408 | 460.9 | 18.24x | faster |
| `base/500/values_set_one` | 121.6 | 76.9 | 1.58x | faster |
| `base/64/set_values_one` | 240.1 | 105.8 | 2.27x | faster |
| `base/500/set_values_one` | 8,458 | 517.3 | 16.35x | faster |
| `base/500/trafo` | 39.6 | 39.6 | 1.00x | mixed |
| `base/500/construct` | 3,580 | 3,652 | 0.98x | slower |
| `collection/5/check_full` | 26.4 | 25.5 | 1.03x | faster |
| `collection/64/check_full` | 216.7 | 66.8 | 3.25x | faster |
| `collection/500/check_full` | 9,697 | 351.6 | 27.58x | faster |
| `collection/500/check_one` | 9,559 | 185.3 | 51.57x | faster |
| `collection/64/values_set_full` | 475.8 | 143.7 | 3.31x | faster |
| `collection/500/values_set_full` | 21,068 | 749.2 | 28.12x | faster |
| `collection/500/set_values_one` | 43,629 | 1,208 | 36.11x | faster |
| `collection/100x5/check_full` | 4,483 | 1,110 | 4.04x | faster |
| `collection/100x5/values_set_full` | 11,842 | 3,344 | 3.54x | faster |
| `collection/100x5/construct` | 1,577 | 1,561 | 1.01x | faster |
| `collection/500/design` | 18,793 | 18,852 | 1.00x | mixed |
| `shadow/64/check_full` | 84.0 | 36.4 | 2.31x | faster |
| `shadow/500/check_full` | 2,675 | 124.2 | 21.54x | faster |
| `shadow/500/values` | 41.9 | 41.5 | 1.01x | faster |

The complete comparison (45 cases) is retained with both process orders,
logs, and the baseline/candidate DSO SHA-256 values under
`.local/graph-lookups-20260905/`. All 45 result keys agree between the
installations. No case is more than 3% slower in both orders: the residual
`base/*/construct` (0.98--0.99x, allocation unchanged), `base/64/check_full`
(0.97x) and `collection/5/set_values_one` (1.00x) differences are at the
level of order-to-order variation and were not reproduced by an alternating
single-CPU rerun of the full 64/500-parameter check. Every hashed replacement
keeps the linear form below a small size threshold, which is why the
five-parameter cases move by 1--6% in either direction only. Recorded
allocation rises modestly on the wide collection cases (indexes and the
per-child source slices), by 8--10% on 500-value BASE writes, and is unchanged
on getters and construction.

Full suite on the final library: 118 files, 1,156 tests, 11,227 passes, six
capability skips (identical to the baseline's), no failures, warnings, or
errors, on R 4.6.1 with eight parallel workers. The baseline build passes the
same suite with its original expectations (11,163 passes). Focused
alternating single-CPU reruns confirm `set_values(.values = 250 of 500)` at
15.3 ms before and 0.7 ms after, and `set_values()` of one value on a stored
500-value BASE set at 9.8 ms before and 0.5--0.6 ms after.

