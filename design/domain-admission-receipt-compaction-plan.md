# Domain admission receipt-compaction plan

Status (2026-08-01): **indexed-root receipt compaction, exact r9 focused
review, the corrected immutable source-bound comparison, the bounded-rchk
phase extraction, and its frozen-candidate proof are complete**. The active
candidate is `refs/paradox-release/candidate-20260801T092108Z`, commit
`4e549f3a8994f513cee1d88d71e037c733a51531`, tree
`4f17819b92f7dfb255c8aafe501ff3e684027d67`; section 12.5 records its passing
release-core and reviewed complete bounded-rchk result.

The implementation passed focused current-R and actual R 3.6 tests plus the
recorded strict GCC and Clang development builds.  Its first balanced A/B
recovered the intended bulk allocation cost but did not recover enough elapsed
time; sections 8--10 record the subsequent bounded recovery and indexed-root
redesign.  Candidate `a153fae` passed the corrected source-bound three-way
comparison recorded in section 11.6. Speed is not permission to weaken the
finalizer, generation, ownership, or closed-dispatch guarantees added by the
final adversarial review.

This is a bounded compaction of one operation-local proof.  It is not a second
Domain admission engine, a caller-trust fast path, persistent validation
metadata, or a rollback of complete sixteen-column structural admission.
`paradox_admit_builtin_domain_schema_row()` remains the only semantic row
owner, `paradox_domain_interpretation_closure()` remains the only mask owner,
and the terminal barrier must still prove that every value handed to a kernel
belongs to one coherent current Domain generation.

## 1. Exact baseline and blocking evidence

The immutable comparison baseline is:

- commit `e923c1a3a544f258c2e925656f9a873213e4ef07`;
- tree `9b282f9e2d555d90b58c885e4564c1f5533ae95b`; and
- the separately installed archive under
  `.local/perf/domain-snapshot-ab-20260731/baseline-library`.

The measured pre-compaction candidate is the exact source snapshot described
by
`.local/perf/domain-snapshot-ab-20260731/candidate-source-sha256.tsv`
(SHA-256
`49175130e68eed82d55f64d85c146d8593f50cd581129382a335fcc94a0634a7`).
The directly relevant source hashes in that snapshot are:

- `src/domain_admission.h`:
  `09abe709ca0153fe72a687761a8012c57f7be0ea3795df7214fa789955e60825`;
- `src/domain_row_admission.c`:
  `65c398ab228b418b069f5cd184fd9874255f81c8f66c6b1c5ec681441ecab469`;
- `src/domain_kernels.c`:
  `11d7850ea4aa8e184b2e6c016daf28659f559dc4fcf6a5c40720da96a15224fb`;
  and
- `src/paramset_check.c`:
  `2dc2e114d307ca654eac583ab29b869fcaa03b14473b35a1b4c404ae5bbd0b8e`.

Both packages were built with the repository's strict optimized GNU C99
configuration under R 4.6.1 and conda-forge GCC 14.3.0.  The benchmark used
one pinned CPU (CPU 15), single-threaded BLAS/OpenMP/MKL/vecLib and data.table,
20 warm-ups, 1,000 samples per workload, and four fresh-process rounds in
balanced AB/BA/AB/BA order.  All twenty semantic result keys agreed.  The raw
round and summary receipts are:

- `.local/perf/domain-snapshot-ab-20260731/results/balanced-rounds.csv`,
  SHA-256
  `526cf1cd9af14fdb6440d7654889167081ec3d1b2a5e2b0388a3b42b24208c4a`;
  and
- `.local/perf/domain-snapshot-ab-20260731/results/balanced-summary.csv`,
  SHA-256
  `c0b5cc37db45feaa698c5fa635fa63da32681d664927bb6e489b3be2a129da2c`.

The following Domain regressions exceeded two percent in every round and both
orders:

| Workload | Candidate / baseline | Allocation delta |
|---|---:|---:|
| `domain_qunif_lgl` | 1.762x | +9,584 bytes |
| `domain_check_int_bulk` | 1.546x | +9,592 bytes |
| `domain_qunif_fct` | 1.527x | +9,584 bytes |
| `domain_check_dbl_one` | 1.520x | +368 bytes |
| `domain_check_dbl_bulk` | 1.515x | +9,592 bytes |
| `domain_sanitize_dbl_atomic` | 1.430x | +9,584 bytes |
| `domain_sanitize_uty_noop` | 1.407x | +368 bytes |
| `domain_qunif_dbl` | 1.387x | +9,584 bytes |
| `domain_sanitize_dbl_list` | 1.350x | +9,584 bytes |
| `domain_qunif_int` | 1.315x | +9,584 bytes |
| `domain_sanitize_int_atomic` | 1.171x | +9,584 bytes |
| `domain_sanitize_int_list` | 1.162x | +9,584 bytes |

The bulk fixtures contain 128 Domain rows.  The old admitted-row carrier had
five `SEXP` slots; the current one has fourteen.  The nine added pointers cost
`9 * 128 * sizeof(SEXP) = 9,216` bytes before vector/header rounding, which
accounts for essentially the complete measured 9,584/9,592-byte delta.
The outer receipt grew from sixteen to forty `SEXP` slots and explains most of
the fixed one-row delta.  The regression is therefore structural and
deterministic, not a thermal or run-order artifact.

The small 3.4--5.7% ParamSet check/assignment regressions in the same gate have
no matching allocation delta and are not attributed to this carrier.  They
remain guard workloads here, but this change must not conceal or speculate
away their separate cause.

## 2. Guarantees that may not change

The compact proof must retain all of the following.

1. **One semantic authority.**  Complete table shell checks stay in the
   adapter.  Every interpreted row still enters
   `paradox_admit_builtin_domain_schema_row()` at the canonical closure of the
   caller's mask.  No optimized kernel, receipt helper, or R fallback may
   restate a Domain rule.
2. **Complete structural boundary.**  Every nonempty or typed-zero-row public
   built-in Domain still has exactly sixteen uniquely named columns with the
   required ordinary type, row count, and attribute-free column shell.  The
   four constructor-owned columns remain structurally checked even when their
   contents are opaque to the operation.
3. **Post-row-name selection.**  The exact row-name carrier is rooted before
   the sole potentially dispatching row-count observation.  If that
   observation replaces the carrier, admission fails.  Columns and metadata
   are reselected after it, so no pre-callback/post-callback splice is used.
4. **Coherent terminal generation.**  A nested mutation before a row is
   owned may select a later generation only if, at the final allocation-free
   barrier, every live interpreted field simultaneously equals the admitted
   copy.  Replacement of a complete column after capture, mutation of an
   already owned row, or mutation of table metadata fails.  The contract is a
   coherent terminal generation, not necessarily the first pointer observed.
5. **Rooting.**  No live `SEXP` exists only in C stack, `R_alloc()`, or other
   unscanned memory across an allocation or callback.  Every selected column,
   metadata identity, source row while it is being owned, and admitted result
   has an ordinary protected R carrier.
6. **Terminal means allocation-free.**  The final simultaneous-generation
   scan may perform structural predicates, raw attribute lookup, element
   reads from already ordinary vectors, bit comparisons, pointer comparisons,
   and the existing allocation-free receipt helpers.  It may not allocate,
   dispatch, materialize ALTREP, hash, evaluate R, or produce the success
   result.
7. **Ownership.**  Levels, tags, special-value shells and typed leaves, and
   interpreted cargo containers retain the current detached ownership
   boundary.  ParamUty and other documented opaque leaves retain identity.
   The transformation remains exact opaque identity.
8. **Exact failure behavior where it is semantic.**  Malformed structure and
   fields retain their established diagnostic families and deterministic
   first failure.  The test-only reentry seam may be narrowed to the public
   grouped-Domain contract; accepting heterogeneous factor groupings in that
   internal seam is not compatibility.
9. **All supported R releases.**  The implementation remains portable C99 and
   uses only the existing R 3.6 facade.  It must not depend on current-R
   internals, write-barrier shortcuts, CHARSXP immortality, or unregistered
   APIs.

Representation-only replacement by an equal spelling of a **closed**
name/class/storage string is not semantic Domain mutation: no kernel observes
those CHARSXP identities and their accepted bytes come from a finite table.
This relaxation does not apply to arbitrary IDs: exact per-row ID rejection is
retained compatibility/proof behavior.  It also does not permit an
allocation-capable string comparison in the terminal barrier.  Arbitrary
factor grouping strings use the common/rare exact-pointer receipt described
below so their existing mixed-encoding equality remains supported before the
barrier.  Physical columns and the metadata identities that the operation
observed remain exact.

## 3. Proposed compact layout

### 3.1 Admitted rows: fourteen slots become six

Use one fixed six-slot row layout:

```text
{ id, levels, special_vals, cargo, tags, trafo }
```

There is no parallel `SOURCE_*` half.

After all destination carriers exist and the post-row-name Domain generation
has been selected, one allocation-free pass fills the ID slot and only those
nested slots selected by the closure-expanded interpretation mask with their
exact source objects.  `Rf_allocVector()` leaves every other nested slot
`R_NilValue`; those slots are never fetched, admitted, terminally compared, or
read by a kernel.  This preserves the current all-row-capture seam without
observing a field outside the operation's contract, and roots complete-column
replacements correctly.  For each interpreted nested field, the ownership
pass then:

1. reads the source from that already rooted slot;
2. protects it while calling the existing canonical snapshot helper;
3. protects the returned owned object;
4. overwrites the same slot with the owned object; and
5. releases the temporary protections only after the write barrier.

For empty or opaque identity fields the admitted object is the source itself,
so no copy or side receipt is created.  For a detached field the owned copy is
also its terminal semantic receipt.  The existing terminal helpers compare
the live current source to that admitted copy:

- levels and tags by exact ordinary payload and allowed metadata;
- cargo by owned interpreted containers plus opaque leaf identity;
- special values by owned shell and kind-specific leaf receipt;
- trafo by exact identity; and
- ID by the exact rooted CHARSXP captured before ownership.

The old source pointer has no remaining proof role after its owned copy is
installed.  Keeping it in a second slot does not strengthen terminal
coherence: the barrier selects the generation by comparing **current live
source to admitted copy**, not by comparing current live source to the first
pointer.  This is the central compaction argument and must be independently
reviewed field by field.

### 3.2 Uniform public spine and rare grouping receipt

Public Domain kernels already reject rows that do not share one grouping, and
the dispatch class fixes one built-in kind and storage type for the whole
table.  After the row-name observation, capture the first admitted
`cls`/`grouping`/`storage_type` CHARSXPs in the three existing one-element
scalar carriers.  In the same pass:

- require every row's class to denote the requested closed kind;
- require every row's storage to denote that kind's closed storage;
- choose the common or rare grouping receipt below; and
- capture every arbitrary ID in its row slot.

The class and storage carriers root the values passed to the owner.  Their
closed values are revalidated by the allocation-free fixed-byte predicates at
the terminal barrier; there is no encoding translation there.  Every row owner
receives these uniform scalar carriers.

Grouping is different: a factor grouping is an arbitrary string, and
`paradox_domain_strings_equal()` allocates on some mixed/native non-ASCII
comparisons.  It may therefore compute groupedness only before the terminal
barrier.  Use two paths:

1. **Common path.**  If every captured grouping CHARSXP is pointer-identical
   to the first, the scalar grouping carrier is the complete exact receipt.
   Terminal comparison of every live element to that pointer is
   allocation-free.
2. **Rare path.**  If the allocation-free pointer scan finds any different
   grouping CHARSXP, allocate one STRSXP of `row_count` elements in the
   optional outer-receipt slot.  That allocation can run a finalizer, so
   reselect and structurally revalidate the complete table, recheck the exact
   row-name carrier, and restart the complete row/numeric/metadata capture.
   The restart overwrites every earlier outer, numeric, ID, and selected
   nested-field capture before any ownership step; unselected nested slots
   remain `R_NilValue`.
   Copy every exact grouping CHARSXP into the rooted rare vector before calling
   `paradox_domain_strings_equal()` over those captured values.  This preserves
   mixed-encoding equality even if the comparison allocates.  The terminal
   barrier does not compare encodings again; it requires each live grouping
   pointer to equal its corresponding rare receipt.

After either path, the terminal barrier scans the complete live spine and
fails if a row-name callback, snapshot finalizer, duplicate hash, or test hook
changed it.  The grouping reentry diagnostic remains
`Corrupt Domain storage: rows must share one grouping`; class/storage
generation drift remains `Domain changed during admission`.

This removes three per-row receipt slots without trusting `domain_info()`:
that earlier scan is only entry evidence and is explicitly not reused after
the row-name callback.  The adapter independently captures and terminally
reauthenticates the post-callback spine.

The internal reentry seam will use grouped fixtures.  No package code outside
the public Domain kernels calls this adapter, and a heterogeneous Domain
already errors on every public operation.  The seam must not force nine
per-row slots into release code merely to admit an object no public caller can
consume.

### 3.3 Outer receipt: forty slots become twenty-one

Use sixteen slots for the exact selected column identities, four for the
metadata identities whose observation is contractual, and one optional rare
grouping receipt:

```text
{
  16 canonical selected columns,
  class,
  row.names,
  .internal.selfref,
  repr,
  rare_grouping_or_NULL
}
```

The current extra sixteen column-name CHARSXPs and four class CHARSXPs repeat
closed information:

- class is an exact four-element sequence determined by `kind`; and
- names are a unique permutation of the sixteen canonical Domain column
  names.

Keep the exact class-vector identity and terminally run the existing exact
class predicate.  For names, extend the canonical column selector with an
optional caller-supplied fixed-size position map (the existing selector
remains the sole structural/name-matching owner).  Capture the sixteen
canonical-to-physical `R_xlen_t` positions after the row-name callback and
compare them to a second terminal selection.  This retains column/name
pairing and physical-order generation without storing sixteen `SEXP` roots.
It also avoids a new name-validation loop in the adapter.

The initial and terminal maps are distinct `R_xlen_t[16]` arrays, with every
absent entry initialized explicitly.  The shared selector accepts an
arbitrary-length malformed list before this adapter's exact-sixteen-column
check, so narrowing a physical index to `int` would be an overflow bug.

The terminal scan still requires:

- the exact captured column object for every canonical column;
- the same canonical-to-physical position for every column;
- the exact captured class, row-name, self-reference, and `repr` identities;
- the exact supported outer attribute count;
- the canonical class bytes;
- the required type, row count, and attribute-free shell of all sixteen
  columns; and
- the row-level semantic receipts described above.

If review finds that changing the selector interface is riskier than its
fixed saving, the fallback is a twenty-two-slot outer receipt retaining the
names vector plus a fixed-size `R_xlen_t` position map and rare grouping slot.
It must not return to sixteen per-name and four per-class `SEXP` slots.

## 4. Allocation and protection accounting

All persistent operation-local R objects continue to be reachable from one
protected `bundle`.

| Carrier | Current | Proposed | Role |
|---|---:|---:|---|
| bundle | 3 `SEXP` slots | unchanged | roots children |
| outer receipt | 40 slots | 21 slots | columns, exact metadata, optional grouping receipt |
| admitted rows | `14 * n` slots | `6 * n` slots | source-then-owned rows |
| scalar carrier list | 7 slots | unchanged | roots scalar vectors |
| scalar vectors | 3 REAL + 4 STR scalar objects | unchanged | owner inputs and spine receipts |
| numeric scratch | `3 * max(n, 1)` doubles when bounds are interpreted | unchanged | exact admitted bounds |
| name position maps | none | two 16-element `R_xlen_t` arrays on the C frame | distinct initial/terminal non-root structural indices |
| rare grouping | none | usually `NULL`; otherwise one STRSXP of `n` elements rooted in the outer receipt | exact mixed-encoding grouping generation |

At 128 rows the row payload falls by
`8 * 128 * sizeof(SEXP) = 8,192` bytes and the outer payload by
`19 * sizeof(SEXP) = 152` bytes.  Relative to `e923c1a`, the predicted common
payload is only one added ID slot per row plus five outer slots:
approximately `1,064` bytes before allocator rounding, instead of the measured
9,584/9,592 bytes.  The one-row payload delta falls from 264 raw pointer bytes
before rounding to about 48.  The rare mixed-encoding grouping path
additionally owns one `n`-element STRSXP; it is not charged to canonical
package-built Domains.

Protection accounting for one detached nested field is deliberately local:

```text
bundle protected by adapter caller
  -> rows roots source
PROTECT(source)
PROTECT(owned = canonical_snapshot(source))
SET_VECTOR_ELT(rows, field, owned)
UNPROTECT(2)
  -> rows now roots owned; source is no longer needed
```

Error paths must count the protected bundle plus the local source/result
protections explicitly.  No pointer retained in the name-position map or
numeric scratch is a GC root; every `SEXP` those arrays describe remains in
the bundle.  The terminal scan starts only after the last possible ownership,
hash, callback, and test-hook allocation, and nothing after it may allocate
before returning the already protected bundle.

### Rare paths

No new rare-path **nested-source** carrier is planned.  Existing canonical
snapshot objects are already the smallest exact receipts:

- a nonempty or otherwise owned nested field leaves its detached copy in the
  row slot;
- an empty ordinary field or opaque identity field leaves the exact source in
  that slot;
- typed special leaves use the shared built-in leaf snapshot/receipt helpers;
- a row-name ALTREP retains its exact outer receipt and is not observed again
  at the terminal barrier; and
- bounds retain their exact bits in the existing native numeric scratch while
  their complete source columns remain rooted by the outer receipt.

The sole planned conditional receipt is the arbitrary grouping STRSXP above.
It exists because retaining mixed-encoding grouping equality and keeping the
terminal barrier allocation-free cannot otherwise both be true.  Its
allocation precedes and forces a complete recapture; semantic equality is
computed from the rooted receipt; terminal authentication is exact pointer
comparison only.

If implementation discovers a field for which the live-source/admitted-copy
comparison cannot prove the current guarantee, that field may receive one
explicit rare side receipt only after the reviewer accepts the proof and its
allocation is benchmarked.  It may not silently restore a full per-row source
half or introduce a second comparator.

## 5. Implementation sequence

1. Obtain independent review of this document.  Resolve every question about
   source rooting, groupedness, name-position ownership, and terminal
   allocation before production edits.
2. Add the optional position output inside the existing canonical column
   selector owner, with its old entry point a thin wrapper if necessary.
   Pin reordered, foreign-encoding, duplicate, missing, and in-place-mutated
   name behavior before changing the adapter.
3. Change the header row enum to the six public/admitted fields only.  Do not
   expose source-receipt indices.
4. Compact the outer receipt and implement the post-row-name uniform-spine
   capture, including the allocation/reselect/complete-recapture rule for the
   rare grouping receipt.
5. Fill every ID slot and only the closure-selected nested slots in one
   allocation-free pass, keep the first test phase after that complete
   capture, and overwrite selected nested source slots with canonical owned
   snapshots row by row.
6. Rewrite the terminal barrier to use the compact receipts.  Keep it one
   visibly allocation-free tail and document why every helper it calls is
   allocation-free.
7. Make the internal test seam phase-aware without adding a second admission
   path: the production wrapper passes one `NULL`/`R_NilValue` hook descriptor
   to the same implementation; the registered test wrapper may request
   `after_complete_capture` and `after_complete_ownership` callbacks.  The two
   checks are fixed per call, never per row, and their null production path is
   part of the A/B gate.  Update only the internal heterogeneous reentry
   fixtures needed by the public grouped contract.  Do not loosen any mutation
   assertion.
8. Run focused correctness gates before any benchmark.  Only a green
   correctness slice may replace the installed candidate in a repeated
   balanced A/B.

## 6. Focused adversarial obligations

The existing one-phase `capture_hook` cannot deterministically mutate an
already owned row: it runs only before ownership.  The phase-aware test seam
above is therefore a required part of this change, not evidence inferred from
the old hook.  The first phase owns capture-time replacement tests; the second
owns already-admitted row and metadata mutation tests.  Both callbacks run
only in the registered test entry, and the production entry always supplies
the null descriptor.

The focused suite must exercise, at minimum:

- whole interpreted-column replacement after complete capture;
- in-place replacement of one nested row while the top-level column pointer
  remains unchanged;
- accepted mutation of a not-yet-owned nested object only when the final
  complete Domain equals every admitted copy;
- mutation of an already owned levels, tags, cargo, special-value, and trafo
  field;
- replacement of class, names, row-names, selfref, and `repr` attributes on
  the same outer Domain; same-vector content changes for the closed class and
  names; and row-name count/shell changes;
- column reorder, name-only reorder, column-only reorder, duplicate/missing
  name, and equivalent foreign-encoding names;
- same-pointer attribute mutation on interpreted and uninterpreted column
  shells;
- class, storage, grouping, and arbitrary-ID mutation after capture;
- row-name `Length` reentry that changes grouping or replaces row names;
- typed and utility special values, opaque identity leaves, nested cargo,
  factor/logical levels, and nonempty tags;
- structural list ALTREP rejection before observation and stable atomic
  ALTREP behavior at the positions the contract admits it;
- zero-row typed Domains and the separate canonical empty Domain;
- all 64 interpretation masks and their idempotent closures;
- forced collections during source selection, nested ownership, semantic
  admission, and immediately before the terminal scan; and
- both current R and R 3.6 strict builds plus the registered direct native
  reentry probe.

Row labels are deliberately count-only.  Selfref and `repr` are opaque exact
identities.  Their receipts reject attribute replacement, but do not and
cannot promise to observe mutation internal to the same opaque attribute
value.  Tests must not overstate that boundary.

There is deliberately no callback/GC claim for the instruction pair between
snapshot return and `SET_VECTOR_ELT`: after the result is protected, that path
contains no allocating or dispatching instruction.  Static control-flow and
protection accounting prove that tiny window; GCT exercises the allocations
inside the snapshot and owner on either side.  The post-ownership test phase
then proves that an already owned receipt rejects a deterministic live-source
mutation before the terminal barrier.

The existing focused files are
`test-native-domain-kernels.R`,
`test-native-domain-nested-admission.R`, and
`test-native-altrep-lifetimes.R`.  Add narrowly named regressions where the
list above exposes a real blind spot; do not turn this focused change into the
full compatibility matrix.

Static review must confirm:

- every allocating call, including mixed-encoding grouping equality, occurs
  before the terminal barrier;
- every source is rooted across its snapshot;
- every selected row slot is initialized before a kernel can read it, and
  every unselected slot remains `R_NilValue` and unread;
- fields outside the interpretation closure remain unread;
- integer overflow checks use the new six-slot stride;
- all `PROTECT`/`UNPROTECT` paths balance on R 3.6 and current R; and
- strict GCC/Clang C99, Clang analysis, and the relevant registered native
  inventory remain clean.

## 7. Performance acceptance and rollback thresholds

Rebuild the exact `e923c1a` baseline and the compact candidate with identical
optimized flags.  Reuse the pinned, single-threaded, balanced AB/BA workload
and add a pre-compaction candidate only when needed to attribute a result.
Validation keys and allocation results are gates, not ancillary output.

The change is accepted only if all of the following hold:

1. every semantic key agrees and every focused correctness obligation passes;
2. no affected workload is stably slower than the pre-compaction candidate by
   more than two percent in both orders;
3. the 128-row ordinary Domain allocation delta versus `e923c1a` is at most
   2,048 bytes, with a design target near the predicted 1,064 bytes;
4. one-row/no-op Domain admission allocates no more than 128 bytes above
   `e923c1a`, with zero profiler-visible delta the target;
5. no bulk Domain workload remains more than five percent slower than
   `e923c1a` in a stable both-order result;
6. no one-row Domain workload remains more than ten percent slower than
   `e923c1a`; a stable 2--10% residue requires explicit maintainer review
   against the fixed correctness cost; and
7. ParamSet, plain-table, and Design guard workloads do not regress stably by
   more than two percent from either their exact baseline or the
   pre-compaction candidate.

The measured candidate uses the ordinary production entry and therefore also
gates the fixed null checks for the two test phases.  A phase check inside the
row loop, persistent hook state, or a separate test-only semantic
implementation is a design failure even if its timing happens to pass.

Any correctness, rooting, analyzer, stack-balance, R-3.6, or terminal-
allocation failure causes immediate rollback of the optimization, not a
defensive hot-path check.  Failure to recover at least eighty percent of the
measured bulk allocation delta, or a stable bulk time ratio above 1.05,
stops implementation for redesign.  A marginal noisy result is repeated in
balanced order after the machine is idle; thresholds are not relaxed by
selecting a favorable order.

The full compatibility, memory, portability, and release matrices remain
deferred until the complete package source converges.  This gate owns only the
receipt compaction and its directly affected hot paths.

## 8. First implementation result and bounded timing recovery

The first compact implementation passed:

- strict GNU C99 builds under repository-local GCC 14 and Clang 22;
- the focused Domain kernel, nested-admission, and ALTREP suites on R 4.6.1;
- a portable GNU C99 install and those same suites on actual R 3.6.3; and
- deterministic second-phase mutation of already owned levels, special
  values, cargo, tags, and transformation receipts, physical and name-only
  column reorder, opaque `selfref`/`repr` replacement, and the mixed
  UTF-8/Latin-1 grouping receipt.

The sealed development evidence is under
`.local/perf/domain-snapshot-compacted-ab-20260731`.  Four 1,000-sample
AB/BA/AB/BA rounds used the same baseline, CPU, workload, and process controls
as section 1.  The raw and summary SHA-256 values are respectively
`9e9369db7af7e656e34e4dae3b0fce4f13023b5a2ed95c424087492ee4f80d54`
and
`5dfd806026f6ea8aeb3f3fcc7cfac531265f49a76104dbdbfb363401d32c9bfa`.
The bulk allocation delta fell from 9,584/9,592 bytes to 1,240/1,248 bytes,
comfortably inside the 2,048-byte gate and recovering about 87% of the
regression.  The one-row delta fell from 368 to 216 bytes, which is a material
improvement but still misses the 128-byte target.

Elapsed time did not recover enough.  Ordinary bulk Domain ratios remain
between about 1.24x and 1.76x versus `e923c1a`; numeric checks remain about
1.44--1.45x.  The near-identical logical-quantile ratio before and after
carrier compaction proves that raw receipt size was the allocation cause, not
the dominant CPU cause.  The remaining work is repeated structural selection,
closed-string comparison, redundant snapshots of shared nested rows, and the
terminal receipt itself.  The receipt is required; only its mechanically
duplicate work may change.

Before another benchmark, implement exactly these bounded follow-ups:

1. **Make the pre-callback probe a shape probe, not a second admission.**
   `check`, `sanitize`, `property`, and `qunif` need only the exact closed
   outer Domain class plus the ordinary `id` carrier's length to choose a
   kernel and size value-side work.  The probe therefore retains the exact
   empty-Domain validator, but for nonempty Domains selects only `id` and
   validates its ordinary character shell.  It does not rescan IDs, class,
   storage, or grouping contents: the canonical adapter remains the sole full
   row/schema owner.  A value-side callback cannot make an observed shape
   inconsistent with the admitted generation because the adapter receives the
   probed kind and size and rejects a different class or row count.  The
   redundant post-value probe in `check` and `qunif` is removed; unchanged
   shapes still admit the complete post-callback generation.
2. **Reuse the pre-row-name selection when observation cannot dispatch.**
   Complete structural validation still precedes the row-name observation.
   For an ordinary row-name carrier, the raw count read cannot allocate or
   reenter, so the already selected columns and position map are the exact
   post-observation generation.  A callback-capable ALTREP row-name carrier
   retains the full reselect/revalidate path.  There must be no allocation
   between selection and the ordinary count read, and no malformed Domain
   gains a callback before structural rejection.
3. **Use a selector-owned indexed terminal receipt.**  The canonical selector
   remains the sole initial name/pairing authority.  Add one allocation-free
   companion in the same translation unit which verifies the exact
   sixteen-column ordinary table and names shells, then checks every captured
   canonical-to-physical `R_xlen_t` position, canonical spelling, and exact
   column identity directly.  Because the captured map is a proven bijection
   over an exact sixteen-column table, this is the same proof in sixteen direct
   checks rather than another nested canonical search.  Column shell
   revalidation remains in the adapter.
4. **Make closed class/storage checks pointer-fast.**  First validate the
   captured row-zero values against the exact kind-specific literals.  Compare
   each later row first with that exact captured CHARSXP and use the existing
   byte predicate only for a representation-equivalent foreign spelling.  The
   terminal scan uses the same pointer-fast/byte-fallback rule, keyed by the
   exact expected literal and kind.  Arbitrary IDs and groupings retain their
   exact receipts.
5. **Reuse adjacent identical nested snapshots with a rooted lookahead
   proof.**  Before overwriting row `r`'s source slot, compare it with row
   `r + 1` while both exact source objects are still rooted by the row receipt
   and retain only the resulting Boolean.  On row `r + 1`, that Boolean
   permits reuse of the previous owned value, which remains rooted by row
   `r`'s slot.  Never retain the previous source only in a raw C local across
   an allocation: a finalizer can replace the outward column and remove that
   source's last R root after row `r` is overwritten.  Apply the same proof
   independently to levels, cargo, tags, and special-value shells.  A reused
   typed special-value shell owns its leaves once; every row still enters the
   canonical special preparation and row owner.
6. **Reuse terminal comparison only for the same live/admitted pair.**  The
   allocation-free terminal loop may skip rewalking nested payload when the
   current source pointer and admitted snapshot pointer are both identical to
   the distinct prior pair just proven for that same field.  Levels, cargo,
   tags, and special values retain separate prior live/admitted pairs.  No allocation,
   interrupt, dispatch, callback, or work-accounting call occurs between
   those comparisons, so R code cannot mutate the shared object in that
   interval.  Every row still checks that it names that exact pair.
7. **Make the canonical selector linear on canonical tables.**  At physical
   position `i`, first compare the name with canonical interned name `i`; only
   a foreign or reordered spelling enters the existing complete search and
   byte fallback.  The accepted name set, duplicate/missing behavior, and
   position receipt do not change.  The shape probe uses this same selector
   for `id`, so the common path performs sixteen pointer checks instead of a
   separate sixteen-string scan.
8. **Measure, then reject, folded receipt-only carriers.**  Making the outer
   receipt the returned root and appending the seven scalar roots to the row
   carrier preserved every ownership boundary, but crossed R vector allocator
   size classes: the measured one-row allocation rose from 216 to 224 bytes
   and the bulk delta from 1,240/1,248 to 1,304/1,312 bytes.  It produced no
   stable timing gain.  The experiment is reverted; keep the compact
   three-slot bundle and separate seven-slot scalar carrier.

These are operation-local indexes and exact-identity reuse, not trusted
metadata or a semantic cache.  Every row still enters the canonical semantic
owner; all sixteen columns, metadata, IDs, groupings, numeric bits, and
interpreted nested fields remain terminally authenticated.  If this batch
does not materially reduce the elapsed ratios, stop: do not cache semantic
admission, trust package-created Domains, persist a validation mark, or weaken
the terminal generation contract merely to satisfy the historical timing
threshold.

## 9. Independently audited recovery state

The earlier “final bounded result” was reopened.  Independent source review
found that its adjacent-reuse rationale incorrectly treated a selected
outward column as a permanent root for an element after a finalizer could
mutate that column.  Section 8 now states the implemented rooted-lookahead
proof.  Review also found that an undetached ordinary empty
`special_vals` shell could gain or lose `names = character(0)` after admission
because the live source and receipt were the same object.  The adapter now
records one preallocated optional-names-presence byte per interpreted row
immediately after canonical special preparation and checks it in the
allocation-free terminal barrier.  Empty levels and tags already require an
attribute-free shell terminally.  A mutable zero-length cargo list is not
admissible; canonical empty typed cargo is `NULL`.  Opaque transformations and
ParamUty leaves retain their documented identity boundary.

The direct-pointer/fused-selector diagnostic run predating only that one-byte
receipt is under
`.local/perf/domain-snapshot-timing-ab-20260731`.  It used the exact `e923c1a`
baseline, CPU 15, four alternating AB/BA rounds, 1,000 samples per workload
and process, and 20 warmups.  Every semantic key agreed.  Raw rounds, summary,
and complete log SHA-256 values are respectively:

- `1124f522091c0f16ac413be41f2ce7182307b30954e9cff734cdf0bff4a68c08`;
- `f5a4837c204d05ebd026419dade972e77bfc65c8afd018b3b4c87108b13cc4d2`;
- `896f880c4a2b4c73d80e415294ecfd3cff1ecdf3c66c836a95dc5d57bdf54afa`.

It materially reduced duplicate work, but the original gates remain binding
and are not met:

- direct double one-row check: `1.358x`, with `+216` allocated bytes;
- no-op utility sanitization: `1.256x`, with `+216` bytes;
- double/integer 128-row checks: `1.157x`/`1.151x`, with
  `+1,248` bytes;
- factor/integer/double/logical quantiles:
  `1.099x`/`1.088x`/`1.063x`/`1.056x`; and
- guard rows `paramset_check_bulk`, `paramset_check_one`, and
  `paramset_assign_bulk`: `1.056x`, `1.029x`, and `1.027x`.

Thus allocation gate 4, direct timing gates 5--6, and guard gate 7 all still
miss.  This is not a frozen result and no later strict Clang or R 3.6 receipt
may be transferred to source that has changed since it ran.  The exact latest
strict GCC and current-R focused receipts before the empty-shell repair were
green (SHA-256
`3f4fdfc4e49d1fe90fbf8ae03f72518b6343cd591ef3bde683900056c47a7263`
and
`4a301e62e6691834bd0ead986e12b5dd4c1606b063469ddae613a0c58c7f4735`);
fresh receipts are required after the next source convergence.

## 10. Planned scalar-owner compaction

The remaining profile attributes a material part of one-row overhead to eight
R allocations used only to present seven one-element vectors to the canonical
row owner: one seven-slot list, three `REALSXP(1)`, and four `STRSXP(1)`
carriers.  The next experiment removes those representation adapters without
removing or duplicating any semantic rule.

### 10.1 One core and two structural sources

`domain_construct.c` will retain one private semantic schema core.  Its input
record contains:

- exact rooted `CHARSXP` values for ID, class, grouping, and storage (ID may
  instead be `R_NilValue` only for the existing constructor case);
- a discriminator selecting either the existing three scalar numeric `SEXP`
  sources or three already captured primitive doubles; and
- the existing cargo, levels, special-values, tags, transformation, mask, and
  special receipt.

One canonical CHARSXP class/storage resolver in `domain_construct.c` owns the
closed-kind mapping.  The public wrapper passes its unpacked CHARSXPs to that
resolver.  The public-Domain adapter resolves the captured row-zero
representative through the same function **before any nested snapshot**,
requires the result to equal the outward dispatch kind, and feeds that exact
resolved result to special preparation and the semantic core.  Later rows
only prove byte-uniformity with that representative.  The adapter's current
parallel `builtin_kind_class()`/`builtin_kind_storage()` maps are removed.

The existing exported-package-private
`paradox_admit_builtin_domain_schema_row()` remains the constructor/full-row
entry.  It performs only ordered, allocation-free scalar-shell unpacking:
ID shape first, class/storage shape second, and grouping shape third.  It then
delegates to the one resolver and private core.  The resolver alone owns the
closed-kind decision; the core alone owns nonempty-ID, grouping, tags, cargo,
transformation, special-value, bounds, and levels semantics and publishes the
same failure field.  The full-row owner consumes the private kind returned by
that same core invocation for default/init checks; it must not call
`domain_kind(cls, storage)` again after allocating semantic work.  For the
legacy numeric-source mode the core does not inspect lower/upper/tolerance
until the existing bounds position, after tags/cargo/special processing, so
allocation/finalizer and diagnostic order do not move.

A second package-private adapter entry accepts the already captured
CHARSXP/double record from `domain_row_admission.c` and delegates immediately
to that same core.  Its primitive doubles are the exact values stored in the
operation receipt before nested ownership; the existing terminal bit receipt
still authenticates them.  This entry is a structural source for the core,
not a second semantic owner.  It may perform cheap internal type assertions,
but it may not repeat domain-kind, grouping, bounds, or nested-field rules.

The legacy numeric `SEXP` sources remain rooted by the caller's existing
scanned R carriers for the complete deferred-bounds call.  The discriminated
C input record merely borrows them and is explicitly not a GC root.

Special-value preparation follows the same pattern: one resolved-kind private
implementation, the existing class/storage `SEXP` wrapper for constructor
callers (using the sole resolver), and one already-resolved-kind adapter for
public Domain operations.  Every row still performs preparation and enters
the semantic core.

### 10.2 Root and carrier layout

The three-slot bundle remains.  Its scalar slot changes from a seven-element
list of seven vectors to one `STRSXP(4)` spine containing:

```text
{ current_id, accepted_class, accepted_grouping, accepted_storage }
```

The accepted class/grouping/storage values are installed once from row zero
and never overwritten.  `current_id` is updated immediately before each row
call; every arbitrary ID also remains rooted permanently in its six-slot row
receipt for the terminal scan.  The spine is an ordinary R GC root for every
CHARSXP passed across allocating semantic work.  No class, grouping, storage,
or ID exists only in a C local or temporary native buffer across allocation.
Numeric values are non-`SEXP` primitives and require no GC root.

There is no `row_count == 1` special semantic path and no live outward column
is passed as a scalar carrier.  A finalizer that mutates an outward column can
therefore neither change the frozen row input nor unroot it; the terminal
generation barrier still rejects the mutated live table.

### 10.3 Ordered proof and rollback

Before implementation, independent review must approve this exact ownership
and control-flow split.  After implementation:

1. strict GNU C99 and Clang builds must remain warning-free;
2. the three focused current-R suites, the deterministic empty-special and
   adjacent-source finalizer regressions, and the registered native probe must
   pass;
3. actual R 3.6 must install and pass the same applicable focused suites;
4. one balanced A/B must show the predicted one-row allocation reduction and
   a real timing benefit without worsening any workload over the present
   direct-pointer result by two percent in both orders; and
5. all original section 7 gates remain binding.

If removing the carriers does not produce the predicted allocation/time
benefit, revert the experiment.  Do not compensate by trusting a caller,
persisting validation metadata, weakening terminal generation proof, or
passing mutable outward scalar columns through the owner.

This remains focused development work.  It is not a substitute for the final
immutable candidate's complete compiler, memory, every-supported-R-minor,
portability, reverse-dependency, and release gates.

### 10.4 Indexed-root carrier redesign

The first scalar-core measurement did not remove the remaining one-row
allocation.  Direct `Rprofmem()` attribution identifies that exact 216-byte
delta as the twenty-one-slot outer receipt, not as semantic ownership or the
native bounds workspace.  The outer receipt, scalar spine, and bundle are
needed only while the adapter is proving its terminal generation; callers use
only the admitted row carrier and native numeric scratch.  They will therefore
be replaced by one fixed indexed protection-stack block rather than by another
R carrier layout.

Independent review approved this redesign subject to all seven invariants
below.

1. Reserve every `PROTECT_WITH_INDEX` slot once, before any value is selected,
   and keep that complete permanent root block until one final balanced
   `UNPROTECT` on success.  Every temporary protection is created above that
   block and balanced locally.  Non-local errors rely only on R's documented
   automatic protection-stack restoration.
2. Immediately `REPROTECT` all sixteen selected columns after every
   allocation-free canonical selection.  The native column and position arrays
   are indexes into those scanned roots; they are never themselves treated as
   roots.
3. Root the originally observed row-name carrier before its sole potentially
   dispatching `Length`.  It is never replaced.  After a callback-capable
   observation, reselect and re-protect all columns and recapture/re-protect
   class, self-reference, and representation metadata before semantic work.
4. Root a newly allocated rare-grouping receipt before `continue`, then perform
   the existing complete selection, shell validation, metadata capture,
   numeric capture, row capture, and protection replacement.  No conclusion
   from the pre-allocation generation is reused.
5. Protect accepted class, grouping, and storage `CHARSXP` representatives in
   distinct indexed slots before the first nested snapshot.  Every arbitrary
   row ID remains rooted in its admitted-row slot.  Thus in-place mutation of a
   protected source column cannot remove the last root of a scalar passed
   across allocating semantic work.
6. The admitted rows vector is the sole returned carrier.  The adapter releases
   its indexed roots immediately before returning that vector; the direct C
   caller protects the return value without an intervening allocation.  The
   native bounds workspace retains its existing enclosing-`.Call` lifetime.
7. Never replace an indexed root while an unbacked raw alias to its old value
   can survive.  Raw `DATAPTR_RO`/`STRING_PTR_RO` views remain confined to the
   allocation-free terminal tail, after the last root replacement, callback,
   interrupt, materialization, and allocation.

This is a representation change to the same proof, not a single-row semantic
path.  The same loops and canonical semantic core serve every row count.  It
removes the fixed outer/scalar/bundle allocations while retaining the
six-slot-per-row ownership receipt, complete sixteen-column terminal proof,
metadata identities, rare mixed-encoding grouping receipt, and exact
finalizer behavior.

Fresh evidence must include strict GCC and Clang C99 builds, focused current-R
and actual-R-3.6 suites, the registered native/GCT inventory, and a
source-bound three-way balanced comparison among the exact `e923c1a` baseline,
the reproducible pre-scalar Domain variant, and this candidate.  Every process
must persist the installed-package fingerprint and semantic validation keys;
the summarizer must reject a mismatched fingerprint or key before reporting a
ratio.  The earlier scalar batch is attribution-confounded by concurrent
metadata-copier source and is not acceptance evidence.

### 10.5 Implemented indexed roots and bounded-metadata convergence

The indexed-root design is implemented.  The adapter reserves its complete
fixed root block before selecting caller state, retains every selected column,
outer metadata value, scalar representative, rare grouping receipt, and
admitted-row carrier in an indexed root, and releases that block only after
the allocation-free terminal generation receipt.  It does not allocate an
outer receipt, scalar bundle, or parallel source-row carrier.  An independent
field-by-field audit accepted the root lifetime, rare-grouping restart,
adjacent nested-reuse proof, and terminal comparison boundary.

The same review exposed a broader old-runtime metadata requirement.  Every raw
selector that follows caller-owned attribute admission must be preceded by one
coherent hard-bounded capture, not merely by an allow-list scan followed by
independent selectors.  The implementation now applies that rule to:

- the five supported outer Domain attributes;
- exact plain capsule tables;
- optional names on special values and interpreted cargo;
- built-in leaf class/metadata receipts and shallow metadata copies; and
- the canonical empty Domain's exact four attributes.

The empty-Domain validator interns `.internal.selfref` before selecting caller
state and then captures `names`, `class`, `row.names`, and
`.internal.selfref` in one allocation-free four-cell pass.  It performs no
special `Rf_getAttrib()` row-name expansion and no later `Rf_install()`, so a
pending finalizer on R 3.6 cannot splice metadata generations.  Duplicate,
unknown, null-valued, cyclic, or fifth cells fail closed.  A focused rogue
metadata regression covers this formerly untested path.

The development source reviewed and exercised for this slice had these exact
hashes:

- `src/domain_row_admission.c`:
  `12f48e6fa620282eb35b560888dc99456c11fd3f44bd21d9099a3d93ce0767da`;
- `src/paramset_domain_common.c`:
  `c2e6789efea4e1d481c06fdb1eed3e8884f47b1eed49df5453a57a394e8a36ab`;
- `src/domain_construct.c`:
  `9ebc7cc048a761451d1ff0d28a6817aa1c2dd10855384328dfeb55a27f14df71`;
  and
- `src/domain_kernels.c`:
  `75cc9fb3c21b85c3e0dbab2200c64b4fcda32b85748a5aefb778a21a38c50a26`.

A fresh GNU17 R 4.6.1 installation and a fresh actual-R-3.6.3 installation
both compiled without warnings.  On each runtime,
`test-core-state-contract.R`, `test-native-domain-kernels.R`,
`test-native-domain-nested-admission.R`, and
`test-native-paramset-construction.R` passed.  Current R had one ordinary
CRAN skip.  R 3.6 had that skip plus the three expected list-ALTREP capability
skips. The original focused run established that R 3.6's public
`row.names<-`/`Rf_setAttrib()` cannot install the test suite's deliberately
no-DATAPTR integer ALTREP as a row-name value, so that run used a compact-
integer fallback. The later complete every-minor release replay showed that
the same base-R construction detail persists through R 4.5. The final
regression therefore uses a no-DATAPTR ALTSTRING row-name fixture, which is
representable on every supported runtime and preserves the full callback/non-
observation proof without a version branch. This is a fixture
representability boundary, not a package support exception; the historical
R-3.6 fallback evidence must not be generalized to R 4.0--4.5.

The focused run also caught two integration regressions before they could be
mistaken for Domain failures: named value assignment selected the source after
attributing names to its destination receipt, and raw one-row Domain metadata
captured compact stored row names where an older check expected only R's
expanded spelling.  Both were corrected; direct named assignment and
`p_int(init = 2L)` smoke tests, construction tests, and capsule contract tests
then passed on both runtimes.

This is bounded development evidence, not release acceptance.  The prepared
three-way A/B harness was deliberately not run while package source was still
changing: its source-hash assertions are stale by design.  Run the exact
baseline/pre-scalar/indexed-root comparison only from the converged immutable
candidate, alongside the remaining strict-compiler, native/GCT, and final
release gates.

### 10.6 Exact r9 review closure

The converged focused snapshot is
`.local/checks/integrated-selectors-r9-20260731T122010Z`; its complete source
manifest SHA-256 is
`0a000dfb692332aed5eacd80119b9ae3fdf037e21a31fa7bf1cf60956584db0a`.
The final review closed the last Domain-local undefined-state gap by
initializing each interpreted row's empty-special names-presence byte before
special-value admission. It also rejected a provisional broad structural-
ALTREP-names exception: package-produced categorical names are materialized
once in `R/ParamFct.R`, package-produced facade row names use compact ordinary
metadata in `R/ParamSet.R`, and hostile structural names still reject before
provider observation. The sibling Condition correction performs bounded
structural re-admission after its callback-capable `Length`; it is included
here because the same review established the common post-observation receipt
rule.

The five directly relevant package-source SHA-256 values are:

- `src/domain_row_admission.c`:
  `68233a976348f6b09ff42f5157fc780a3451fb52c797a91c85cbf82cc71e38c5`;
- `src/builtin_condition.c`:
  `621ce8498691a7ff60ba39e8cff617024664738c505a295cfa310cfd854aceff`;
- `src/r_utils.c`:
  `f9b81c1d3aca0f99bae5bfc3625ba252eb513eb177c2e1b6dbf8d760110104fb`;
- `R/ParamFct.R`:
  `4ee3b9179ff78880969acedcaafefb2f0eb8356c87d8a35b3d5ee590e16a3d53`;
  and
- `R/ParamSet.R`:
  `e676006d826fc97089d56ff5852fa369b79de4f9250b44cb7bfba7b3a66d940d`.

The retained current-R `Condition`, `native-design-transpose`,
`native-domain-construction`, and `native-public-accessor-ownership` logs and
the actual-R-3.6.3 `native-public-accessor-ownership` log all ended `DONE` with
exit zero. Their SHA-256 values, in that order, are
`7e494791778fad9946d60883cf0d514b403dfd11b312c80af2bd3c550fd2a27d`,
`eb1940f1604ef0174515ed11db7ad6584f7b78ecb1c3e2389178da29221e91e0`,
`033caeaef2f31c11d05f2bcaefb584d47edc60bcfe3cf2737d61a9a980d24df3`,
`9c97379a6cb472533ee656ceb683e83f850c7c15f9ced6e507bb38f086e4d3d6`,
and
`c95f6a004903c3bd1d4d0bdc70847254fcd2a8410438632ee770c5feec791b2f`.
The r9 C/R implementation is byte-identical to r8 manifest
`6ed37e6c07ca17d8f487a949cfc2e4cf510eaded448e89fc1b11c7750a114634`,
which owns the retained strict GCC/Clang GNU99 and current/R-3.6 package
installation proof. This is focused development evidence only.

The source-bound three-way timing is still unrun and must bind the next clean
immutable candidate. The exact every-minor supported-runtime selection—R
3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2, plus current R 4.6.1 in
the full native lane—and every other full release gate remain pending.

## 11. First immutable-candidate timing and final recovery plan

### 11.1 Exact failed attempt

The first immutable candidate for this comparison was
`refs/paradox-release/candidate-20260731T124039Z`, commit
`024e28f770f8dd7a8802b70a4a8e866ac2cb344f`, tree
`25bd8a930099dd0bb8d0f0ce78eb901be9546c2c`. The source-bound helper archives
that ref and the exact `e923c1a` baseline with `git archive`; it never copies
the live worktree. Its pre-scalar derivative changes the candidate archive
only at `src/domain_admission.h`, `src/domain_construct.c`, and
`src/domain_row_admission.c`, plus a mechanically restored historical
`paradox_api_stored_attribute_count()` declaration and definition in
`src/r_api_compat.[ch]`. The three original Domain files and both restored
function bodies are hash-pinned. The restoration preserves the pre-scalar
source's three cross-translation-unit calls and exact `R_mapAttrib()` traversal
instead of giving that control the candidate's newer bounded-mapper fast path.

The first helper invocation exposed and fixed two harness-only defects before
measurement: it had inherited `CONDA_PREFIX` from the calling shell, and the
pre-scalar source referred to the compatibility function removed after its
snapshot. Neither reached a benchmark. The repaired helper sets the exact
repository-local toolchain prefix itself and records the bounded compatibility
adapter. Its adapter receipt SHA-256 is
`4402d941dc4c3862f89bdeaf543338c8e628f5a92c45744dabd573d8d41f2e5f`.

The six-permutation run then completed all eighteen processes and failed its
timing policy, so its directory remains deliberately named
`.local/perf/domain-indexed-root-final-threeway-20260731/results.incomplete`.
It is diagnostic evidence, not acceptance. The exact round, summary, gate, and
summarizer-log SHA-256 values are respectively
`a4a854c70d33eff92db7e9fe3cf0e55686ec7d282214096bef07c263076e711c`,
`57ffc3939920747d7812c6cf40f54992729d81eb18b8918d70c5b408aa41c46d`,
`eb9b833b26b03a06f3b9a7d8e355e91f369b1db9e65375bd4ff5085d9cc25209`,
and
`3042fdd8daf1c3128e53e92ec956df8b1daba2d9e7177b3db9872d9fd675fabe`.

The direct candidate-versus-baseline regressions are real and stable:

| Workload | Candidate / baseline | Candidate / pre-scalar | Allocation delta vs baseline |
|---|---:|---:|---:|
| `domain_check_dbl_one` | 1.213x | 0.829x | 0 bytes |
| `domain_sanitize_uty_noop` | 1.141x | 0.844x | 0 bytes |

All bulk Domain timing and allocation gates pass; the indexed-root candidate
also removes the pre-scalar control's fixed 216-byte allocation on both
one-row paths. The remaining cost is therefore CPU work in the proof, not
allocation or a failure of the indexed-root redesign.

The ParamSet guard timing against the synthetic pre-scalar DSO is not a valid
causal gate. The reached ParamSet source is byte-identical, current executes
0.55% fewer Callgrind instructions, and the relevant symbols have
instruction-identical bodies shifted by 704 bytes. On this Ivy Bridge host the
shift produces 1.42% more simulated I1 misses and 7.98% more conditional branch
mispredictions. This explains wall-time guard ratios as high as 1.172x without
extra semantic work or allocation. Do not pad or align source for one
architecture-specific linker layout. The final whole-candidate release
benchmark remains the product-level performance authority.

### 11.2 Profile attribution

Pinned-CPU Callgrind measurements over 2,000 direct native entries give:

| Workload | Baseline | Pre-scalar | Candidate |
|---|---:|---:|---:|
| double check | 15,821 Ir/call | 27,611 | 21,993 |
| utility sanitize | 13,059 Ir/call | 23,233 | 18,787 |

The pre-callback shape probe is already slightly cheaper than the baseline:
3,171 versus 3,228 Ir/call for check and 3,286 versus 3,328 for sanitize.
Almost the entire remaining excess is row admission: 17,060 versus 11,333
Ir/call for check and 15,366 versus 9,658 for sanitize.

The adapter currently traverses the same five-cell outward Domain attribute
spine thirteen times: class selection, shape names proof/selection, initial
outer capture, admission names proof/selection, two row-count
proof/selections, terminal outer capture, terminal names proof/selection, and
the second row-count proof/selection. The duplicate selector and row-count
layers cost about 5,700 instructions per call. They do not prove independent
semantic facts; they repeatedly recover names and row names already selected
by one of the three generation captures that really are required.

Canonical kind dispatch is a second bounded inefficiency. It interleaves
pointer and byte comparisons, so a canonical `ParamUty` pays four failed
`strcmp()` calls before its pointer match. Exact property profiles rise
monotonically from 17,135 Ir/call for `ParamDbl` to 18,195 for `ParamUty`.
Repeated `.internal.selfref`/`repr` interning and byte-only built-in class-tail
checks add smaller avoidable costs.

### 11.3 Implementation batch

This batch must preserve three distinct exact outward generations:

1. the pre-callback shape generation;
2. the post-allocation, pre-ownership admission generation; and
3. the allocation-free terminal receipt generation.

It may fuse observations within one generation, but it must never reuse the
shape capture after an allocation or callback, reuse the admission capture as
the terminal receipt, or weaken complete sixteen-column structural admission.

Implement the following in order:

1. Move the exact supported outward-Domain metadata record and its hard-bounded
   five-cell capture into the common Domain module. Intern
   `.internal.selfref` and `repr` once during package initialization.
2. Add allocation-free column-selection and selected-column-receipt entries
   that consume the names carrier from that exact capture. Existing general
   selectors remain for unrelated callers.
3. Add a count-only row-name helper over the captured row-name carrier.
   Admission and terminal proof consume their own captured names, class,
   row names, self-reference, and representation instead of rescanning the
   table attributes. The callback-capable row-name `Length` path must still
   recapture and reroot the complete generation afterward.
4. Let the shape probe obtain class, names, ID, and its sizing row count from
   the ID carrier in one exact shape capture. It must not observe `row.names`:
   admission remains the sole owner of callback-capable row-name `Length`,
   after all sixteen column shells have validated, so a row-name callback
   cannot repair malformed schema before it is rejected. The canonical
   empty-Domain validator retains its separate exact four-cell contract.
5. Resolve canonical built-in kinds in two phases: all interned pointer
   identities first, then byte fallback for foreign ordinary spellings. Share
   the same pointer-fast canonical class-tail resolver between shape and
   admission. This changes neither accepted bytes nor diagnostics.

No capsule verification stamp, caller-trust metadata, cached admission
decision, skipped generation, R fallback, or special one-row semantic path is
permitted.

### 11.4 Focused proof and corrected timing

Before another full release gate:

- add focused current-R and actual-R-3.6 regressions for malformed, duplicate,
  cyclic, overlong, callback-replaced, and terminally replaced outer metadata;
- run strict GNU C99 GCC and Clang builds plus the relevant registered native
  and GCT probes;
- confirm with Callgrind that the direct admission layer removes the predicted
  repeated traversals and does not add work to bulk paths; and
- rerun a fresh source-bound comparison against a replacement immutable
  candidate.

The timing helper must also be corrected before that rerun. Persist full
deterministic input and output fingerprints, read assigned values back from
the ParamSet, suppress patch backup files, and assert the exact five-file
synthetic diff. Isolate workloads or rotate their order across processes, add
an exact-package A/A path-copy control and a sham-relink control, and classify
ParamSet rows in this Domain-specific comparison as review guards. A guard
hard-fails here for allocation or executed-instruction growth or for evidence
that changed code is reached; the independent sealed release benchmark owns
whole-product wall-time regression policy.

The direct one-row gates remain unchanged: neither direct workload may be
stably more than 1.10x the exact baseline, no Domain workload may be stably
more than 1.02x the faithful pre-scalar control, and all earlier bulk
allocation/timing gates remain binding. If the unified capture does not
recover the profiled work, stop and review rather than weakening the proof.

### 11.5 Implemented unified capture and focused proof

The bounded recovery is implemented.  One common hard-bounded capture now
selects the exact five supported outer Domain metadata cells.  Admission and
terminal proof select columns from their own captured names carriers and
count their own captured row-name carriers; package initialization interns
`.internal.selfref` and `repr` once.  Built-in kind and canonical class-tail
resolution try all interned pointer identities before the byte fallback.  The
shape generation obtains its sizing row count from the selected ID column and
does not observe `row.names`.  Admission remains the sole owner of a
callback-capable row-name `Length`, after all sixteen column shells have
passed structural validation.  The pre-callback shape, post-callback
admission, and allocation-free terminal generations therefore remain exact
and distinct.

Review found and closed four issues during implementation:

- the first shape draft observed an ALTREP row-name carrier before column
  validation, which could have let its callback repair a malformed column;
  the shape path no longer observes it, and a focused regression proves the
  callback count remains zero on that rejection;
- typed class resolution initially tested the empty-class case before its
  pointer-fast typed case and repeated one predicate; the final resolver is
  typed-first and has one predicate owner;
- malformed factor `levels` such as an environment could reach
  `XLENGTH()` and expose a raw R diagnostic; a narrow type gate now produces
  the existing informative field diagnostic, while `NULL` levels retain
  their earlier short-circuit and valid paths gain no duplicate semantic
  engine; and
- a proposed runtime fixture for a genuinely cyclic raw attribute spine would
  itself have required the forbidden `SET_ATTRIB` API.  No policy exception
  or package fixture was added.  That unconstructible-through-public-API case
  remains at the reviewed hard-bound/static-proof boundary; duplicate,
  overlong, callback-replaced, and terminally replaced metadata retain direct
  runtime regressions.

The exact package-source and test hashes for this focused batch are:

- `src/domain_admission.h`:
  `068f8948faa95dd0dd8873b0e215413182390e1b4bb76ffd130305ef77350a00`;
- `src/domain_construct.c`:
  `139b2e66c65dd903e74bdfadb31a356d24ba80e26f8a59c91e607786f5810da5`;
- `src/domain_kernels.c`:
  `bc9ee970fad781990425977dad914431239677e32e22aa6a8db6401eb2aaff01`;
- `src/domain_row_admission.c`:
  `a3f3e39f45284cd27df30f3f7ed672d1b5d9dd9bbc1eff484b12283753e514b0`;
- `src/paramset_domain_common.c`:
  `ec6d1312317b9b3fe5a628950f2a5af900f1d0371608aad939aace21709fcd0c`;
- `src/paramset_domain_common.h`:
  `f27f809cfcc068923f50a34dae9b628e978921f007e831860d59fc0e48a2f8da`;
- `src/r_utils.c`:
  `5afdba33cb976889f2804ffbde7a94a9a858d26a61ec3899bce65015642bf840`;
- `src/r_utils.h`:
  `aea56d6530c81e783f160156ea81abad564c473b5c4dc60178302db51505fd01`;
  and
- `tests/testthat/test-native-domain-kernels.R`:
  `4d416f6b2c8883ff1ca7fb2b258abc7be1d87b7b56620523e7d46d351b8fd73e`.

The textual `git diff` over exactly those nine package-facing source/test
files against plan commit `aa0bff7` has SHA-256
`8413a97b358ba9a13a45e1137559dd1a4d4d7477a5562d5180f026d82701ac98`.
The focused current-R evidence is retained under
`.local/tmp/domain-capture-final.JVRYbq`.  Strict GNU C99 GCC and Clang install
logs have SHA-256 values
`584609789585a6f8d49141b742938f3bd12cc7d7203f23b9a26f87b8f60c5e3a`
and
`9b81d1accc20458fdaa079d714abb8a2ea60c5865aefacda9ec2ca174955bf01`.
The focused current-R test log ended `DONE` and has SHA-256
`30125069780d5c650055436a9a168092cbaf4f4bd8428db41d1705716861e78b`.
The registered plain native probe inventory contains 230 pre-summary records
and zero failures; its empty diagnostic log and TSV have SHA-256 values
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`
and
`63c1bcf383aefec6ae729cee086ef775651e22865f5c6f2d654f1c4e7b5f1366`.
The public-API audit passed, with report SHA-256
`2a68d9c8376847945c88837caa2342ac184fb8f0b180f64d1e05f53f57a7febc`.

The corrected actual-R-3.6.3 final evidence is retained separately under
`.local/tmp/domain-capture-final-r36.lljfGr`.  Its `install-final.log` and
`tests-focused-final.log` have SHA-256 values
`9b905c9f57f46009f289be8dc5d8cd6f402c0e11c4874283635fe52cae14e3b9`
and
`11a3146ba597a593faf31845e1e59314d962e0423db7ebea34e5414851f46589`.
The tests ended `DONE`; their only three skips are the expected pre-R-4.3
list-ALTREP fixture-capability skips.

Pinned Callgrind measurements show that the intended work was removed:

| Workload | Final | Rejected candidate | Final / rejected | `e923c1a` baseline | Final / baseline |
|---|---:|---:|---:|---:|---:|
| double check | 15,520.245 Ir/call | 21,993 | 0.706x | 15,821 | 0.981x |
| utility sanitize | 11,355.074 Ir/call | 18,787 | 0.604x | 13,059 | 0.870x |

The admission-only portions are 12,188.521 versus 17,060 Ir/call for check
and 9,630.074 versus 15,366 for utility sanitize.  The final bulk probe uses
31,015,173 instructions over 50 calls versus 31,279,825 at exact `aa0bff7`,
a 0.99154x total ratio and a 0.98914x C-entry ratio.  The final direct-check,
direct-sanitize, final-bulk, and exact-`aa0bff7`-bulk Callgrind reports have
SHA-256 values, respectively,
`8f8e6a68bc16a1c70469f6ea944acb04dda124fcdd7e799a98b7540b62e98898`,
`54b31a72e4deb17b399c2a18f2892b235cf57126ff7427983d8fe819937fe4d6`,
`a4a710ee103597f5b0bb3279c71c281e546707683c9826039402379950bb9766`,
and
`5292ddb5661cab24d7357d2c90bb38869a94d5b0155044b2b8c66a154391781c`.

These exact hashes, tests, audits, and instruction profiles are focused
development evidence only.  They do not promote a mutable worktree, satisfy
the corrected source-bound three-way wall-time policy, or replace any complete
compiler, memory, every-supported-R-minor, portability, reverse-dependency,
or release gate.  The failed
`refs/paradox-release/candidate-20260731T124039Z` ref remains diagnostic only.
The required next step was to freeze a replacement immutable candidate and
rerun the corrected three-way comparison before complete release gates.
Section 11.6 records that completion.

### 11.6 Corrected immutable comparison

The replacement comparison is complete against
`refs/paradox-release/candidate-20260731T150816Z`, commit
`a153faeeed8735c4aeeba512a70fb71b11235469`, tree
`2fe241b90b0e86381a6f884fd29d3a80baab6b7b`. Retained results are under
`.local/perf/domain-indexed-root-final-threeway-candidate-a153fae-repaired-r5/results`.
All eight gates pass:

- Domain timing versus the faithful pre-scalar control;
- bulk and one-row allocation versus the exact baseline;
- bulk and one-row timing versus the exact baseline; and
- ParamSet review-guard allocation, package-self instructions, and absence of
  changed-code execution versus the pre-scalar control.

The completion, gate, round, summary, layout-control, and review-guard
SHA-256 values are respectively
`379d4c1839c2d05f8c7c8c9d867f492d68355ef3fc22eae46fdf36a389a417d7`,
`e87450b0ab084a0f43867daeb6aee7473eea888b952af9a306c894fa7f89dce2`,
`0cfb24b08727eb930ee34f42384242abaece108181e70e66bbc1bddea7c60d8c`,
`ed1d3d6efc8f311d80e8371e0d055aebcdc87f3f5c19d6d1950ab3386672cd43`,
`117aa75c0a0379efbfeee486d8042061f481b62f40146ecd0f3b3759df3d7c67`,
and
`1f34721e4f83322d9c3a78855f8f06b827546ef679996106b0b826bb91cd4a04`.
The before/after library receipts are byte-identical. This closes the bounded
performance obligation. The later rejection of `a153fae` by unrelated C23
harness and formal-S4/legacy-migration correctness findings does not reopen
these slices; final whole-product benchmarking remains a separate release
gate.

## 12. Bounded-rchk phase extraction before the final freeze

### 12.1 Blocking evidence and decision

The exact `bf0b68f` release-core candidate later reached the bounded rchk gate.
Discovery `release-candidate-bf0b68f-rchk-discovery-r1` exhausted both the
former 800,000-state main analysis and the independent 1,000,000-state
allocator-discovery analysis in `admit_public_domain_table_impl`. Raising both
finite caps to 3,000,000 retained the 20-GiB analyzer address-space limit,
serial execution, and 16-GiB protected-host reserve. A fresh authenticated
source donor was required because the source proof also binds the analyzer
inputs.

The resulting
`release-candidate-bf0b68f-rchk-discovery-r2` again exhausted both exact
3,000,000-state analyses in the same function. Its 3,061,641 reported
main-analysis states differ from the earlier 861,641 by exactly the
2,200,000-state cap increase. The invariant 61,641-state remainder and the
same exhausted function show that another cap increase would only postpone
the failure. The old function compiled to 330 rchk CFG blocks, with 310
branches, 350 calls, and 141 stack allocations. Under the release rule that a
second package-local exhaustion requires source simplification, a further cap
increase, whitelist, partial report, or policy exception is forbidden.

The change is deliberately representational. It may expose phases to an
unoptimized analyzer, but it must not create another Domain semantic owner,
change any R-visible behavior, add a runtime fallback, weaken a receipt, or
add production helper-call overhead.

### 12.2 Locked implementation

Keep `admit_public_domain_table_impl()` as the sole owner of:

- the complete 25-slot indexed protection block and the `rows` carrier;
- conditional native workspaces and the one balanced final `UNPROTECT`;
- both test-hook calls and their exact relative order;
- terminal status-to-error selection;
- admitted-table publication.

Extract exactly four standard-C `static inline` phases:

1. select and root the complete public Domain shell before the one
   callback-capable row-name observation;
2. capture the exact post-callback generation, including allocation/restart
   for the rare-grouping receipt;
3. own every selected nested row and call the one canonical row-admission
   owner;
4. perform the allocation- and callback-free simultaneous-generation
   receipt.

One operation-local context record carries only borrowed aliases and scalar
receipts. Every `SEXP` retained in it remains owned by an indexed root or by
`rows`; the record is never a GC root. The terminal helper returns one of
`current`, `changed`, or `grouping changed`; only the outer owner emits the
existing exact errors. A single cached `has_rare_grouping` fact governs both
raw-pointer initialization and its dereference, encoding their relationship
for Clang's analyzer without a runtime defensive branch.

At analyzer `-O0`, the orchestrator, shell selection, generation capture, row
ownership, and terminal receipt contain respectively 26, 27, 101, 91, and 95
CFG blocks. No replacement approaches the old 330-block path space. In
optimized GCC 14 and Clang 22 builds, all four phases must inline completely:
no helper symbol or call may remain, and the optimized external-call inventory
must remain identical. The implementation stays portable C99 and must compile
against the R 3.6 headers.

### 12.3 Focused proof before replacement freeze

The source adds two permanent regressions:

- both phase hooks run in order in one call; the first forces a full
  collection, the second mutates an already selected levels entry, and the
  terminal receipt rejects that generation;
- all 64 interpretation masks execute for all five closed built-in Domain
  kinds with the exact inert two-hook carrier.

The existing nested mutation, finalizer, reentry, ALTREP, malformed-shell,
stack-balance, and registered native-probe suites remain binding. Independent
reviews must separately cover protection/lifetime/order and semantic/error
equivalence. Focused validation must include strict GCC and Clang, both
ordinary static analyzers, registered probes, current R, actual R 3.6.3, and a
GCT probe before or as part of the new source donor.

The exact converged pre-freeze source/test SHA-256 values are:

- `src/domain_row_admission.c`:
  `e754b67e4a29c6f106cc4c25ebd4234a5cbb0595ff598a549d63093f846f9aae`;
- `tests/testthat/test-native-domain-kernels.R`:
  `aedc2210d674ed4325c5cd2128cfde7dccc5da5b8be88679a1ec94c499383538`;
  and
- `tests/testthat/test-native-domain-nested-admission.R`:
  `2cd2fca4a075e1cf57011db515f327b6ed8cdfe13a93826d084db6d345c9b01f`.

The exact strict-Clang plus Clang-analyzer/probe run is retained at
`.local/checks/domain-admission-refactor-clang-r1`. Its completion,
source-manifest, source-tree, and modes-tree SHA-256 values are respectively
`0ae07d3232f815c6cd06dcc579eea9fe57e6f89140d8a63ed25b3c104ad47330`,
`2c624af29e67a557ded40f4f8d20052f0427fe1f029639a79f0e4a2db5e1c782`,
`9f230d475f12fc584b3e41a90c1fa7b409d9f96bd69a47f6b4c455708926db87`,
and
`a9abbe99f8431e843ebb05bf8755ba80aa2ffd1c860d6aac36c51adca3fd7668`.
The run predates only comment restoration, whose optimized package bytes are
unchanged. Independent lifetime and semantic reviews found no defect; the
former relational Clang false positive is closed by the cached Boolean above.

Before freezing, run a short exact-source balanced comparison against the
strict-GCC `bf0b68f` library on one pinned CPU with single-thread controls.
Semantic results and allocations must be exact. Public Domain operations must
not be stably slower by more than two percent in both AB and BA order; a
test-only direct seam may be reviewed against instruction counts but cannot
override a public regression. Then freeze a replacement package-facing
candidate, rerun complete release-core across every R minor from 3.6 through
current, create a fresh donor, and rerun bounded discovery at the unchanged
3,000,000/3,000,000 caps. Only a complete report receives block-by-block
review and a new exact policy; a fresh post-policy donor is required for final
combined memory acceptance.

### 12.4 Focused freeze gates completed

Actual R 3.6.3 validation of the exact converged package files passed under
`.local/checks/r36-domain-admission-focused-20260801T085833Z-r3`. A clean
source build and install produced DSO SHA-256
`b0d6eafc56f55964d9a775da46edf1a01f5f411dd4e32d4aee1b08083947a6a6`.
The Domain-kernel, nested-admission, and ALTREP-lifetime files passed 1,140
assertions with zero failures, errors, or warnings and the exact 12 reviewed
pre-R-4.3 list-ALTREP capability skips. The official runtime probe passed all
39 cases; the old-R undefined-symbol policy passed; and a direct native smoke
exercised the interpretation closure plus both phase seams, including full GC
and terminal finalizer mutation, with all three cases passing. The 615-file
frozen source manifest, source tarball, and final artifact hash-list SHA-256
values are respectively
`5d526d95129f1c52331fd0d8f0d5865bfdbcde5d974c9393f0ec7d9e6bc4b4b1`,
`807ba60af1436343a7bd256dc4c86cd5eae8869a26766501b03668760c991a59`,
and
`49d5d7c4edf5c23419ab56956ce629b758a46257a85ddb0f599dd314d13d0ac1`.
The generic current-R native-probe runner is deliberately not old-R evidence:
it uses `tools::sha256sum`, which R 3.6 does not export. The official runtime
probe, exact symbol audit, focused files, and direct registered smoke cover
the relevant old-R paths without changing that current-R harness contract.

The source-exact balanced benchmark also passes. The exact strict-GCC
candidate is
`.local/checks/domain-admission-refactor-perf-final-r1`; its source manifest
and DSO SHA-256 values are
`79e87e052ab3d0ae16da9e7d77e7dc2f03d3d5cd2a3efa17c4c964f7cc8712d8`
and
`527b4efd8ddfe8011bfae9bb5de881888cfb82ada6ba2bd8a79fe56043b91fac`.
The exact `bf0b68f` comparison DSO SHA-256 is
`60559bb2a36ac679e24ece3a85067e83cd94aa8b5f95cb26a00f1757e7e8f874`.
Four pinned-CPU, single-thread AB/BA pairs used preheating, rotated workloads,
400 batched samples, and independent allocation profiling. Outputs, opaque
identity, and allocations agree exactly. No workload is more than two percent
slower in both order strata. Median public-operation ratios range from about
0.98x to 1.01x; the direct full-mask scalar seam is faster in both strata.
Deterministic 100-call Callgrind total-instruction ratios are 1.00135x to
1.00287x. The inlined admission body itself costs 2.8--3.4% more for scalar
calls and 0.6--1.6% for bulk calls, but is only about 0.13--0.29% of the
complete public operations. The factor-quantile wall profile is process-hash
noisy; its stable package-self instruction ratio is 1.00544x. This bounded
proof overhead is accepted: it creates no material public hot-path regression
and no allocation, result, or identity change.

The package-facing refactor therefore met its focused freeze gates.

### 12.5 Frozen-candidate and bounded-rchk result

The replacement is frozen at
`refs/paradox-release/candidate-20260801T092108Z`, commit
`4e549f3a8994f513cee1d88d71e037c733a51531`, tree
`4f17819b92f7dfb255c8aafe501ff3e684027d67`. Its exact
`.local/verify/runs/release-candidate-4e549f3-r1` coordinator passed all nine
`release-core` tasks, including every minor series from R 3.6 through current.
Completion/JSON-summary/TSV-summary SHA-256 values are
`c492eeb65a578cbcc868e09d7c222788a524f6795328c6d0a600e84730c16406`,
`c1e5bb86dfbfb9f56d7ce64667f47d65dbd03cf72a2a33d3c27652bae68c030c`,
and
`59d95159bf1f6bdbb50f07cf1087ed0c2f0dc604d1102a81c53c5364f845f855`.

The refactor achieved its analyzer objective without another cap change.
`release-candidate-4e549f3-rchk-discovery-r1` completed at 1,288 functions
and 202,140 states. Its 115 blocks contain 396 UP and 30 PB diagnostics;
fficheck reports 110 routines. Raw bcheck and semantic SHA-256 values are
`cda7598e591bcfa7b3866acdc09530d24dc1643de17fff144056b46cde76e78f`
and
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`.
Three disjoint, independently performed source-review partitions covered every
observed block and found no C defect. The generated exact
policy/block/rationale SHA-256 values are
`e2e7ccc9cc225240986bcb99fc6bd64f8b828924d4aaf0d5765a8a847c346087`,
`88363b5a2c173b378ebcdf2f6e3f05f8b7234240b9f04fc63b343871b1d21067`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The exact policy is now checked in. Combined memory awaits the mandatory fresh
post-policy donor; this result does not replace the remaining portability,
compatibility, downstream, documentation, or release-benchmark gates.
