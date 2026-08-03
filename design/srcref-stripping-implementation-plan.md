# Srcref stripping for stored callbacks: implementation plan

Status: implemented in the current pre-freeze batch (2026-07-26). This is a
package-facing change and therefore belongs to the current pre-freeze batch:
it must land before the replacement candidate is frozen and is covered by that
candidate's full gate matrix per `design/release-2.0.0.md` and `AGENTS.md`.
During development, only focused validation is intended. Agents must never
perform remote writes; issue and PR updates listed in §9 are performed by the
user.

Implementation resolution: recommendations D1--D6 were accepted. Known
Paradox-1 package-generated crate shapes are handled by narrow authentication
and reconstruction: categorical mapping, collection-flattened
`in_tune_fn`, tuning-ParamSet transformation, and detached collection
transformation/constraint adapters. This normalizes captured user callbacks
without generally walking or mutating closure environments. ParamSet carriers
captured by an authenticated detached collection adapter are explicit
migration dependencies, so direct conversion cannot leave a hidden Paradox-1
child behind. Arbitrary closures and environments remain opaque.
Package-owned categorical and integer-logscale factories normalize their
returned closure as well, because development and differential installs built
with `--with-keep.source` can otherwise retain package source references. The
same rule covers generated Shadow/Collection callback adapters and the
internal-tuning namespace adapter created by
`ParamSetCollection$flatten()`: package-generated callback state is not exempt
from the stored-callback invariant. The generated retired-API active-binding
diagnostic installed during legacy migration follows the same rule; its
session-local provenance token remains intact while its returned closure drops
package source metadata. On supported R releases,
`identical(ignore.srcref = TRUE)` does not consistently ignore source metadata
nested inside a formals-default AST; semantic equality remains unchanged, but
the implementation and tests do not promise `identical()` between an original
complex source-bearing closure and its stripped copy.

Related: issue mlr-org/paradox#395 ("remove sourcerefs of function values and
custom_check") — this change resolves its callback half and explicitly
wontfixes its `$values` half (§9). Issue #393 / PR #394 (crate default
custom_check) are verified obsolete (the default has been `NULL` since 1.0.0)
and are closed alongside, with no code action. Documents to amend:
`design/contract-first-2.0.0.md`,
`design/compatibility.md`, `NEWS.md`.

## 1. Summary of the change

Functions that become **package-interpreted callback state** — `custom_check`,
per-parameter `trafo`, `extra_trafo`, `constraint`, and (per D1) the
internal-tuning cargo callbacks — have their source references (`srcref`,
`srcfile`, `wholeSrcref` attributes, recursively) removed from the *stored
copy* at the admission boundary. At ordinary current-object admission,
functions without source references are stored unchanged at pointer identity
(no copy). A global option disables stripping for debugging. The legacy
detached-collection wrapper exception is narrower: an authenticated wrapper is
always rebuilt with a fresh closure environment so captured ParamSet children
can be rebound without mutating the serialized input. With the option disabled,
that rebuild retains source metadata but does not retain wrapper pointer or
environment identity.

Source-reference normalization **never touches** `$values` entries:
function-valued parameter values, `ParamUty` payloads, `special_vals`,
`default`, and `init` leaves remain opaque and identity-preserved under this
normalizer exactly as the contract specifies today. The separately documented
recursive legacy upgrader may still discover and identity-migrate a legacy
ParamSet shell stored inside an opaque value.

Motivation: closures defined in scripts or interactively (where
`options(keep.source = TRUE)` is the default) carry srcref attributes whose
`srcfile` environments can drag entire source-file texts into every serialized
ParamSet, tuning archive, and saved learner. Stripping at admission removes
that bloat for the state paradox owns, and makes serialize()-based hashes of
equivalent ParamSets independent of the file/session their callbacks were
written in.

Contract framing (use this in the amended documents): this is **not** an
exception to the opaque-leaf clause — that clause covers `$values`/payload
leaves and is unchanged. It is an extension of the existing admission rule
that *representation-only metadata is discarded at the boundary*, exactly like
names on scalar Domain constructor inputs ("representation-only and …
discarded from the native snapshot"). The one contract sentence that changes
is the callback-identity sentence: "documented user callbacks retain identity"
becomes "… retain identity from the point of admission; admission discards
source-reference attributes (see `paradox.strip_srcrefs`)".

Explicitly **out of scope**: any change to `$values` storage or reads; any
source-reference walk of TuneToken content beyond the `to_tune(aggr=)`
constructor site (S7); sampler fields such as `Sampler1DRfun`'s `rfun` (not
capsule state); general environment traversal or compilation inside the strip
helper; and printer changes beyond the incidental deparse effect documented in
§6. Existing package-generated factories and narrowly authenticated legacy
wrapper reconstruction may still use their documented `crate()`/`cmpfun()`
boundaries.

## 2. Locked decisions (maintainer-approved 2026-07-26; do not re-litigate)

1. Strip `custom_check`; extend the same treatment to `trafo` and
   `constraint` (and `extra_trafo`, which is the same admission family).
2. Source-reference normalization never traverses `$values` (including token
   content already stored there). Existing value/token admission may still
   snapshot its structural shell; opaque leaves reached through that shell
   stay untouched. Rationale: hot path, and it is user payload under the
   opaque-leaf clause, not package-interpreted state.
3. Provide a global option to disable stripping.
4. The change is normalization-at-admission, not a weakening of the sealed-box
   promise for user payload; document it with the scalar-name precedent.

## 3. Resolved implementation decisions

- **D1 — internal-tuning cargo callbacks.** Include `aggr` and `in_tune_fn`
  (they enter the same cargo admission as `custom_check`, at the same cold
  sites), for uniformity of the invariant "stored
  callbacks carry no srcrefs". `disable_in_tune` is a named *value list*, not
  a function: never touched.
- **D2 — the `repr` language carrier.** The captured constructor call
  (`Domain.R:215-231`) embeds the parsed `trafo`/`custom_check`/`depends`
  *expressions*; inline `function(x) …` expressions parsed with
  `keep.source = TRUE` carry their own srcref → srcfile chain. Capsules drop
  `repr`, but standalone Domains keep it, and an `ObjectTuneToken` carrying a
  Domain sits in `$values` and is serialized into tuning archives with that
  attribute. Strip the captured `param_repr` language **before**
  the printable-id computation (`Domain.R:233`), using the same helper, so the
  id string and later prints agree. Consequence to accept and pin: for
  srcref-carrying inline functions, `deparse`'s default `"useSource"` control
  means the rendered repr/id loses original comments/formatting (cosmetic;
  §6, §7.S13). Component normalization must test `NULL` before
  `is.pairlist()`: base R reports `is.pairlist(NULL)` as true, and assigning
  the unchanged `NULL` back with `reprargs[[index]] <- NULL` deletes that
  named representation element and invalidates the fixed traversal indices.
- **D3 — legacy upgrader normalization.** When the migration path carries
  legacy `.trafos`, `.extra_trafo`, `.constraint`, and cargo `custom_check`
  into current capsules (`R/upgrade_paradox_object.R:676`, `:687-688`, plus
  the Domain/cargo rebuild path), apply the same strip: migrated objects get the size
  benefit, and objects built fresh vs. migrated compare equal under
  `all.equal()`/hashing.
- **D4 — option name/default/read point.**
  `options(paradox.strip_srcrefs = FALSE)` to disable; default effective value
  `TRUE`; read via `isTRUE(getOption("paradox.strip_srcrefs", TRUE))` at each
  admission site (construction-time; flipping it later does not retro-process
  stored state — document this). Precedent: `paradox.legacy_object_action`.
  Document it as a **debugging aid** ("keep `setBreakpoint()`/source display
  working on stored callbacks; reconstruct the object after setting it"), not
  a general configuration knob, because it makes stored bytes — and therefore
  serialize-based hashes — session-setting-dependent.
- **D5 — helper implementation.** Use a hand-rolled internal helper
  (§4) rather than `utils::removeSource()`, for three reasons: (a) the
  no-copy-when-clean identity guarantee requires a read-only pre-scan that
  `removeSource()` does not provide; (b) exact, specified coverage of
  closures *and* language objects *and* formals defaults; (c) no new
  base-package import question.
- **D6 — differential coverage.** Add one differential case whose fixtures
  are built with `parse(keep.source = TRUE)` so the v1-keeps/v2-strips delta
  is pinned as an expected-differences row (§8). Without such a case the
  differential is srcref-blind (its cases run under `Rscript`, where
  `keep.source` is `FALSE`) and the change would be invisible to that gate.

## 4. Normative semantics of the strip helper

Two internal functions in `R/helper.R` (names indicative):

```
.paradox_has_srcref(x)    # read-only recursive scan; does not copy x
.paradox_strip_srcref(x)  # returns x unchanged, or a stripped copy
```

- **Target attributes:** exactly `srcref`, `srcfile`, and `wholeSrcref`.
  No other attribute is added, removed, or reordered.
- **Coverage (recursive):** for a closure — the closure's own attributes, the
  complete body AST (every pairlist/call node), and every formals default
  expression. For a language object (D2) — every node of the expression. The
  scan and the strip must cover the same node set, so the post-condition is:
  `.paradox_has_srcref(.paradox_strip_srcref(x))` is `FALSE`.
- **Identity guarantee:** if the scan finds nothing (the overwhelmingly common
  case: package-defined callbacks are installed without source references),
  return the input itself — **no copy**. This keeps the ordinary ecosystem
  path at one failed source-attribute scan per syntax node and preserves
  pointer identity for srcref-free callbacks.
- **Environment guarantee:** the stripped copy keeps the *same* environment
  object (`environment(copy)` is pointer-identical to
  `environment(original)`). This is not `crate()`; captured state is
  untouched.
- **Bytecode:** rebuilding the body of a byte-compiled closure drops its
  compiled body; the stored copy is recompiled by the JIT on use. Behavior
  and default `identical()` (which ignores bytecode) are unaffected; do not
  attempt to preserve or re-compile bytecode in the helper.
- **Non-closures:** atomic values, `NULL`, and anything that is neither a
  closure nor a language object pass through unchanged; type validation stays
  where it is today (the helper validates nothing).
- **Option-off:** with `paradox.strip_srcrefs = FALSE` the strip function
  returns its input unchanged (scan may be skipped).
- **Double application** is a no-op with identity (second call finds nothing,
  copies nothing) — relied on by the Shadow write-through path (S6).

These identity, environment, and option-off guarantees describe the strip
helper and ordinary current-object admission. Authenticated legacy detached
collection wrappers are always reconstructed in a fresh environment so their
captured carrier lists can be rebound safely. The option controls whether
source metadata is retained in that fresh wrapper; it cannot make migration
reuse the serialized wrapper or its environment.

The stripping helper and every current-object admission site are R-only:
stripping happens in the thin wrappers before the `.Call`, which is sanctioned
language-level capture work. Consequence to accept: a direct `.Call` with an
unstripped callback bypasses stripping — harmless, because this is a
size/normalization feature, not a safety invariant.

Implementation discovered one necessary cold migration exception to the
original no-C-change expectation. An authenticated Paradox-1 detached
collection crate captures a list of ParamSet carriers. R cannot distinguish an
ordinary list from a VECSXP ALTREP without observing its length machinery.
`C_upgrade_carrier_list_snapshot` therefore performs only a structural
non-ALTREP/non-S4/nonobject check and a shallow list-shell snapshot before R
counts or traverses those dependencies. It is not a second stripping or
migration engine. This small registered-routine addition changes the DSO and
native inventory, so strict compiler/registration/static checks are required
for this batch and the frozen candidate must regenerate its source-bound
native/analyzer evidence.

## 5. Code map (entry points verified 2026-07-26 at HEAD `590c6dd`)

Strip **before** the value is embedded, captured into cargo, or passed to a
native constructor:

| Site | Location | What to strip | Notes |
|---|---|---|---|
| S1 | `R/ParamUty.R:4-27` | `custom_check` (after its existing validation at `:8-16`), plus D1: `aggr`, `in_tune_fn` | before cargo assembly at `:22-25` |
| S2 | `R/ParamDbl.R:3-11` | `trafo`, D1 cargo | before the `Domain(...)` call; note `logscale && trafo` is already mutually exclusive (`domain_construct.c:1340-1342`). Package-generated integer logscale closures are normalized at their factory return for keep-source installs. |
| S3 | `R/ParamInt.R:10-19` | same as S2 | |
| S4 | `R/ParamFct.R:13-50` | **critical:** strip the user `trafo` *before* `.make_p_fct_trafo(levels, trafo)` embeds it (`ParamFct.R:31`; factory at `:1-11` captures it via `force(trafo)`) | stripping the stored factory closure would NOT reach the captured user function — it would survive inside the factory's environment and keep dragging the srcfile |
| S5 | `R/ParamLgl.R` | `trafo`, D1 cargo | same pattern as S2 |
| S6 | `R/ParamSet.R:1195-1210` | `f` in the `extra_trafo` and `constraint` active bindings, before `.Call(C_param_set_set_callback, private, self, f, 0L/1L)` | this one site also covers `ps(.extra_trafo=, .constraint=)` (`R/ps.R:72-73` routes through these bindings) and `ParamSetShadow$extra_trafo` write-through (`R/ParamSetShadow.R:184-195` assigns `origin$extra_trafo`). `ParamSetCollection` callbacks are read-only (`R/ParamSetCollection.R:403-427`) — no site |
| S7 | `R/to_tune.R:165-224` | `aggr`, after `assert_function` (`:170`), before `content$aggr = aggr` (`:224`) | the search-space conversion later copies `tt$content$aggr` into cargo (`:285-287`) — already stripped by then |
| S8 (D2) | `R/Domain.R` representation-capture block | non-`NULL` closure, language, and pairlist components of `reprargs`, before the fresh `param_repr` call is assembled and passed to `C_domain_simple_repr_id` | component-wise normalization has the same recursive coverage as scanning the fresh carrier, but avoids walking every node of an ordinary callback-free constructor call; `NULL` must bypass `[[<-` so the named element is not deleted |
| S9 (D3) | `R/upgrade_paradox_object.R:676` (`.trafos` list), `:687-688` (`extra_trafo`, `constraint`), plus the legacy-cargo `custom_check`/`aggr`/`in_tune_fn` carry site (locate in the Domain/cargo preparation helpers) | migrated callbacks | keep this inside the *preparation* phase (before offside construction), never in the commit wave |
| S10 | `R/ParamSetShadow.R:7-18`; `R/ParamSetCollection.R:8-62,317-336` | package-generated Shadow/Collection constraint and transformation adapters, plus the flattened internal-tuning namespace adapter | normalize the returned closure because keep-source package builds can attach package source metadata after the original user callback has already been normalized |

Placement alternative considered: a single strip inside `Domain()` for
`trafo` + cargo would reduce S1–S3/S5 to one site, but S4 is required
regardless (pre-embed), and per-constructor stripping keeps the rule visible
at each public boundary. Either layout is acceptable; do not do both for the
same object path (harmless but confusing). Whichever is chosen, the invariant
under test is §7.S1–S5, not the layout.

## 6. Watch-outs (read before writing code or tests)

1. **Test fixtures must manufacture srcrefs explicitly.** `R CMD check`,
   `Rscript`, and the release harness run with `keep.source = FALSE`, so
   `function(x) x` in a test file has no srcrefs and every stripping test
   would pass vacuously. Build fixtures with
   `eval(parse(text = "function(x) {\n# comment\nx\n}", keep.source = TRUE))`
   and, for size assertions, `srcfilecopy("big.R", <large character vector>)`
   so the srcfile drag is real and measurable. Add one guard expectation that
   the fixture *does* carry srcrefs before asserting they are gone.
2. **Assert stripping via attributes, never via `identical()`.** Default
   `identical()` has `ignore.srcref = TRUE`, so it cannot distinguish stripped
   from unstripped. Use the recursive scan helper in tests; use
   `data.table::address()` for the identity/no-copy assertions.
3. **Deparse/print effects.** `deparse`/`print` use `"useSource"` by default,
   so any *stored* function or (D2) repr that carried srcrefs now renders as
   canonical deparse (comments/formatting lost). Ordinary package-defined
   callbacks are unaffected in ordinary release installs. Keep-source package
   builds can attach metadata to generated factory closures, which S10
   normalizes. Pin with a snapshot test; mention in NEWS.
4. **The one real behavior change:** after construction, `debug(f)` /
   `setBreakpoint()` on the user's original function no longer affects the
   stored copy (today they are the same object). This is the sentence for
   NEWS and the documented reason the D4 option exists.
5. **Uniformity or divergence.** Every admission path (S1–S9, including the
   upgrader if D3 = yes) must strip, or logically identical objects built via
   different paths stop being `all.equal()`/hash-equal. The §7 matrix has a
   dedicated cross-path test.
6. **Never touch:** anything reached through `$values` (including functions
   as parameter values and TuneTokens already stored there), `ParamUty`
   payloads, `special_vals`/`default`/`init` leaves, condition `rhs`,
   `Design$data`, and user inputs to `$trafo(x)`/checks (only *stored state*
   is normalized, never operation inputs or results).
7. **Option semantics:** read at admission; no retro-processing; stored bytes
   differ across sessions with different settings (hash consequence —
   document, keep the option positioned as debugging-only).
8. **covr sanity:** downstream packages' coverage of their own `custom_check`s
   is expected to be unaffected (instrumentation lives in injected body code,
   which the strip preserves; only srcref *attributes* are removed — but note
   covr-instrumented functions do carry srcrefs, so the stored copy diverges
   from the instrumented original only in attributes). Spot-check one
   downstream consumer row with coverage during the consumer gate rather than
   reasoning it out.
9. **`sys.function()` inside a running callback** returns the stripped stored
   copy; anything downstream that re-deparses it sees canonical deparse. No
   known consumer does this; covered by the general "implementation frames
   are not contracts" clause.

## 7. Test matrix (new file `tests/testthat/test-srcref-stripping.R`)

All fixtures per §6.1. "SR" = srcref-carrying fixture, "clean" = ordinary
function without srcrefs.

- **S1–S5 per-site stripping:** for each of `p_uty(custom_check=)`,
  `p_dbl(trafo=)`, `p_int(trafo=)`, `p_lgl(trafo=)`, and D1 `aggr`/
  `in_tune_fn`: construct with SR input; assert the stored callback (via
  `$params` cargo / `$domains` / capsule accessors) has no srcref anywhere
  (recursive scan) and still computes the same results as the SR original.
- **S4 embed coverage:** `p_fct(levels = list(a = 1, b = 2), trafo = SR)`;
  assert the *embedded* user trafo inside the stored factory closure's
  environment is stripped (reach it via `environment(stored_trafo)$trafo`),
  and mapping + user trafo still compose correctly.
- **S6 setters:** `ps$extra_trafo <- SR`, `ps$constraint <- SR`,
  `ps(.extra_trafo = SR, .constraint = SR)`, and Shadow write-through
  `shadow$extra_trafo <- SR` (assert the *origin's* stored callback is
  stripped); collection-derived live/detached callbacks over a child with SR
  callbacks expose only stripped state.
- **S7:** `to_tune(..., aggr = SR)` token content is stripped;
  `$search_space()` conversion carries the stripped `aggr` into the result's
  cargo.
- **S8 (if D2):** standalone Domain `repr` attribute language carries no
  srcrefs; printable id/`print()` snapshot pinned; an `ObjectTuneToken`
  holding such a Domain serializes without the fixture's srcfile text.
- **S9 (if D3):** a legacy fixture (or synthesized legacy shell) with SR
  callbacks migrates to stripped current state; fresh-vs-migrated objects are
  `all.equal()`.
- **Generated adapters:** live and detached Shadow/Collection constraint and
  transformation adapters carry no source metadata, including a Shadow over a
  Collection; a flattened prefixed internal-tuning callback is clean and still
  translates parameter names correctly.
- **Identity/no-copy (ordinary current admission):** clean input ⇒ stored callback is pointer-identical
  (`data.table::address`); SR input ⇒ different address, same environment
  address, `identical()` (defaults) still `TRUE` vs the original.
- **Recursion completeness:** SR fixture with a nested inner
  `function() …` in the body and a function-valued formals default; scan
  finds nothing after strip (including `wholeSrcref`/`srcfile` on the body).
- **Bytecode:** `compiler::cmpfun(SR)` input ⇒ stripped, callable, equal
  results.
- **Option off (ordinary current admission):** `paradox.strip_srcrefs = FALSE` (set locally with
  `withr`-style scoping/`on.exit`) ⇒ stored callback keeps srcrefs and
  pointer identity with the input; flipping the option after construction
  changes nothing already stored.
- **Legacy option off:** an authenticated detached collection wrapper is
  rebuilt in a fresh environment without stripping its source metadata; the
  serialized input and captured carrier bindings remain byte-for-byte
  unchanged.
- **Size:** with a `srcfilecopy` fixture of ~10^5 characters,
  `length(serialize(ps, NULL))` shrinks by at least the srcfile payload vs.
  option-off construction of the same set.
- **Values untouched:** a function assigned as a `p_uty` *value* and inside
  `special_vals`/`default` keeps srcrefs and pointer identity.
- **Round trips:** clone/`serialize()`/`unserialize()` keep the stripped
  state; no path re-introduces or re-strips (addresses stable across reads).
- **Differential (D6):** one case in `compat/differential/cases.R` with
  `parse(keep.source = TRUE)` fixtures + its `expected-differences.tsv` row.

Roughly 14–16 `test_that` blocks. Also extend one existing collection and one
shadow characterization test with an SR callback to prove no interaction with
dormant-values activity filtering (constraint still receives the filtered
active subset; stripping happens at admission, filtering at evaluation —
orthogonal).

## 8. Documentation and contract edits (same change set)

- `design/contract-first-2.0.0.md`: amend the callback-identity sentence
  (§1 framing); add srcref attributes to the representation-only-metadata
  admission rule alongside scalar names; note the option.
- `design/architecture.md`: one paragraph in "Closed kinds and callbacks"
  (next to the existing crate/cmpfun paragraph, which already explains why
  environments are minimal by construction) naming the helper, the sites, and
  the no-copy-when-clean rule.
- `design/compatibility.md`: one row in the intentional-changes table
  ("Stored callbacks retain srcref attributes" → "admission discards
  srcref/srcfile/wholeSrcref from stored callbacks; option
  `paradox.strip_srcrefs=FALSE` restores; `$values` unaffected") including
  the `debug()` note.
- `NEWS.md`: user-facing bullet under the 2.0.0 native-state section: smaller
  serialized objects, hash stability across sessions/files, the `debug()`
  caveat, the option, and the explicit "`$values` functions are not touched;
  use `removeSource()` or `options(keep.source = FALSE)` for those".
- Roxygen: document the option where the callbacks are documented
  (`Domain`/`p_uty` templates, `extra_trafo`/`constraint` field templates)
  plus one central mention in the package-level help, following the
  `paradox.legacy_object_action` pattern; regenerate `man/`.
- `benchmarks/`: no new workload required; verify the constructor and
  value-mutation rows stay within their tiers (expected effect: one failed
  attribute lookup per callback on clean paths — noise).

## 9. Process checklist

1. Implement helper + S1–S7 (+S8/S9 per D2/D3), tests, docs. Focused loop:
   parse/static harness checks → `test-srcref-stripping.R` + touched suites →
   strict compiler/registration checks for the cold carrier snapshot. The
   complete unit/native/analyzer matrix belongs to the later frozen candidate,
   not this in-flight batch.
2. Land in the same pre-freeze batch as the other converging package-facing
   items; it does not get its own candidate. The replacement candidate's full
   matrix (native, runtime,
   differential with the new row, memory, downstream, documentation,
   benchmark, portability) covers it.
3. Downstream: no bridge changes expected (no maintained consumer reads
   srcrefs of stored callbacks); the consumer corpus rerun is the check, plus
   the §6.8 covr spot-check.
4. User-performed tracker updates after the batch lands: close #395 with the
   scoped resolution ("custom_check/trafo/extra_trafo/constraint(+cargo):
   fixed at admission; function *values*: wontfix by design — remedies:
   `removeSource()`, `options(keep.source = FALSE)`, or the future value
   owner strips before assignment"), and close #393 + PR #394 as obsolete
   (default `custom_check` has been `NULL` since 1.0.0; v2 synthesizes no
   default; PR targets the removed R6 `Param` layout).

## Appendix: verified facts this plan relies on

- `p_uty(custom_check = NULL)` stores `NULL`; nothing is synthesized
  (`R/ParamUty.R:22`; `function(x) TRUE` absent from `R/` and `src/`; C
  admits `function_or_null` at `src/domain_construct.c:439` and skips `NULL`
  at `src/paramset_check.c:668-673`).
- `logscale` and user `trafo` are mutually exclusive for numeric constructors
  (`src/domain_construct.c:1340-1342`), so only `p_fct` embeds a user
  function into a package factory (`R/ParamFct.R:1-11,31`).
- `ParamSetCollection` `extra_trafo`/`constraint` are read-only
  (`R/ParamSetCollection.R:403-427`); Shadow `extra_trafo` writes through to
  the origin's BASE setter (`R/ParamSetShadow.R:184-195`); `ps()` routes its
  `.extra_trafo`/`.constraint` through the BASE setters (`R/ps.R:72-73`).
  Hence S6 is a single choke point.
- User-supplied BASE callback assignment enters one native mutation entry
  (`.Call(C_param_set_set_callback, …, 0L/1L)`, `R/ParamSet.R:1195-1210`).
- Package-generated Shadow/Collection adapters and collection-flattening cargo
  wrappers are separate S10 factory boundaries.
- Default `identical()` ignores srcrefs (`ignore.srcref = TRUE`), and
  `deparse`/`print` default control includes `"useSource"`.
- `R CMD check`/`Rscript` run with `keep.source = FALSE`; interactive
  sessions default to `TRUE` — the reason fixtures must parse with
  `keep.source = TRUE` and the reason real-world bloat comes from
  script-defined callbacks, not package-defined ones.
- The upgrader carries legacy callbacks at `R/upgrade_paradox_object.R:676`
  (`.trafos`) and `:687-688` (`extra_trafo`/`constraint`).
