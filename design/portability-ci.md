# Paradox 2 portability and CI policy

This policy applies to the contract-first candidate. Historical workflow runs
for the superseded compatibility-first source are not accepted evidence.

## Supported baseline

- R >= 3.6;
- portable ISO C99 through the toolchain selected by R;
- Linux x86-64, Windows x86-64, and macOS Apple-silicon ARM64;
- data.table >= 1.18.4 as an outward interoperability dependency;
- public R C APIs available at the selected supported runtime, except for the
  exact versioned old-runtime binding/promise compatibility entries described
  below.

The source must not rely on GNU-only C behavior, x86 floating-point details,
unaligned access, little-endian layout, pointer ordering, `long` width,
architecture-specific vector instructions, or private data.table APIs. Use
`R_xlen_t`, checked conversions, R's NA/NaN predicates, and portable math.

## Local supported-runtime matrix

Repository-local pinned prefixes exercise R 3.6.3, R 4.3.3, and R 4.5.2
independently of the development R 4.6.1 prefix. Pinned R 3.6.0, 4.0.0, and
4.2.0 headers are explicit compile/API axes, so support is not inferred only
from endpoint runtimes and every old-R preprocessor transition is compiled:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
scripts/test-runtime-matrix --runtime all \
  --source-ref refs/paradox-release/candidate-YYYYMMDDTHHMMSSZ \
  --run-id release-runtime-YYYYMMDDTHHMMSSZ
scripts/verify-runtime-matrix-evidence \
  --run-id release-runtime-YYYYMMDDTHHMMSSZ
```

The three stages use isolated libraries and caches and may overlap only when
the memory-aware resource report admits the workers. Nested make, testthat,
parallel/future, BLAS, and OpenMP pools remain one.

R 3.6.3 uses the exact conda runtime lock plus a separately authenticated
source-package library from
`environment/runtime-r-3.6.3-packages.lock`. That snapshot-compatible closure
includes the package imports, test framework, and focused test support without
changing the prefix or host libraries. R 4.3 has no authenticated conda-forge
R-4.3 build of data.table 1.18.4. That stage therefore authenticates the source row in
`environment/r-packages-linux-64.lock`, copies the cached exact archive into its
retained inputs, installs it into the fresh stage library, verifies the resolved
version, and then builds Paradox. R 4.5.2 already contains the exact minimum.
This overlay never changes the host, persistent runtime prefix, or package
minimum.

Each stage must discover the current test inventory, run supported contract
tests, audit the DSO's undefined R symbols against the public API plus the exact
exception ledger, and seal source, build,
library, inputs, commands, logs, compiler, package, and session information.
Historical fixed file/skip/expectation counts are removed; a narrow exclusion
must name a current unsupported runtime capability and be independently
validated.

## R API discipline and non-forcing compatibility facade

Shipped C compiles against the pinned R 3.6.0, 4.0.0, 4.2.0, 4.3.0, 4.4.0,
4.5.2, and development headers. Version adapters live in
`src/r_api_compat.c` and may
select equivalent APIs, but may not select a different semantic engine. For
older supported headers, the adapters use the public, documented `FORMALS` and `ATTRIB`
backports for `R_ClosureFormals`, `ANY_ATTRIB`, `R_getAttribCount`, and
`R_hasAttrib`; raw stored-attribute selection uses `R_mapAttrib()` on R >= 4.6
and that established `ATTRIB` traversal on R 3.6--4.5. They do not evaluate
R-level `formals()`/`attributes()` helpers or call data.table code.
Code also must not assume that an R API predicate has the same signed integer
typedef in every supported header. A predicate retained in C state is
normalized by an explicit truth comparison (for example, `Rf_isObject(x) !=
FALSE`); the old-header GCC/Clang matrix rejects implicit signedness
conversions.

The remaining post-3.6 API differences are public and local: fresh raw/complex
destinations use public vector access before the element-setter declarations
appear, and the documented default `identical()` flags are represented by
their old-header value before `IDENT_USE_CLOENV` is named. Collection parameter
reads pass their already admitted, rooted core directly to the shared params
loader, eliminating the temporary environment and any need for `R_NewEnv()`.
These adapters are inline or remove work; they do not slow the current path.

The only exceptions to the public-API rule are the exact versioned entries
needed to inspect a stored environment binding or already reached promise
without forcing it. R 3.6--4.5 has no public non-forcing classifier for one
binding. The public R-level `substitute()` workaround is non-forcing but
insufficient for simultaneous generation/TuneToken receipt scans and recursive
object-graph migration: it returns a promise expression, not an unambiguous
binding-kind/generation receipt.
The runtime matrix therefore exercises the registered native plain-binding
classifier with realized and delayed literal logical, NULL, language, symbol,
environment, closure, and external-pointer values. In particular, a realized
language object or symbol must remain a direct value while a promise carrying
an expression of the same type remains delayed; expression/type shape must
never stand in for a binding-type query.
The pinned R 4.5 Writing R Extensions manual explicitly says that detailed
delayed-binding information is unavailable in the API. The R 4.6 manual labels
`R_GetBindingType` experimental and continues to classify
`Rf_findVarInFrame` as too low-level for the API; R 4.6 `tools` includes the
latter in its warned non-API symbols. Thus this exception is a reviewed
supported-runtime compromise, not a CRAN allowlist justification.

R 3.6--4.1 also lacks the public `R_existsVarInFrame()`. Optional, cold
absence-tolerant lookup uses `base::exists(..., inherits = FALSE)`, whose
implementation does not invoke active bindings; mandatory admitted ordinary
bindings stay on the allocation-free classifier. On newer R, the ordinary
frame path remains allocation-free. Callers keep one conservative rooting
proof across all supported facade branches because hostile class metadata can
allocate during admission, and recognized callback-backed user databases are
rejected before binding inspection. The terminal optional receipt scan
cannot evaluate or allocate. It therefore uses the header-declared/exported
`R_HasFancyBindings()` only on R 3.6--4.1 to reject a fancy frame before
reading a stored cell. This is an exact old-runtime exception, not a general
environment-layout API. The facade first rejects
`UserDefinedDatabase` environments with the public inheritance predicate R
itself uses: their callback-backed object table is outside the supported
ordinary-frame boundary, and old `R_HasFancyBindings()` assumes a hash-vector
layout that they do not have.

Consequently, the R < 4.6 branch in `src/r_api_compat.c` calls the
header-declared/exported `Rf_findVarInFrame` once and may inspect a returned
`PROMSXP` through
the header-declared/exported `R_PromiseExpr`, `PRENV`, and `PRVALUE`. It never
forces the promise.
R >= 4.6 uses the documented experimental binding classifier plus delayed/
forced-binding expression/environment accessors and does not compile the
`Rf_findVarInFrame` path. The public dots API covers `...`; strict headers hide
all three detached-promise accessors, so the facade compiles none of them on
R >= 4.6 and a `PROMSXP` reached outside a binding/dots cell is opaque.

R 3.6 exposes neither `R_ActiveBindingFunction()` nor an equivalent R
accessor. Direct and recursive legacy ParamSet-family migration fail closed
when active-binding inspection is required, with an instruction to load and
migrate the object under R >= 4.0. Paradox-1 ParamSet-family R6 shells
themselves contain active bindings, so practical ParamSet/Collection object and
graph migration requires R >= 4.0. Current Paradox-2 operations and idempotent
current-object conversion remain supported on R 3.6. Exact built-in current
shells also retain recursive traversal through their authenticated capsule;
their package active facades are opaque because unsupported in-place
replacement cannot be distinguished when exact shell receipts are preserved.
Such a binding is never invoked. Additive shells and modifications that fail
exact authentication instead fail closed. Standalone legacy Domain/Condition
conversion and graphs without arbitrary active bindings remain supported.

`environment/r-api-exceptions.tsv` is the exact ledger. It records each
exceptional symbol, source, raw-token count, version branch, and rationale; the
pinned-header and real-runtime matrices exercise both sides. These symbols are
not CRAN-allowlisted. R 3.6--4.1 inventories additionally contain the exact
old-only `R_HasFancyBindings` scan entry; R 3.6--4.5 DSO inventories contain
the ledgered old binding/promise set, while R >= 4.6 inventories must exclude
`Rf_findVarInFrame`, include the documented experimental binding entries
selected by the crawler, and exclude `R_PromiseExpr`, `PRENV`, and `PRVALUE`.
The
facade does not permit another internal API, caller, forced promise, or
different semantic engine.
`native-check --mode symbols`, the old-runtime matrix, and the retained-evidence
validator derive their expected undefined-symbol inventories from that
versioned ledger; do not retain the superseded one-symbol hard-coded policy.

The registered routine table has fixed arities and dynamic lookup disabled.
Symbol audits reject too-new R APIs in older-runtime DSOs, any unledgered
internal R API, any binding/promise count/path/version-branch mismatch, and any
private data.table symbols. Direct probes exercise each registered routine on
valid and malformed inputs.

R did not expose VECSXP ALTREP classes before R 4.3. The package's adversarial
list-ALTREP fixture is compiled and run only where that facility exists;
atomic ALTREP fixtures still execute on R 3.6. Production list-ALTREP behavior
is not skipped on old R—the corresponding object kind cannot exist there, so
the semantic branch is vacuous.

## Floating-point portability

Comparisons, tolerances, quantiles, exponential transformations, and integer
conversion receive cross-architecture boundary tests. Do not require fused
multiply-add contraction or a particular intermediate rounding result. Avoid
tests that compare stochastic floating output bit-for-bit when the public
contract is tolerance-based, but keep exact tests for deterministic categorical
selection, integer bounds, NA/NaN/Inf classification, and RNG state restoration.

Apple ARM64 previously exposed an FMA-sensitive defect in different source.
Fresh candidate evidence must exercise the focused floating-point boundary
tests and real hardware CI. The historical correction is guidance, not
evidence.

## GitHub portability workflow

The release workflow checks the exact frozen candidate ref on at least:

- Windows release R, x86-64, with the matching Rtools;
- macOS release R on a pinned Apple-silicon runner label;
- Linux release/development R as appropriate for package-check integration.

Every platform job records checked-out ref/commit/tree and verifies that
provenance before building. Shell and R steps use failure-propagating wrappers:
R errors, failed `R CMD check`, missing logs, or absent final `Status: OK` must
terminate the job nonzero. The matrix has `fail-fast: false`, so every required
platform runs. A separate always-run completion job waits for the entire matrix
and rejects its aggregate `needs` result unless it is exactly `success`; the
offline acceptance verifier additionally enumerates and requires success from
each individual REST job and required step. The completion gate must not use an
R expression such as
`if (Sys.getenv(...) )` without strict logical parsing; the earlier false-green
`missing value where TRUE/FALSE needed` incident is a permanent harness
regression case.

CI uploads source/check artifacts and logs even on failure, but artifact upload
steps use `if: always()` without masking the preceding command's conclusion.
The acceptance verifier checks job conclusions, log terminal status, source
identity, artifact hashes, and required manifests rather than trusting the
workflow's top-level green mark alone.

## Windows-specific checks

- strict registration and DLL loading with no unresolved symbols;
- C99 compilation under the selected Rtools GCC;
- path, encoding, line-ending, temporary-directory, and file-lock behavior;
- no POSIX-only shipped code or shell dependency in package execution;
- serialization and data.table facade behavior;
- complete package check with examples/tests/vignettes relevant to Windows.

## macOS ARM64-specific checks

- Apple Clang C99 warning-clean compile and link;
- ARM64 floating/quantile/tolerance boundary suite;
- alignment-safe object access and checked integer widths;
- no x86-only compiler flags or intrinsic assumptions;
- current serialization, callbacks, stable/base ALTREP materialization, hostile
  custom-ALTREP safety (no replay or Paradox-caused crash/corruption),
  direct-assignment rejection versus the sole `set_values(.values=)` shell
  snapshot, typed-special ALTREP/pointer-S4 and ParamUty base-`identical()`
  boundaries, exact BASE Object-token receipts and sealed search capabilities,
  ordinary non-ALTREP/non-S4 structural shell rejection, documented tables with
  one shared strict suffix classifier, no prefix-induced ordinary-shell copy,
  ALTREP-snapshot canonicalization without dispatch, malformed
  class-vector rejection, suffix-classified top-level
  ALTREP shells, owned names
  under reentry, strict cache carriers, count-only ordinary/stable-ALTREP row
  names, zero-column Design semantics, semantic ALTREP columns, data.table
  facades, and package check;
- no temporary source-tree detritus that makes a check falsely dirty.

## Sanitizers and analyzers

The package-DSO ASan and UBSan profiles use the reviewed Clang toolchain and
remain separate; strict warning builds cover both GCC and Clang. GCT and
Valgrind focus on R object lifetime; rchk covers protection, allocation, and
registration statically. Long-vector/arithmetic and corrupt capsule tests are
platform-neutral requirements, not Linux-only analyzer extras.

No sanitizer flag or analyzer suppression may weaken ordinary semantics. Every
suppression is source- and tool-version-specific, minimal, documented, and
reviewed. Zero exit without parsed reports is insufficient.

## Remote-write boundary

Agents may prepare workflow changes, local tags/refs only when explicitly
authorized, and PR-ready downstream commits. They do not push branches, publish
tags, dispatch remote workflows, or open PRs. The user performs each remote
write manually. After publication, agents may read and verify the resulting CI
run and artifacts.

## Acceptance

Portability is accepted only when the local R matrix, pinned-header/symbol and
exact exception-ledger audits, Windows x86-64, and real macOS ARM64 rows all
name the same candidate source and their independent verifiers pass. Any source
change affecting C, registration, R wrappers, tests, build configuration, or
portability harness reopens the corresponding rows.
