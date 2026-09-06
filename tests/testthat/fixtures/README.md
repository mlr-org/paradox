# Serialized compatibility fixtures

`paradox-1.0.1-sampler-unif.rds.b64` is the base64 encoding of an actual
gzip-compressed RDS produced with Paradox 1.0.1:

```r
library(paradox)
saveRDS(
  SamplerUnif$new(ps(x = p_dbl(0, 1))),
  "paradox-1.0.1-sampler-unif.rds"
)
```

The decoded RDS SHA-256 is
`986335bcddd991854aab155d6a38779284206f8c6c13bb2c587104ca647e65a2`.
It is text-encoded only so the fixture remains reviewable and patchable in the
source repository. The focused test uses a base-R decoder and therefore adds
no package or old-runtime dependency.

`paradox-1.0.1-sampler-1d-legacy-formals.rds.b64` contains actual Paradox
1.0.1 `Sampler1DRfun`, `Sampler1DCateg`, and `Sampler1DNormal` shells:

```r
library(paradox)
pd = ps(x = p_dbl(0, 1))
pf = ps(f = p_fct(c("a", "b")))
rfun = Sampler1DRfun$new(pd, stats::runif, trunc = FALSE)
categ = Sampler1DCateg$new(pf)
normal = Sampler1DNormal$new(pd, mean = 0.5, sd = 0.1)
# `rfun` was a documented mutable public field in Paradox 1. Replacing the
# self-capturing constructor closure keeps this source fixture small while
# retaining the exact Normal R6 hierarchy and serialized method stubs.
normal$rfun = stats::rnorm
saveRDS(
  list(rfun = rfun, categ = categ, normal = normal),
  "paradox-1.0.1-sampler-1d-legacy-formals.rds",
  version = 2
)
```

The decoded RDS SHA-256 is
`b988490baa354c2a32191f61d0c8e22422ed98509d3b40a71b5f793f02477b75`.
These three shells retain the Paradox 1 private stubs whose
`Sampler1D$as_dt_col(x)` and
`Sampler1DRfun$sample_truncated(n, rfun)` formals predate the extra arguments
used by their Paradox 2 implementations.
