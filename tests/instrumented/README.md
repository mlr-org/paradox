# Instrumented compact-row-name root regression

This focused probe compiles three deterministic GC reachability barriers into
a disposable package build. Normal builds preprocess the three call sites to
`((void) 0)` and neither compile nor register the counter routine.

After `. scripts/activate`, run:

```sh
scripts/environment/test-gc-row-names-roots
```

The wrapper keys its reusable installation by the exact Git-visible package
inputs, R ABI and runtime files, public headers, compiler and build-tool
identity, strict Makevars and expanded flags, hard-dependency trees, and the
wrapper, receipt helper, and driver bytes. A cache hit is accepted only after
its completion seal and full installed-library tree receipt verify. The driver
still runs on every invocation; it requires the instrumented-only registered
counter, resets it between operations, and checks the exact two store-local,
two collection-local, and two carrier-root hits.

This cache is a fast development regression, not retained release evidence.
Release gates continue to use immutable source snapshots and fresh run IDs.
