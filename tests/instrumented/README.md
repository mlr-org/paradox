# Instrumented GC-root regressions

This focused probe compiles a deterministic capsule-graph GC reachability
barrier into a disposable package build. Normal builds preprocess the barrier
call site away and do not register its counter routine.

After `. scripts/activate`, run:

```sh
scripts/environment/test-core-graph-roots
```

The wrapper keys its reusable installation by the exact Git-visible package
inputs, R ABI and runtime files, public headers, compiler and build-tool
identity, strict Makevars and expanded flags, hard-dependency trees, and the
wrapper, receipt helper, and driver bytes. A cache hit is accepted only after
its completion seal and full installed-library tree receipt verify. The driver
still runs on every invocation; it requires the instrumented-only registered
counter routine, resets it before traversal, and checks exactly 21 deep
capsule-graph hits. Each barrier temporarily detaches the selected `.core`,
forces collection and pending finalizers, and requires the validator's
active-path carrier to keep that exact generation alive before restoring the
binding.

This cache is a fast development regression, not retained release evidence.
Release gates continue to use immutable source snapshots and fresh run IDs.
