# Native portability CI

The normal package check matrix treats portability as a release requirement,
not as a best-effort downstream check. It covers:

- release and development R on Linux x86-64;
- release R without Suggests on Linux x86-64;
- release R on Windows x86-64, using the suitable Rtools version selected by
  `r-lib/actions/setup-r`; and
- release R on the standard `macos-15` GitHub-hosted runner. GitHub documents
  that label as an Apple silicon ARM64 image; it is pinned instead of relying
  on a moving `macos-latest` architecture.

The matrix checks both GitHub's runner architecture and R's reported
architecture. Each entry then performs a clean source installation, loads the
resulting shared library, and verifies that registered `.Call` routines are
present before running the ordinary package check. This makes a missing native
build visible even if a future check configuration happens to reuse an
installed package.

The workflow follows the current major-version recommendations from the
[`r-lib/actions` examples](https://github.com/r-lib/actions/tree/v2/examples).
The runner labels and architectures are defined in GitHub's
[`actions/runner-images` inventory](https://github.com/actions/runner-images#available-images).
No compiler path or platform-specific compilation flags are overridden: R,
Apple Clang, and Rtools remain responsible for selecting their supported C17
toolchains.
