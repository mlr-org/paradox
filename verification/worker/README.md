# Worker image recipe

This recipe supplies only the ordinary Linux commands used around the
repository-mounted, lockfile-built `.local/toolchain`. It deliberately does
not copy the checkout or install R packages. The ordinary command contract
includes ripgrep because structural harness checks use `rg` for literal and
regular-expression source-policy assertions, and a plain `/usr/bin`
compatibility surface because authenticated helpers deliberately name the
distribution's plain `/usr/bin` tools. On an unmerged base, the recipe installs
plain compatibility
copies of the small exact `/bin` command set used there. It does not use
symlinks because security-sensitive helpers deliberately reject symbolic
executables. This contract therefore does not depend on which Debian
generation supplied the base.

The image need only provide the `C.UTF-8` locale; task execution pins that
locale and UTC in `scripts/environment/verify-task-entry`. Do not rely on an
image's default locale, which differs between otherwise compatible Debian
bases and changes R's serialized parse metadata.

Use a Debian-compatible base by exact digest. The mounted project toolchain,
not packages copied into the image, supplies R and Clang. The base userland
must therefore be ABI-compatible with those binaries and with Clang's
preloaded sanitizer runtime. "Newer glibc" is not automatically compatible:
the previously used glibc 2.42 worker started ordinary R but crashed in R's
XDR lazy-load path under ASan before package code was loaded. On the current
Linux x86-64 machine the reviewed glibc 2.31 Bullseye base is:

```sh
podman build \
  --build-arg 'BASE_IMAGE=docker.io/library/debian@sha256:cba95a21c96c1f5fc2470081829363eed57706634f7dc26e8c6712934303d57a' \
  --tag localhost/paradox-verification-worker:local \
  --file verification/worker/Containerfile \
  verification/worker
podman image inspect localhost/paradox-verification-worker:local
```

Docker uses the equivalent `docker build` command. Image acquisition and this
build are explicit provisioning operations and may use the network; real
verification tasks always use `--pull=never` and default to `--network=none`.
Supply the immutable image ID (or a repository digest) returned by the engine:

```sh
export PARADOX_VERIFY_WORKER_IMAGE='sha256:...'
scripts/verify doctor
```

The controller inspects this reference and executes the resulting immutable
ID/digest with `--pull=never`; a mutable tag is never the execution identity.
Its active probe uses the real worker UID and the same read-only checkout plus
nested writable bind layout as task workers. SELinux labels are explicitly
disabled for these already authenticated local bind paths, so behavior does
not depend on a host's automatic relabel policy.

`scripts/verify doctor` proves containment and ordinary tool startup. A release
native run that selects ASan additionally performs an exact preloaded-R startup
and XDR `saveRDS()`/`readRDS()` round-trip before any expensive compiler mode.
That source-bound preflight is the authoritative sanitizer/userland
compatibility check and fails closed; do not weaken seccomp, capabilities,
namespaces, sanitizer options, or read-only mounts to work around it. Such
relaxations did not fix the glibc/XDR incompatibility and would reduce the
quality of otherwise valid evidence. Build and pin a compatible worker
instead.

Do not build this image merely to work around a failed containment probe.
Rootless Podman on cgroup v1 will still ignore resource controls regardless of
the image contents. It is usable for parallel workers only when the complete
controller/container tree is independently enclosed by the proved aggregate
systemd backend described in `verification/README.md`; individual Podman
limits remain disabled in that mode.
