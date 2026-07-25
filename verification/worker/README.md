# Worker image recipe

This recipe supplies only the ordinary Linux commands used around the
repository-mounted, lockfile-built `.local/toolchain`. It deliberately does
not copy the checkout or install R packages.

Use a Debian-compatible base by exact digest:

```sh
podman build \
  --build-arg 'BASE_IMAGE=debian@sha256:REPLACE_WITH_REVIEWED_DIGEST' \
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

Do not build this image merely to work around a failed containment probe.
Rootless Podman on cgroup v1 will still ignore resource controls regardless of
the image contents. It is usable for parallel workers only when the complete
controller/container tree is independently enclosed by the proved aggregate
systemd backend described in `verification/README.md`; individual Podman
limits remain disabled in that mode.
