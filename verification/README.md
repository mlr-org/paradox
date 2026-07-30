# Verification coordinator

`scripts/verify` is the unattended front end for Paradox development and
release verification. It does not replace the semantic logic in
`scripts/native-check`, `scripts/memory-check`, the runtime matrix, or the
`compat/` evidence runners. It orders those existing gates as a resource-aware
DAG, gives each coarse task a deadline and resource contract, retains every
log, and decides which independent work remains informative after a failure.

The controller uses only Python 3.10+'s standard library. `scripts/verify` activates
the repository-local toolchain when it is installed, so it does not select the
host R installation. If the local toolchain predates its Python component, the
launcher falls back to a suitable system Python and otherwise gives one
bootstrap diagnostic.

## Start here

Inspect the machine before doing work:

```sh
scripts/verify doctor
scripts/verify plan --profile focused --since origin/main
```

A contained run also needs a pinned worker image that is already present in
the selected engine:

```sh
export PARADOX_VERIFY_WORKER_IMAGE='registry.example/paradox-worker@sha256:REPLACE_WITH_EXACT_DIGEST'
scripts/verify doctor
scripts/verify run --profile focused --since origin/main
```

The image is not pulled or built by a test task. It must provide a normal Linux
userland (`sh`, Bash, GNU coreutils, util-linux, tar, findutils, and procps)
compatible with the repository's mounted `.local/toolchain`. This includes ABI
compatibility between the worker libc, mounted R/libR, and the mounted Clang
sanitizer runtime; a newer libc is not presumed compatible. When ASan is
selected, `scripts/native-check` exercises the exact preloaded R startup and an
XDR serialization round-trip before any expensive native mode and seals the
one-line result. An incompatible image must be rebuilt and repinned, never
worked around by weakening isolation or silently moving that mode onto the
host. The checkout, toolchain, dependency libraries, and undeclared state are
read-only. Each task
gets private home, temporary, and runtime directories plus only the reviewed
`writable_paths` in `tasks.json`; downstream candidate, dependency, bridge, and
extra libraries are explicitly remounted read-only below their writable
evidence parent. Generic activation state and the runtime matrix's development
library, temporary, cache, and runtime subtrees are backed by attempt-private
mounts; retained prefixes, receipts, and sealed dependency libraries remain
read-only. The task entry pins `C.UTF-8`, UTC, and an empty `LANGUAGE` before
any reviewed command, so R parse/serialization metadata and text ordering do
not inherit an image's locale defaults. The network is disabled unless a reviewed task explicitly
requests it. A different platform/toolchain can instead use a task-specific
image plus overridden platform/machine constraints in `tasks.json`. All
currently shipped local tasks inherit the repository's Linux/x86-64 toolchain
constraint; native Windows and macOS release evidence remains the hosted
workflow's responsibility. The planner reports this boundary before execution.

The `native-release` task additionally consumes the prepared
`.local/configspace` tree read-only and remains networkless. Provisioning is
an explicit pre-task step owned by `scripts/bootstrap` and
`scripts/bootstrap-configspace`, not by reticulate, uv, a resolver, or the
worker. The full native driver authenticates the exact current and old
Python/ConfigSpace prefixes before and after testing and passes their
interpreters by exact environment-variable path. A missing or incomplete
profile fails closed; it is never repaired or deleted by verification.

Its current-R package checks construct their repository metadata inside the
attempt. The source-bound helper authenticates the exact locked source-archive
closure, adds the candidate archive, writes and seals the standard CRAN
indexes, and provides a separate empty local Bioconductor index. Thus the
worker does not contact package repositories but still runs recursive
dependency-cycle analysis. Remote-current repository and clock evidence are
left to hosted checks; local warnings are neither suppressed nor whitelisted.

`verification/worker/Containerfile` is a portable starting point when no
organization worker image exists. Build it as an explicit preparation step
from a digest-pinned Debian-compatible base; the resulting image digest—not its
mutable tag—is the value supplied to the controller. The worker recipe records
the exact glibc 2.31 Bullseye base validated with the current mounted
R/Clang-ASan combination and explicitly supplies plain, non-symbolic
compatibility copies for the authenticated `/usr/bin` command set when the
base is unmerged. Another platform must establish its own matching userland
rather than substitute an arbitrary newer base. Package/image downloads and
builds remain outside test execution.

`make verify-doctor`, `make verify-plan`, `make verify-smoke`,
`make verify-focused`, `make verify-compat`, `make verify-harness`,
`make verify-release-core`, `make verify-downstream`, `make verify-reverse`,
`make verify-documentation`, and `make verify-release-compat` are
conveniences.
Make does not schedule jobs: the controller must own heterogeneous CPU, memory,
PID, and scratch reservations. Pass additional arguments with `VERIFY_ARGS`.

### One-time aggregate-containment setup on this host

This host has rootless Podman on cgroup v1. It cannot enforce useful limits on
each rootless container, but a system service can enforce one hard ceiling
around the controller and every local Podman child. The repository therefore
ships a narrow root-owned launcher. After reviewing its source and example
configuration, install them manually:

```sh
sudo install -d -o root -g root -m 0755 /usr/local/libexec
sudo install -o root -g root -m 0755 \
  verification/systemd/paradox-verify-systemd \
  /usr/local/libexec/paradox-verify-systemd
sudo install -o root -g root -m 0644 \
  verification/systemd/paradox-verify-systemd.conf.example \
  /etc/paradox-verify-systemd.conf
```

Inspect `/etc/paradox-verify-systemd.conf` before use. Its checked-in values
are specific to this checkout and user, and the file deliberately holds only
what genuinely needs root: machine identity (repository, uid/gid, home) and
the safety rails (`memory_max_mib` as the self-service ceiling,
`memory_reserve_mib`, `cpu_reserve`, `tasks_max`, `runtime_max_sec`).
Everything that churns is user-owned state below `.local/verify/`:

- `worker-image.pin` holds the immutable reference of the locally built
  worker image; `scripts/verify` exports it as
  `PARADOX_VERIFY_WORKER_IMAGE` (an explicit environment value still wins)
  and the launcher's existing `--worker-image` override carries it into the
  service, so a rebuilt image digest never needs a root reinstall. Leave
  `worker_image` empty in the root configuration.
- `systemd-tunables.conf` may set `memory_target_mib` to spend up to — never
  beyond — the root-pinned `memory_max_mib` ceiling and must stay above the
  protected reserve. The root launcher parses it as data with the same
  strict grammar as its own configuration, requires it to be owned by the
  configured user, and fails closed on anything else. An absent file means
  "use the full ceiling".

Root intervention therefore remains for exactly three things: installing or
updating the reviewed launcher itself, changing machine identity, and
raising the rails. Routine harness development, image rebuilds, and capacity
tuning within the rails are unprivileged.

The installed helper applies only bounded, fixed systemd properties as root.
It then uses systemd's `--uid`/`--gid` boundary before any mutable repository
code executes. Never run the checkout copy itself with `sudo`. The installed
copy refuses to operate unless it and its configuration are root-owned and not
writable by group or other.

Interactive use needs no sudoers change. If unattended invocations must enter
the service without a password prompt, review and install the narrow example:

```sh
sudo visudo -cf verification/systemd/paradox-verify-systemd.sudoers.example
sudo install -o root -g root -m 0440 \
  verification/systemd/paradox-verify-systemd.sudoers.example \
  /etc/sudoers.d/paradox-verify-systemd
```

Once the helper is installed, ordinary `scripts/verify doctor|plan|run`
commands from the configured checkout enter the aggregate envelope
automatically on this machine. Other checkouts do not silently use this
checkout-specific helper.
`PARADOX_VERIFY_SYSTEMD_DEFAULT=off` bypasses that machine-local default, and
`scripts/verify-systemd ...` requests it explicitly. `scripts/verify
self-test` always remains unprivileged and daemon-free.

The supplied policy allows up to 48 GiB but clamps every invocation to
current `MemAvailable` minus the larger of 12 GiB and 25% of available
memory, so the protected outside reserve holds even when the target is
generous. It reserves two online CPUs, so this 32-CPU machine gets a 30-CPU
quota, and limits the complete process tree to 8,192 tasks. The clamp is
evaluated by the root-owned helper immediately before creating the service;
it is not a promise that 48 GiB is always available. The effective target
within that ceiling is the user-owned `memory_target_mib` tunable; only the
ceiling and reserves live in the root-owned configuration. A machine still
carrying an older configuration keeps a valid, merely smaller envelope (and
ignores the tunables file) until `/etc/paradox-verify-systemd.conf` and the
launcher are reinstalled once from the refreshed examples.

The root helper exists only because this host runs the hybrid cgroup-v1
hierarchy, where an unprivileged user cannot create a memory-limited
service at all. The structural endgame is booting the machine with
`systemd.unified_cgroup_hierarchy=1` (cgroup v2): systemd then delegates
the memory/pids controllers to the user manager, `systemd-run --user` can
create the envelope without any root helper, and rootless Podman gains real
per-worker limits that this controller's per-worker containment mode
already knows how to prove. That is one boot-parameter change plus a
reboot; on this shared machine it needs coordination with the
administrator, and the controller's aggregate probe would need to learn the
user-manager unit shape before the helper can be retired, so it is recorded
here as the intended direction rather than assumed.

## Hard containment is proved, not inferred

Engine selection is `PARADOX_CONTAINER_ENGINE=auto|podman|docker`, or
`--engine`. Containment selection is `--containment
auto|worker|aggregate`. `auto` prefers proved per-worker limits when available
and otherwise uses a proved aggregate service with local rootless Podman.
`worker` and `aggregate` require the named mechanism. Docker is never accepted
for aggregate containment because its daemon can place payloads outside the
controller's service. The presence of a CLI is not enough.

For per-worker containment the controller:

1. records client/server, cgroup, rootless, architecture, and storage identity;
2. runs a 64-MiB sacrificial container with CPU, PID, memory, and no-swap
   ceilings;
3. reads the effective cgroup values inside that container;
4. proves the real worker UID can read the read-only checkout and write only a
   nested writable bind (with explicit SELinux `label=disable`);
5. requires an over-limit allocation to be reported as `OOMKilled`.

Every real worker in this mode receives
`--memory`, equal `--memory-swap`, `--cpus`, `--pids-limit`, a bounded
`--shm-size`, no implicit pull, no privilege escalation, and dropped
capabilities.

For aggregate containment the controller instead proves all of the following:

1. it is in a dedicated
   `/system.slice/paradox-verify-aggregate-u<uid>-*.service` leaf;
2. the memory, CPU, PID, and systemd controller paths agree and have finite
   leaf and effective ceilings;
3. systemd reports the expected unprivileged user/group, `Type=exec`,
   accounting, `OOMPolicy=kill`, `OOMScoreAdjust=1000`,
   `KillMode=control-group`, and `Delegate=no`;
4. swap is disabled through `MemorySwapMax=0` on cgroup v2, or the host has no
   swap at all on cgroup v1 (a visible legacy `memsw` file is not enough);
5. a real rootless Podman payload remains in the exact service cgroup and
   satisfies the same read-only-checkout/nested-writable-bind sandbox.

Aggregate workers deliberately omit ineffective individual memory, CPU, and
PID flags and use `--cgroups=disabled --cgroupns=host`. Their manifest
allocations are scheduler reservations inside one hard envelope, not separate
hard task limits. This permits multiple independent checks to run in parallel
without pretending cgroup v1 can isolate them individually. The worker entry
point and nested `resource-jobs` calls cooperatively cap Make/test waves by
the assigned CPU and RAM. Schema-2 resource reports record their containment
mode explicitly, and the aggregate policy applies only after the process
proves it sits inside the dedicated service cgroup: consumer rows then use
the measured 2-GiB cooperative weight with an eight-row cap, light-test jobs
the 1-GiB weight, and cooperative profiles a 4-GiB intra-envelope reserve
floor, because the root-owned launcher already withheld the host reserve when
sizing the hard ceiling. Ordinary live/direct admission retains the
conservative 8-GiB estimate, 16-GiB reserve floor, and four-row cap. Thus a
16-GiB coarse allocation can run eight ordinary rows when CPU permits without
weakening the aggregate hard ceiling. The nested planner's live
headroom-derived check remains an independent fail-closed guard that shrinks
later waves as sibling usage grows. An increase in a
memory/PID event counter, a changed limit or cgroup path, or loss of the
authenticated unit contract aborts the whole run and terminates every sibling.
Successful artifacts from completed siblings remain available for an exact
resume.

`TasksMax` contains a fork storm within the transient service, but it does not
identify or kill one offending worker. Complete PID saturation can therefore
delay Podman cleanup subprocesses; the controller fails the run, and
systemd's `KillMode=control-group` removes the remaining service processes when
the controller exits. An operator can also stop the reported transient unit
directly if a pathological process prevents timely teardown.

In both hard modes, the inspected image is executed by its immutable image
ID/digest, never by the possibly mutable name supplied on the command line.
Summed reservations must fit the current hard envelope and the global
`MemAvailable` outside the protected host reserve; the reserve is subtracted
only once. Free disk, memory, PID availability, and aggregate-cgroup state are
recomputed before new waves and at a bounded cadence during a run. Crossing a
protected boundary is a fatal infrastructure event. One per-user machine lock
prevents controllers from different run IDs or checkouts from independently
spending the same aggregate budget. A new run also removes stale same-user
labelled workers left by an abruptly killed older controller before admitting
work.

Without the installed aggregate launcher, this development host's rootless
Podman/cgroup-v1 configuration remains correctly classified as unable to
enforce per-worker limits. With the launcher, `doctor` must report
`scope=aggregate`, the exact service cgroup, and a hard aggregate ceiling
before parallel work is admitted. The environment marker alone is never
accepted as proof.

For small development diagnostics only, an explicit fallback exists:

```sh
scripts/verify run --profile smoke --best-effort
```

Best-effort mode runs one coarse task at a time, gives each process an
`RLIMIT_AS`, watches aggregate Linux process-tree RSS, retains the large host
reserve, and still lets the selected task use its allotted CPUs internally.
It is not a memory cgroup: a fork tree and page cache can escape those
per-process mechanisms. Its receipts say `best-effort-host`, and it is never
release evidence.

## Scheduling and failure policy

`verification/tasks.json` contains reviewed argv arrays, dependencies, phases,
input scopes, impact patterns, time estimates, and CPU/RAM/PID/scratch
requests. There are no shell command strings. The scheduler:

- completes lower phases before admitting later expensive phases;
- puts a directly impacted task ahead of a generic task in the same phase;
- then favors a previous exact-key failure and information per estimated time;
- admits ready tasks continuously while all resource sums fit;
- admits a task at its reviewed CPU/RAM minimum on a smaller machine, then
  gives spare capacity to higher-priority peers up to their reviewed ceiling;
- sets nested BLAS/OpenMP/testthat parallelism to one by default and gives
  Make only the CPUs reserved for that coarse worker; a reviewed driver may
  re-enable parallel testthat for the Paradox suite itself within the same
  assigned envelope (the full native check does this for its `--as-cran`
  test run through an explicit light-test admission);
- records a separate append-only attempt log for every retry/resume.

The default `adaptive` policy finishes independent peers in the current phase,
blocks descendants of a failed prerequisite, and does not start later
expensive phases after a non-advisory failure. `keep-going` runs unrelated
later branches, while `fail-fast` cancels outstanding work after the first
failure. A `fatal` task or host/cache/provenance corruption aborts every policy.
An ordinary nonzero exit, timeout, contained OOM, infrastructure failure,
cancel, and dependency block remain distinct results.

Static CPU, physical/cgroup RAM, filesystem, cgroup PID, platform, and machine
ceilings decide whether a task can ever fit. Live free-resource budgets decide
only whether it can start now. A static incompatibility blocks that task and
its descendants but is not an executed failure, so independent work continues
under every policy; temporary pressure instead enters the bounded wait path.
Static fit applies the configured RAM reserve fraction to machine/cgroup
capacity; live admission recomputes it from current availability, so a
pressure-heavy startup cannot freeze the run at a permanently low budget.
`plan` labels every such task `unsupported` and exits nonzero when the selected
profile is not fully runnable.

Profiles are intentionally coarse:

- `smoke`: changed-file-oriented harness/native probes;
- `harness`: every deterministic harness fixture, without real consumers;
- `focused`: default internal development validation;
- `compat`: focused validation plus the offline Paradox-1 differential;
- `release-core`: fresh native/API/runtime/differential foundation for a clean
  frozen ref;
- `prepared-downstream`: exact focused and broad consumer gates for an already
  installed and authenticated candidate overlay on either reviewed axis;
- `prepared-reverse`: real priority-zero/one pinned reverse dependencies for a
  prepared Paradox-2 candidate;
- `prepared-documentation`: the real all-scope documentation/configuration
  workload gate for a prepared Paradox-2 candidate;
- `prepared-release-compat`: all three real prepared compatibility branches in
  one keep-going DAG, so independent evidence survives an ordinary peer
  failure.

Live cgroup-aware RAM, disk, and PID budgets are refreshed before a new wave
and at a bounded cadence while workers run. Temporary outside pressure waits
with bounded backoff instead of becoming a false dependency deadlock. Crossing
a protected reserve while work is active still aborts immediately.

Impact mapping changes development selection/order only. It never reduces a
release profile. `release-core` requires a clean tree and resolves
`source_ref^{commit,tree}` before planning; it must equal the captured HEAD
commit/tree, so runtime evidence cannot silently test a different source than
native, API, and differential gates.

```sh
scripts/verify run --profile release-core \
  --param source_ref=refs/paradox-release/CANDIDATE \
  --worker-image "$PARADOX_VERIFY_WORKER_IMAGE"
```

## Loosening resource allotments after a limit event

The scheduler is deliberately aggressive inside the aggregate envelope, and
the envelope is a tripwire: on cgroup v1 even a successfully reclaimed limit
contact increments `memory.failcnt`, which the controller treats as a fatal
infrastructure event. When a run dies that way, or a row is OOM-killed, do
not weaken the envelope's outside protection; raise the one allotment that
was actually too tight and start a replacement run. Diagnose first:

- **Aggregate memory/PID event, whole run invalidated.** The summary names
  the event counter. The aggregate usage approached the hard ceiling, so the
  cooperative estimate of at least one concurrently running task was too low.
  Identify the heavy task from the attempt logs, then raise that one task's
  `memory_mib_min`/`memory_mib` in `verification/tasks.json` (a larger
  reservation admits fewer siblings next to it). Reservation sums for the
  phase-30 compatibility branches are pinned by `verification/test_harness.py`;
  run `scripts/verify self-test` after editing.
- **One row killed inside a wave (exit 137 / killed by signal 9) while the
  run survived.** A single consumer/test row exceeded its cooperative weight
  badly. Either raise the aggregate per-row weight — `consumer` 2048 MiB or
  `light-test` 1024 MiB in `scripts/environment/resource-jobs` — or lower the
  wave width for that gate only with the lowering-only knobs below. A weight
  change must be applied in lockstep to the re-derivation validators
  (`compat/repository-runner.R`, `compat/reverse-runner.R`,
  `scripts/native-check`, `scripts/environment/validate-native-source-run`,
  `scripts/environment/run-compiler-batch`, `run-compiler-wave`) and their
  self-tests (`test-resource-jobs`, `test-verification-economy`, the compat
  self-tests); the structural tests fail loudly on any partial edit.
- **A task is blocked as `unsupported` or waits forever on admission.** Its
  reviewed minimum no longer fits the live budget. Lower the task's
  `cpu_min`/`memory_mib_min` (after proving the inner driver degrades safely
  there) or raise the envelope.
- **`resource-jobs` fails closed** (`insufficient memory for one ... job`).
  Outside the envelope this is the direct policy protecting the host — free
  memory or use the envelope. Inside the envelope it means the service is
  genuinely near its ceiling; let running work finish or raise the envelope.

Lowering-only per-gate knobs (never raise the derived ceiling):
`PARADOX_REVERSE_JOBS` (reverse waves), `PARADOX_RUNTIME_MATRIX_JOBS`
(runtime stages, 1–4), `PARADOX_API_JOBS` (header syntax compiles),
`PARADOX_NATIVE_COMPILE_JOBS` / `PARADOX_NATIVE_TEST_JOBS` (native install
and both direct and `--as-cran` testthat parallelism), and each helper's
`--max-jobs`.

Raising the envelope target within the root-approved ceiling: edit
`memory_target_mib` in the user-owned
`.local/verify/systemd-tunables.conf` — no root involved. Only raising the
ceiling itself (`memory_max_mib`) or changing `memory_reserve_mib` requires
reinstalling the root-owned `/etc/paradox-verify-systemd.conf` from
`verification/systemd/paradox-verify-systemd.conf.example`. The launcher's
live clamp keeps the outside reserve intact regardless of the target. Never
mirror a bigger target by weakening `memory_reserve_mib` below what
interactive sessions on this host need.

## Cache and resume

Development success entries live below
`.local/verify/cache/results/<task>/<key>/`. A key covers:

- the reviewed task specification and controller bytes;
- every content-hashed declared input;
- dependency keys and only the semantic parameters consumed by that task;
- reviewed OS/architecture, toolchain content, hard-backend/cgroup generation,
  immutable worker-image content, and the activated repository R-library tree.

Input patterns prefixed with `?` declare optional package payloads such as
`configure`, `inst/`, or `data/`: absence is valid, while creating or changing
the path changes the key. Exclusions are evaluated relative to the checkout,
so an ancestor directory named `.local` does not silently remove source files
from the receipt.

Mutable differential baseline names are resolved to their offline mirror
commit before keying. Dynamic engine-capacity observations, timestamps, and
generated/attempt run IDs are not semantic inputs. Cache validation
requires exactly one authenticated receipt and log; missing, modified,
symbolic, or unexpected paths fail closed. Failures are retained in history to
raise their next-run priority, but never become success hits.

Kernel release, engine version/path/storage driver, live capacity, aggregate
unit name/path, and local repository aliases attached to the same immutable
image are invocation observations, not test semantics. They are freshly
proved and retained on each machine without invalidating an otherwise
identical hard-worker plan. Per-worker and aggregate hard containment remain
different semantic backends. The
explicit non-release best-effort backend remains keyed to its exact host
platform and controller Python because it lacks that containment boundary.

The cache is an optimization, not evidence transfer. Release/prepared profiles
disable generic result reuse and publication. Their existing source-bound gate
receipts remain authoritative. The R-library tree remains in task identity
even for those profiles because coordinator resume can reuse a successful row
independently of generic cache policy.

Use a stable run ID to resume an interruption:

```sh
scripts/verify run --profile focused --run-id my-run \
  --worker-image "$PARADOX_VERIFY_WORKER_IMAGE"
scripts/verify run --profile focused --run-id my-run --resume \
  --worker-image "$PARADOX_VERIFY_WORKER_IMAGE"
```

Resume accepts only the same manifest, task keys, parameters, and backend/image
identity. A successful row is reused only when `result.json` is byte-identical
to its numbered immutable result and authenticates its exact attempt log and
original invocation receipt. A revalidation task and its full descendant
closure run again, including when the retained coordinator completion was
already successful. Incomplete or failed rows
receive a new attempt log, private HOME/tmp state, and attempt-specific child
run ID, so retained output from an interrupted child cannot make its retry fail
immediately. The focused
downstream driver also receives that attempt ID for its private directories;
if its candidate-wide public stage was published just before interruption, the
retry verifies and reports that stage instead of rerunning or overwriting it.
Every executed attempt also retains `attempt-NNN.result.json` before updating
the latest `result.json`. Each coordinator invocation writes a hashed current
host/engine receipt, so moving a resumable plan to a larger machine is visible
without making momentary host capacity part of semantic plan identity.
Capacity/dependency/policy blocks are retained with an explicit origin and do
not pollute the exact-key execution-failure priority history.

## Memory analyzers remain a separate release gate

`release-core` deliberately does not wrap `scripts/memory-check --mode all`.
That driver combines ordinary GCT/Valgrind work with rchk, which launches its
own pinned Podman image. Running it inside the generic read-only,
capability-dropped worker would require nested Podman and would apply the inner
16-GiB host reserve a second time. A nominal `memory-release` task would
therefore be either unrunnable or misleading.

After a successful release foundation, run the existing source-bound memory
gate directly using the `native-release` result's `child_run_id`. Its own
admission/evidence contract remains authoritative. The intended coordinator
refactor is to put GCT/Valgrind in an ordinary hard worker and make the pinned
rchk image the outer worker, with no nested engine. Until that refactor exists
and is tested, do not describe the memory gate as container-contained by this
coordinator.

```sh
scripts/memory-check --mode all \
  --source-run NATIVE_RELEASE_CHILD_RUN_ID \
  --run-id MEMORY_RUN_ID
```

## Frozen downstream context

The downstream profile does not accept a loose collection of paths and hashes.
Copy `candidate-context.example.json` below `.local`, fill its exact frozen
candidate/overlay fields, and pass:

```sh
scripts/verify plan --profile prepared-downstream \
  --candidate-context .local/verify/candidate-context.json
```

Before scheduling, the controller verifies the clean detached source
ref/commit/tree, the run-owned candidate library, content sentinel, provenance
TSV and seal, and the distinct dependency library. For selected consumer or
documentation tasks it additionally authenticates the run-owned bridge and
the relevant extra-library roles. It then exports one consistent context to
the existing verifiers, which independently reauthenticate their boundaries.

The detached source path is the exact commit-owned
`.local/compat/candidate-snapshots/<commit>` directory, and any bridge library
is candidate-run-owned. Context validation is selection-aware: a reverse-only
profile authenticates only the base candidate/dependency tuple and therefore
does not require an unrelated consumer or documentation overlay.

`extra_libraries` contains only ordinary consumer additions (currently the
reviewed mlr3verse hard-import library). `documentation_libraries` contains
documentation-only dependencies. Documentation receives bridge, consumer, and
documentation libraries in that exact order; repository consumers never see
the documentation-only layer. Paths containing the platform path separator
are rejected because they cannot be flattened and remounted unambiguously.

The real reverse and documentation tasks opt into
`scripts/activate-compat-system` inside the worker. Native, API, runtime, and
differential workers retain ordinary activation. These two real gates are
currently Linux x86-64 gates because their authenticated compatibility-system,
TinyTeX, and toolchain artifacts are Linux x86-64; hosted macOS and Windows
jobs remain separate evidence.

Reverse verification first runs a fresh real miesmuschel plan-only preflight,
then keeps one stable full-stage child ID. Attempt one requires an empty
reservation; retries require an authenticated reservation marker and use a
durable initialized marker as the only semantic-resume boundary. An
interruption before that boundary is reset safely, accepted rows/install cache
survive after it, and a completed task is revalidated on coordinator resume.
Documentation has no row-level resume and receives an attempt-specific ID;
its entire essential corpus runs before any advisory full render. Both drivers
accept only a controller-reserved exact output directory, leaving historical
sibling runs behind the read-only checkout mount. Their Linux/x86-64
requirement is declarative and appears during planning rather than as a late
wrapper surprise.

Under proved aggregate containment, the four independent phase-30 compatibility
branches have cooperative minima of 8 GiB for reverse dependencies, 8 GiB for
the repository corpus, and 4 GiB each for documentation and focused downstream
checks. Their 24-GiB memory, 6,144-PID, and 8-GiB scratch totals fit below the
ordinary aggregate budgets on the development host, so the controller may run
all four at once. Reverse and corpus tasks start at four CPUs and can expand
to 16 GiB and 16 CPUs when peers leave capacity, which the aggregate 2-GiB
consumer weight translates into up to eight concurrent rows per branch. These
figures are scheduler reservations, not claims
about measured peaks; the aggregate cgroup and live disk-reserve monitor remain
the hard safety boundaries, and any memory/PID event or protected-disk pressure
invalidates the run. The `resource-jobs consumer` profile retains its
conservative direct-run policy outside the envelope. The completed `4c4cb53`
diagnostic run used one row per wave because its 9,699-MiB corpus allocation
was divided by the historical 8-GiB assigned-envelope weight; the measured
aggregate peak was 10,670 MiB with zero OOM, memory-failure, or PID-limit
events. That evidence motivated first the 4-GiB and now the containment-aware
2-GiB cooperative row weight with the eight-row cap.

Both consumer runners also refill within a wave — a wave may plan up to
three rows per admitted worker while only the admitted count runs
concurrently, so a slow row no longer idles its siblings' slots — and both
order their queue heaviest-first from the measured `4c4cb53` single-row
durations, so the longest checks start immediately and the refill packs the
tail. The measured table is a scheduling hint inside the runners, never
evidence; an unhinted row keeps its reviewed inventory position. The
documentation gate deliberately stays serial: its retained workload labels
embed the execution-order index, every workload is bracketed by whole-tree
protected-input attestations, and its rows are fully hidden behind the
corpus/reverse branches in `prepared-release-compat`. Parallelizing it would
rename retained evidence for approximately zero critical-path gain; revisit
only if documentation ever becomes the phase-30 long pole. The runtime
matrix's supported stages run the package suite with a capability-probed
two-worker parallel testthat (testthat 3.2+ with callr in that stage's
sealed library); old runtimes keep the exact serial path, and the retained
stage log records the decision as `testthat_parallel`/`testthat_workers`.

Run the complete prepared compatibility DAG unattended with:

```sh
scripts/verify run --profile prepared-release-compat \
  --candidate-context .local/verify/candidate-context.json \
  --worker-image "$PARADOX_VERIFY_WORKER_IMAGE"
```

## Extending the DAG

Prefer a coarse existing evidence driver over reproducing its internals in the
controller. New tasks must declare bounded resources, a deadline, all semantic
input groups, change impacts, failure class, and whether generic development
success caching is sound. Use CPU/RAM minima only after proving that the inner
driver degrades safely at that allocation. Pulling images, provisioning
toolchains, and installing shared dependencies are serialized preparation
steps, not hidden test actions.

Run the daemon-free regression after changing the controller or manifest:

```sh
scripts/verify self-test
```

It covers manifest rejection, dependency closure, deleted-file selection,
change priority, weighted admission, adaptive failure collection,
exact-content cache invalidation and tamper rejection, pre-execution input
reauthentication, source-ref identity, attempt IDs, global/run locking,
rootless-cgroup-v1 rejection, per-worker inner-cgroup proof, aggregate v1/v2
leaf and systemd proof, aggregate event monitoring, non-duplicated reserve
accounting, strict exited-state and stale worker cleanup, root-relative
optional inputs, cgroup PID/live-memory budgets, adaptive resource ranges,
interrupt cleanup, narrow writable mounts, required container flags, and
actual parallel aggregate scheduling. It also covers static-versus-live
capacity, independent work after scheduler blocks, immutable
attempt/invocation receipts, platform constraints, and reserved-output
materialization.
