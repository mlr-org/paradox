"""Cheap deterministic regressions for :mod:`verification.harness`."""

from __future__ import annotations

import dataclasses
import json
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

from verification import harness


def write_manifest(
    root: pathlib.Path,
    *,
    tasks: list[dict] | None = None,
    profiles: dict | None = None,
) -> pathlib.Path:
    (root / "input.txt").write_text("one\n", encoding="utf-8")
    tasks = tasks or [
        {
            "id": "base",
            "description": "base fixture",
            "command": ["true"],
            "dependencies": [],
            "phase": 0,
            "input_groups": ["base"],
            "impacts": ["src/**"],
        }
    ]
    profiles = profiles or {
        "test": {
            "description": "test profile",
            "tasks": [task["id"] for task in tasks],
            "always": [tasks[0]["id"]],
            "changed_only": False,
            "policy": "adaptive",
            "reuse_results": True,
            "publish_results": True,
            "require_clean": False,
            "require_hard_isolation": True,
        }
    }
    payload = {
        "schema": 1,
        "defaults": {
            "cpu": 1,
            "memory_mib": 128,
            "pids": 32,
            "scratch_mib": 32,
            "timeout_seconds": 30,
            "host_memory_reserve_mib": 1,
            "host_memory_reserve_fraction": 0.01,
            "host_disk_reserve_mib": 1,
            "engine_overhead_mib": 1,
        },
        "parameters": {},
        "input_groups": {"base": ["input.txt"]},
        "profiles": profiles,
        "tasks": tasks,
    }
    path = root / "tasks.json"
    path.write_text(json.dumps(payload), encoding="utf-8")
    return path


def task(
    task_id: str,
    command: tuple[str, ...],
    *,
    dependencies: tuple[str, ...] = (),
    phase: int = 0,
    score: float = 0,
    failure_class: str = "semantic",
) -> harness.PlannedTask:
    specification = harness.Task(
        task_id=task_id,
        description=task_id,
        command=command,
        dependencies=dependencies,
        phase=phase,
        priority=0,
        information=1,
        estimated_seconds=1,
        resources=harness.Resources(
            cpu=1,
            memory_mib=256,
            pids=32,
            scratch_mib=16,
            timeout_seconds=20,
        ),
        failure_class=failure_class,
        cache="none",
        input_groups=(),
        inputs=(),
        impacts=(),
        environment={},
        required_parameters=(),
        required_values={},
        isolation="worker",
        network=False,
        image=None,
        writable_paths=(),
        readonly_paths=(),
        platforms=(),
        machines=(),
        revalidate_on_resume=False,
    )
    return harness.PlannedTask(
        task=specification,
        command=command,
        environment={},
        image=None,
        image_identity=None,
        writable_paths=(),
        readonly_paths=(),
        impacted=False,
        input_digest="0" * 64,
        cache_key=(task_id.encode().hex() + "0" * 64)[:64],
        score=score,
    )


def aggregate_v1_fixture(
    root: pathlib.Path,
    *,
    memory_limit_mib: int = 28 * 1024,
    memory_usage_mib: int = 128,
) -> tuple[pathlib.Path, pathlib.Path, pathlib.Path]:
    """Create the hybrid-v1 layout used by this development host."""

    proc_root = root / "proc"
    cgroup_root = root / "cgroup"
    unit = f"paradox-verify-aggregate-u{os.getuid()}-fixture.service"
    cgroup_path = f"/system.slice/{unit}"
    (proc_root / "self").mkdir(parents=True)
    (proc_root / "self" / "cgroup").write_text(
        "\n".join(
            [
                f"10:pids:{cgroup_path}",
                f"9:memory:{cgroup_path}",
                f"6:cpu,cpuacct:{cgroup_path}",
                f"1:name=systemd:{cgroup_path}",
                f"0::{cgroup_path}",
                "",
            ]
        ),
        encoding="ascii",
    )
    (proc_root / "meminfo").write_text(
        "MemTotal:       67108864 kB\n"
        "MemAvailable:   41943040 kB\n"
        "SwapTotal:             0 kB\n",
        encoding="ascii",
    )
    relative = pathlib.Path(cgroup_path.lstrip("/"))
    memory = cgroup_root / "memory" / relative
    cpu = cgroup_root / "cpu,cpuacct" / relative
    pids = cgroup_root / "pids" / relative
    systemd = cgroup_root / "systemd" / relative
    unified = cgroup_root / relative
    for directory in (memory, cpu, pids, systemd, unified):
        directory.mkdir(parents=True)
    for directory in (memory, cpu, pids):
        (directory / "cgroup.procs").write_text(
            f"{os.getpid()}\n", encoding="ascii"
        )
    (memory / "memory.limit_in_bytes").write_text(
        f"{memory_limit_mib * 1024 * 1024}\n", encoding="ascii"
    )
    (memory / "memory.usage_in_bytes").write_text(
        f"{memory_usage_mib * 1024 * 1024}\n", encoding="ascii"
    )
    (memory / "memory.failcnt").write_text("0\n", encoding="ascii")
    (memory / "memory.oom_control").write_text(
        "oom_kill_disable 0\nunder_oom 0\noom_kill 0\n",
        encoding="ascii",
    )
    (cpu / "cpu.cfs_quota_us").write_text("3000000\n", encoding="ascii")
    (cpu / "cpu.cfs_period_us").write_text("100000\n", encoding="ascii")
    (pids / "pids.max").write_text("8192\n", encoding="ascii")
    (pids / "pids.current").write_text("1\n", encoding="ascii")
    (pids / "pids.events").write_text("max 0\n", encoding="ascii")
    return proc_root, cgroup_root, memory


class ManifestTests(unittest.TestCase):
    def test_valid_manifest_and_dependency_closure(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            tasks = [
                {
                    "id": "parent",
                    "description": "parent",
                    "command": ["true"],
                    "dependencies": [],
                    "phase": 0,
                    "input_groups": ["base"],
                    "impacts": [],
                },
                {
                    "id": "child",
                    "description": "child",
                    "command": ["true"],
                    "dependencies": ["parent"],
                    "phase": 1,
                    "input_groups": ["base"],
                    "impacts": ["src/**"],
                },
            ]
            manifest = harness.load_manifest(root, write_manifest(root, tasks=tasks))
            profile = manifest.profiles["test"]
            selected = harness.select_tasks(
                manifest, profile, ["src/x.c"], ["child"], False
            )
            self.assertEqual(selected, {"parent", "child"})

    def test_duplicate_json_key_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            path = root / "tasks.json"
            path.write_text('{"schema":1,"schema":1}', encoding="utf-8")
            with self.assertRaisesRegex(harness.HarnessError, "duplicate JSON key"):
                harness.load_manifest(root, path)

    def test_cycle_unknown_field_and_unsafe_input_are_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            tasks = [
                {
                    "id": "a",
                    "description": "a",
                    "command": ["true"],
                    "dependencies": ["b"],
                    "phase": 0,
                    "input_groups": ["base"],
                    "impacts": [],
                },
                {
                    "id": "b",
                    "description": "b",
                    "command": ["true"],
                    "dependencies": ["a"],
                    "phase": 0,
                    "input_groups": ["base"],
                    "impacts": [],
                },
            ]
            with self.assertRaisesRegex(harness.HarnessError, "cycle"):
                harness.load_manifest(root, write_manifest(root, tasks=tasks))

            path = write_manifest(root)
            payload = json.loads(path.read_text(encoding="utf-8"))
            payload["tasks"][0]["typo"] = True
            path.write_text(json.dumps(payload), encoding="utf-8")
            with self.assertRaisesRegex(harness.HarnessError, "unknown field"):
                harness.load_manifest(root, path)

            path = write_manifest(root)
            payload = json.loads(path.read_text(encoding="utf-8"))
            payload["tasks"][0]["inputs"] = ["../outside"]
            path.write_text(json.dumps(payload), encoding="utf-8")
            with self.assertRaisesRegex(harness.HarnessError, "unsafe"):
                harness.load_manifest(root, path)

            tasks = [
                {
                    "id": "late",
                    "description": "late",
                    "command": ["true"],
                    "dependencies": [],
                    "phase": 2,
                    "input_groups": ["base"],
                    "impacts": [],
                },
                {
                    "id": "early",
                    "description": "early",
                    "command": ["true"],
                    "dependencies": ["late"],
                    "phase": 1,
                    "input_groups": ["base"],
                    "impacts": [],
                },
            ]
            with self.assertRaisesRegex(harness.HarnessError, "phase precedes"):
                harness.load_manifest(root, write_manifest(root, tasks=tasks))

    def test_actual_unknown_name_drives_impact(self) -> None:
        item = dataclasses.replace(
            task("x", ("true",)).task,
            impacts=("src/**", "tests/testthat/test-param*.R"),
        )
        self.assertTrue(harness.task_impacted(item, ["src/param.c"]))
        self.assertFalse(harness.task_impacted(item, ["man/ParamSet.Rd"]))

    def test_deleted_files_are_change_inputs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            subprocess.run(["git", "init", "-q", str(root)], check=True)
            subprocess.run(
                ["git", "-C", str(root), "config", "user.name", "Fixture"],
                check=True,
            )
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(root),
                    "config",
                    "user.email",
                    "fixture@example.invalid",
                ],
                check=True,
            )
            source = root / "src" / "removed.c"
            source.parent.mkdir()
            source.write_text("one\n", encoding="utf-8")
            subprocess.run(["git", "-C", str(root), "add", "."], check=True)
            subprocess.run(
                ["git", "-C", str(root), "commit", "-qm", "fixture"], check=True
            )
            source.unlink()
            changed, _ = harness.changed_files(root, None)
            self.assertIn("src/removed.c", changed)

    def test_repository_manifest_wires_high_value_preflights(self) -> None:
        root = pathlib.Path(__file__).resolve().parents[1]
        manifest = harness.load_manifest(root, root / "verification" / "tasks.json")
        self.assertNotIn("memory-release", manifest.tasks)
        self.assertNotIn("memory-release", manifest.profiles["release-core"].tasks)
        self.assertEqual(
            manifest.tasks["harness-portability"].failure_class, "blocker"
        )
        self.assertIn(
            "harness-native", manifest.tasks["native-release"].dependencies
        )
        self.assertIn(
            "harness-runtime", manifest.tasks["runtime-supported"].dependencies
        )
        self.assertIn("reverse-dependencies", manifest.profiles["prepared-reverse"].tasks)
        self.assertIn(
            "documentation-compatibility",
            manifest.profiles["prepared-documentation"].tasks,
        )
        self.assertIn(
            "reverse-dependencies",
            manifest.profiles["prepared-release-compat"].tasks,
        )
        self.assertIn(
            "documentation-compatibility",
            manifest.profiles["prepared-release-compat"].tasks,
        )
        self.assertEqual(
            manifest.tasks["reverse-dependencies"].environment[
                "PARADOX_VERIFY_COMPAT_SYSTEM"
            ],
            "1",
        )
        documentation = manifest.tasks["documentation-compatibility"]
        self.assertEqual(
            documentation.environment["PARADOX_DOCUMENTATION_EXTRA_LIBS"],
            "{documentation_extra_libs}",
        )
        self.assertIn("{candidate_run_root}", documentation.readonly_paths)
        self.assertEqual(
            documentation.writable_paths,
            (".local/compat/runs/{attempt_id}",),
        )
        reverse = manifest.tasks["reverse-dependencies"]
        self.assertEqual(reverse.dependencies, ("reverse-preflight",))
        self.assertTrue(reverse.revalidate_on_resume)
        self.assertIn(
            ".local/compat/reverse-runs/{run_id}", reverse.writable_paths
        )
        self.assertEqual(
            manifest.tasks["harness-reverse"].command,
            ("scripts/environment/test-reverse-reserved-output-contract",),
        )
        self.assertEqual(
            manifest.tasks["reverse-preflight"].required_values["paradox_axis"],
            ("paradox2",),
        )
        self.assertEqual(
            manifest.tasks["downstream-overlay"].failure_class, "blocker"
        )
        for task_id in (
            "reverse-preflight",
            "reverse-dependencies",
            "documentation-compatibility",
            "downstream-overlay",
            "downstream-focused",
            "downstream-corpus",
        ):
            self.assertEqual(manifest.tasks[task_id].platforms, ("linux",))
            self.assertEqual(manifest.tasks[task_id].machines, ("x86_64",))
        self.assertEqual(manifest.tasks["native-focused"].platforms, ("linux",))
        self.assertEqual(
            manifest.tasks["native-focused"].machines, ("x86_64",)
        )
        self.assertEqual(documentation.resources.cpu, 1)
        focused = manifest.tasks["downstream-focused"]
        self.assertEqual(
            focused.environment["PARADOX_CONSUMER_EXTRA_LIBS"],
            "{consumer_extra_libs}",
        )
        self.assertEqual(
            focused.environment["PARADOX_VERIFY_ATTEMPT_ID"], "{attempt_id}"
        )
        self.assertIn("--resume", focused.command)
        self.assertEqual(focused.resources.memory_mib, 8192)
        smoke = manifest.profiles["smoke"]
        selected = harness.select_tasks(
            manifest,
            smoke,
            [".github/workflows/r-cmd-check.yml"],
            [],
            False,
        )
        self.assertIn("harness-portability", selected)
        selected = harness.select_tasks(
            manifest,
            smoke,
            ["tests/testthat/test-values.R"],
            [],
            False,
        )
        self.assertIn("native-focused", selected)
        selected = harness.select_tasks(
            manifest,
            smoke,
            ["scripts/environment/run-runtime-matrix-stage"],
            [],
            False,
        )
        self.assertIn("harness-runtime", selected)

    def test_resource_ranges_are_validated(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            tasks = [
                {
                    "id": "variable",
                    "description": "variable",
                    "command": ["true"],
                    "dependencies": [],
                    "phase": 0,
                    "cpu": 4,
                    "cpu_min": 2,
                    "memory_mib": 512,
                    "memory_mib_min": 256,
                    "input_groups": ["base"],
                    "impacts": [],
                }
            ]
            manifest = harness.load_manifest(root, write_manifest(root, tasks=tasks))
            resources = manifest.tasks["variable"].resources
            self.assertEqual(resources.minimum_cpu, 2)
            self.assertEqual(resources.minimum_memory_mib, 256)
            tasks[0]["cpu_min"] = 5
            with self.assertRaisesRegex(harness.HarnessError, "minima"):
                harness.load_manifest(root, write_manifest(root, tasks=tasks))

    def test_task_parameter_value_constraints_are_declarative(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            path = write_manifest(root)
            payload = json.loads(path.read_text(encoding="utf-8"))
            payload["parameters"] = {"axis": "paradox2"}
            payload["tasks"][0]["required_values"] = {
                "axis": ["paradox2"]
            }
            path.write_text(json.dumps(payload), encoding="utf-8")
            manifest = harness.load_manifest(root, path)
            harness.validate_required_parameters(
                manifest.tasks, {"base"}, {"axis": "paradox2"}
            )
            with self.assertRaisesRegex(
                harness.HarnessError, "reject parameter values"
            ):
                harness.validate_required_parameters(
                    manifest.tasks, {"base"}, {"axis": "paradox1"}
                )

    def test_platform_constraints_block_before_execution(self) -> None:
        unsupported_platform = "darwin" if sys.platform != "darwin" else "linux"
        item = task("platform-bound", ("true",))
        item.task = dataclasses.replace(
            item.task, platforms=(unsupported_platform,)
        )
        self.assertIn(
            "outside", harness.task_platform_incompatibility(item.task)
        )
        host = harness.HostResources(
            2, 4096, 4096, 1, 1, 2, 1024, 1024, "fixture"
        )
        with self.assertRaisesRegex(harness.FatalRunError, "platform"):
            harness.validate_task_fits(item, host)

    def test_release_ref_must_identify_exact_head(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            subprocess.run(["git", "init", "-q", str(root)], check=True)
            subprocess.run(
                ["git", "-C", str(root), "config", "user.name", "Fixture"],
                check=True,
            )
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(root),
                    "config",
                    "user.email",
                    "fixture@example.invalid",
                ],
                check=True,
            )
            (root / "x").write_text("one\n", encoding="utf-8")
            subprocess.run(["git", "-C", str(root), "add", "x"], check=True)
            subprocess.run(
                ["git", "-C", str(root), "commit", "-qm", "one"], check=True
            )
            first_commit = subprocess.check_output(
                ["git", "-C", str(root), "rev-parse", "HEAD"], text=True
            ).strip()
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(root),
                    "update-ref",
                    "refs/paradox-release/one",
                    first_commit,
                ],
                check=True,
            )
            first_tree = subprocess.check_output(
                ["git", "-C", str(root), "rev-parse", "HEAD^{tree}"], text=True
            ).strip()
            harness.validate_source_ref_identity(
                root, "refs/paradox-release/one", first_commit, first_tree
            )
            (root / "x").write_text("two\n", encoding="utf-8")
            subprocess.run(["git", "-C", str(root), "commit", "-qam", "two"], check=True)
            second_commit = subprocess.check_output(
                ["git", "-C", str(root), "rev-parse", "HEAD"], text=True
            ).strip()
            second_tree = subprocess.check_output(
                ["git", "-C", str(root), "rev-parse", "HEAD^{tree}"], text=True
            ).strip()
            with self.assertRaisesRegex(harness.HarnessError, "clean current HEAD"):
                harness.validate_source_ref_identity(
                    root,
                    "refs/paradox-release/one",
                    second_commit,
                    second_tree,
                )


class EngineTests(unittest.TestCase):
    def test_root_owned_systemd_launcher_has_fail_closed_properties(self) -> None:
        launcher = (
            pathlib.Path(harness.__file__).parent
            / "systemd"
            / "paradox-verify-systemd"
        ).read_text(encoding="utf-8")
        for contract in (
            "--uid=$uid",
            "--gid=$gid",
            "--working-directory=$repository",
            "--service-type=exec",
            "--property=MemoryMax=${cap_mib}M",
            "--property=CPUQuota=${cpu_quota_percent}%",
            "--property=TasksMax=$tasks_max",
            "--property=OOMPolicy=kill",
            "--property=OOMScoreAdjust=1000",
            "--property=KillMode=control-group",
            "--property=Delegate=no",
            "--setenv=PARADOX_VERIFY_AGGREGATE_SYSTEMD=1",
        ):
            self.assertIn(contract, launcher)
        self.assertNotIn(" --user ", launcher)
        self.assertIn(
            '"$repository/scripts/verify" "${verify_arguments[@]}"',
            launcher,
        )

    def test_worker_image_identity_ignores_local_repository_aliases(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            executable = root / "podman"
            base = {
                "Id": "sha256:" + "a" * 64,
                "Architecture": harness.platform.machine(),
                "Os": "linux",
            }
            inspections = [
                subprocess.CompletedProcess(
                    [str(executable)],
                    0,
                    stdout=json.dumps(
                        [{**base, "RepoDigests": ["one@sha256:" + "b" * 64]}]
                    ),
                    stderr="",
                ),
                subprocess.CompletedProcess(
                    [str(executable)],
                    0,
                    stdout=json.dumps(
                        [
                            {
                                **base,
                                "RepoDigests": [
                                    "one@sha256:" + "b" * 64,
                                    "two@sha256:" + "c" * 64,
                                ],
                            }
                        ]
                    ),
                    stderr="",
                ),
            ]
            with mock.patch.object(
                harness, "run_capture", side_effect=inspections
            ):
                first = harness._image_identity(
                    executable, "podman", "one", root
                )
                second = harness._image_identity(
                    executable, "podman", "two", root
                )
            self.assertEqual(first, second)

    def test_worker_image_must_match_host_toolchain_platform(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            executable = root / "podman"
            other_arch = (
                "aarch64"
                if harness.platform.machine().lower() not in {"aarch64", "arm64"}
                else "x86_64"
            )
            inspected = subprocess.CompletedProcess(
                [str(executable), "image", "inspect", "worker"],
                0,
                stdout=json.dumps(
                    [
                        {
                            "Id": "sha256:" + "a" * 64,
                            "Architecture": other_arch,
                            "Os": "linux",
                        }
                    ]
                ),
                stderr="",
            )
            with mock.patch.object(harness, "run_capture", return_value=inspected):
                with self.assertRaisesRegex(
                    harness.HarnessError, "incompatible with host toolchain"
                ):
                    harness._image_identity(executable, "podman", "worker", root)

    def test_rootless_podman_cgroup_v1_is_never_hard(self) -> None:
        data = {
            "host": {
                "cgroupVersion": "v1",
                "cgroupManager": "cgroupfs",
                "security": {"rootless": True},
            },
            "store": {"graphDriverName": "overlay"},
            "version": {"Version": "5", "APIVersion": "5", "OsArch": "linux/amd64"},
        }
        identity, eligible, reason, _ = harness._parse_engine_info(
            "podman", pathlib.Path("/usr/bin/podman"), json.dumps(data)
        )
        self.assertTrue(identity["rootless"])
        self.assertFalse(eligible)
        self.assertIn("cannot enforce", reason)

    def test_explicit_worker_mode_does_not_fall_back_to_aggregate(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            executable = root / "podman"
            executable.write_text("#!/bin/sh\n", encoding="ascii")
            executable.chmod(0o700)
            aggregate = harness.AggregateProbe(
                requested=True,
                hard=True,
                reason="fixture",
                identity={"layout": "v1"},
            )
            information = {
                "host": {
                    "cgroupVersion": "v1",
                    "cgroupManager": "cgroupfs",
                    "security": {"rootless": True},
                    "serviceIsRemote": False,
                },
                "store": {"graphDriverName": "overlay"},
                "version": {
                    "Version": "5",
                    "APIVersion": "5",
                    "OsArch": "linux/amd64",
                },
            }
            observed = subprocess.CompletedProcess(
                [str(executable), "info"],
                0,
                stdout=json.dumps(information),
                stderr="",
            )
            with mock.patch.object(
                harness, "run_capture", return_value=observed
            ):
                probe = harness.probe_engine(
                    root,
                    str(executable),
                    None,
                    active=False,
                    containment="worker",
                    aggregate=aggregate,
                )
            self.assertFalse(probe.hard)
            self.assertEqual(probe.limit_mode, "worker")
            self.assertIn("cannot enforce", probe.reason)

    def test_inner_cgroup_values_are_authoritative(self) -> None:
        ok, _ = harness._parse_limit_probe(
            "layout=v2\nmemory=67108864\nswap=0\npids=64\ncpu=50000 100000\n"
        )
        self.assertTrue(ok)
        ok, reason = harness._parse_limit_probe(
            "layout=v2\nmemory=9223372036854771712\nswap=0\n"
            "pids=64\ncpu=50000 100000\n"
        )
        self.assertFalse(ok)
        self.assertIn("not effective", reason)

    def test_container_command_has_hard_limits_and_no_pull(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            (root / ".local").mkdir()
            (root / ".cache").mkdir()
            run_root = root / ".local" / "verify" / "runs" / "r"
            (run_root / "tasks").mkdir(parents=True)
            probe = harness.EngineProbe(
                requested="podman",
                kind="podman",
                executable="/usr/bin/podman",
                usable=True,
                hard=True,
                reason="fixture",
                identity={},
                image="image@sha256:abc",
                image_identity="a" * 64,
                storage_driver="overlay",
            )
            item = task("one", ("true",))
            item.image = "image@sha256:abc"
            writable = root / ".local" / "evidence"
            protected = writable / "candidate"
            protected.mkdir(parents=True)
            item.writable_paths = (str(writable),)
            item.readonly_paths = (str(protected),)
            command, name = harness.container_command(
                root, probe, item, run_root, "r-one-a001", 1
            )
            retry_command, _ = harness.container_command(
                root, probe, item, run_root, "r-one-a002", 2
            )
            joined = " ".join(command)
            retry_joined = " ".join(retry_command)
            self.assertIn("--pull=never", command)
            self.assertIn("--network=none", command)
            self.assertIn("--memory 256m", joined)
            self.assertIn("--memory-swap 256m", joined)
            self.assertIn("--pids-limit 32", joined)
            self.assertIn("--cpus 1", joined)
            self.assertIn("--userns=keep-id", command)
            self.assertIn("--security-opt=label=disable", joined)
            self.assertIn(
                f"--label=org.mlr-org.paradox.verify.uid={os.getuid()}",
                command,
            )
            self.assertIn("/state/r-one-a001/", joined)
            self.assertNotIn("/state/r-one-a001/", retry_joined)
            self.assertIn("/state/r-one-a002/", retry_joined)
            self.assertIn("PARADOX_VERIFY_TASK_ATTEMPT=1", command)
            self.assertIn("PARADOX_VERIFY_TASK_ATTEMPT=2", retry_command)
            self.assertNotIn(
                f"type=bind,src={root / '.local'},dst={root / '.local'},rw",
                command,
            )
            probe_flags = harness._base_limit_flags("docker", "probe")
            self.assertIn(f"{os.getuid()}:{os.getgid()}", probe_flags)
            self.assertIn("--security-opt=label=disable", " ".join(probe_flags))
            self.assertLess(
                command.index(
                    f"type=bind,src={writable},dst={writable},rw"
                ),
                command.index(
                    f"type=bind,src={protected},dst={protected},ro"
                ),
            )
            self.assertTrue(name.startswith("paradox-v-"))

    def test_aggregate_worker_inheritance_and_command_contract(self) -> None:
        aggregate = harness.AggregateProbe(
            requested=True,
            hard=True,
            reason="fixture",
            identity={
                "layout": "v1",
                "cgroup_path": (
                    f"/system.slice/paradox-verify-aggregate-u{os.getuid()}"
                    "-fixture.service"
                ),
            },
        )
        path = aggregate.identity["cgroup_path"]
        output = (
            "__PARADOX_CGROUP_BEGIN__\n"
            f"10:pids:{path}\n"
            f"9:memory:{path}\n"
            f"6:cpu,cpuacct:{path}\n"
            f"1:name=systemd:{path}\n"
            f"0::{path}\n"
            "__PARADOX_CGROUP_END__\n"
        )
        inherited, reason = harness._aggregate_worker_inherits(
            output, aggregate
        )
        self.assertTrue(inherited, reason)
        escaped, _ = harness._aggregate_worker_inherits(
            output.replace(str(path), "/user.slice/escaped"), aggregate
        )
        self.assertFalse(escaped)

        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            (root / ".local").mkdir()
            (root / ".cache").mkdir()
            run_root = root / ".local" / "verify" / "runs" / "r"
            (run_root / "tasks").mkdir(parents=True)
            probe = harness.EngineProbe(
                requested="podman",
                kind="podman",
                executable="/usr/bin/podman",
                usable=True,
                hard=True,
                reason="fixture",
                identity={"cgroup_version": "v1"},
                image="sha256:" + "a" * 64,
                image_identity="b" * 64,
                storage_driver="overlay",
                limit_mode="aggregate",
                aggregate=aggregate,
            )
            item = task("one", ("true",))
            item.image = probe.image
            command, _ = harness.container_command(
                root, probe, item, run_root, "r-one-a001", 1
            )
            joined = " ".join(command)
            for option in ("--memory", "--memory-swap", "--cpus", "--pids-limit"):
                self.assertNotIn(option, command)
            self.assertIn("--cgroups=disabled", command)
            self.assertIn("--cgroupns=host", command)
            self.assertIn("--oom-score-adj=1000", command)
            self.assertIn("--read-only", command)
            self.assertIn("--security-opt=no-new-privileges", joined)
            self.assertEqual(
                harness.hard_backend_name(probe), "podman-aggregate-hard"
            )

    def test_probe_cleanup_failure_is_not_hidden_by_probe_timeout(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            executable = root / "podman"
            executable.write_text("#!/bin/sh\n", encoding="utf-8")
            executable.chmod(0o700)

            def fake_capture(command, **_kwargs):
                if command[1] == "info":
                    return subprocess.CompletedProcess(
                        command,
                        0,
                        stdout=json.dumps(
                            {
                                "host": {
                                    "cgroupVersion": "v2",
                                    "cgroupManager": "systemd",
                                    "security": {"rootless": True},
                                },
                                "store": {"graphDriverName": "overlay"},
                                "version": {
                                    "Version": "5",
                                    "APIVersion": "5",
                                    "OsArch": "linux/amd64",
                                },
                            }
                        ),
                        stderr="",
                    )
                if command[1:3] == ["image", "inspect"]:
                    return subprocess.CompletedProcess(
                        command,
                        0,
                        stdout=json.dumps(
                            [
                                {
                                    "Id": "sha256:" + "a" * 64,
                                    "RepoDigests": [],
                                    "Architecture": harness.platform.machine(),
                                    "Os": "linux",
                                }
                            ]
                        ),
                        stderr="",
                    )
                if command[1] == "ps":
                    return subprocess.CompletedProcess(
                        command, 0, stdout="", stderr=""
                    )
                raise harness.HarnessError("synthetic engine timeout")

            with mock.patch.object(harness, "run_capture", side_effect=fake_capture):
                with mock.patch.object(
                    harness, "_remove_container", return_value=False
                ):
                    probe = harness.probe_engine(
                        root, str(executable), "worker", active=True
                    )
            self.assertFalse(probe.hard)
            self.assertIn("could not be removed", probe.reason)

    def test_container_outcome_requires_one_completed_exited_state(self) -> None:
        probe = harness.EngineProbe(
            "podman",
            "podman",
            "/fixture/podman",
            True,
            True,
            "fixture",
            {},
            "worker",
            "a" * 64,
            "overlay",
        )

        def outcome(state):
            completed = subprocess.CompletedProcess(
                ["podman", "inspect"],
                0,
                stdout=json.dumps([{"State": state}]),
                stderr="",
            )
            with mock.patch.object(harness, "run_capture", return_value=completed):
                with mock.patch.object(
                    harness, "_remove_container", return_value=True
                ):
                    return harness.container_outcome(
                        probe, pathlib.Path("/"), "worker"
                    )

        result = outcome(
            {
                "Running": False,
                "Status": "exited",
                "ExitCode": 0,
                "OOMKilled": False,
            }
        )
        self.assertTrue(result[2])
        self.assertTrue(result[3])
        self.assertEqual(result[1], 0)

        for state in (
            {"Running": True, "Status": "running", "ExitCode": 0},
            {"Running": False, "Status": "created", "ExitCode": 0},
            {"Running": False, "Status": "exited", "ExitCode": "0"},
        ):
            with self.subTest(state=state):
                self.assertFalse(outcome(state)[3])

    def test_stale_worker_cleanup_failure_aborts_admission(self) -> None:
        probe = harness.EngineProbe(
            "podman",
            "podman",
            "/fixture/podman",
            True,
            True,
            "fixture",
            {},
            "worker",
            "a" * 64,
            "overlay",
        )
        listed = subprocess.CompletedProcess(
            ["podman", "ps"],
            0,
            stdout="paradox-v-123-fixture-456\n",
            stderr="",
        )
        with mock.patch.object(harness, "run_capture", return_value=listed):
            with mock.patch.object(harness, "_remove_container", return_value=False):
                with self.assertRaisesRegex(
                    harness.FatalRunError, "did not confirm removal"
                ):
                    harness.reap_stale_workers(pathlib.Path("/"), probe)


class ResourceTests(unittest.TestCase):
    def test_startup_pressure_does_not_become_static_capacity(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            helper = root / "scripts" / "environment" / "resource-jobs"
            helper.parent.mkdir(parents=True)
            helper.write_text("#!/bin/sh\n", encoding="utf-8")
            helper.chmod(0o700)
            pressure = subprocess.CompletedProcess(
                [str(helper)],
                1,
                stdout="",
                stderr=(
                    "resource-jobs: insufficient memory for one light-test job "
                    "while retaining the 12000-MiB safety reserve "
                    "(available=10000 MiB, required_per_job=2048 MiB, "
                    "cpu_limit=8)\n"
                ),
            )
            defaults = {
                "host_memory_reserve_mib": 12000,
                "host_memory_reserve_fraction": 0.25,
                "host_disk_reserve_mib": 1,
                "engine_overhead_mib": 1000,
                "host_pid_budget": 1000,
            }
            with (
                mock.patch.object(
                    harness, "run_capture", return_value=pressure
                ),
                mock.patch.object(
                    harness,
                    "memory_capacity_mib",
                    return_value=(64000, "fixture"),
                ),
                mock.patch.object(
                    harness,
                    "cgroup_pid_limits",
                    return_value=(None, None, "fixture"),
                ),
            ):
                host = harness.discover_host_resources(root, defaults)
            self.assertEqual(host.cpus, 8)
            self.assertEqual(host.memory_available_mib, 10000)
            self.assertEqual(host.memory_source, "resource_jobs_pressure")
            self.assertEqual(host.memory_reserve_mib, 12000)
            self.assertEqual(host.memory_budget_mib, 0)
            self.assertEqual(host.memory_capacity_mib, 47000)
            with (
                mock.patch.object(
                    harness,
                    "live_memory_available_mib",
                    return_value=(60000, "recovered"),
                ),
                mock.patch.object(
                    harness,
                    "cgroup_pid_available",
                    return_value=(None, "fixture"),
                ),
            ):
                recovered = harness.live_host_budget(root, host)
            self.assertEqual(recovered.memory_reserve_mib, 15000)
            self.assertEqual(recovered.memory_budget_mib, 44000)

    def test_live_memory_uses_cgroup_aware_helper_and_pressure_measurement(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            helper = root / "scripts" / "environment" / "resource-jobs"
            helper.parent.mkdir(parents=True)
            helper.write_text("#!/bin/sh\n", encoding="utf-8")
            helper.chmod(0o700)
            report = subprocess.CompletedProcess(
                [str(helper)],
                0,
                stdout=(
                    "memory_available_mib\t4096\n"
                    "memory_source\tcgroup_v2_available\n"
                ),
                stderr="",
            )
            with mock.patch.object(harness, "run_capture", return_value=report):
                self.assertEqual(
                    harness.live_memory_available_mib(root),
                    (4096, "cgroup_v2_available"),
                )
            pressure = subprocess.CompletedProcess(
                [str(helper)],
                1,
                stdout="",
                stderr="resource-jobs: insufficient memory; available=777 MiB\n",
            )
            with mock.patch.object(harness, "run_capture", return_value=pressure):
                self.assertEqual(
                    harness.live_memory_available_mib(root),
                    (777, "resource_jobs_pressure"),
                )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_pid_budget_uses_tightest_cgroup_ancestor(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            fixture = pathlib.Path(temporary)
            proc_root = fixture / "proc"
            cgroup_root = fixture / "cgroup"
            (proc_root / "self").mkdir(parents=True)
            (proc_root / "self" / "cgroup").write_text(
                "0::/parent/worker\n", encoding="ascii"
            )
            worker = cgroup_root / "parent" / "worker"
            worker.mkdir(parents=True)
            (worker / "pids.max").write_text("100\n", encoding="ascii")
            (worker / "pids.current").write_text("30\n", encoding="ascii")
            parent = cgroup_root / "parent"
            (parent / "pids.max").write_text("50\n", encoding="ascii")
            (parent / "pids.current").write_text("10\n", encoding="ascii")
            self.assertEqual(
                harness.cgroup_pid_available(
                    proc_root=proc_root, cgroup_root=cgroup_root
                ),
                (40, "cgroup_pids"),
            )
            self.assertEqual(
                harness.cgroup_pid_limits(
                    proc_root=proc_root, cgroup_root=cgroup_root
                ),
                (40, 50, "cgroup_pids"),
            )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_memory_capacity_uses_tightest_cgroup_ancestor(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            fixture = pathlib.Path(temporary)
            proc_root = fixture / "proc"
            cgroup_root = fixture / "cgroup"
            (proc_root / "self").mkdir(parents=True)
            (proc_root / "self" / "cgroup").write_text(
                "0::/parent/worker\n", encoding="ascii"
            )
            worker = cgroup_root / "parent" / "worker"
            worker.mkdir(parents=True)
            (worker / "memory.max").write_text(
                str(8 * 1024 * 1024 * 1024), encoding="ascii"
            )
            parent = cgroup_root / "parent"
            (parent / "memory.max").write_text(
                str(4 * 1024 * 1024 * 1024), encoding="ascii"
            )
            self.assertEqual(
                harness.cgroup_memory_capacity_mib(
                    proc_root=proc_root, cgroup_root=cgroup_root
                ),
                (4096, "cgroup_memory_capacity"),
            )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_hybrid_v1_aggregate_proof_and_event_monitor(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, memory = aggregate_v1_fixture(root)
            defaults = {
                "host_memory_reserve_mib": 12288,
                "host_memory_reserve_fraction": 0.25,
            }
            probe = harness.probe_aggregate_containment(
                root,
                defaults,
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertTrue(probe.hard, probe.reason)
            self.assertEqual(probe.identity["layout"], "v1")
            self.assertEqual(
                probe.identity["memory_effective_limit_bytes"],
                28 * 1024 * 1024 * 1024,
            )
            self.assertIsNone(harness.aggregate_violation(probe))
            (memory / "memory.failcnt").write_text("1\n", encoding="ascii")
            violation = harness.aggregate_violation(probe)
            self.assertIsNotNone(violation)
            self.assertIn("memory_events counter failcnt increased", violation)

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_v1_aggregate_rejects_swap_even_with_memsw_controller(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, memory = aggregate_v1_fixture(root)
            (proc_root / "meminfo").write_text(
                "MemTotal:       67108864 kB\n"
                "MemAvailable:   41943040 kB\n"
                "SwapTotal:       1048576 kB\n",
                encoding="ascii",
            )
            (memory / "memory.memsw.limit_in_bytes").write_text(
                f"{28 * 1024 * 1024 * 1024}\n", encoding="ascii"
            )
            probe = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertFalse(probe.hard)
            self.assertIn("requires host swap to be disabled", probe.reason)

    def test_aggregate_violation_classification_is_fail_closed(self) -> None:
        self.assertEqual(
            harness.aggregate_violation_status(
                "aggregate memory_events counter oom_kill increased (0 -> 1)"
            ),
            "oom",
        )
        for reason in (
            "aggregate memory_events counter oom_kill was reset or removed",
            "aggregate pids_events counter max increased (0 -> 1)",
            "aggregate cgroup contract changed during execution: memory_constraints",
        ):
            with self.subTest(reason=reason):
                self.assertEqual(
                    harness.aggregate_violation_status(reason),
                    "infrastructure",
                )

    def test_aggregate_startup_refresh_cannot_normalize_an_event(self) -> None:
        probe = harness.AggregateProbe(
            requested=True,
            hard=True,
            reason="fixture",
            identity={},
        )
        with mock.patch.object(
            harness,
            "aggregate_violation",
            return_value=(
                "aggregate memory_events counter failcnt increased (0 -> 1)"
            ),
        ):
            with self.assertRaisesRegex(
                harness.HarnessError,
                "changed during the active worker probe",
            ):
                harness.require_stable_aggregate(
                    probe,
                    phase="during the active worker probe",
                )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_aggregate_systemd_contract_is_authenticated(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, _ = aggregate_v1_fixture(root)
            # Production must continue to authenticate the fixed host
            # systemctl path.  The deliberately small worker image need not
            # contain systemd, so exercise the identical plain-file,
            # root-owner, mode, and executable checks with its required
            # coreutils executable instead of weakening or skipping them.
            self.assertEqual(
                harness.SYSTEMCTL,
                pathlib.Path("/usr/bin/systemctl"),
            )
            trusted_executable_fixture = pathlib.Path("/usr/bin/env")
            unit = (
                f"paradox-verify-aggregate-u{os.getuid()}-fixture.service"
            )
            properties = {
                "Id": unit,
                "Type": "exec",
                "ControlGroup": f"/system.slice/{unit}",
                "User": str(os.getuid()),
                "Group": str(os.getgid()),
                "MemoryAccounting": "yes",
                "CPUAccounting": "yes",
                "TasksAccounting": "yes",
                "OOMPolicy": "kill",
                "OOMScoreAdjust": "1000",
                "KillMode": "control-group",
                "Delegate": "no",
            }
            observed = subprocess.CompletedProcess(
                ["systemctl"],
                0,
                stdout="".join(f"{key}={value}\n" for key, value in properties.items()),
                stderr="",
            )
            with (
                mock.patch.object(
                    harness,
                    "SYSTEMCTL",
                    trusted_executable_fixture,
                ),
                mock.patch.object(
                    harness, "run_capture", return_value=observed
                ) as captured,
            ):
                probe = harness.probe_aggregate_containment(
                    root,
                    {
                        "host_memory_reserve_mib": 12288,
                        "host_memory_reserve_fraction": 0.25,
                    },
                    requested=True,
                    proc_root=proc_root,
                    cgroup_root=cgroup_root,
                    require_systemd=True,
                )
            self.assertTrue(probe.hard, probe.reason)
            self.assertEqual(probe.identity["systemd"]["OOMPolicy"], "kill")
            self.assertEqual(
                captured.call_args.args[0][0],
                str(trusted_executable_fixture),
            )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_aggregate_systemd_contract_is_reauthenticated(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, _ = aggregate_v1_fixture(root)
            probe = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            expected = {"OOMPolicy": "kill", "Delegate": "no"}
            probe = dataclasses.replace(
                probe,
                identity={**probe.identity, "systemd": expected},
            )
            with mock.patch.object(
                harness,
                "_systemd_unit_identity",
                return_value={"OOMPolicy": "stop", "Delegate": "no"},
            ):
                violation = harness.aggregate_violation(
                    probe,
                    reauthenticate_systemd=True,
                    root=root,
                )
            self.assertEqual(
                violation,
                "aggregate systemd unit contract changed during execution",
            )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_unified_v2_aggregate_proof(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root = root / "proc"
            cgroup_root = root / "cgroup"
            unit = (
                f"paradox-verify-aggregate-u{os.getuid()}-fixture.service"
            )
            cgroup_path = f"/system.slice/{unit}"
            (proc_root / "self").mkdir(parents=True)
            (proc_root / "self" / "cgroup").write_text(
                f"0::{cgroup_path}\n", encoding="ascii"
            )
            (proc_root / "meminfo").write_text(
                "MemTotal:       67108864 kB\n"
                "MemAvailable:   41943040 kB\n"
                "SwapTotal:       1048576 kB\n",
                encoding="ascii",
            )
            leaf = cgroup_root / cgroup_path.lstrip("/")
            leaf.mkdir(parents=True)
            values = {
                "cgroup.procs": f"{os.getpid()}\n",
                "memory.max": f"{16 * 1024 * 1024 * 1024}\n",
                "memory.current": f"{128 * 1024 * 1024}\n",
                "memory.swap.max": "0\n",
                "memory.events": (
                    "low 0\nhigh 0\nmax 0\noom 0\noom_kill 0\n"
                ),
                "cpu.max": "400000 100000\n",
                "pids.max": "4096\n",
                "pids.current": "1\n",
                "pids.events": "max 0\n",
            }
            for name, value in values.items():
                (leaf / name).write_text(value, encoding="ascii")
            probe = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertTrue(probe.hard, probe.reason)
            self.assertEqual(probe.identity["layout"], "v2")
            self.assertEqual(probe.identity["swap_max_bytes"], 0)

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_aggregate_requires_finite_dedicated_leaf_not_only_parent(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, memory = aggregate_v1_fixture(root)
            (memory / "memory.limit_in_bytes").write_text(
                f"{(1 << 63) - 4096}\n", encoding="ascii"
            )
            parent = memory.parent
            (parent / "memory.limit_in_bytes").write_text(
                f"{28 * 1024 * 1024 * 1024}\n", encoding="ascii"
            )
            probe = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertFalse(probe.hard)
            self.assertIn("service leaf has no finite memory limit", probe.reason)

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_aggregate_requires_finite_cpu_quota_on_service_leaf(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, memory = aggregate_v1_fixture(root)
            relative = memory.relative_to(cgroup_root / "memory")
            cpu = cgroup_root / "cpu,cpuacct" / relative
            (cpu / "cpu.cfs_quota_us").write_text("-1\n", encoding="ascii")
            (cpu.parent / "cpu.cfs_quota_us").write_text(
                "3000000\n", encoding="ascii"
            )
            (cpu.parent / "cpu.cfs_period_us").write_text(
                "100000\n", encoding="ascii"
            )
            probe = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertFalse(probe.hard)
            self.assertIn(
                "service leaf has no finite CPU quota",
                probe.reason,
            )

    @unittest.skipUnless(sys.platform == "linux", "Linux cgroup fixture")
    def test_aggregate_headroom_accounts_for_tighter_ancestor_usage(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, memory = aggregate_v1_fixture(root)
            parent = memory.parent
            (parent / "memory.limit_in_bytes").write_text(
                f"{20 * 1024 * 1024 * 1024}\n", encoding="ascii"
            )
            (parent / "memory.usage_in_bytes").write_text(
                f"{19 * 1024 * 1024 * 1024}\n", encoding="ascii"
            )
            (parent / "memory.failcnt").write_text("0\n", encoding="ascii")
            (parent / "memory.oom_control").write_text(
                "oom_kill_disable 0\nunder_oom 0\noom_kill 0\n",
                encoding="ascii",
            )
            probe = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertTrue(probe.hard, probe.reason)
            self.assertEqual(
                probe.identity["memory_effective_limit_bytes"],
                20 * 1024 * 1024 * 1024,
            )
            self.assertEqual(
                probe.identity["memory_headroom_bytes"],
                1024 * 1024 * 1024,
            )
            self.assertIsNone(harness.aggregate_violation(probe))
            (parent / "memory.failcnt").write_text("1\n", encoding="ascii")
            violation = harness.aggregate_violation(probe)
            self.assertIsNotNone(violation)
            self.assertIn("memory_events counter", violation)
            self.assertIn("failcnt increased", violation)

    def test_aggregate_budget_does_not_subtract_outside_reserve_twice(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, _ = aggregate_v1_fixture(root)
            defaults = {
                "host_memory_reserve_mib": 12288,
                "host_memory_reserve_fraction": 0.25,
                "host_disk_reserve_mib": 1,
                "engine_overhead_mib": 1024,
                "host_pid_budget": 8192,
            }
            aggregate = harness.probe_aggregate_containment(
                root,
                defaults,
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertTrue(aggregate.hard, aggregate.reason)
            with (
                mock.patch.object(
                    harness, "memory_available_mib", return_value=(40960, "fixture")
                ),
                mock.patch.object(
                    harness,
                    "_proc_meminfo_values",
                    return_value={
                        "MemTotal": 65536 * 1024,
                        "MemAvailable": 40960 * 1024,
                        "SwapTotal": 0,
                    },
                ),
                mock.patch.object(
                    harness,
                    "cgroup_pid_limits",
                    return_value=(8191, 8192, "fixture"),
                ),
                mock.patch.object(
                    harness.os,
                    "sched_getaffinity",
                    return_value=set(range(32)),
                ),
            ):
                host = harness.discover_host_resources(
                    root, defaults, aggregate
                )
            self.assertEqual(host.cpus, 30)
            self.assertEqual(host.cpu_budget, 30)
            self.assertEqual(host.memory_reserve_mib, 12288)
            self.assertEqual(host.aggregate_memory_capacity_mib, 28672)
            self.assertEqual(host.memory_capacity_mib, 27648)
            self.assertEqual(host.memory_budget_mib, 27520)
            self.assertGreaterEqual(host.memory_capacity_mib, 26624)

    def test_live_aggregate_budget_uses_headroom_once_and_restores_active_reservations(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            proc_root, cgroup_root, memory = aggregate_v1_fixture(root)
            aggregate = harness.probe_aggregate_containment(
                root,
                {
                    "host_memory_reserve_mib": 12288,
                    "host_memory_reserve_fraction": 0.25,
                },
                requested=True,
                proc_root=proc_root,
                cgroup_root=cgroup_root,
                require_systemd=False,
            )
            self.assertTrue(aggregate.hard, aggregate.reason)
            (memory / "memory.usage_in_bytes").write_text(
                f"{8 * 1024 * 1024 * 1024}\n", encoding="ascii"
            )
            host = harness.HostResources(
                cpus=30,
                memory_available_mib=40960,
                disk_available_mib=20000,
                memory_reserve_mib=12288,
                disk_reserve_mib=8192,
                cpu_budget=30,
                memory_budget_mib=27520,
                disk_budget_mib=11808,
                memory_source="fixture",
                pid_budget=8000,
                pid_source="fixture",
                memory_capacity_mib=27648,
                disk_capacity_mib=11808,
                pid_capacity=8000,
                engine_overhead_mib=128,
                memory_reserve_min_mib=12288,
                memory_reserve_fraction=0.25,
            )
            with (
                mock.patch.object(
                    harness,
                    "memory_available_mib",
                    return_value=(40000, "fixture"),
                ),
                mock.patch.object(
                    harness,
                    "cgroup_pid_available",
                    return_value=(8000, "fixture"),
                ),
            ):
                live = harness.live_host_budget(root, host, aggregate)
            self.assertEqual(live.memory_reserve_mib, 12288)
            self.assertEqual(live.aggregate_memory_available_mib, 20480)
            self.assertEqual(live.memory_budget_mib, 20352)

            active = harness.RunningTask(
                planned=task("active", ("true",)),
                process=mock.Mock(),
                log=mock.Mock(),
                log_path=root / "active.log",
                result_path=root / "active.json",
                started_monotonic=0,
                started_at="fixture",
                attempt=1,
                container_name=None,
                allocation=harness.Allocation(
                    cpu=1,
                    memory_mib=4096,
                    pids=32,
                    scratch_mib=16,
                ),
            )
            restored = harness.include_active_reservations(live, (active,))
            self.assertEqual(restored.memory_budget_mib, 24448)
            self.assertEqual(
                restored.memory_budget_mib - active.allocation.memory_mib,
                live.memory_budget_mib,
            )

    def test_live_budget_recovers_above_startup_pressure_within_capacity(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            host = harness.HostResources(
                cpus=4,
                memory_available_mib=800,
                disk_available_mib=300,
                memory_reserve_mib=100,
                disk_reserve_mib=100,
                cpu_budget=4,
                memory_budget_mib=500,
                disk_budget_mib=200,
                memory_source="fixture",
                pid_budget=20,
                pid_source="fixture",
                memory_capacity_mib=3000,
                disk_capacity_mib=2000,
                pid_capacity=300,
                engine_overhead_mib=50,
            )
            usage = shutil.disk_usage(root)
            with (
                mock.patch.object(
                    harness,
                    "live_memory_available_mib",
                    return_value=(2500, "recovered"),
                ),
                mock.patch.object(
                    harness.shutil,
                    "disk_usage",
                    return_value=usage._replace(free=1500 * 1024 * 1024),
                ),
                mock.patch.object(
                    harness,
                    "cgroup_pid_available",
                    return_value=(250, "fixture"),
                ),
            ):
                current = harness.live_host_budget(root, host)
            self.assertEqual(current.memory_budget_mib, 2350)
            self.assertEqual(current.disk_budget_mib, 1400)
            self.assertEqual(current.pid_budget, 122)


class CacheTests(unittest.TestCase):
    def test_resume_identity_always_tracks_activated_r_library(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            library_file = root / ".local" / "R" / "library" / "pkg" / "Meta"
            library_file.parent.mkdir(parents=True)
            library_file.write_text("one\n", encoding="utf-8")
            manifest = harness.load_manifest(root, write_manifest(root))
            profile = manifest.profiles["test"]
            parameters = {
                "root": str(root),
                "run_id": "r",
                "profile": "test",
                "task_id": "",
                "attempt_id": harness.ATTEMPT_ID_SENTINEL,
                "git_commit": "0" * 40,
                "git_tree": "1" * 40,
            }
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            first = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                probe,
                reuse_cache=False,
            )["base"].cache_key
            library_file.write_text("two\n", encoding="utf-8")
            second = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                probe,
                reuse_cache=False,
            )["base"].cache_key
            self.assertNotEqual(first, second)

    def test_toolchain_identity_normalizes_install_prefix_provenance(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            parent = pathlib.Path(temporary)
            identities = []
            for name in ("checkout-one", "checkout-two"):
                root = parent / name
                prefix = root / ".local" / "toolchain"
                binary = prefix / "bin" / "R"
                history = prefix / "conda-meta" / "history"
                binary.parent.mkdir(parents=True)
                history.parent.mkdir()
                lock = root / "environment" / "toolchain-linux-64.lock"
                lock.parent.mkdir()
                lock.write_text("@EXPLICIT\nfixture\n", encoding="utf-8")
                binary.write_text(
                    f"#!/bin/sh\nR_HOME_DIR={prefix}/lib/R\n",
                    encoding="utf-8",
                )
                history.write_text(
                    f"# cmd: install --prefix {prefix}\n",
                    encoding="utf-8",
                )
                identities.append(
                    harness.execution_environment_identity(
                        root, include_r_library=False
                    )["identity_sha256"]
                )
            self.assertEqual(identities[0], identities[1])

    def test_cache_key_ignores_incidental_kernel_and_engine_observation(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            manifest = harness.load_manifest(root, write_manifest(root))
            profile = manifest.profiles["test"]
            parameters = {
                "root": str(root),
                "run_id": "r",
                "profile": "test",
                "task_id": "",
                "attempt_id": harness.ATTEMPT_ID_SENTINEL,
                "git_commit": "0" * 40,
                "git_tree": "1" * 40,
            }
            first_probe = harness.EngineProbe(
                "podman",
                "podman",
                "/one/podman",
                True,
                True,
                "fixture one",
                {"server_version": "1", "cgroup_version": "2"},
                "sha256:" + "a" * 64,
                "b" * 64,
                "overlay",
            )
            second_probe = dataclasses.replace(
                first_probe,
                executable="/two/podman",
                reason="fixture two",
                identity={"server_version": "99", "cgroup_version": "2"},
                storage_driver="btrfs",
            )
            first = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                first_probe,
                reuse_cache=False,
            )["base"].cache_key
            with (
                mock.patch.object(
                    harness.platform,
                    "platform",
                    return_value="different-kernel",
                ),
                mock.patch.object(
                    harness.platform,
                    "python_version",
                    return_value="3.99.0",
                ),
            ):
                second = harness.plan_tasks(
                    root,
                    manifest,
                    profile,
                    {"base"},
                    [],
                    parameters,
                    second_probe,
                    reuse_cache=False,
                )["base"].cache_key
            self.assertEqual(first, second)

            aggregate_one = harness.AggregateProbe(
                True,
                True,
                "one",
                {
                    "layout": "v1",
                    "unit": "one.service",
                    "cgroup_path": "/system.slice/one.service",
                    "memory_effective_limit_bytes": 1,
                },
            )
            aggregate_two = dataclasses.replace(
                aggregate_one,
                reason="two",
                identity={
                    "layout": "v1",
                    "unit": "two.service",
                    "cgroup_path": "/system.slice/two.service",
                    "memory_effective_limit_bytes": 2,
                },
            )
            aggregate_probe = dataclasses.replace(
                first_probe,
                identity={"cgroup_version": "v1"},
                limit_mode="aggregate",
                aggregate=aggregate_one,
            )
            aggregate_key_one = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                aggregate_probe,
                reuse_cache=False,
            )["base"].cache_key
            aggregate_key_two = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                dataclasses.replace(aggregate_probe, aggregate=aggregate_two),
                reuse_cache=False,
            )["base"].cache_key
            worker_v1_key = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                dataclasses.replace(
                    aggregate_probe, limit_mode="worker", aggregate=None
                ),
                reuse_cache=False,
            )["base"].cache_key
            self.assertEqual(aggregate_key_one, aggregate_key_two)
            self.assertNotEqual(aggregate_key_one, worker_v1_key)

            soft_probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            with (
                mock.patch.object(
                    harness.platform, "platform", return_value="kernel-one"
                ),
                mock.patch.object(
                    harness.platform, "python_version", return_value="3.10.0"
                ),
            ):
                host_first = harness.plan_tasks(
                    root,
                    manifest,
                    profile,
                    {"base"},
                    [],
                    parameters,
                    soft_probe,
                    reuse_cache=False,
                )["base"].cache_key
            with (
                mock.patch.object(
                    harness.platform, "platform", return_value="kernel-two"
                ),
                mock.patch.object(
                    harness.platform, "python_version", return_value="3.14.0"
                ),
            ):
                host_second = harness.plan_tasks(
                    root,
                    manifest,
                    profile,
                    {"base"},
                    [],
                    parameters,
                    soft_probe,
                    reuse_cache=False,
                )["base"].cache_key
            self.assertNotEqual(host_first, host_second)

    def test_optional_inputs_track_future_payload_below_dot_local_ancestor(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary) / ".local" / "checkout"
            root.mkdir(parents=True)
            path = write_manifest(root)
            payload = json.loads(path.read_text(encoding="utf-8"))
            payload["input_groups"]["base"].append("?future/**")
            path.write_text(json.dumps(payload), encoding="utf-8")
            manifest = harness.load_manifest(root, path)
            specification = manifest.tasks["base"]
            first, rows = harness.input_receipt(root, manifest, specification)
            self.assertEqual([row["path"] for row in rows], ["input.txt"])

            future = root / "future" / "payload"
            future.parent.mkdir()
            future.write_text("new package payload\n", encoding="utf-8")
            second, rows = harness.input_receipt(root, manifest, specification)
            self.assertNotEqual(first, second)
            self.assertIn("future/payload", [row["path"] for row in rows])

    def test_relevant_content_not_mtime_changes_key(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            manifest = harness.load_manifest(root, write_manifest(root))
            profile = manifest.profiles["test"]
            parameters = {
                "root": str(root),
                "run_id": "r",
                "profile": "test",
                "task_id": "",
                "git_commit": "0" * 40,
                "git_tree": "1" * 40,
            }
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            first = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                probe,
                reuse_cache=False,
            )["base"].cache_key
            original = (root / "input.txt").stat()
            (root / "input.txt").write_text("two\n", encoding="utf-8")
            os.utime(
                root / "input.txt",
                times=(original.st_atime, original.st_mtime),
            )
            second = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                probe,
                reuse_cache=False,
            )["base"].cache_key
            self.assertNotEqual(first, second)
            (root / "irrelevant").write_text("ignored", encoding="utf-8")
            third = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                probe,
                reuse_cache=False,
            )["base"].cache_key
            self.assertEqual(second, third)

    def test_cache_tampering_and_unexpected_paths_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            path = harness.cache_entry(root, "task", "a" * 64)
            path.mkdir(parents=True)
            log = path / "task.log"
            log.write_text("ok\n", encoding="utf-8")
            receipt = {
                "schema": 1,
                "task_id": "task",
                "cache_key": "a" * 64,
                "status": "passed",
                "log_sha256": harness.sha256_file(log),
                "original_result": {
                    "task_id": "task",
                    "cache_key": "a" * 64,
                    "status": "passed",
                },
            }
            (path / "receipt.json").write_text(
                json.dumps(receipt), encoding="utf-8"
            )
            harness.validate_cache_entry(path, "task", "a" * 64)
            log.write_text("tampered\n", encoding="utf-8")
            with self.assertRaises(harness.FatalRunError):
                harness.validate_cache_entry(path, "task", "a" * 64)
            log.write_text("ok\n", encoding="utf-8")
            (path / "unexpected").write_text("x", encoding="utf-8")
            with self.assertRaisesRegex(harness.FatalRunError, "unexpected"):
                harness.validate_cache_entry(path, "task", "a" * 64)

    def test_candidate_context_authenticates_one_consistent_tuple(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            source = root / ".local" / "compat" / "candidate-snapshots" / "one"
            source.mkdir(parents=True)
            subprocess.run(["git", "init", "-q", str(source)], check=True)
            subprocess.run(
                ["git", "-C", str(source), "config", "user.name", "Fixture"],
                check=True,
            )
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(source),
                    "config",
                    "user.email",
                    "fixture@example.invalid",
                ],
                check=True,
            )
            (source / "file").write_text("candidate\n", encoding="utf-8")
            subprocess.run(["git", "-C", str(source), "add", "file"], check=True)
            subprocess.run(
                ["git", "-C", str(source), "commit", "-qm", "candidate"],
                check=True,
            )
            commit = subprocess.check_output(
                ["git", "-C", str(source), "rev-parse", "HEAD"], text=True
            ).strip()
            tree = subprocess.check_output(
                ["git", "-C", str(source), "rev-parse", "HEAD^{tree}"], text=True
            ).strip()
            commit_source = source.parent / commit
            source.rename(commit_source)
            source = commit_source
            ref = "refs/paradox-release/fixture"
            subprocess.run(
                ["git", "-C", str(source), "update-ref", ref, commit], check=True
            )
            run_id = "candidate-run"
            library = (
                root
                / ".local"
                / "compat"
                / "runs"
                / run_id
                / "library-candidate"
            )
            dependency = root / ".local" / "compat" / "R" / "library-dependencies"
            consumer = root / ".local" / "compat" / "R" / "library-consumer"
            documentation = (
                root / ".local" / "compat" / "R" / "library-documentation"
            )
            bridge = (
                root
                / ".local"
                / "compat"
                / "runs"
                / run_id
                / "library-downstream-bridges-profile-paradox2"
            )
            for directory in (
                library,
                dependency,
                consumer,
                documentation,
                bridge,
            ):
                directory.mkdir(parents=True)
            content = "a" * 64
            provenance = library / ".paradox-candidate-provenance.tsv"
            provenance.write_text(
                "\n".join(
                    [
                        "key\tvalue",
                        "schema\t2",
                        f"candidate_run_id\t{run_id}",
                        f"candidate_ref\t{ref}",
                        f"candidate_commit\t{commit}",
                        f"candidate_tree\t{tree}",
                        f"candidate_library\t{library}",
                        f"dependency_library\t{dependency}",
                        f"candidate_content_sha256\t{content}",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            (
                library / ".paradox-candidate-provenance.sha256"
            ).write_text(
                f"{harness.sha256_file(provenance)}  {provenance.name}\n",
                encoding="ascii",
            )
            sentinel = library / ".paradox-candidate-content-sha256"
            sentinel.write_text(content + "\n", encoding="ascii")
            context = root / "context.json"
            context.write_text(
                json.dumps(
                    {
                        "schema": 1,
                        "run_id": run_id,
                        "ref": ref,
                        "commit": commit,
                        "tree": tree,
                        "source": str(source),
                        "library": str(library),
                        "dependency_library": str(dependency),
                        "content_sha256": content,
                        "evidence_profile": "profile",
                        "axis": "paradox2",
                        "bridge_library": str(bridge),
                        "extra_libraries": [str(consumer)],
                        "documentation_libraries": [str(documentation)],
                    }
                ),
                encoding="utf-8",
            )
            values = harness.load_candidate_context(root, str(context))
            self.assertEqual(values["candidate_commit"], commit)
            self.assertEqual(values["candidate_library"], str(library))
            self.assertEqual(
                values["consumer_extra_libs"],
                os.pathsep.join((str(bridge), str(consumer))),
            )
            self.assertEqual(
                values["documentation_extra_libs"],
                os.pathsep.join(
                    (str(bridge), str(consumer), str(documentation))
                ),
            )
            context_payload = json.loads(context.read_text(encoding="utf-8"))
            minimal_payload = {
                key: value
                for key, value in context_payload.items()
                if key
                not in {
                    "bridge_library",
                    "extra_libraries",
                    "documentation_libraries",
                }
            }
            context.write_text(json.dumps(minimal_payload), encoding="utf-8")
            reverse_values = harness.load_candidate_context(
                root, str(context), required_roles=frozenset()
            )
            self.assertNotIn("bridge_library", reverse_values)
            context.write_text(json.dumps(context_payload), encoding="utf-8")
            context_payload["extra_libraries"] = [str(consumer) + os.pathsep + "bad"]
            context.write_text(json.dumps(context_payload), encoding="utf-8")
            with self.assertRaisesRegex(harness.HarnessError, "represented safely"):
                harness.load_candidate_context(root, str(context))
            context_payload["extra_libraries"] = [str(consumer)]
            context.write_text(json.dumps(context_payload), encoding="utf-8")
            sentinel.write_text("b" * 64 + "\n", encoding="ascii")
            with self.assertRaisesRegex(harness.HarnessError, "sentinel"):
                harness.load_candidate_context(root, str(context))

    def test_planned_inputs_are_reauthenticated_before_execution(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            manifest = harness.load_manifest(root, write_manifest(root))
            profile = manifest.profiles["test"]
            parameters = {
                "root": str(root),
                "run_id": "r",
                "profile": "test",
                "task_id": "",
                "attempt_id": harness.ATTEMPT_ID_SENTINEL,
                "git_commit": "0" * 40,
                "git_tree": "1" * 40,
            }
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            item = harness.plan_tasks(
                root,
                manifest,
                profile,
                {"base"},
                [],
                parameters,
                probe,
                reuse_cache=False,
            )["base"]
            harness.authenticate_planned_inputs(root, manifest, item)
            (root / "input.txt").write_text("changed\n", encoding="utf-8")
            with self.assertRaisesRegex(harness.FatalRunError, "after planning"):
                harness.authenticate_planned_inputs(root, manifest, item)


class SchedulerTests(unittest.TestCase):
    def test_resume_success_authenticates_attempt_and_invocation_receipts(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            run_root = root / ".local" / "verify" / "runs" / "resume"
            task_root = run_root / "tasks" / "one"
            invocation_root = run_root / "invocations"
            task_root.mkdir(parents=True)
            invocation_root.mkdir()
            item = task("one", ("true",))
            invocation_id = "invocation-001"
            invocation_path = invocation_root / f"{invocation_id}.json"
            harness.atomic_write(
                invocation_path,
                harness.canonical_json(
                    {
                        "schema": 1,
                        "invocation_id": invocation_id,
                        "resume": False,
                        "pid": 123,
                        "started_at": "2026-01-01T00:00:00Z",
                        "engine": {},
                    }
                ),
            )
            log = task_root / "attempt-001.log"
            log.write_text("passed\n", encoding="utf-8")
            result = {
                "schema": harness.RESULT_SCHEMA,
                "run_id": "resume",
                "task_id": "one",
                "cache_key": item.cache_key,
                "status": "passed",
                "origin": "execution",
                "return_code": 0,
                "failure_class": item.task.failure_class,
                "reason": "command completed successfully",
                "attempt": 1,
                "child_run_id": harness.attempt_run_id("resume", "one", 1),
                "backend": "best-effort-host",
                "resources": {
                    "cpu": 1,
                    "memory_mib": 256,
                    "pids": 32,
                    "scratch_mib": 16,
                },
                "started_at": "2026-01-01T00:00:00Z",
                "finished_at": "2026-01-01T00:00:01Z",
                "elapsed_seconds": 1.0,
                "peak_rss_mib": 1,
                "invocation_id": invocation_id,
                "invocation_sha256": harness.sha256_file(invocation_path),
                "log": str(log),
                "log_sha256": harness.sha256_file(log),
            }
            latest = task_root / "result.json"
            immutable = task_root / "attempt-001.result.json"
            harness.atomic_write(latest, harness.canonical_json(result))
            harness.atomic_write(immutable, harness.canonical_json(result))
            self.assertEqual(
                set(harness.load_resume_successes(run_root, {"one": item})),
                {"one"},
            )

            tampered = dict(result)
            tampered["reason"] = "forged latest view"
            harness.atomic_write(latest, harness.canonical_json(tampered))
            with self.assertRaisesRegex(
                harness.FatalRunError, "differs from its immutable attempt"
            ):
                harness.load_resume_successes(run_root, {"one": item})

            harness.atomic_write(latest, harness.canonical_json(result))
            duplicate = (
                harness.canonical_json(result).decode("utf-8").rstrip()[:-1]
                + ',"status":"passed"}\n'
            )
            latest.write_text(duplicate, encoding="utf-8")
            immutable.write_text(duplicate, encoding="utf-8")
            with self.assertRaisesRegex(
                harness.FatalRunError, "resumed result is malformed"
            ):
                harness.load_resume_successes(run_root, {"one": item})

            harness.atomic_write(latest, harness.canonical_json(result))
            harness.atomic_write(immutable, harness.canonical_json(result))
            invocation_path.unlink()
            with self.assertRaisesRegex(
                harness.HarnessError, "invocation receipt is absent"
            ):
                harness.load_resume_successes(run_root, {"one": item})

    def test_completed_resume_records_invocation_and_revalidates_opted_in_task(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            entry = root / "scripts" / "environment" / "verify-task-entry"
            entry.parent.mkdir(parents=True)
            entry.write_text(
                "#!/bin/sh\nset -eu\nshift 4\nexec \"$@\"\n",
                encoding="utf-8",
            )
            entry.chmod(0o700)
            task_script = root / "task.sh"
            task_script.write_text(
                "#!/bin/sh\nset -eu\nprintf 'base\\n' >> .local/count\n",
                encoding="utf-8",
            )
            task_script.chmod(0o700)
            child_script = root / "child.sh"
            child_script.write_text(
                "#!/bin/sh\nset -eu\nprintf 'child\\n' >> .local/count\n",
                encoding="utf-8",
            )
            child_script.chmod(0o700)
            manifest_path = write_manifest(root)
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest["input_groups"]["base"].extend(
                [
                    "task.sh",
                    "child.sh",
                    "scripts/environment/verify-task-entry",
                ]
            )
            manifest["tasks"][0]["command"] = ["sh", "task.sh"]
            manifest["tasks"][0]["cache"] = "success"
            manifest["tasks"][0]["revalidate_on_resume"] = True
            child = dict(manifest["tasks"][0])
            child.update(
                {
                    "id": "child",
                    "description": "revalidation descendant",
                    "command": ["sh", "child.sh"],
                    "dependencies": ["base"],
                    "revalidate_on_resume": False,
                }
            )
            manifest["tasks"].append(child)
            manifest["profiles"]["test"]["tasks"] = ["base", "child"]
            manifest["profiles"]["test"]["always"] = ["base", "child"]
            manifest["profiles"]["test"]["reuse_results"] = True
            manifest["profiles"]["test"]["publish_results"] = True
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
            subprocess.run(["git", "init", "-q", str(root)], check=True)
            subprocess.run(
                ["git", "-C", str(root), "config", "user.name", "Fixture"],
                check=True,
            )
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(root),
                    "config",
                    "user.email",
                    "fixture@example.invalid",
                ],
                check=True,
            )
            subprocess.run(["git", "-C", str(root), "add", "."], check=True)
            subprocess.run(
                ["git", "-C", str(root), "commit", "-qm", "fixture"],
                check=True,
            )
            arguments = [
                "--root",
                str(root),
                "--manifest",
                str(manifest_path),
                "run",
                "--profile",
                "test",
                "--engine",
                "none",
                "--best-effort",
                "--run-id",
                "completed",
            ]
            self.assertEqual(harness.main(arguments), 0)
            self.assertEqual(
                harness.main([*arguments, "--resume"]),
                0,
            )
            self.assertEqual(
                (root / ".local" / "count").read_text(encoding="utf-8"),
                "base\nchild\nbase\nchild\n",
            )
            invocation_root = (
                root
                / ".local"
                / "verify"
                / "runs"
                / "completed"
                / "invocations"
            )
            self.assertEqual(
                sorted(path.name for path in invocation_root.iterdir()),
                ["invocation-001.json", "invocation-002.json"],
            )

    def run_single_task(
        self, command: tuple[str, ...], *, timeout_seconds: int = 20
    ) -> dict:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = pathlib.Path(temporary.name)
        item = task("one", command)
        item.task = dataclasses.replace(
            item.task,
            resources=dataclasses.replace(
                item.task.resources, timeout_seconds=timeout_seconds
            ),
        )
        run_root = root / ".local" / "verify" / "runs" / "single"
        (run_root / "tasks").mkdir(parents=True)
        host = harness.HostResources(
            4, 8192, 8192, 1, 1, 4, 1024, 1024, "fixture"
        )
        profile = harness.Profile(
            "single",
            "single",
            ("one",),
            ("one",),
            False,
            "adaptive",
            False,
            False,
            False,
            False,
        )
        probe = harness.EngineProbe(
            "none", None, None, False, False, "fixture", {}, None, None, None
        )
        results, _ = harness.run_scheduler(
            root=root,
            manifest=None,  # type: ignore[arg-type]
            profile=profile,
            policy="adaptive",
            run_id="single",
            run_root=run_root,
            planned={"one": item},
            probe=probe,
            host=host,
            best_effort=True,
            publish_results=False,
            resume=False,
        )
        return results["one"]

    def test_aggregate_backend_launches_independent_workers_in_parallel(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            peer_a = root / "a.started"
            peer_b = root / "b.started"

            def peer_command(own: pathlib.Path, other: pathlib.Path) -> tuple[str, ...]:
                code = (
                    "import pathlib,time,sys;"
                    f"own=pathlib.Path({str(own)!r});"
                    f"other=pathlib.Path({str(other)!r});"
                    "own.write_text('started');"
                    "deadline=time.monotonic()+4;"
                    "\nwhile not other.exists() and time.monotonic()<deadline:"
                    "\n time.sleep(.02)"
                    "\nsys.exit(0 if other.exists() else 9)"
                )
                return (sys.executable, "-c", code)

            planned = {
                "a": task("a", peer_command(peer_a, peer_b), score=2),
                "b": task("b", peer_command(peer_b, peer_a), score=1),
            }
            run_root = root / ".local" / "verify" / "runs" / "aggregate"
            (run_root / "tasks").mkdir(parents=True)
            host = harness.HostResources(
                4, 8192, 8192, 1, 1, 4, 4096, 4096, "fixture"
            )
            profile = harness.Profile(
                "aggregate",
                "aggregate",
                ("a", "b"),
                ("a", "b"),
                False,
                "adaptive",
                False,
                False,
                False,
                True,
            )
            aggregate = harness.AggregateProbe(
                True, True, "fixture", {"layout": "v1"}
            )
            probe = harness.EngineProbe(
                "podman",
                "podman",
                "/fixture/podman",
                True,
                True,
                "fixture",
                {"cgroup_version": "v1"},
                "sha256:" + "a" * 64,
                "b" * 64,
                "overlay",
                limit_mode="aggregate",
                aggregate=aggregate,
            )

            def host_command(
                _root,
                _probe,
                item,
                _run_root,
                _attempt_id,
                _attempt,
                _allocation,
            ):
                return list(item.command), None

            with (
                mock.patch.object(
                    harness, "container_command", side_effect=host_command
                ),
                mock.patch.object(
                    harness, "aggregate_violation", return_value=None
                ),
                mock.patch.object(
                    harness, "live_host_budget", return_value=host
                ),
            ):
                results, _ = harness.run_scheduler(
                    root=root,
                    manifest=None,  # type: ignore[arg-type]
                    profile=profile,
                    policy="adaptive",
                    run_id="aggregate",
                    run_root=run_root,
                    planned=planned,
                    probe=probe,
                    host=host,
                    best_effort=False,
                    publish_results=False,
                    resume=False,
                )
            self.assertEqual(
                {result["status"] for result in results.values()}, {"passed"}
            )
            self.assertEqual(
                {result["backend"] for result in results.values()},
                {"podman-aggregate-hard"},
            )

    def run_policy_fixture(self, policy: str) -> tuple[dict, pathlib.Path]:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = pathlib.Path(temporary.name)
        later = root / "later"
        planned = {
            "a": task(
                "a",
                (sys.executable, "-c", "raise SystemExit(3)"),
                score=100,
            ),
            "later": task(
                "later",
                (
                    sys.executable,
                    "-c",
                    f"from pathlib import Path; Path({str(later)!r}).write_text('yes')",
                ),
                phase=1,
            ),
        }
        run_root = root / ".local" / "verify" / "runs" / policy
        (run_root / "tasks").mkdir(parents=True)
        host = harness.HostResources(
            4, 8192, 8192, 1, 1, 4, 1024, 1024, "fixture"
        )
        profile = harness.Profile(
            policy,
            policy,
            ("a", "later"),
            ("a",),
            False,
            policy,
            False,
            False,
            False,
            False,
        )
        probe = harness.EngineProbe(
            "none", None, None, False, False, "fixture", {}, None, None, None
        )
        results, _ = harness.run_scheduler(
            root=root,
            manifest=None,  # type: ignore[arg-type]
            profile=profile,
            policy=policy,
            run_id=policy,
            run_root=run_root,
            planned=planned,
            probe=probe,
            host=host,
            best_effort=True,
            publish_results=False,
            resume=False,
        )
        return results, later

    def test_best_effort_materializes_outputs_and_retains_attempt_result(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            reserved = root / ".local" / "reserved"
            item = task(
                "reserved",
                (
                    sys.executable,
                    "-c",
                    (
                        "import os,pathlib; "
                        f"assert pathlib.Path({str(reserved)!r}).is_dir(); "
                        "assert os.environ['PARADOX_VERIFY_TASK_ATTEMPT'] == '1'"
                    ),
                ),
            )
            item.writable_paths = (str(reserved),)
            run_root = root / ".local" / "verify" / "runs" / "reserved"
            (run_root / "tasks").mkdir(parents=True)
            host = harness.HostResources(
                2, 4096, 4096, 1, 1, 2, 1024, 1024, "fixture"
            )
            profile = harness.Profile(
                "reserved",
                "reserved",
                ("reserved",),
                ("reserved",),
                False,
                "adaptive",
                False,
                False,
                False,
                False,
            )
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            results, _ = harness.run_scheduler(
                root=root,
                manifest=None,  # type: ignore[arg-type]
                profile=profile,
                policy="adaptive",
                run_id="reserved",
                run_root=run_root,
                planned={"reserved": item},
                probe=probe,
                host=host,
                best_effort=True,
                publish_results=False,
                resume=False,
            )
            self.assertEqual(results["reserved"]["status"], "passed")
            task_root = run_root / "tasks" / "reserved"
            immutable = json.loads(
                (task_root / "attempt-001.result.json").read_text(encoding="utf-8")
            )
            latest = json.loads(
                (task_root / "result.json").read_text(encoding="utf-8")
            )
            self.assertEqual(immutable, latest)

    def test_only_execution_failures_raise_future_priority(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            base = {
                "run_id": "r",
                "cache_key": "a" * 64,
                "status": "blocked",
                "origin": "host_capacity",
                "elapsed_seconds": 0,
                "peak_rss_mib": 0,
                "failure_class": "semantic",
                "reason": "small host",
            }
            harness.append_history(root, {**base, "task_id": "blocked"})
            harness.append_history(
                root,
                {
                    **base,
                    "task_id": "failed",
                    "status": "failed",
                    "origin": "execution",
                },
            )
            self.assertEqual(
                harness.history_failures(root), {("failed", "a" * 64)}
            )

    def test_weighted_fit_uses_cpu_memory_and_scratch(self) -> None:
        host = harness.HostResources(
            cpus=4,
            memory_available_mib=1000,
            disk_available_mib=1000,
            memory_reserve_mib=1,
            disk_reserve_mib=1,
            cpu_budget=4,
            memory_budget_mib=100,
            disk_budget_mib=100,
            memory_source="fixture",
        )
        a = task("a", ("true",))
        a.task = dataclasses.replace(
            a.task,
            resources=dataclasses.replace(
                a.task.resources, cpu=3, memory_mib=60, scratch_mib=60
            ),
        )
        c = task("c", ("true",))
        c.task = dataclasses.replace(
            c.task,
            resources=dataclasses.replace(
                c.task.resources, cpu=1, memory_mib=40, scratch_mib=40
            ),
        )
        b = task("b", ("true",))
        b.task = dataclasses.replace(
            b.task,
            resources=dataclasses.replace(
                b.task.resources, cpu=2, memory_mib=60, scratch_mib=60
            ),
        )
        active = [
            harness.RunningTask(
                planned=a,
                process=None,  # type: ignore[arg-type]
                log=None,  # type: ignore[arg-type]
                log_path=pathlib.Path("a"),
                result_path=pathlib.Path("a"),
                started_monotonic=0,
                started_at="",
                attempt=1,
                container_name=None,
            )
        ]
        self.assertTrue(harness.can_fit(c, active, host, False))
        self.assertFalse(harness.can_fit(b, active, host, False))

    def test_adaptive_allocation_preserves_minima_then_rewards_priority(
        self,
    ) -> None:
        host = harness.HostResources(
            cpus=8,
            memory_available_mib=1000,
            disk_available_mib=1000,
            memory_reserve_mib=1,
            disk_reserve_mib=1,
            cpu_budget=6,
            memory_budget_mib=700,
            disk_budget_mib=100,
            memory_source="fixture",
            pid_budget=64,
        )
        high = task("high", ("true",), score=100)
        low = task("low", ("true",), score=1)
        for item in (high, low):
            item.task = dataclasses.replace(
                item.task,
                resources=dataclasses.replace(
                    item.task.resources,
                    cpu=4,
                    cpu_min=1,
                    memory_mib=400,
                    memory_mib_min=100,
                    pids=32,
                ),
            )
        allocated = harness.allocate_ready_tasks(
            [high, low], [], host, best_effort=False
        )
        self.assertEqual([item.task.task_id for item, _ in allocated], ["high", "low"])
        self.assertEqual(allocated[0][1].cpu, 4)
        self.assertEqual(allocated[0][1].memory_mib, 400)
        self.assertEqual(allocated[1][1].cpu, 2)
        self.assertEqual(allocated[1][1].memory_mib, 300)

        pid_limited = dataclasses.replace(host, pid_budget=32)
        allocated = harness.allocate_ready_tasks(
            [high, low], [], pid_limited, best_effort=False
        )
        self.assertEqual([item.task.task_id for item, _ in allocated], ["high"])

    def test_task_fit_uses_reviewed_minimum_not_desired_ceiling(self) -> None:
        item = task("variable", ("true",))
        item.task = dataclasses.replace(
            item.task,
            resources=dataclasses.replace(
                item.task.resources,
                cpu=8,
                cpu_min=2,
                memory_mib=1024,
                memory_mib_min=256,
            ),
        )
        host = harness.HostResources(
            cpus=2,
            memory_available_mib=512,
            disk_available_mib=100,
            memory_reserve_mib=1,
            disk_reserve_mib=1,
            cpu_budget=2,
            memory_budget_mib=256,
            disk_budget_mib=100,
            memory_source="fixture",
        )
        harness.validate_task_fits(item, host)
        self.assertTrue(harness.can_fit(item, [], host, False))

        pressured = dataclasses.replace(
            host,
            memory_budget_mib=64,
            disk_budget_mib=8,
            pid_budget=8,
            memory_capacity_mib=256,
            disk_capacity_mib=100,
            pid_capacity=32,
        )
        # Temporary pressure prevents immediate admission but is not
        # misclassified as permanent machine incapacity.
        harness.validate_task_fits(item, pressured)
        self.assertFalse(harness.can_fit(item, [], pressured, False))

    def test_unfit_branch_does_not_discard_independent_information(self) -> None:
        for policy in ("adaptive", "fail-fast", "keep-going"):
            with self.subTest(policy=policy), tempfile.TemporaryDirectory() as temporary:
                root = pathlib.Path(temporary)
                marker = root / "independent"
                unfit = task("unfit", ("true",), score=100)
                unfit.task = dataclasses.replace(
                    unfit.task,
                    resources=dataclasses.replace(
                        unfit.task.resources,
                        memory_mib=2048,
                        memory_mib_min=2048,
                    ),
                )
                dependent = task(
                    "dependent", ("true",), dependencies=("unfit",), phase=1
                )
                independent = task(
                    "independent",
                    (
                        sys.executable,
                        "-c",
                        f"from pathlib import Path; Path({str(marker)!r}).write_text('ok')",
                    ),
                    phase=1,
                )
                run_root = root / ".local" / "verify" / "runs" / "unfit"
                (run_root / "tasks").mkdir(parents=True)
                host = harness.HostResources(
                    2, 4096, 4096, 1, 1, 2, 1024, 1024, "fixture"
                )
                profile = harness.Profile(
                    "unfit",
                    "unfit",
                    ("unfit", "dependent", "independent"),
                    ("unfit",),
                    False,
                    policy,
                    False,
                    False,
                    False,
                    False,
                )
                probe = harness.EngineProbe(
                    "none",
                    None,
                    None,
                    False,
                    False,
                    "fixture",
                    {},
                    None,
                    None,
                    None,
                )
                planned = {
                    "unfit": unfit,
                    "dependent": dependent,
                    "independent": independent,
                }
                results, _ = harness.run_scheduler(
                    root=root,
                    manifest=None,  # type: ignore[arg-type]
                    profile=profile,
                    policy=policy,
                    run_id="unfit",
                    run_root=run_root,
                    planned=planned,
                    probe=probe,
                    host=host,
                    best_effort=True,
                    publish_results=False,
                    resume=False,
                )
                self.assertEqual(results["unfit"]["status"], "blocked")
                self.assertEqual(results["unfit"]["origin"], "host_capacity")
                self.assertEqual(results["dependent"]["status"], "blocked")
                self.assertEqual(results["dependent"]["origin"], "dependency")
                self.assertEqual(results["independent"]["status"], "passed")
                self.assertTrue(marker.is_file())

    def test_adaptive_finishes_phase_peer_and_blocks_later_phase(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            marker_b = root / "b"
            marker_c = root / "c"
            planned = {
                "a": task(
                    "a",
                    (sys.executable, "-c", "raise SystemExit(3)"),
                    score=100,
                ),
                "b": task(
                    "b",
                    (
                        sys.executable,
                        "-c",
                        f"from pathlib import Path; Path({str(marker_b)!r}).write_text('b')",
                    ),
                    score=50,
                ),
                "c": task(
                    "c",
                    (
                        sys.executable,
                        "-c",
                        f"from pathlib import Path; Path({str(marker_c)!r}).write_text('c')",
                    ),
                    phase=1,
                    score=1000,
                ),
            }
            run_root = root / ".local" / "verify" / "runs" / "fixture"
            (run_root / "tasks").mkdir(parents=True)
            host = harness.HostResources(
                cpus=4,
                memory_available_mib=8192,
                disk_available_mib=8192,
                memory_reserve_mib=1,
                disk_reserve_mib=1,
                cpu_budget=4,
                memory_budget_mib=1024,
                disk_budget_mib=1024,
                memory_source="fixture",
            )
            profile = harness.Profile(
                name="fixture",
                description="fixture",
                tasks=("a", "b", "c"),
                always=("a",),
                changed_only=False,
                policy="adaptive",
                reuse_results=False,
                publish_results=False,
                require_clean=False,
                require_hard_isolation=False,
            )
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            results, interrupted = harness.run_scheduler(
                root=root,
                manifest=None,  # type: ignore[arg-type]
                profile=profile,
                policy="adaptive",
                run_id="fixture",
                run_root=run_root,
                planned=planned,
                probe=probe,
                host=host,
                best_effort=True,
                publish_results=False,
                resume=False,
            )
            self.assertFalse(interrupted)
            self.assertEqual(results["a"]["status"], "failed")
            self.assertEqual(results["b"]["status"], "passed")
            self.assertEqual(results["c"]["status"], "blocked")
            self.assertTrue(marker_b.is_file())
            self.assertFalse(marker_c.exists())

    def test_plan_fingerprint_ignores_live_cache_status(self) -> None:
        item = task("a", ("true",))
        planned = {"a": item}
        profile = harness.Profile(
            "p", "p", ("a",), ("a",), False, "adaptive", True, True, False, True
        )
        manifest = harness.Manifest(
            pathlib.Path("/manifest"), "a" * 64, {}, {}, {}, {"a": item.task}, {"p": profile}
        )
        probe = harness.EngineProbe(
            "none", None, None, False, False, "fixture", {}, None, None, None
        )
        host = harness.HostResources(1, 1, 1, 1, 1, 1, 1, 1, "fixture")
        first = harness.plan_payload(
            manifest, profile, "adaptive", "r", [], "fixture", {}, probe, host, planned
        )
        item.status = "cached"
        item.reason = "hit"
        second = harness.plan_payload(
            manifest, profile, "adaptive", "r", [], "fixture", {}, probe, host, planned
        )
        self.assertEqual(first["plan_fingerprint"], second["plan_fingerprint"])
        observed_elsewhere = dataclasses.replace(
            probe,
            executable="/different/engine",
            reason="different current host",
            identity={
                "server_version": "different",
                "cgroup_version": "2",
                "storage_driver": "different",
            },
        )
        third = harness.plan_payload(
            manifest,
            profile,
            "adaptive",
            "r",
            [],
            "fixture",
            {},
            observed_elsewhere,
            dataclasses.replace(host, memory_budget_mib=999),
            planned,
        )
        self.assertEqual(first["plan_fingerprint"], third["plan_fingerprint"])

    def test_keep_going_runs_unrelated_later_phase(self) -> None:
        results, marker = self.run_policy_fixture("keep-going")
        self.assertEqual(results["a"]["status"], "failed")
        self.assertEqual(results["later"]["status"], "passed")
        self.assertTrue(marker.is_file())

    def test_fail_fast_blocks_unstarted_later_phase(self) -> None:
        results, marker = self.run_policy_fixture("fail-fast")
        self.assertEqual(results["a"]["status"], "failed")
        self.assertEqual(results["later"]["status"], "blocked")
        self.assertFalse(marker.exists())

    def test_kernel_lock_prevents_concurrent_resume(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            first = harness.acquire_run_lock(root)
            try:
                with self.assertRaisesRegex(harness.HarnessError, "another coordinator"):
                    harness.acquire_run_lock(root)
            finally:
                first.close()
            second = harness.acquire_run_lock(root)
            second.close()

    def test_global_lock_prevents_different_run_ids_overcommitting(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            first = harness.acquire_global_execution_lock(root, "run-one")
            self.assertEqual(
                sorted(path.name for path in root.iterdir()),
                [f".paradox-verify-{os.getuid()}.execution.lock"],
            )
            try:
                with self.assertRaisesRegex(
                    harness.HarnessError, "already admitting"
                ):
                    harness.acquire_global_execution_lock(root, "run-two")
            finally:
                first.close()

    def test_each_coordinator_invocation_retains_current_host_receipt(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            run_root = root / ".local" / "verify" / "runs" / "r"
            run_root.mkdir(parents=True)
            host = harness.HostResources(
                2, 4096, 8192, 1024, 1024, 2, 2048, 4096, "fixture"
            )
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            first_id, first_hash = harness.record_invocation(
                root, run_root, host=host, probe=probe, resume=False
            )
            abandoned = (
                run_root
                / "invocations"
                / ".invocation-002.json.new.123.456"
            )
            abandoned.write_text("partial", encoding="ascii")
            second_id, second_hash = harness.record_invocation(
                root,
                run_root,
                host=dataclasses.replace(host, memory_budget_mib=1024),
                probe=probe,
                resume=True,
            )
            self.assertEqual(first_id, "invocation-001")
            self.assertEqual(second_id, "invocation-002")
            self.assertFalse(abandoned.exists())
            self.assertNotEqual(first_hash, second_hash)
            second = json.loads(
                (
                    run_root / "invocations" / "invocation-002.json"
                ).read_text(encoding="utf-8")
            )
            self.assertTrue(second["resume"])
            self.assertEqual(
                second["engine"]["host"]["memory_budget_mib"], 1024
            )

    def test_attempt_run_ids_are_unique_and_bounded(self) -> None:
        first = harness.attempt_run_id("release", "native-release", 1)
        second = harness.attempt_run_id("release", "native-release", 2)
        self.assertNotEqual(first, second)
        self.assertLessEqual(len(first), 128)
        long = harness.attempt_run_id("x" * 128, "native-release", 999)
        self.assertLessEqual(len(long), 128)
        self.assertRegex(long, harness.SAFE_NAME)

    def test_timeout_and_launch_failure_remain_distinct(self) -> None:
        missing = self.run_single_task(("/definitely/absent/paradox-command",))
        self.assertEqual(missing["status"], "infrastructure")
        timed = self.run_single_task(
            (sys.executable, "-c", "import time; time.sleep(10)"),
            timeout_seconds=1,
        )
        self.assertEqual(timed["status"], "timeout")

    def test_interrupt_during_launch_cancels_the_new_worker(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = pathlib.Path(temporary)
            item = task(
                "interrupt",
                (
                    sys.executable,
                    "-c",
                    (
                        "import os, signal, time; "
                        "os.kill(os.getppid(), signal.SIGTERM); time.sleep(30)"
                    ),
                ),
            )
            run_root = root / ".local" / "verify" / "runs" / "interrupt"
            (run_root / "tasks").mkdir(parents=True)
            host = harness.HostResources(
                2, 4096, 4096, 1, 1, 2, 1024, 1024, "fixture"
            )
            profile = harness.Profile(
                "interrupt",
                "interrupt",
                ("interrupt",),
                ("interrupt",),
                False,
                "adaptive",
                False,
                False,
                False,
                False,
            )
            probe = harness.EngineProbe(
                "none", None, None, False, False, "fixture", {}, None, None, None
            )
            results, interrupted = harness.run_scheduler(
                root=root,
                manifest=None,  # type: ignore[arg-type]
                profile=profile,
                policy="adaptive",
                run_id="interrupt",
                run_root=run_root,
                planned={"interrupt": item},
                probe=probe,
                host=host,
                best_effort=True,
                publish_results=False,
                resume=False,
            )
            self.assertTrue(interrupted)
            self.assertEqual(results["interrupt"]["status"], "cancelled")


if __name__ == "__main__":
    unittest.main()
