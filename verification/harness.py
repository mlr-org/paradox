#!/usr/bin/env python3
"""Portable, resource-contained orchestration for Paradox verification.

This controller deliberately treats the existing gate drivers as semantic
authorities.  It supplies the missing cross-gate DAG, exact development-result
cache, change-aware ordering, weighted resource admission, resumability, and a
backend-neutral Podman/Docker containment boundary.

Only the Python standard library is used.  The code is intentionally usable by
the repository-local Python and by an ordinary recent system Python for
``doctor`` and ``plan``.
"""

from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import fnmatch
import fcntl
import hashlib
import json
import math
import os
import pathlib
import platform
import re
import resource
import shutil
import signal
import stat
import string
import subprocess
import sys
import tempfile
import time
from collections import defaultdict, deque
from typing import Any, BinaryIO, Iterable, Mapping, Sequence


SCHEMA = 1
RESULT_SCHEMA = 1
CACHE_SCHEMA = 1
PLAN_SCHEMA = 1
SAFE_NAME = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
SAFE_PARAMETER = re.compile(r"^[a-z][a-z0-9_]{0,63}$")
TERMINAL_SUCCESS = {"passed", "cached"}
TERMINAL_FAILURE = {
    "failed",
    "timeout",
    "oom",
    "infrastructure",
    "cancelled",
    "blocked",
}
ALLOWED_FAILURE_CLASSES = {"fatal", "blocker", "semantic", "advisory"}
ALLOWED_CACHE = {"none", "success"}
ALLOWED_POLICIES = {"adaptive", "keep-going", "fail-fast"}
ALLOWED_ISOLATION = {"worker"}
ALLOWED_CONTAINMENT = {"auto", "worker", "aggregate"}
EXCLUDED_INPUT_PARTS = {".git", ".local", ".cache", "__pycache__"}
FORMATTER = string.Formatter()
ATTEMPT_ID_SENTINEL = "__PARADOX_VERIFY_ATTEMPT_ID__"
CGROUP_UNLIMITED_THRESHOLD = 1 << 60
SYSTEMCTL = pathlib.Path("/usr/bin/systemctl")
AGGREGATE_UNIT = re.compile(
    r"^paradox-verify-aggregate-u([0-9]+)-[A-Za-z0-9_.-]+\.service$"
)
ACTIVATION_WRITABLE_PATHS = (
    ".local/reticulate",
    ".local/texmf/var",
    ".local/texmf/config",
    ".local/texmf/home",
    ".local/texmf/cache",
    ".local/texmf/fonts",
    ".cache/R",
    ".cache/ccache",
    ".cache/pip",
    ".cache/uv",
)
ACTIVATION_PRIVATE_PATHS = (
    ".local/tmp",
    ".local/runtime",
    ".local/runtime-matrix/libraries",
    ".local/runtime-matrix/tmp",
    ".local/runtime-matrix/runtime",
    ".cache/runtime-matrix",
)


class HarnessError(RuntimeError):
    """A user-facing harness or invariant error."""


class FatalRunError(HarnessError):
    """An invariant failure that invalidates the complete run."""


def canonical_json(value: Any) -> bytes:
    return (
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True)
        + "\n"
    ).encode("utf-8")


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sha256_file(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        while True:
            block = stream.read(1024 * 1024)
            if not block:
                return digest.hexdigest()
            digest.update(block)


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def generated_run_id(profile_name: str) -> str:
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    return f"verify-{profile_name}-{stamp}-{os.getpid()}"


def reject_duplicate_keys(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise HarnessError(f"duplicate JSON key: {key}")
        result[key] = value
    return result


def require_plain_file(path: pathlib.Path, label: str) -> pathlib.Path:
    try:
        details = path.lstat()
    except FileNotFoundError as exc:
        raise HarnessError(f"{label} is absent: {path}") from exc
    if stat.S_ISLNK(details.st_mode) or not stat.S_ISREG(details.st_mode):
        raise HarnessError(f"{label} is not one plain regular file: {path}")
    return path


def require_plain_directory(path: pathlib.Path, label: str, create: bool = False) -> pathlib.Path:
    if create and not path.exists():
        path.mkdir(parents=True)
    try:
        details = path.lstat()
    except FileNotFoundError as exc:
        raise HarnessError(f"{label} is absent: {path}") from exc
    if stat.S_ISLNK(details.st_mode) or not stat.S_ISDIR(details.st_mode):
        raise HarnessError(f"{label} is not one plain directory: {path}")
    return path


def ensure_managed_directory(root: pathlib.Path, path: pathlib.Path) -> pathlib.Path:
    root = root.resolve()
    try:
        relative = path.relative_to(root)
    except ValueError as exc:
        raise FatalRunError(f"managed path escapes the repository: {path}") from exc
    current = root
    for part in relative.parts:
        if part in {"", ".", ".."}:
            raise FatalRunError(f"unsafe managed path component: {path}")
        current = current / part
        if current.is_symlink():
            raise FatalRunError(f"managed path follows a symbolic link: {current}")
        if current.exists() and not current.is_dir():
            raise FatalRunError(f"managed path component is not a directory: {current}")
        if not current.exists():
            current.mkdir()
    return path


def atomic_write(path: pathlib.Path, payload: bytes, mode: int = 0o600) -> None:
    require_plain_directory(path.parent, "atomic output parent")
    temporary = path.parent / f".{path.name}.new.{os.getpid()}.{time.time_ns()}"
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    descriptor = os.open(temporary, flags, mode)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        try:
            temporary.unlink()
        except FileNotFoundError:
            pass


def run_capture(
    command: Sequence[str],
    *,
    cwd: pathlib.Path,
    timeout: float = 20,
    environment: Mapping[str, str] | None = None,
    check: bool = True,
) -> subprocess.CompletedProcess[str]:
    try:
        result = subprocess.run(
            list(command),
            cwd=cwd,
            env=None if environment is None else dict(environment),
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise HarnessError(f"could not execute {' '.join(command)}: {exc}") from exc
    if check and result.returncode != 0:
        diagnostic = (result.stderr or result.stdout).strip()
        if len(diagnostic) > 600:
            diagnostic = diagnostic[-600:]
        raise HarnessError(
            f"command failed ({result.returncode}): {' '.join(command)}"
            + (f": {diagnostic}" if diagnostic else "")
        )
    return result


def placeholders(value: str) -> set[str]:
    result: set[str] = set()
    try:
        parsed = FORMATTER.parse(value)
        for _, field, format_spec, conversion in parsed:
            if field is None:
                continue
            if not SAFE_PARAMETER.fullmatch(field) or format_spec or conversion:
                raise HarnessError(f"unsupported placeholder in {value!r}")
            result.add(field)
    except ValueError as exc:
        raise HarnessError(f"malformed placeholder string: {value!r}") from exc
    return result


def expand(value: str, parameters: Mapping[str, str]) -> str:
    missing = placeholders(value) - parameters.keys()
    if missing:
        raise HarnessError(
            f"missing parameter(s) {', '.join(sorted(missing))} for {value!r}"
        )
    return value.format_map(parameters)


@dataclasses.dataclass(frozen=True)
class Resources:
    # cpu/memory_mib are desired ceilings. The scheduler may lower them to the
    # explicit minima so the same task can run efficiently on smaller hosts.
    cpu: float
    memory_mib: int
    pids: int
    scratch_mib: int
    timeout_seconds: int
    cpu_min: float | None = None
    memory_mib_min: int | None = None

    @property
    def minimum_cpu(self) -> float:
        return self.cpu if self.cpu_min is None else self.cpu_min

    @property
    def minimum_memory_mib(self) -> int:
        return self.memory_mib if self.memory_mib_min is None else self.memory_mib_min


@dataclasses.dataclass(frozen=True)
class Allocation:
    cpu: float
    memory_mib: int
    pids: int
    scratch_mib: int


@dataclasses.dataclass(frozen=True)
class Task:
    task_id: str
    description: str
    command: tuple[str, ...]
    dependencies: tuple[str, ...]
    phase: int
    priority: int
    information: float
    estimated_seconds: int
    resources: Resources
    failure_class: str
    cache: str
    input_groups: tuple[str, ...]
    inputs: tuple[str, ...]
    impacts: tuple[str, ...]
    environment: Mapping[str, str]
    required_parameters: tuple[str, ...]
    required_values: Mapping[str, tuple[str, ...]]
    isolation: str
    network: bool
    image: str | None
    writable_paths: tuple[str, ...]
    readonly_paths: tuple[str, ...]
    platforms: tuple[str, ...]
    machines: tuple[str, ...]
    revalidate_on_resume: bool


def task_platform_incompatibility(task: Task) -> str:
    failures: list[str] = []
    if task.platforms and sys.platform not in task.platforms:
        failures.append(
            f"platform {sys.platform!r} is outside {', '.join(task.platforms)}"
        )
    machine = platform.machine()
    if task.machines and machine not in task.machines:
        failures.append(
            f"machine {machine!r} is outside {', '.join(task.machines)}"
        )
    return "; ".join(failures)


@dataclasses.dataclass(frozen=True)
class Profile:
    name: str
    description: str
    tasks: tuple[str, ...]
    always: tuple[str, ...]
    changed_only: bool
    policy: str
    reuse_results: bool
    publish_results: bool
    require_clean: bool
    require_hard_isolation: bool


@dataclasses.dataclass(frozen=True)
class Manifest:
    path: pathlib.Path
    digest: str
    defaults: Mapping[str, Any]
    input_groups: Mapping[str, tuple[str, ...]]
    parameter_defaults: Mapping[str, str]
    tasks: Mapping[str, Task]
    profiles: Mapping[str, Profile]


def _require_keys(value: Mapping[str, Any], allowed: set[str], label: str) -> None:
    extras = set(value) - allowed
    if extras:
        raise HarnessError(f"{label} has unknown field(s): {', '.join(sorted(extras))}")


def _positive_int(value: Any, label: str, maximum: int = 10**9) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or not (1 <= value <= maximum):
        raise HarnessError(f"{label} must be a positive integer <= {maximum}")
    return value


def _nonnegative_int(value: Any, label: str, maximum: int = 10**9) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or not (0 <= value <= maximum):
        raise HarnessError(f"{label} must be a non-negative integer <= {maximum}")
    return value


def _positive_number(value: Any, label: str, maximum: float = 10**6) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise HarnessError(f"{label} must be a positive number")
    converted = float(value)
    if not math.isfinite(converted) or converted <= 0 or converted > maximum:
        raise HarnessError(f"{label} must be a positive finite number <= {maximum}")
    return converted


def _string_list(
    value: Any,
    label: str,
    allow_empty: bool = True,
    unique: bool = True,
) -> tuple[str, ...]:
    if not isinstance(value, list) or (not allow_empty and not value):
        raise HarnessError(f"{label} must be a JSON array of strings")
    if any(not isinstance(item, str) or not item or "\x00" in item for item in value):
        raise HarnessError(f"{label} must contain non-empty strings without NUL")
    if unique and len(value) != len(set(value)):
        raise HarnessError(f"{label} contains duplicates")
    return tuple(value)


def load_manifest(root: pathlib.Path, path: pathlib.Path) -> Manifest:
    root = root.resolve()
    path = require_plain_file(path, "verification manifest")
    if path.stat().st_size > 4 * 1024 * 1024:
        raise HarnessError("verification manifest is unexpectedly large")
    try:
        raw = path.read_bytes()
        data = json.loads(raw, object_pairs_hook=reject_duplicate_keys)
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise HarnessError(f"cannot parse verification manifest: {exc}") from exc
    if not isinstance(data, dict):
        raise HarnessError("verification manifest must be one JSON object")
    _require_keys(
        data,
        {
            "schema",
            "defaults",
            "parameters",
            "input_groups",
            "profiles",
            "tasks",
        },
        "verification manifest",
    )
    if data.get("schema") != SCHEMA:
        raise HarnessError(f"verification manifest schema must be {SCHEMA}")

    defaults = data.get("defaults")
    if not isinstance(defaults, dict):
        raise HarnessError("manifest defaults must be one object")
    _require_keys(
        defaults,
        {
            "cpu",
            "memory_mib",
            "pids",
            "scratch_mib",
            "timeout_seconds",
            "failure_class",
            "cache",
            "estimated_seconds",
            "information",
            "host_memory_reserve_mib",
            "host_memory_reserve_fraction",
            "host_disk_reserve_mib",
            "host_pid_budget",
            "engine_overhead_mib",
            "admission_timeout_seconds",
            "platforms",
            "machines",
        },
        "manifest defaults",
    )
    default_resources = Resources(
        cpu=_positive_number(defaults.get("cpu", 1), "defaults.cpu"),
        memory_mib=_positive_int(
            defaults.get("memory_mib", 2048), "defaults.memory_mib"
        ),
        pids=_positive_int(defaults.get("pids", 256), "defaults.pids"),
        scratch_mib=_positive_int(
            defaults.get("scratch_mib", 1024), "defaults.scratch_mib"
        ),
        timeout_seconds=_positive_int(
            defaults.get("timeout_seconds", 3600),
            "defaults.timeout_seconds",
            7 * 24 * 3600,
        ),
    )
    default_platforms = _string_list(
        defaults.get("platforms", []), "defaults.platforms"
    )
    if any(value not in {"linux", "darwin", "win32"} for value in default_platforms):
        raise HarnessError(
            "defaults.platforms contains an unsupported Python platform"
        )
    default_machines = _string_list(
        defaults.get("machines", []), "defaults.machines"
    )
    if any(
        not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,63}", value)
        for value in default_machines
    ):
        raise HarnessError("defaults.machines contains an unsafe machine name")

    parameter_data = data.get("parameters", {})
    if not isinstance(parameter_data, dict):
        raise HarnessError("manifest parameters must be one object")
    parameter_defaults: dict[str, str] = {}
    for name, default in parameter_data.items():
        if not SAFE_PARAMETER.fullmatch(name):
            raise HarnessError(f"unsafe manifest parameter name: {name}")
        if not isinstance(default, str) or "\x00" in default:
            raise HarnessError(f"default for parameter {name} must be a string")
        parameter_defaults[name] = default

    groups_data = data.get("input_groups")
    if not isinstance(groups_data, dict) or not groups_data:
        raise HarnessError("manifest input_groups must be one non-empty object")
    input_groups: dict[str, tuple[str, ...]] = {}
    for name, patterns_data in groups_data.items():
        if not SAFE_PARAMETER.fullmatch(name):
            raise HarnessError(f"unsafe input group name: {name}")
        patterns = _string_list(patterns_data, f"input group {name}", allow_empty=False)
        for pattern in patterns:
            _validate_input_pattern(pattern, f"input group {name}")
        input_groups[name] = patterns

    tasks_data = data.get("tasks")
    if not isinstance(tasks_data, list) or not tasks_data:
        raise HarnessError("manifest tasks must be one non-empty array")
    tasks: dict[str, Task] = {}
    for index, item in enumerate(tasks_data):
        label = f"task {index + 1}"
        if not isinstance(item, dict):
            raise HarnessError(f"{label} must be one object")
        _require_keys(
            item,
            {
                "id",
                "description",
                "command",
                "dependencies",
                "phase",
                "priority",
                "information",
                "estimated_seconds",
                "cpu",
                "cpu_min",
                "memory_mib",
                "memory_mib_min",
                "pids",
                "scratch_mib",
                "timeout_seconds",
                "failure_class",
                "cache",
                "input_groups",
                "inputs",
                "impacts",
                "environment",
                "required_parameters",
                "required_values",
                "isolation",
                "network",
                "image",
                "writable_paths",
                "readonly_paths",
                "platforms",
                "machines",
                "revalidate_on_resume",
            },
            label,
        )
        task_id = item.get("id")
        if not isinstance(task_id, str) or not SAFE_NAME.fullmatch(task_id):
            raise HarnessError(f"{label}.id has an unsafe shape")
        if task_id in tasks:
            raise HarnessError(f"duplicate task id: {task_id}")
        description = item.get("description")
        if not isinstance(description, str) or not description.strip():
            raise HarnessError(f"{label}.description must be non-empty")
        command = _string_list(
            item.get("command"),
            f"{label}.command",
            allow_empty=False,
            unique=False,
        )
        dependencies = _string_list(
            item.get("dependencies", []), f"{label}.dependencies"
        )
        phase = _nonnegative_int(item.get("phase", 0), f"{label}.phase", 1000)
        priority = item.get("priority", 0)
        if isinstance(priority, bool) or not isinstance(priority, int) or abs(priority) > 10**6:
            raise HarnessError(f"{label}.priority must be an integer of bounded size")
        information = _positive_number(
            item.get("information", defaults.get("information", 1)),
            f"{label}.information",
        )
        estimated_seconds = _positive_int(
            item.get(
                "estimated_seconds", defaults.get("estimated_seconds", 300)
            ),
            f"{label}.estimated_seconds",
            7 * 24 * 3600,
        )
        maximum_cpu = _positive_number(
            item.get("cpu", default_resources.cpu), f"{label}.cpu"
        )
        minimum_cpu = _positive_number(
            item.get("cpu_min", maximum_cpu), f"{label}.cpu_min"
        )
        maximum_memory = _positive_int(
                item.get("memory_mib", default_resources.memory_mib),
                f"{label}.memory_mib",
        )
        minimum_memory = _positive_int(
            item.get("memory_mib_min", maximum_memory),
            f"{label}.memory_mib_min",
        )
        if minimum_cpu > maximum_cpu or minimum_memory > maximum_memory:
            raise HarnessError(
                f"{label} resource minima may not exceed their desired ceilings"
            )
        resources = Resources(
            cpu=maximum_cpu,
            memory_mib=maximum_memory,
            pids=_positive_int(
                item.get("pids", default_resources.pids), f"{label}.pids"
            ),
            scratch_mib=_positive_int(
                item.get("scratch_mib", default_resources.scratch_mib),
                f"{label}.scratch_mib",
            ),
            timeout_seconds=_positive_int(
                item.get("timeout_seconds", default_resources.timeout_seconds),
                f"{label}.timeout_seconds",
                7 * 24 * 3600,
            ),
            cpu_min=minimum_cpu,
            memory_mib_min=minimum_memory,
        )
        failure_class = item.get(
            "failure_class", defaults.get("failure_class", "semantic")
        )
        if failure_class not in ALLOWED_FAILURE_CLASSES:
            raise HarnessError(
                f"{label}.failure_class must be one of "
                f"{', '.join(sorted(ALLOWED_FAILURE_CLASSES))}"
            )
        cache = item.get("cache", defaults.get("cache", "success"))
        if cache not in ALLOWED_CACHE:
            raise HarnessError(
                f"{label}.cache must be one of {', '.join(sorted(ALLOWED_CACHE))}"
            )
        group_names = _string_list(
            item.get("input_groups", []), f"{label}.input_groups"
        )
        unknown_groups = set(group_names) - input_groups.keys()
        if unknown_groups:
            raise HarnessError(
                f"{label} references unknown input group(s): "
                f"{', '.join(sorted(unknown_groups))}"
            )
        inputs = _string_list(item.get("inputs", []), f"{label}.inputs")
        impacts = _string_list(item.get("impacts", []), f"{label}.impacts")
        for pattern in inputs:
            _validate_input_pattern(pattern, f"{label}.inputs")
        for pattern in impacts:
            _validate_input_pattern(pattern, f"{label}.impacts")
        environment = item.get("environment", {})
        if not isinstance(environment, dict):
            raise HarnessError(f"{label}.environment must be one object")
        checked_environment: dict[str, str] = {}
        for name, value in environment.items():
            if not re.fullmatch(r"[A-Z][A-Z0-9_]{0,127}", name):
                raise HarnessError(f"{label} has unsafe environment name: {name}")
            if not isinstance(value, str) or "\x00" in value:
                raise HarnessError(f"{label} environment {name} must be a string")
            checked_environment[name] = value
        required_parameters = _string_list(
            item.get("required_parameters", []), f"{label}.required_parameters"
        )
        if any(not SAFE_PARAMETER.fullmatch(name) for name in required_parameters):
            raise HarnessError(f"{label}.required_parameters has an unsafe name")
        required_values_data = item.get("required_values", {})
        if not isinstance(required_values_data, dict):
            raise HarnessError(f"{label}.required_values must be one object")
        required_values: dict[str, tuple[str, ...]] = {}
        for name, allowed_data in required_values_data.items():
            if not SAFE_PARAMETER.fullmatch(name):
                raise HarnessError(
                    f"{label}.required_values has an unsafe parameter name"
                )
            if name not in parameter_defaults:
                raise HarnessError(
                    f"{label}.required_values references undeclared parameter {name}"
                )
            allowed = _string_list(
                allowed_data,
                f"{label}.required_values.{name}",
                allow_empty=False,
            )
            required_values[name] = allowed
        isolation = item.get("isolation", "worker")
        if isolation not in ALLOWED_ISOLATION:
            raise HarnessError(
                f"{label}.isolation must be one of "
                f"{', '.join(sorted(ALLOWED_ISOLATION))}"
            )
        network = item.get("network", False)
        if not isinstance(network, bool):
            raise HarnessError(f"{label}.network must be Boolean")
        image = item.get("image")
        if image is not None and (
            not isinstance(image, str) or not image or "\x00" in image
        ):
            raise HarnessError(f"{label}.image must be a non-empty string or null")
        writable_paths = _string_list(
            item.get("writable_paths", []), f"{label}.writable_paths"
        )
        readonly_paths = _string_list(
            item.get("readonly_paths", []), f"{label}.readonly_paths"
        )
        platforms = _string_list(
            item.get("platforms", list(default_platforms)), f"{label}.platforms"
        )
        if any(value not in {"linux", "darwin", "win32"} for value in platforms):
            raise HarnessError(
                f"{label}.platforms contains an unsupported Python platform"
            )
        machines = _string_list(
            item.get("machines", list(default_machines)), f"{label}.machines"
        )
        if any(
            not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,63}", value)
            for value in machines
        ):
            raise HarnessError(f"{label}.machines contains an unsafe machine name")
        revalidate_on_resume = item.get("revalidate_on_resume", False)
        if not isinstance(revalidate_on_resume, bool):
            raise HarnessError(f"{label}.revalidate_on_resume must be Boolean")
        for managed_path in (*writable_paths, *readonly_paths):
            if "\n" in managed_path or "\r" in managed_path:
                raise HarnessError(f"{label} has a malformed managed path")
        all_template_values = (
            list(command)
            + list(checked_environment.values())
            + list(writable_paths)
            + list(readonly_paths)
        )
        if image is not None:
            all_template_values.append(image)
        referenced = set().union(*(placeholders(value) for value in all_template_values))
        declared = (
            set(parameter_defaults)
            | set(required_parameters)
            | set(required_values)
            | {
            "root",
            "run_id",
            "profile",
            "task_id",
            "git_commit",
            "git_tree",
            "attempt_id",
            }
        )
        undeclared = referenced - declared
        if undeclared:
            raise HarnessError(
                f"{label} uses undeclared parameter(s): "
                f"{', '.join(sorted(undeclared))}"
            )
        tasks[task_id] = Task(
            task_id=task_id,
            description=description,
            command=command,
            dependencies=dependencies,
            phase=phase,
            priority=priority,
            information=information,
            estimated_seconds=estimated_seconds,
            resources=resources,
            failure_class=failure_class,
            cache=cache,
            input_groups=group_names,
            inputs=inputs,
            impacts=impacts,
            environment=checked_environment,
            required_parameters=required_parameters,
            required_values=required_values,
            isolation=isolation,
            network=network,
            image=image,
            writable_paths=writable_paths,
            readonly_paths=readonly_paths,
            platforms=platforms,
            machines=machines,
            revalidate_on_resume=revalidate_on_resume,
        )

    for task in tasks.values():
        unknown = set(task.dependencies) - tasks.keys()
        if unknown:
            raise HarnessError(
                f"task {task.task_id} has unknown dependencies: "
                f"{', '.join(sorted(unknown))}"
            )
        if task.task_id in task.dependencies:
            raise HarnessError(f"task {task.task_id} depends on itself")
        for dependency in task.dependencies:
            if tasks[dependency].phase > task.phase:
                raise HarnessError(
                    f"task {task.task_id} phase precedes dependency {dependency}"
                )
    _validate_acyclic(tasks)

    profiles_data = data.get("profiles")
    if not isinstance(profiles_data, dict) or not profiles_data:
        raise HarnessError("manifest profiles must be one non-empty object")
    profiles: dict[str, Profile] = {}
    for name, item in profiles_data.items():
        label = f"profile {name}"
        if not SAFE_NAME.fullmatch(name):
            raise HarnessError(f"unsafe profile name: {name}")
        if not isinstance(item, dict):
            raise HarnessError(f"{label} must be one object")
        _require_keys(
            item,
            {
                "description",
                "tasks",
                "always",
                "changed_only",
                "policy",
                "reuse_results",
                "publish_results",
                "require_clean",
                "require_hard_isolation",
            },
            label,
        )
        description = item.get("description")
        if not isinstance(description, str) or not description.strip():
            raise HarnessError(f"{label}.description must be non-empty")
        selected = _string_list(item.get("tasks"), f"{label}.tasks", allow_empty=False)
        always = _string_list(item.get("always", []), f"{label}.always")
        unknown = (set(selected) | set(always)) - tasks.keys()
        if unknown:
            raise HarnessError(
                f"{label} references unknown task(s): {', '.join(sorted(unknown))}"
            )
        if not set(always).issubset(selected):
            raise HarnessError(f"{label}.always must be a subset of its tasks")
        changed_only = item.get("changed_only", False)
        reuse_results = item.get("reuse_results", True)
        publish_results = item.get("publish_results", True)
        require_clean = item.get("require_clean", False)
        require_hard = item.get("require_hard_isolation", True)
        for field_name, field_value in {
            "changed_only": changed_only,
            "reuse_results": reuse_results,
            "publish_results": publish_results,
            "require_clean": require_clean,
            "require_hard_isolation": require_hard,
        }.items():
            if not isinstance(field_value, bool):
                raise HarnessError(f"{label}.{field_name} must be Boolean")
        policy = item.get("policy", "adaptive")
        if policy not in ALLOWED_POLICIES:
            raise HarnessError(
                f"{label}.policy must be one of {', '.join(sorted(ALLOWED_POLICIES))}"
            )
        profiles[name] = Profile(
            name=name,
            description=description,
            tasks=selected,
            always=always,
            changed_only=changed_only,
            policy=policy,
            reuse_results=reuse_results,
            publish_results=publish_results,
            require_clean=require_clean,
            require_hard_isolation=require_hard,
        )

    return Manifest(
        path=path,
        digest=sha256_bytes(raw),
        defaults=defaults,
        input_groups=input_groups,
        parameter_defaults=parameter_defaults,
        tasks=tasks,
        profiles=profiles,
    )


def _validate_input_pattern(pattern: str, label: str) -> None:
    actual = pattern[1:] if pattern.startswith("?") else pattern
    path = pathlib.PurePosixPath(actual)
    if (
        not actual
        or path.is_absolute()
        or ".." in path.parts
        or "\x00" in actual
    ):
        raise HarnessError(f"{label} contains unsafe repository pattern: {pattern}")
    if any(part in EXCLUDED_INPUT_PARTS for part in path.parts):
        raise HarnessError(f"{label} reaches a generated/private tree: {pattern}")


def _validate_acyclic(tasks: Mapping[str, Task]) -> None:
    visiting: set[str] = set()
    visited: set[str] = set()

    def visit(task_id: str) -> None:
        if task_id in visited:
            return
        if task_id in visiting:
            raise HarnessError(f"verification task dependency cycle reaches {task_id}")
        visiting.add(task_id)
        for dependency in tasks[task_id].dependencies:
            visit(dependency)
        visiting.remove(task_id)
        visited.add(task_id)

    for task_id in tasks:
        visit(task_id)


@dataclasses.dataclass(frozen=True)
class AggregateProbe:
    """Kernel- and systemd-proved containment of the complete controller tree."""

    requested: bool
    hard: bool
    reason: str
    identity: Mapping[str, Any]


def _plain_text(path: pathlib.Path, label: str) -> str:
    require_plain_file(path, label)
    try:
        return path.read_text(encoding="ascii", errors="strict").strip()
    except (OSError, UnicodeError) as exc:
        raise HarnessError(f"could not read {label}: {path}: {exc}") from exc


def _positive_cgroup_integer(value: str, label: str) -> int:
    if not value.isdigit() or int(value) <= 0:
        raise HarnessError(f"{label} is not a positive cgroup integer")
    return int(value)


def _nonnegative_cgroup_integer(value: str, label: str) -> int:
    if not value.isdigit():
        raise HarnessError(f"{label} is not a non-negative cgroup integer")
    return int(value)


def _proc_meminfo_values(proc_root: pathlib.Path) -> dict[str, int]:
    path = proc_root / "meminfo"
    rows: dict[str, int] = {}
    for row in _plain_text(path, "process memory information").splitlines():
        fields = row.split()
        if len(fields) == 3 and fields[0].endswith(":") and fields[2] == "kB":
            name = fields[0][:-1]
            if name in {"MemTotal", "MemAvailable", "SwapTotal"}:
                if name in rows or not fields[1].isdigit():
                    raise HarnessError(f"malformed or duplicate {name} in {path}")
                rows[name] = int(fields[1])
    missing = {"MemTotal", "MemAvailable", "SwapTotal"} - rows.keys()
    if missing or rows["MemTotal"] <= 0 or rows["MemAvailable"] <= 0:
        raise HarnessError(
            "process memory information lacks positive MemTotal/MemAvailable "
            f"or SwapTotal: {', '.join(sorted(missing))}"
        )
    return rows


def _proc_cgroup_rows(proc_root: pathlib.Path) -> list[tuple[str, tuple[str, ...], str]]:
    path = proc_root / "self" / "cgroup"
    rows: list[tuple[str, tuple[str, ...], str]] = []
    seen: set[str] = set()
    for number, row in enumerate(
        _plain_text(path, "process cgroup membership").splitlines(), 1
    ):
        fields = row.split(":", 2)
        if (
            len(fields) != 3
            or not fields[0].isdigit()
            or not fields[2].startswith("/")
        ):
            raise HarnessError(f"malformed process cgroup row {number}")
        relative = pathlib.PurePosixPath(fields[2])
        if ".." in relative.parts or "//" in fields[2] or "\x00" in fields[2]:
            raise HarnessError(f"unsafe process cgroup path on row {number}")
        controllers = tuple(value for value in fields[1].split(",") if value)
        for controller in controllers:
            if controller in seen:
                raise HarnessError(f"duplicate cgroup controller membership: {controller}")
            seen.add(controller)
        rows.append((fields[0], controllers, fields[2]))
    if not rows:
        raise HarnessError("process cgroup membership is empty")
    return rows


def _controller_path(
    rows: Sequence[tuple[str, tuple[str, ...], str]], controller: str
) -> str | None:
    values = [path for _hierarchy, controllers, path in rows if controller in controllers]
    if len(values) > 1:
        raise HarnessError(f"ambiguous {controller} cgroup membership")
    return values[0] if values else None


def _unified_path(
    rows: Sequence[tuple[str, tuple[str, ...], str]]
) -> str | None:
    values = [path for _hierarchy, controllers, path in rows if not controllers]
    if len(values) > 1:
        raise HarnessError("ambiguous unified cgroup membership")
    return values[0] if values else None


def _safe_cgroup_directory(
    controller_root: pathlib.Path, cgroup_path: str, label: str
) -> pathlib.Path:
    controller_root = controller_root.resolve()
    relative = pathlib.PurePosixPath(cgroup_path.lstrip("/"))
    if ".." in relative.parts:
        raise HarnessError(f"unsafe {label} cgroup path")
    current = controller_root
    for part in relative.parts:
        current = current / part
        if current.is_symlink():
            raise HarnessError(f"{label} cgroup path follows a symbolic link: {current}")
    try:
        current.relative_to(controller_root)
    except ValueError as exc:
        raise HarnessError(f"{label} cgroup path escapes its controller") from exc
    if not current.is_dir():
        raise HarnessError(f"{label} cgroup leaf is absent: {current}")
    return current


def _cgroup_process_present(leaf: pathlib.Path, pid: int) -> None:
    values = _plain_text(leaf / "cgroup.procs", "aggregate cgroup process list").split()
    if str(pid) not in values or any(not value.isdigit() for value in values):
        raise HarnessError("the verification controller is not in the proved cgroup leaf")


def _walk_numeric_cgroup_limits(
    leaf: pathlib.Path,
    controller_root: pathlib.Path,
    filename: str,
    *,
    unlimited: frozenset[str],
    label: str,
) -> tuple[int, list[dict[str, Any]]]:
    controller_root = controller_root.resolve()
    current = leaf
    values: list[dict[str, Any]] = []
    while True:
        path = current / filename
        if path.is_file():
            raw = _plain_text(path, f"{label} limit")
            if raw not in unlimited:
                numeric = _positive_cgroup_integer(raw, f"{label} limit")
                if numeric < CGROUP_UNLIMITED_THRESHOLD:
                    values.append({"path": str(path), "value": numeric})
        if current == controller_root:
            break
        try:
            current.relative_to(controller_root)
        except ValueError as exc:
            raise HarnessError(f"{label} cgroup traversal escaped its controller") from exc
        current = current.parent
    if not values:
        raise HarnessError(f"aggregate {label} cgroup has no finite limit")
    return min(value["value"] for value in values), values


def _constraint_headroom(
    constraints: Sequence[Mapping[str, Any]],
    usage_filename: str,
    *,
    label: str,
) -> int:
    """Return the tightest remaining capacity at the matching hierarchy level."""

    headrooms: list[int] = []
    for constraint in constraints:
        try:
            limit = int(constraint["value"])
            limit_path = pathlib.Path(str(constraint["path"]))
        except (KeyError, TypeError, ValueError) as exc:
            raise HarnessError(f"aggregate {label} constraint is malformed") from exc
        usage = _nonnegative_cgroup_integer(
            _plain_text(
                limit_path.parent / usage_filename,
                f"aggregate {label} usage",
            ),
            f"aggregate {label} usage",
        )
        headrooms.append(max(0, limit - usage))
    if not headrooms:
        raise HarnessError(f"aggregate {label} has no constrained headroom")
    return min(headrooms)


def _walk_cpu_limits_v1(
    leaf: pathlib.Path, controller_root: pathlib.Path
) -> tuple[float, list[dict[str, Any]]]:
    controller_root = controller_root.resolve()
    current = leaf
    values: list[dict[str, Any]] = []
    while True:
        quota_path = current / "cpu.cfs_quota_us"
        period_path = current / "cpu.cfs_period_us"
        if quota_path.is_file() and period_path.is_file():
            quota_raw = _plain_text(quota_path, "aggregate CPU quota")
            period = _positive_cgroup_integer(
                _plain_text(period_path, "aggregate CPU period"),
                "aggregate CPU period",
            )
            if re.fullmatch(r"-?[0-9]+", quota_raw):
                quota = int(quota_raw)
                if quota > 0:
                    values.append(
                        {
                            "quota_path": str(quota_path),
                            "period_path": str(period_path),
                            "quota": quota,
                            "period": period,
                            "cpus": quota / period,
                        }
                    )
            else:
                raise HarnessError("aggregate CPU quota is malformed")
        if current == controller_root:
            break
        current = current.parent
    if not values:
        raise HarnessError("aggregate CPU cgroup has no finite quota")
    return min(float(value["cpus"]) for value in values), values


def _walk_cpu_limits_v2(
    leaf: pathlib.Path, controller_root: pathlib.Path
) -> tuple[float, list[dict[str, Any]]]:
    controller_root = controller_root.resolve()
    current = leaf
    values: list[dict[str, Any]] = []
    while True:
        path = current / "cpu.max"
        if path.is_file():
            fields = _plain_text(path, "aggregate CPU quota").split()
            if len(fields) != 2 or not fields[1].isdigit() or int(fields[1]) <= 0:
                raise HarnessError("aggregate CPU quota is malformed")
            if fields[0] != "max":
                quota = _positive_cgroup_integer(fields[0], "aggregate CPU quota")
                period = int(fields[1])
                values.append(
                    {
                        "path": str(path),
                        "quota": quota,
                        "period": period,
                        "cpus": quota / period,
                    }
                )
        if current == controller_root:
            break
        current = current.parent
    if not values:
        raise HarnessError("aggregate CPU cgroup has no finite quota")
    return min(float(value["cpus"]) for value in values), values


def _event_file(path: pathlib.Path, label: str) -> dict[str, int]:
    if not path.exists():
        return {}
    rows: dict[str, int] = {}
    for number, row in enumerate(_plain_text(path, label).splitlines(), 1):
        fields = row.split()
        if len(fields) != 2 or not fields[1].isdigit() or fields[0] in rows:
            raise HarnessError(f"malformed {label} row {number}")
        rows[fields[0]] = int(fields[1])
    return rows


def _constraint_event_key(
    directory: pathlib.Path,
    leaf: pathlib.Path,
    controller_root: pathlib.Path,
    event: str,
) -> str:
    if directory == leaf:
        return event
    try:
        relative = directory.relative_to(controller_root.resolve())
    except ValueError as exc:
        raise HarnessError("aggregate event path escaped its controller") from exc
    return f"{relative.as_posix()}:{event}"


def _hierarchical_scalar_events(
    constraints: Sequence[Mapping[str, Any]],
    leaf: pathlib.Path,
    controller_root: pathlib.Path,
    filename: str,
    *,
    label: str,
) -> tuple[dict[str, int], dict[str, str]]:
    counters: dict[str, int] = {}
    paths: dict[str, str] = {}
    for constraint in constraints:
        try:
            directory = pathlib.Path(str(constraint["path"])).parent
        except KeyError as exc:
            raise HarnessError(f"aggregate {label} constraint is malformed") from exc
        path = directory / filename
        event = filename.rsplit(".", 1)[-1]
        key = _constraint_event_key(
            directory, leaf, controller_root, event
        )
        counters[key] = _nonnegative_cgroup_integer(
            _plain_text(path, f"aggregate {label} counter"),
            f"aggregate {label} counter",
        )
        paths[key] = str(path)
    return counters, paths


def _hierarchical_named_events(
    constraints: Sequence[Mapping[str, Any]],
    leaf: pathlib.Path,
    controller_root: pathlib.Path,
    filename: str,
    names: Sequence[str],
    *,
    label: str,
    required: bool = True,
) -> tuple[dict[str, int], dict[str, str]]:
    counters: dict[str, int] = {}
    paths: dict[str, str] = {}
    for constraint in constraints:
        try:
            directory = pathlib.Path(str(constraint["path"])).parent
        except KeyError as exc:
            raise HarnessError(f"aggregate {label} constraint is malformed") from exc
        path = directory / filename
        if not path.exists() and not required:
            continue
        events = _event_file(path, f"aggregate {label} events")
        missing = set(names) - events.keys()
        if missing:
            if not required:
                continue
            raise HarnessError(
                f"aggregate {label} events lack "
                + ", ".join(sorted(missing))
            )
        for name in names:
            key = _constraint_event_key(
                directory, leaf, controller_root, name
            )
            counters[key] = events[name]
            paths[key] = str(path)
    return counters, paths


def _aggregate_kernel_identity(
    *,
    proc_root: pathlib.Path | None = None,
    cgroup_root: pathlib.Path | None = None,
) -> dict[str, Any]:
    """Read the dedicated service leaf and every effective ancestor limit."""

    if sys.platform != "linux":
        raise HarnessError("aggregate systemd containment is Linux-only")
    proc_root = pathlib.Path("/proc") if proc_root is None else proc_root
    cgroup_root = (
        pathlib.Path("/sys/fs/cgroup") if cgroup_root is None else cgroup_root
    )
    rows = _proc_cgroup_rows(proc_root)
    memory_path = _controller_path(rows, "memory")
    cpu_path = _controller_path(rows, "cpu") or _controller_path(rows, "cpuacct")
    pids_path = _controller_path(rows, "pids")
    systemd_path = _controller_path(rows, "name=systemd")
    unified_path = _unified_path(rows)
    meminfo = _proc_meminfo_values(proc_root)

    if memory_path is not None:
        layout = "v1"
        paths = {memory_path, cpu_path, pids_path, systemd_path}
        if None in paths or len(paths) != 1:
            raise HarnessError(
                "aggregate v1 memory/CPU/PID/systemd controllers do not share one leaf"
            )
        cgroup_path = memory_path
        memory_root = cgroup_root / "memory"
        cpu_root = next(
            (
                cgroup_root / name
                for name in ("cpu,cpuacct", "cpuacct,cpu", "cpu")
                if (cgroup_root / name).is_dir()
                and not (cgroup_root / name).is_symlink()
            ),
            None,
        )
        if cpu_root is None:
            raise HarnessError("aggregate v1 CPU controller mount is absent")
        pids_root = cgroup_root / "pids"
        memory_leaf = _safe_cgroup_directory(memory_root, cgroup_path, "memory")
        cpu_leaf = _safe_cgroup_directory(cpu_root, cgroup_path, "CPU")
        pids_leaf = _safe_cgroup_directory(pids_root, cgroup_path, "PID")
        _cgroup_process_present(memory_leaf, os.getpid())
        memory_effective, memory_constraints = _walk_numeric_cgroup_limits(
            memory_leaf,
            memory_root,
            "memory.limit_in_bytes",
            unlimited=frozenset({"-1", "max"}),
            label="memory",
        )
        memory_leaf_limit = _positive_cgroup_integer(
            _plain_text(
                memory_leaf / "memory.limit_in_bytes",
                "aggregate leaf memory limit",
            ),
            "aggregate leaf memory limit",
        )
        if memory_leaf_limit >= CGROUP_UNLIMITED_THRESHOLD:
            raise HarnessError("aggregate service leaf has no finite memory limit")
        memory_current = _nonnegative_cgroup_integer(
            _plain_text(
                memory_leaf / "memory.usage_in_bytes",
                "aggregate memory usage",
            ),
            "aggregate memory usage",
        )
        memory_headroom = _constraint_headroom(
            memory_constraints,
            "memory.usage_in_bytes",
            label="memory",
        )
        cpu_effective, cpu_constraints = _walk_cpu_limits_v1(cpu_leaf, cpu_root)
        leaf_cpu_constraints = [
            value
            for value in cpu_constraints
            if pathlib.Path(str(value["quota_path"])).parent == cpu_leaf
        ]
        if not leaf_cpu_constraints:
            raise HarnessError(
                "aggregate service leaf has no finite CPU quota"
            )
        cpu_leaf_limit = min(
            float(value["cpus"]) for value in leaf_cpu_constraints
        )
        pids_effective, pids_constraints = _walk_numeric_cgroup_limits(
            pids_leaf,
            pids_root,
            "pids.max",
            unlimited=frozenset({"max", "-1"}),
            label="PID",
        )
        pids_leaf_limit = _positive_cgroup_integer(
            _plain_text(pids_leaf / "pids.max", "aggregate leaf PID limit"),
            "aggregate leaf PID limit",
        )
        pids_current = _positive_cgroup_integer(
            _plain_text(pids_leaf / "pids.current", "aggregate PID usage"),
            "aggregate PID usage",
        )
        memory_events, memory_event_paths = _hierarchical_scalar_events(
            memory_constraints,
            memory_leaf,
            memory_root,
            "memory.failcnt",
            label="memory failure",
        )
        oom_events, oom_event_paths = _hierarchical_named_events(
            memory_constraints,
            memory_leaf,
            memory_root,
            "memory.oom_control",
            ("oom_kill",),
            label="memory OOM",
            required=False,
        )
        memory_events.update(oom_events)
        memory_event_paths.update(oom_event_paths)
        pids_events, pids_event_paths = _hierarchical_named_events(
            pids_constraints,
            pids_leaf,
            pids_root,
            "pids.events",
            ("max",),
            label="PID",
        )
        swap_max: int | None = None
        swap_path = memory_leaf / "memory.memsw.limit_in_bytes"
        if meminfo["SwapTotal"] != 0:
            raise HarnessError(
                "cgroup v1 aggregate containment requires host swap to be disabled"
            )
        if swap_path.is_file():
            swap_max = _positive_cgroup_integer(
                _plain_text(swap_path, "aggregate memory+swap limit"),
                "aggregate memory+swap limit",
            )
        event_paths = {
            **{
                f"memory:{key}": value
                for key, value in memory_event_paths.items()
            },
            **{
                f"pids:{key}": value
                for key, value in pids_event_paths.items()
            },
        }
    elif unified_path is not None:
        layout = "v2"
        cgroup_path = unified_path
        unified_root = cgroup_root
        memory_leaf = _safe_cgroup_directory(unified_root, cgroup_path, "unified")
        _cgroup_process_present(memory_leaf, os.getpid())
        memory_effective, memory_constraints = _walk_numeric_cgroup_limits(
            memory_leaf,
            unified_root,
            "memory.max",
            unlimited=frozenset({"max", "-1"}),
            label="memory",
        )
        memory_leaf_limit = _positive_cgroup_integer(
            _plain_text(memory_leaf / "memory.max", "aggregate leaf memory limit"),
            "aggregate leaf memory limit",
        )
        memory_current_raw = _plain_text(
            memory_leaf / "memory.current", "aggregate memory usage"
        )
        if not memory_current_raw.isdigit():
            raise HarnessError("aggregate memory usage is malformed")
        memory_current = int(memory_current_raw)
        memory_headroom = _constraint_headroom(
            memory_constraints,
            "memory.current",
            label="memory",
        )
        cpu_effective, cpu_constraints = _walk_cpu_limits_v2(
            memory_leaf, unified_root
        )
        leaf_cpu_constraints = [
            value
            for value in cpu_constraints
            if pathlib.Path(str(value["path"])).parent == memory_leaf
        ]
        if not leaf_cpu_constraints:
            raise HarnessError(
                "aggregate service leaf has no finite CPU quota"
            )
        cpu_leaf_limit = min(
            float(value["cpus"]) for value in leaf_cpu_constraints
        )
        pids_effective, pids_constraints = _walk_numeric_cgroup_limits(
            memory_leaf,
            unified_root,
            "pids.max",
            unlimited=frozenset({"max", "-1"}),
            label="PID",
        )
        pids_leaf_limit = _positive_cgroup_integer(
            _plain_text(memory_leaf / "pids.max", "aggregate leaf PID limit"),
            "aggregate leaf PID limit",
        )
        pids_current = _positive_cgroup_integer(
            _plain_text(memory_leaf / "pids.current", "aggregate PID usage"),
            "aggregate PID usage",
        )
        swap_raw = _plain_text(
            memory_leaf / "memory.swap.max", "aggregate swap limit"
        )
        if swap_raw != "0":
            raise HarnessError("aggregate cgroup v2 service does not disable swap")
        swap_max = 0
        memory_events, memory_event_paths = _hierarchical_named_events(
            memory_constraints,
            memory_leaf,
            unified_root,
            "memory.events",
            ("max", "oom", "oom_kill"),
            label="memory",
        )
        pids_events, pids_event_paths = _hierarchical_named_events(
            pids_constraints,
            memory_leaf,
            unified_root,
            "pids.events",
            ("max",),
            label="PID",
        )
        event_paths = {
            **{
                f"memory:{key}": value
                for key, value in memory_event_paths.items()
            },
            **{
                f"pids:{key}": value
                for key, value in pids_event_paths.items()
            },
        }
    else:
        raise HarnessError("process has no usable memory cgroup controller")

    unit = pathlib.PurePosixPath(cgroup_path).name
    match = AGGREGATE_UNIT.fullmatch(unit)
    if (
        not match
        or int(match.group(1)) != os.getuid()
        or cgroup_path != f"/system.slice/{unit}"
    ):
        raise HarnessError(
            "controller is not in a dedicated paradox aggregate system service leaf"
        )
    if memory_headroom <= 0:
        raise HarnessError("aggregate cgroup has no positive memory headroom")
    return {
        "layout": layout,
        "unit": unit,
        "cgroup_path": cgroup_path,
        "memory_leaf_limit_bytes": memory_leaf_limit,
        "memory_effective_limit_bytes": memory_effective,
        "memory_current_bytes": memory_current,
        "memory_headroom_bytes": memory_headroom,
        "memory_constraints": memory_constraints,
        "memory_events": memory_events,
        "cpu_leaf_limit": cpu_leaf_limit,
        "cpu_effective": cpu_effective,
        "cpu_constraints": cpu_constraints,
        "pids_leaf_limit": pids_leaf_limit,
        "pids_effective_limit": pids_effective,
        "pids_current": pids_current,
        "pids_events": pids_events,
        "swap_max_bytes": swap_max,
        "host_swap_total_kib": meminfo["SwapTotal"],
        "event_paths": event_paths,
        "proc_root": str(proc_root),
        "cgroup_root": str(cgroup_root),
    }


def _systemd_unit_identity(
    root: pathlib.Path, kernel: Mapping[str, Any]
) -> dict[str, str]:
    executable = SYSTEMCTL
    require_plain_file(executable, "trusted host systemctl")
    systemctl_stat = executable.stat()
    if (
        systemctl_stat.st_uid != 0
        or systemctl_stat.st_mode & 0o022
        or not os.access(executable, os.X_OK)
    ):
        raise HarnessError(
            "trusted host systemctl must be root-owned, executable, and "
            "not writable by group or other"
        )
    properties = (
        "Id",
        "Type",
        "ControlGroup",
        "User",
        "Group",
        "MemoryAccounting",
        "CPUAccounting",
        "TasksAccounting",
        "OOMPolicy",
        "OOMScoreAdjust",
        "KillMode",
        "Delegate",
    )
    result = run_capture(
        [
            str(executable),
            "show",
            str(kernel["unit"]),
            "--no-pager",
            *[f"--property={value}" for value in properties],
        ],
        cwd=root,
        timeout=20,
    )
    rows: dict[str, str] = {}
    for row in result.stdout.splitlines():
        if "=" not in row:
            continue
        name, value = row.split("=", 1)
        if name in rows:
            raise HarnessError(f"systemd returned duplicate unit property {name}")
        rows[name] = value
    if set(rows) != set(properties):
        raise HarnessError(
            "systemd aggregate unit lacks required properties: "
            + ", ".join(sorted(set(properties) - rows.keys()))
        )
    try:
        import grp
        import pwd

        user_values = {str(os.getuid()), pwd.getpwuid(os.getuid()).pw_name}
        group_values = {str(os.getgid()), grp.getgrgid(os.getgid()).gr_name}
    except (ImportError, KeyError):
        user_values = {str(os.getuid())}
        group_values = {str(os.getgid())}
    expected = {
        "Id": str(kernel["unit"]),
        "Type": "exec",
        "ControlGroup": str(kernel["cgroup_path"]),
        "MemoryAccounting": "yes",
        "CPUAccounting": "yes",
        "TasksAccounting": "yes",
        "OOMPolicy": "kill",
        "OOMScoreAdjust": "1000",
        "KillMode": "control-group",
        "Delegate": "no",
    }
    failures = [
        f"{name}={rows[name]!r}, expected {value!r}"
        for name, value in expected.items()
        if rows[name] != value
    ]
    if rows["User"] not in user_values:
        failures.append(f"User={rows['User']!r} is not the controller user")
    if rows["Group"] not in group_values:
        failures.append(f"Group={rows['Group']!r} is not the controller group")
    if failures:
        raise HarnessError(
            "systemd aggregate unit contract is not effective: " + "; ".join(failures)
        )
    return rows


def probe_aggregate_containment(
    root: pathlib.Path,
    defaults: Mapping[str, Any],
    *,
    requested: bool,
    proc_root: pathlib.Path | None = None,
    cgroup_root: pathlib.Path | None = None,
    require_systemd: bool = True,
) -> AggregateProbe:
    if not requested:
        return AggregateProbe(
            requested=False,
            hard=False,
            reason="aggregate systemd containment was not requested",
            identity={},
        )
    try:
        kernel = _aggregate_kernel_identity(
            proc_root=proc_root, cgroup_root=cgroup_root
        )
        actual_proc_root = pathlib.Path(kernel["proc_root"])
        meminfo = _proc_meminfo_values(actual_proc_root)
        reserve_min = _positive_int(
            defaults.get("host_memory_reserve_mib", 12288),
            "defaults.host_memory_reserve_mib",
        )
        fraction = defaults.get("host_memory_reserve_fraction", 0.25)
        if (
            isinstance(fraction, bool)
            or not isinstance(fraction, (int, float))
            or not 0 < float(fraction) < 1
        ):
            raise HarnessError(
                "defaults.host_memory_reserve_fraction must be between zero and one"
            )
        available_mib = meminfo["MemAvailable"] // 1024
        reserve_mib = max(reserve_min, int(available_mib * float(fraction)))
        additional_mib = int(kernel["memory_headroom_bytes"]) // (1024 * 1024)
        # A small tolerance covers page-cache/accounting movement between the
        # root launcher and this first Python observation. The outer cap still
        # remains far below physical memory and live admission is stricter.
        if additional_mib > max(0, available_mib - reserve_mib) + 512:
            raise HarnessError(
                "aggregate memory headroom does not preserve the configured "
                f"outside reserve ({additional_mib} MiB > "
                f"{max(0, available_mib - reserve_mib)} MiB + tolerance)"
            )
        physical_safe = max(
            0, meminfo["MemTotal"] // 1024 - reserve_min
        )
        effective_mib = int(kernel["memory_effective_limit_bytes"]) // (1024 * 1024)
        if effective_mib <= 1024 or effective_mib > physical_safe:
            raise HarnessError(
                "aggregate memory limit is absent, too small, or leaves too "
                "little physical host reserve"
            )
        systemd = _systemd_unit_identity(root, kernel) if require_systemd else {}
        identity = dict(kernel)
        identity["systemd"] = systemd
        identity["outside_reserve_mib"] = reserve_mib
        return AggregateProbe(
            requested=True,
            hard=True,
            reason=(
                f"dedicated systemd {kernel['layout']} cgroup "
                f"{kernel['unit']} enforces an effective "
                f"{effective_mib}-MiB aggregate memory ceiling"
            ),
            identity=identity,
        )
    except HarnessError as exc:
        return AggregateProbe(
            requested=True,
            hard=False,
            reason=str(exc),
            identity={},
        )


def aggregate_violation(
    probe: AggregateProbe,
    *,
    reauthenticate_systemd: bool = False,
    root: pathlib.Path | None = None,
) -> str | None:
    """Return a fatal reason when the proved envelope changed or hit a limit."""

    if not probe.hard:
        return None
    expected = probe.identity
    try:
        current = _aggregate_kernel_identity(
            proc_root=pathlib.Path(str(expected["proc_root"])),
            cgroup_root=pathlib.Path(str(expected["cgroup_root"])),
        )
    except (HarnessError, KeyError) as exc:
        return f"aggregate cgroup proof disappeared or became malformed: {exc}"
    stable_fields = (
        "layout",
        "unit",
        "cgroup_path",
        "memory_leaf_limit_bytes",
        "memory_effective_limit_bytes",
        "memory_constraints",
        "cpu_leaf_limit",
        "cpu_effective",
        "cpu_constraints",
        "pids_leaf_limit",
        "pids_effective_limit",
        "swap_max_bytes",
    )
    changed = [name for name in stable_fields if current.get(name) != expected.get(name)]
    if changed:
        return "aggregate cgroup contract changed during execution: " + ", ".join(
            changed
        )
    expected_systemd = expected.get("systemd")
    if reauthenticate_systemd and expected_systemd:
        try:
            current_systemd = _systemd_unit_identity(
                pathlib.Path.cwd() if root is None else root,
                current,
            )
        except HarnessError as exc:
            return f"aggregate systemd proof disappeared or became malformed: {exc}"
        if current_systemd != expected_systemd:
            return "aggregate systemd unit contract changed during execution"
    for category in ("memory_events", "pids_events"):
        old_events = expected.get(category, {})
        new_events = current.get(category, {})
        if not isinstance(old_events, Mapping) or not isinstance(new_events, Mapping):
            return f"aggregate {category} evidence is malformed"
        for name, old_value in old_events.items():
            new_value = new_events.get(name)
            if not isinstance(new_value, int) or new_value < int(old_value):
                return f"aggregate {category} counter {name} was reset or removed"
            if new_value > int(old_value):
                return (
                    f"aggregate {category} counter {name} increased "
                    f"({old_value} -> {new_value})"
                )
    return None


def aggregate_violation_status(reason: str) -> str:
    """Classify only a newly consumed memory limit as a contained OOM."""

    if re.search(
        r"\Aaggregate memory_events counter \S+ increased \([0-9]+ -> [0-9]+\)\Z",
        reason,
    ):
        return "oom"
    return "infrastructure"


def require_stable_aggregate(
    probe: AggregateProbe,
    *,
    phase: str,
    root: pathlib.Path | None = None,
    reauthenticate_systemd: bool = False,
) -> None:
    violation = aggregate_violation(
        probe,
        reauthenticate_systemd=reauthenticate_systemd,
        root=root,
    )
    if violation is not None:
        raise HarnessError(
            f"aggregate containment changed {phase}: {violation}"
        )


@dataclasses.dataclass(frozen=True)
class HostResources:
    cpus: int
    memory_available_mib: int
    disk_available_mib: int
    memory_reserve_mib: int
    disk_reserve_mib: int
    cpu_budget: float
    memory_budget_mib: int
    disk_budget_mib: int
    memory_source: str
    pid_budget: int = 8192
    pid_source: str = "configured_safe_cap"
    # Static ceilings answer whether a task can ever fit this machine.  The
    # ordinary budgets above are deliberately live and may shrink under
    # unrelated temporary pressure.  Tests/older callers that omit these
    # fields retain the historical budget-as-capacity behavior.
    memory_capacity_mib: int | None = None
    disk_capacity_mib: int | None = None
    pid_capacity: int | None = None
    engine_overhead_mib: int = 0
    memory_reserve_min_mib: int | None = None
    memory_reserve_fraction: float | None = None
    aggregate_memory_available_mib: int | None = None
    aggregate_memory_capacity_mib: int | None = None


def memory_available_mib() -> tuple[int, str]:
    meminfo = pathlib.Path("/proc/meminfo")
    if meminfo.is_file() and not meminfo.is_symlink():
        for line in meminfo.read_text(encoding="ascii", errors="strict").splitlines():
            if line.startswith("MemAvailable:"):
                fields = line.split()
                if len(fields) == 3 and fields[1].isdigit() and fields[2] == "kB":
                    value = int(fields[1]) // 1024
                    if value > 0:
                        return value, "proc_memavailable"
        raise HarnessError("could not parse a positive /proc/meminfo MemAvailable")
    if sys.platform == "darwin":
        total = int(
            run_capture(["sysctl", "-n", "hw.memsize"], cwd=pathlib.Path("/")).stdout
        )
        page_size = int(
            run_capture(["sysctl", "-n", "hw.pagesize"], cwd=pathlib.Path("/")).stdout
        )
        output = run_capture(["vm_stat"], cwd=pathlib.Path("/")).stdout
        pages = 0
        for line in output.splitlines():
            if re.match(r"^Pages (free|inactive|speculative):", line):
                match = re.search(r"([0-9]+)", line.split(":", 1)[1])
                if match:
                    pages += int(match.group(1))
        available = min(total, pages * page_size) // (1024 * 1024)
        if available > 0:
            return available, "darwin_vm_stat"
    raise HarnessError("cannot determine live available memory on this platform")


def memory_capacity_mib() -> tuple[int, str]:
    """Return physical memory, tightened by the current cgroup when present."""

    if sys.platform == "linux":
        meminfo = pathlib.Path("/proc/meminfo")
        if not meminfo.is_file() or meminfo.is_symlink():
            raise HarnessError("cannot determine physical memory capacity")
        total_mib: int | None = None
        for line in meminfo.read_text(encoding="ascii", errors="strict").splitlines():
            if line.startswith("MemTotal:"):
                fields = line.split()
                if len(fields) == 3 and fields[1].isdigit() and fields[2] == "kB":
                    total_mib = int(fields[1]) // 1024
                break
        if total_mib is None or total_mib <= 0:
            raise HarnessError("could not parse a positive /proc/meminfo MemTotal")
        cgroup_mib, cgroup_source = cgroup_memory_capacity_mib()
        if cgroup_mib is not None:
            return min(total_mib, cgroup_mib), cgroup_source
        return total_mib, "physical_memory"
    if sys.platform == "darwin":
        total = int(
            run_capture(["sysctl", "-n", "hw.memsize"], cwd=pathlib.Path("/")).stdout
        )
        if total > 0:
            return total // (1024 * 1024), "physical_memory"
    raise HarnessError("cannot determine memory capacity on this platform")


def cgroup_memory_capacity_mib(
    *,
    proc_root: pathlib.Path | None = None,
    cgroup_root: pathlib.Path | None = None,
) -> tuple[int | None, str]:
    """Return the tightest numeric memory ceiling across cgroup ancestors."""

    if sys.platform != "linux":
        return None, "unlimited_or_unavailable"
    proc_root = pathlib.Path("/proc") if proc_root is None else proc_root
    cgroup_root = (
        pathlib.Path("/sys/fs/cgroup") if cgroup_root is None else cgroup_root
    )
    cgroup_file = proc_root / "self" / "cgroup"
    if not cgroup_file.is_file() or cgroup_file.is_symlink():
        return None, "unlimited_or_unavailable"
    candidates: list[
        tuple[pathlib.Path, pathlib.PurePosixPath, str]
    ] = []
    for row in cgroup_file.read_text(encoding="ascii", errors="strict").splitlines():
        fields = row.split(":", 2)
        if len(fields) != 3 or not fields[2].startswith("/"):
            continue
        relative = pathlib.PurePosixPath(fields[2].lstrip("/"))
        if ".." in relative.parts:
            raise HarnessError("unsafe memory cgroup path")
        controllers = fields[1].split(",") if fields[1] else []
        if not fields[1]:
            candidates.append((cgroup_root, relative, "memory.max"))
        elif "memory" in controllers:
            candidates.append(
                (cgroup_root / "memory", relative, "memory.limit_in_bytes")
            )
    ceilings: list[int] = []
    for controller_root, relative, filename in candidates:
        controller_root = controller_root.resolve()
        current = controller_root.joinpath(*relative.parts)
        while True:
            try:
                current.relative_to(controller_root)
            except ValueError as exc:
                raise HarnessError(
                    "memory cgroup traversal escaped its controller"
                ) from exc
            limit_path = current / filename
            if limit_path.is_file():
                value = limit_path.read_text(encoding="ascii").strip()
                if value not in {"max", "-1"}:
                    if not value.isdigit() or int(value) <= 0:
                        raise HarnessError("malformed memory cgroup limit")
                    ceilings.append(int(value))
            if current == controller_root:
                break
            current = current.parent
    if not ceilings:
        return None, "unlimited_or_unavailable"
    return min(ceilings) // (1024 * 1024), "cgroup_memory_capacity"


def live_memory_available_mib(root: pathlib.Path) -> tuple[int, str]:
    """Reuse the cgroup-aware repository helper for live admission decisions."""

    helper = root / "scripts" / "environment" / "resource-jobs"
    if helper.is_file() and not helper.is_symlink() and os.access(helper, os.X_OK):
        result = run_capture(
            [str(helper), "light-test", "--report"],
            cwd=root,
            timeout=30,
            check=False,
        )
        if result.returncode == 0:
            fields: dict[str, str] = {}
            for number, row in enumerate(result.stdout.splitlines(), 1):
                values = row.split("\t")
                if len(values) != 2 or values[0] in fields:
                    raise HarnessError(
                        f"resource-jobs returned malformed live report row {number}"
                    )
                fields[values[0]] = values[1]
            try:
                value = int(fields["memory_available_mib"])
                source = fields["memory_source"]
            except (KeyError, ValueError) as exc:
                raise HarnessError(
                    "resource-jobs live report lacks usable memory fields"
                ) from exc
            if value <= 0:
                raise HarnessError("resource-jobs returned non-positive live memory")
            return value, source
        diagnostic = (result.stderr or result.stdout).strip()
        # Under pressure resource-jobs intentionally refuses to admit even one
        # light worker, but its diagnostic still authenticates the measured
        # cgroup-aware availability needed by the outer wait/abort policy.
        match = re.search(r"\bavailable=([0-9]+) MiB\b", diagnostic)
        if match and int(match.group(1)) > 0:
            return int(match.group(1)), "resource_jobs_pressure"
        raise HarnessError(
            "could not refresh cgroup-aware live memory"
            + (f": {diagnostic[-600:]}" if diagnostic else "")
        )
    return memory_available_mib()


def cgroup_pid_limits(
    *,
    proc_root: pathlib.Path | None = None,
    cgroup_root: pathlib.Path | None = None,
) -> tuple[int | None, int | None, str]:
    if sys.platform != "linux":
        return None, None, "unlimited_or_unavailable"
    proc_root = pathlib.Path("/proc") if proc_root is None else proc_root
    cgroup_root = (
        pathlib.Path("/sys/fs/cgroup") if cgroup_root is None else cgroup_root
    )
    cgroup_file = proc_root / "self" / "cgroup"
    if not cgroup_file.is_file() or cgroup_file.is_symlink():
        return None, None, "unlimited_or_unavailable"
    candidates: list[tuple[pathlib.Path, pathlib.PurePosixPath]] = []
    for row in cgroup_file.read_text(encoding="ascii", errors="strict").splitlines():
        fields = row.split(":", 2)
        if len(fields) != 3 or not fields[2].startswith("/"):
            continue
        relative = pathlib.PurePosixPath(fields[2].lstrip("/"))
        if ".." in relative.parts:
            raise HarnessError("unsafe PID cgroup path")
        controllers = fields[1].split(",") if fields[1] else []
        if not fields[1]:
            candidates.append((cgroup_root, relative))
        elif "pids" in controllers:
            candidates.append((cgroup_root / "pids", relative))
    available: list[int] = []
    capacities: list[int] = []
    for controller_root, relative in candidates:
        controller_root = controller_root.resolve()
        current = controller_root.joinpath(*relative.parts)
        while True:
            try:
                current.relative_to(controller_root)
            except ValueError as exc:
                raise HarnessError("PID cgroup traversal escaped its controller") from exc
            maximum_path = current / "pids.max"
            current_path = current / "pids.current"
            if maximum_path.is_file() and current_path.is_file():
                maximum_text = maximum_path.read_text(encoding="ascii").strip()
                current_text = current_path.read_text(encoding="ascii").strip()
                if current_text.isdigit() and (
                    maximum_text == "max" or maximum_text.isdigit()
                ):
                    if maximum_text != "max":
                        maximum = int(maximum_text)
                        capacities.append(maximum)
                        available.append(max(0, maximum - int(current_text)))
                else:
                    raise HarnessError("malformed PID cgroup limit/current value")
            if current == controller_root:
                break
            current = current.parent
    if not available:
        return None, None, "unlimited_or_unavailable"
    return min(available), min(capacities), "cgroup_pids"


def cgroup_pid_available(
    *,
    proc_root: pathlib.Path | None = None,
    cgroup_root: pathlib.Path | None = None,
) -> tuple[int | None, str]:
    available, _capacity, source = cgroup_pid_limits(
        proc_root=proc_root, cgroup_root=cgroup_root
    )
    return available, source


def discover_host_resources(
    root: pathlib.Path,
    defaults: Mapping[str, Any],
    aggregate: AggregateProbe | None = None,
) -> HostResources:
    aggregate_hard = aggregate is not None and aggregate.hard
    helper = root / "scripts" / "environment" / "resource-jobs"
    if helper.is_file() and not helper.is_symlink() and os.access(helper, os.X_OK):
        result = run_capture(
            [str(helper), "light-test", "--report"],
            cwd=root,
            timeout=30,
            check=False,
        )
        if result.returncode == 0:
            fields: dict[str, str] = {}
            for number, row in enumerate(result.stdout.splitlines(), 1):
                values = row.split("\t")
                if len(values) != 2 or values[0] in fields:
                    raise HarnessError(
                        f"resource-jobs returned malformed report row {number}"
                    )
                fields[values[0]] = values[1]
            try:
                cpus = int(fields["cpu_limit"])
                memory_mib = int(fields["memory_available_mib"])
                memory_source = fields["memory_source"]
            except (KeyError, ValueError) as exc:
                raise HarnessError(
                    "resource-jobs report lacks usable CPU/memory fields"
                ) from exc
        else:
            diagnostic = (result.stderr or result.stdout).strip()
            pressure = re.search(
                r"\bavailable=([0-9]+) MiB\b.*\bcpu_limit=([0-9]+)\b",
                diagnostic,
            )
            if not pressure or any(int(value) <= 0 for value in pressure.groups()):
                raise HarnessError(
                    "could not discover host resources"
                    + (f": {diagnostic[-600:]}" if diagnostic else "")
                )
            memory_mib = int(pressure.group(1))
            cpus = int(pressure.group(2))
            memory_source = "resource_jobs_pressure"
    else:
        if hasattr(os, "sched_getaffinity"):
            cpus = len(os.sched_getaffinity(0))
        else:
            cpus = os.cpu_count() or 1
        memory_mib, memory_source = memory_available_mib()
    aggregate_available: int | None = None
    aggregate_capacity: int | None = None
    if aggregate_hard:
        # The dedicated outer cgroup already excludes the OS/Codex reserve.
        # Keep raw host availability separate from that envelope so admission
        # can use min(host availability - reserve, aggregate headroom) without
        # subtracting the reserve from the aggregate limit a second time.
        memory_mib, raw_source = memory_available_mib()
        memory_source = f"{raw_source}+systemd_aggregate"
        aggregate_available = (
            int(aggregate.identity["memory_headroom_bytes"]) // (1024 * 1024)
        )
        aggregate_capacity = (
            int(aggregate.identity["memory_effective_limit_bytes"])
            // (1024 * 1024)
        )
    if aggregate_hard:
        cpus = min(
            cpus,
            max(1, int(math.floor(float(aggregate.identity["cpu_effective"])))),
        )
    cpus = max(1, cpus)
    disk_usage = shutil.disk_usage(root)
    disk_mib = disk_usage.free // (1024 * 1024)
    disk_capacity = disk_usage.total // (1024 * 1024)
    if aggregate_hard:
        memory_capacity = _proc_meminfo_values(pathlib.Path("/proc"))["MemTotal"] // 1024
    else:
        memory_capacity, _memory_capacity_source = memory_capacity_mib()
    reserve_min = _positive_int(
        defaults.get("host_memory_reserve_mib", 12288),
        "defaults.host_memory_reserve_mib",
    )
    reserve_fraction = defaults.get("host_memory_reserve_fraction", 0.25)
    if (
        isinstance(reserve_fraction, bool)
        or not isinstance(reserve_fraction, (int, float))
        or not 0 < float(reserve_fraction) < 1
    ):
        raise HarnessError(
            "defaults.host_memory_reserve_fraction must be between zero and one"
        )
    # Live admission retains a fraction of the memory currently available.
    # Static fit uses the corresponding capacity fraction below. Keeping both
    # values prevents startup pressure from becoming a permanent ceiling
    # without withholding a capacity-sized reserve on a busy large machine.
    memory_reserve = max(
        reserve_min, int(memory_mib * float(reserve_fraction))
    )
    static_memory_reserve = max(
        reserve_min, int(memory_capacity * float(reserve_fraction))
    )
    disk_reserve = _positive_int(
        defaults.get("host_disk_reserve_mib", 8192),
        "defaults.host_disk_reserve_mib",
    )
    engine_overhead = _positive_int(
        defaults.get("engine_overhead_mib", 1024),
        "defaults.engine_overhead_mib",
    )
    configured_pid_budget = _positive_int(
        defaults.get("host_pid_budget", 8192), "defaults.host_pid_budget"
    )
    pid_available, pid_ceiling, pid_source = cgroup_pid_limits()
    pid_budget = configured_pid_budget
    if pid_available is not None:
        pid_budget = min(configured_pid_budget, max(0, pid_available - 128))
    pid_capacity = configured_pid_budget
    if pid_ceiling is not None:
        pid_capacity = min(configured_pid_budget, max(0, pid_ceiling - 128))
    # The aggregate launcher has already reserved host CPUs in its enforced
    # CPUQuota. Per-worker mode still needs the ordinary coordinator reserve.
    cpu_reserve = 0 if aggregate_hard else (2 if cpus >= 16 else (1 if cpus >= 4 else 0))
    live_host_memory_budget = max(0, memory_mib - memory_reserve)
    static_host_memory_budget = max(0, memory_capacity - static_memory_reserve)
    if aggregate_hard:
        assert aggregate_available is not None
        assert aggregate_capacity is not None
        live_memory_budget = max(
            0, min(live_host_memory_budget, aggregate_available) - engine_overhead
        )
        static_memory_budget = max(
            0,
            min(static_host_memory_budget, aggregate_capacity) - engine_overhead,
        )
    else:
        live_memory_budget = max(
            0, live_host_memory_budget - engine_overhead
        )
        static_memory_budget = max(
            0, static_host_memory_budget - engine_overhead
        )
    return HostResources(
        cpus=cpus,
        memory_available_mib=memory_mib,
        disk_available_mib=disk_mib,
        memory_reserve_mib=memory_reserve,
        disk_reserve_mib=disk_reserve,
        cpu_budget=float(max(1, cpus - cpu_reserve)),
        memory_budget_mib=live_memory_budget,
        disk_budget_mib=max(0, disk_mib - disk_reserve),
        memory_source=memory_source,
        pid_budget=pid_budget,
        pid_source=pid_source,
        memory_capacity_mib=static_memory_budget,
        disk_capacity_mib=max(0, disk_capacity - disk_reserve),
        pid_capacity=pid_capacity,
        engine_overhead_mib=engine_overhead,
        memory_reserve_min_mib=reserve_min,
        memory_reserve_fraction=float(reserve_fraction),
        aggregate_memory_available_mib=aggregate_available,
        aggregate_memory_capacity_mib=aggregate_capacity,
    )


@dataclasses.dataclass(frozen=True)
class EngineProbe:
    requested: str
    kind: str | None
    executable: str | None
    usable: bool
    hard: bool
    reason: str
    identity: Mapping[str, Any]
    image: str | None
    image_identity: str | None
    storage_driver: str | None
    limit_mode: str = "worker"
    aggregate: AggregateProbe | None = None


def hard_backend_name(probe: EngineProbe) -> str:
    if not probe.hard or not probe.kind:
        return "best-effort-host"
    if probe.limit_mode == "aggregate":
        return f"{probe.kind}-aggregate-hard"
    return f"{probe.kind}-hard"


def semantic_cgroup_layout(probe: EngineProbe) -> str | None:
    """Normalize only the cgroup ABI generation relevant to hard workers."""

    if not probe.hard:
        return None
    if probe.limit_mode == "aggregate" and probe.aggregate is not None:
        value = str(probe.aggregate.identity.get("layout", "")).strip().lower()
    else:
        value = str(probe.identity.get("cgroup_version", "")).strip().lower()
    if value in {"1", "v1"}:
        return "v1"
    if value in {"2", "v2"}:
        return "v2"
    return "unknown"


def _engine_candidates(root: pathlib.Path, requested: str) -> list[tuple[str, pathlib.Path]]:
    if requested not in {"auto", "podman", "docker", "none"} and "/" not in requested:
        raise HarnessError("--engine must be auto, podman, docker, none, or a path")
    if requested == "none":
        return []
    candidates: list[tuple[str, pathlib.Path]] = []
    requested_path = pathlib.Path(requested)
    if "/" in requested:
        kind = "docker" if "docker" in requested_path.name else "podman"
        return [(kind, requested_path.resolve())]

    def add(kind: str, value: str | pathlib.Path | None) -> None:
        if value is None:
            return
        path = pathlib.Path(value).resolve()
        if all(path != existing for _, existing in candidates):
            candidates.append((kind, path))

    if requested in {"auto", "podman"}:
        add("podman", shutil.which("podman"))
        local = root / "scripts" / "podman-local"
        if local.is_file() and not local.is_symlink() and os.access(local, os.X_OK):
            add("podman", local)
    if requested in {"auto", "docker"}:
        add("docker", shutil.which("docker"))
    return candidates


def _parse_engine_info(
    kind: str, executable: pathlib.Path, output: str
) -> tuple[dict[str, Any], bool, str, str | None]:
    try:
        data = json.loads(output, object_pairs_hook=reject_duplicate_keys)
    except (json.JSONDecodeError, HarnessError) as exc:
        raise HarnessError(f"{kind} info did not return JSON: {exc}") from exc
    if not isinstance(data, dict):
        raise HarnessError(f"{kind} info did not return one JSON object")
    if kind == "podman":
        host = data.get("host", {})
        version = data.get("version", {})
        store = data.get("store", {})
        security = host.get("security", {}) if isinstance(host, dict) else {}
        rootless = bool(security.get("rootless"))
        cgroup = str(host.get("cgroupVersion", "unknown"))
        storage = str(store.get("graphDriverName", "unknown"))
        identity = {
            "kind": kind,
            "executable": str(executable),
            "version": version.get("Version"),
            "api_version": version.get("APIVersion"),
            "os_arch": version.get("OsArch"),
            "rootless": rootless,
            "service_is_remote": bool(host.get("serviceIsRemote")),
            "cgroup_version": cgroup,
            "cgroup_manager": host.get("cgroupManager"),
            "storage_driver": storage,
            "graph_root": store.get("graphRoot"),
            "graph_available_bytes": (
                int(store.get("graphRootAllocated", 0))
                - int(store.get("graphRootUsed", 0))
            ),
            "engine_memory_total_bytes": host.get("memTotal"),
        }
        if rootless and cgroup.lower() in {"v1", "1"}:
            return (
                identity,
                False,
                "rootless Podman on cgroup v1 cannot enforce memory/CPU/PID limits",
                storage,
            )
        return identity, True, "engine is eligible for an active limit probe", storage
    server_version = data.get("ServerVersion")
    if not server_version:
        raise HarnessError("Docker client is present but its daemon is not usable")
    security_options = data.get("SecurityOptions") or []
    identity = {
        "kind": kind,
        "executable": str(executable),
        "server_version": server_version,
        "client_version": data.get("ClientInfo", {}).get("Debug")
        if isinstance(data.get("ClientInfo"), dict)
        else None,
        "os": data.get("OperatingSystem"),
        "architecture": data.get("Architecture"),
        "cgroup_version": data.get("CgroupVersion", "unknown"),
        "cgroup_driver": data.get("CgroupDriver"),
        "rootless": any("rootless" in str(value).lower() for value in security_options),
        "storage_driver": data.get("Driver"),
        "docker_root_dir": data.get("DockerRootDir"),
        "engine_memory_total_bytes": data.get("MemTotal"),
    }
    return identity, True, "engine is eligible for an active limit probe", data.get("Driver")


def _image_identity(
    executable: pathlib.Path, kind: str, image: str, root: pathlib.Path
) -> tuple[str, str]:
    result = run_capture(
        [str(executable), "image", "inspect", image],
        cwd=root,
        timeout=30,
    )
    try:
        records = json.loads(
            result.stdout, object_pairs_hook=reject_duplicate_keys
        )
    except (json.JSONDecodeError, HarnessError) as exc:
        raise HarnessError(f"cannot parse {kind} image identity: {exc}") from exc
    if not isinstance(records, list) or len(records) != 1 or not isinstance(records[0], dict):
        raise HarnessError(f"{kind} returned an ambiguous image identity for {image}")
    record = records[0]
    identity = {
        "id": record.get("Id") or record.get("ID"),
        "digest": record.get("Digest"),
        "repo_digests": sorted(record.get("RepoDigests") or []),
        "architecture": record.get("Architecture"),
        "os": record.get("Os"),
    }
    image_os = str(identity["os"] or "").lower()
    image_arch = str(identity["architecture"] or "").lower()
    aliases = {
        "amd64": "x86_64",
        "x86_64": "x86_64",
        "arm64": "aarch64",
        "aarch64": "aarch64",
    }
    host_arch = aliases.get(platform.machine().lower(), platform.machine().lower())
    normalized_image_arch = aliases.get(image_arch, image_arch)
    if image_os != "linux" or normalized_image_arch != host_arch:
        raise HarnessError(
            f"{kind} image platform {image_os or '?'}"
            f"/{image_arch or '?'} is incompatible with host toolchain "
            f"linux/{platform.machine()}"
        )
    if not identity["id"] and not identity["digest"] and not identity["repo_digests"]:
        raise HarnessError(f"{kind} image has no content identity: {image}")
    immutable: str | None = None
    raw_id = identity["id"]
    if isinstance(raw_id, str) and re.fullmatch(r"sha256:[0-9a-f]{64}", raw_id):
        immutable = raw_id
    if immutable is None:
        for candidate in identity["repo_digests"]:
            if isinstance(candidate, str) and re.search(
                r"@sha256:[0-9a-f]{64}$", candidate
            ):
                immutable = candidate
                break
    if immutable is None:
        raise HarnessError(
            f"{kind} image {image} has no immutable executable ID/digest"
        )
    # Repository tags/digest aliases attached to the same local image vary
    # between engines and machines. Execution uses the immutable ID/digest, so
    # only that content identity plus its reviewed platform is semantic.
    semantic_identity = {
        "immutable": immutable,
        "architecture": normalized_image_arch,
        "os": image_os,
    }
    return sha256_bytes(canonical_json(semantic_identity)), immutable


LIMIT_PROBE_SCRIPT = r"""
set -eu
if [ -r /sys/fs/cgroup/memory.max ]; then
  printf 'layout=v2\n'
  printf 'memory=%s\n' "$(cat /sys/fs/cgroup/memory.max)"
  printf 'swap=%s\n' "$(cat /sys/fs/cgroup/memory.swap.max)"
  printf 'pids=%s\n' "$(cat /sys/fs/cgroup/pids.max)"
  printf 'cpu=%s\n' "$(cat /sys/fs/cgroup/cpu.max)"
elif [ -r /sys/fs/cgroup/memory/memory.limit_in_bytes ]; then
  printf 'layout=v1\n'
  printf 'memory=%s\n' "$(cat /sys/fs/cgroup/memory/memory.limit_in_bytes)"
  printf 'swap=%s\n' "$(cat /sys/fs/cgroup/memory/memory.memsw.limit_in_bytes)"
  printf 'pids=%s\n' "$(cat /sys/fs/cgroup/pids/pids.max)"
  printf 'cpu=%s/%s\n' \
    "$(cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us)" \
    "$(cat /sys/fs/cgroup/cpu/cpu.cfs_period_us)"
else
  printf 'layout=none\n'
fi
test -r "$1/DESCRIPTION"
if printf 'unexpected-root-write\n' >"$3/root-write-must-fail" 2>/dev/null; then
  exit 41
fi
test ! -e "$3/root-write-must-fail"
printf 'nested-bind-ok\n' >"$2/write-proof"
test "$(cat "$2/write-proof")" = nested-bind-ok
""".strip()

AGGREGATE_PROBE_SCRIPT = r"""
set -eu
printf '%s\n' '__PARADOX_CGROUP_BEGIN__'
cat /proc/self/cgroup
printf '%s\n' '__PARADOX_CGROUP_END__'
test -r "$1/DESCRIPTION"
if printf 'unexpected-root-write\n' >"$3/root-write-must-fail" 2>/dev/null; then
  exit 41
fi
test ! -e "$3/root-write-must-fail"
printf 'nested-bind-ok\n' >"$2/write-proof"
test "$(cat "$2/write-proof")" = nested-bind-ok
""".strip()


def _parse_limit_probe(output: str) -> tuple[bool, str]:
    rows: dict[str, str] = {}
    for line in output.splitlines():
        if "=" not in line:
            continue
        key, value = line.split("=", 1)
        if key in rows:
            return False, f"limit probe duplicated {key}"
        rows[key] = value.strip()
    if rows.get("layout") not in {"v1", "v2"}:
        return False, "worker cannot observe a memory cgroup"
    try:
        memory = int(rows["memory"])
        swap = int(rows["swap"])
        pids = int(rows["pids"])
        cpu = rows["cpu"].replace("/", " ").split()
        quota, period = int(cpu[0]), int(cpu[1])
    except (KeyError, ValueError, IndexError):
        return False, "worker returned malformed cgroup limits"
    if not (60 * 1024 * 1024 <= memory <= 68 * 1024 * 1024):
        return False, f"requested 64-MiB memory ceiling is not effective ({memory})"
    if rows["layout"] == "v2":
        if swap != 0:
            return False, f"requested no-swap ceiling is not effective ({swap})"
    elif not (60 * 1024 * 1024 <= swap <= 68 * 1024 * 1024):
        return False, f"requested no-swap ceiling is not effective ({swap})"
    if not (1 <= pids <= 64):
        return False, f"requested PID ceiling is not effective ({pids})"
    if quota <= 0 or period <= 0 or quota / period > 0.51:
        return False, f"requested half-CPU quota is not effective ({quota}/{period})"
    return True, "visible cgroup memory/CPU/PID ceilings match the request"


def _parse_worker_cgroup_rows(output: str) -> list[tuple[str, tuple[str, ...], str]]:
    lines = output.splitlines()
    try:
        start = lines.index("__PARADOX_CGROUP_BEGIN__")
        end = lines.index("__PARADOX_CGROUP_END__", start + 1)
    except ValueError as exc:
        raise HarnessError("aggregate worker did not delimit its cgroup membership") from exc
    if end <= start + 1:
        raise HarnessError("aggregate worker returned empty cgroup membership")
    rows: list[tuple[str, tuple[str, ...], str]] = []
    seen: set[str] = set()
    for number, row in enumerate(lines[start + 1 : end], 1):
        fields = row.split(":", 2)
        if (
            len(fields) != 3
            or not fields[0].isdigit()
            or not fields[2].startswith("/")
        ):
            raise HarnessError(f"malformed aggregate worker cgroup row {number}")
        path = pathlib.PurePosixPath(fields[2])
        if ".." in path.parts or "//" in fields[2] or "\x00" in fields[2]:
            raise HarnessError("aggregate worker returned an unsafe cgroup path")
        controllers = tuple(value for value in fields[1].split(",") if value)
        for controller in controllers:
            if controller in seen:
                raise HarnessError(
                    f"aggregate worker duplicated cgroup controller {controller}"
                )
            seen.add(controller)
        rows.append((fields[0], controllers, fields[2]))
    return rows


def _aggregate_worker_inherits(
    output: str, aggregate: AggregateProbe
) -> tuple[bool, str]:
    try:
        rows = _parse_worker_cgroup_rows(output)
        expected = str(aggregate.identity["cgroup_path"])
        layout = aggregate.identity["layout"]
        if layout == "v1":
            paths = {
                _controller_path(rows, "memory"),
                _controller_path(rows, "cpu") or _controller_path(rows, "cpuacct"),
                _controller_path(rows, "pids"),
                _controller_path(rows, "name=systemd"),
            }
            if paths != {expected}:
                raise HarnessError(
                    "aggregate worker escaped or obscured the controller's "
                    f"v1 cgroup leaf: {sorted(str(value) for value in paths)}"
                )
        elif layout == "v2":
            if _unified_path(rows) != expected:
                raise HarnessError(
                    "aggregate worker escaped or obscured the controller's "
                    "unified cgroup leaf"
                )
        else:
            raise HarnessError("aggregate controller has an unknown cgroup layout")
    except (HarnessError, KeyError) as exc:
        return False, str(exc)
    return True, "worker process remained in the dedicated aggregate cgroup leaf"


def _base_limit_flags(kind: str, name: str) -> list[str]:
    flags = [
        "run",
        "--name",
        name,
        "--pull=never",
        "--network=none",
        "--memory=64m",
        "--memory-swap=64m",
        "--pids-limit=64",
        "--cpus=0.5",
        "--read-only",
        "--cap-drop=all",
        "--security-opt=no-new-privileges",
        "--security-opt=label=disable",
        "--label=org.mlr-org.paradox.verify=worker",
        f"--label=org.mlr-org.paradox.verify.uid={os.getuid()}",
        "--log-driver=none",
    ]
    if kind == "podman":
        flags.append("--userns=keep-id")
    else:
        flags.extend(["--user", f"{os.getuid()}:{os.getgid()}"])
    return flags


def _base_aggregate_flags(name: str) -> list[str]:
    return [
        "run",
        "--name",
        name,
        "--pull=never",
        "--network=none",
        "--cgroups=disabled",
        "--cgroupns=host",
        "--oom-score-adj=1000",
        "--read-only",
        "--cap-drop=all",
        "--security-opt=no-new-privileges",
        "--security-opt=label=disable",
        "--label=org.mlr-org.paradox.verify=worker",
        f"--label=org.mlr-org.paradox.verify.uid={os.getuid()}",
        "--log-driver=none",
        "--userns=keep-id",
    ]


def _remove_container(
    executable: pathlib.Path, root: pathlib.Path, name: str
) -> bool:
    try:
        result = run_capture(
            [str(executable), "rm", "--force", name],
            cwd=root,
            timeout=20,
            check=False,
        )
        return result.returncode == 0
    except HarnessError:
        # Cleanup is deliberately no-throw so a wedged engine cannot mask the
        # original task/probe result. The unique name remains in diagnostics.
        return False


def probe_engine(
    root: pathlib.Path,
    requested: str,
    image: str | None,
    *,
    active: bool,
    containment: str = "auto",
    aggregate: AggregateProbe | None = None,
) -> EngineProbe:
    if containment not in ALLOWED_CONTAINMENT:
        raise HarnessError(
            "--containment must be auto, worker, or aggregate"
        )
    failures: list[str] = []
    soft_probe: EngineProbe | None = None
    for kind, executable in _engine_candidates(root, requested):
        if not executable.is_file() or executable.is_symlink() or not os.access(executable, os.X_OK):
            failures.append(f"{executable}: not one plain executable")
            continue
        command = (
            [str(executable), "info", "--format", "json"]
            if kind == "podman"
            else [str(executable), "info", "--format", "{{json .}}"]
        )
        try:
            result = run_capture(command, cwd=root, timeout=20, check=False)
        except HarnessError as exc:
            failures.append(f"{executable}: {exc}")
            continue
        if result.returncode != 0:
            diagnostic = (result.stderr or result.stdout).strip().splitlines()
            failures.append(
                f"{executable}: "
                + (diagnostic[-1] if diagnostic else f"exit {result.returncode}")
            )
            continue
        try:
            identity, eligible, reason, storage = _parse_engine_info(
                kind, executable, result.stdout
            )
        except HarnessError as exc:
            failures.append(f"{executable}: {exc}")
            continue
        aggregate_eligible = bool(
            aggregate is not None
            and aggregate.hard
            and kind == "podman"
            and bool(identity.get("rootless"))
            and not bool(identity.get("service_is_remote"))
        )
        use_aggregate = aggregate_eligible and (
            containment == "aggregate"
            or (containment == "auto" and not eligible)
        )
        if containment == "aggregate" and not aggregate_eligible:
            candidate = EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=False,
                reason=(
                    "aggregate containment requires local rootless Podman whose "
                    "payload remains a descendant of the controller service"
                ),
                identity=identity,
                image=image,
                image_identity=None,
                storage_driver=storage,
                limit_mode="aggregate",
                aggregate=aggregate,
            )
            if soft_probe is None:
                soft_probe = candidate
            continue
        if not eligible and not use_aggregate:
            candidate = EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=False,
                reason=reason,
                identity=identity,
                image=image,
                image_identity=None,
                storage_driver=storage,
                limit_mode="worker",
                aggregate=aggregate,
            )
            if soft_probe is None:
                soft_probe = candidate
            continue
        if active:
            # The global execution lock is held by the caller. Remove
            # same-UID labelled leftovers before the sacrificial probes so a
            # killed earlier doctor cannot distort capacity or OOM evidence.
            reap_stale_workers(
                root,
                EngineProbe(
                    requested=requested,
                    kind=kind,
                    executable=str(executable),
                    usable=True,
                    hard=False,
                    reason="pre-probe stale cleanup",
                    identity=identity,
                    image=image,
                    image_identity=None,
                    storage_driver=storage,
                    limit_mode="aggregate" if use_aggregate else "worker",
                    aggregate=aggregate,
                ),
                require_hard=False,
            )
        if image is None:
            candidate = EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=False,
                reason="no pinned, locally available worker/probe image was supplied",
                identity=identity,
                image=None,
                image_identity=None,
                storage_driver=storage,
                limit_mode="aggregate" if use_aggregate else "worker",
                aggregate=aggregate,
            )
            if soft_probe is None:
                soft_probe = candidate
            continue
        try:
            image_identity, immutable_image = _image_identity(
                executable, kind, image, root
            )
        except HarnessError as exc:
            failures.append(f"{executable}: {exc}")
            continue
        if not active:
            candidate = EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=False,
                reason="active cgroup enforcement probe was not requested",
                identity=identity,
                image=immutable_image,
                image_identity=image_identity,
                storage_driver=storage,
                limit_mode="aggregate" if use_aggregate else "worker",
                aggregate=aggregate,
            )
            if soft_probe is None:
                soft_probe = candidate
            continue
        if use_aggregate:
            assert aggregate is not None
            name = f"paradox-verify-probe-{os.getpid()}-{time.time_ns()}"
            if "," in str(root):
                failures.append(
                    f"{executable}: repository path cannot be represented as a bind mount"
                )
                continue
            probe_state = root / ".local" / "verify" / "probes" / name
            probe_writable = ensure_managed_directory(
                root, probe_state / "writable"
            )
            probe_command = (
                [str(executable)]
                + _base_aggregate_flags(name)
                + [
                    "--workdir",
                    str(root),
                    "--mount",
                    f"type=bind,src={root},dst={root},ro",
                    "--mount",
                    f"type=bind,src={probe_writable},dst={probe_writable},rw",
                    immutable_image,
                    "sh",
                    "-c",
                    AGGREGATE_PROBE_SCRIPT,
                    "aggregate-probe",
                    str(root),
                    str(probe_writable),
                    str(probe_state),
                ]
            )
            probe_error: HarnessError | None = None
            aggregate_result: subprocess.CompletedProcess[str] | None = None
            inherited = False
            inheritance_reason = "aggregate worker probe did not complete"
            try:
                aggregate_result = run_capture(
                    probe_command, cwd=root, timeout=30, check=False
                )
                inherited, inheritance_reason = _aggregate_worker_inherits(
                    aggregate_result.stdout, aggregate
                )
            except HarnessError as exc:
                probe_error = exc
            finally:
                probe_removed = _remove_container(executable, root, name)
            proof = probe_writable / "write-proof"
            try:
                bind_proved = (
                    proof.is_file()
                    and not proof.is_symlink()
                    and proof.read_text(encoding="ascii") == "nested-bind-ok\n"
                )
            except (OSError, UnicodeError):
                bind_proved = False
            try:
                shutil.rmtree(probe_state)
                probe_state_removed = not probe_state.exists()
            except OSError:
                probe_state_removed = False
            if not probe_removed or not probe_state_removed:
                failures.append(
                    f"{executable}: aggregate probe state/container cleanup failed"
                )
                continue
            if probe_error is not None:
                failures.append(
                    f"{executable}: active aggregate probe failed: {probe_error}"
                )
                continue
            if (
                aggregate_result is None
                or aggregate_result.returncode != 0
                or not inherited
                or not bind_proved
            ):
                candidate = EngineProbe(
                    requested=requested,
                    kind=kind,
                    executable=str(executable),
                    usable=True,
                    hard=False,
                    reason=(
                        f"active aggregate cgroup/bind probe failed: "
                        f"{inheritance_reason}; nested_bind="
                        f"{str(bind_proved).lower()}; engine exit="
                        f"{aggregate_result.returncode if aggregate_result else 'none'}"
                    ),
                    identity=identity,
                    image=immutable_image,
                    image_identity=image_identity,
                    storage_driver=storage,
                    limit_mode="aggregate",
                    aggregate=aggregate,
                )
                if soft_probe is None:
                    soft_probe = candidate
                continue
            require_stable_aggregate(
                aggregate,
                phase="during the active worker probe",
                root=root,
            )
            refreshed_kernel = _aggregate_kernel_identity(
                proc_root=pathlib.Path(str(aggregate.identity["proc_root"])),
                cgroup_root=pathlib.Path(str(aggregate.identity["cgroup_root"])),
            )
            refreshed_identity = dict(refreshed_kernel)
            refreshed_identity.update(
                {
                    "systemd": aggregate.identity.get("systemd", {}),
                    "outside_reserve_mib": aggregate.identity.get(
                        "outside_reserve_mib"
                    ),
                }
            )
            refreshed = dataclasses.replace(
                aggregate, identity=refreshed_identity
            )
            return EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=True,
                reason=(
                    f"{refreshed.reason}; {inheritance_reason}; "
                    "checkout and nested writable bind were proved"
                ),
                identity=identity,
                image=immutable_image,
                image_identity=image_identity,
                storage_driver=storage,
                limit_mode="aggregate",
                aggregate=refreshed,
            )
        name = f"paradox-verify-probe-{os.getpid()}-{time.time_ns()}"
        if "," in str(root):
            failures.append(
                f"{executable}: repository path cannot be represented as a bind mount"
            )
            continue
        probe_state = (
            root / ".local" / "verify" / "probes" / name
        )
        probe_writable = ensure_managed_directory(
            root, probe_state / "writable"
        )
        probe_command = (
            [str(executable)]
            + _base_limit_flags(kind, name)
            + [
                "--workdir",
                str(root),
                "--mount",
                f"type=bind,src={root},dst={root},ro",
                "--mount",
                f"type=bind,src={probe_writable},dst={probe_writable},rw",
                immutable_image,
                "sh",
                "-c",
                LIMIT_PROBE_SCRIPT,
                "limit-probe",
                str(root),
                str(probe_writable),
                str(probe_state),
            ]
        )
        probe_error: HarnessError | None = None
        result: subprocess.CompletedProcess[str] | None = None
        ok = False
        limit_reason = "active cgroup probe did not complete"
        try:
            result = run_capture(probe_command, cwd=root, timeout=30, check=False)
            ok, limit_reason = _parse_limit_probe(result.stdout)
        except HarnessError as exc:
            probe_error = exc
        finally:
            probe_removed = _remove_container(executable, root, name)
        proof = probe_writable / "write-proof"
        try:
            bind_proved = (
                proof.is_file()
                and not proof.is_symlink()
                and proof.read_text(encoding="ascii") == "nested-bind-ok\n"
            )
        except (OSError, UnicodeError):
            bind_proved = False
        try:
            shutil.rmtree(probe_state)
            probe_state_removed = not probe_state.exists()
        except OSError:
            probe_state_removed = False
        if not probe_removed:
            failures.append(
                f"{executable}: active cgroup probe container could not be removed"
            )
            continue
        if not probe_state_removed:
            failures.append(
                f"{executable}: active bind probe state could not be removed"
            )
            continue
        if probe_error is not None:
            failures.append(
                f"{executable}: active cgroup probe failed: {probe_error}"
            )
            continue
        if result is None:
            failures.append(f"{executable}: active cgroup probe produced no result")
            continue
        if result.returncode != 0 or not ok or not bind_proved:
            candidate = EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=False,
                reason=(
                    f"active cgroup/bind probe failed: {limit_reason}; "
                    f"nested_bind={str(bind_proved).lower()}; "
                    f"engine exit={result.returncode}"
                ),
                identity=identity,
                image=immutable_image,
                image_identity=image_identity,
                storage_driver=storage,
            )
            if soft_probe is None:
                soft_probe = candidate
            continue
        # Reading the exact cgroup ceilings is the primary proof.  Also require
        # the engine to classify a deliberate over-limit allocation as OOM.
        oom_name = f"paradox-verify-oom-probe-{os.getpid()}-{time.time_ns()}"
        oom_script = (
            "set -eu; "
            "x=$(dd if=/dev/zero bs=1M count=96 2>/dev/null | tr '\\000' x); "
            "test \"${#x}\" -eq 100663296"
        )
        oom_command = (
            [str(executable)]
            + _base_limit_flags(kind, oom_name)
            + [immutable_image, "sh", "-c", oom_script]
        )
        oom_error: HarnessError | None = None
        oom_result: subprocess.CompletedProcess[str] | None = None
        inspect: subprocess.CompletedProcess[str] | None = None
        try:
            oom_result = run_capture(
                oom_command, cwd=root, timeout=45, check=False
            )
            inspect = run_capture(
                [str(executable), "inspect", oom_name],
                cwd=root,
                timeout=20,
                check=False,
            )
        except HarnessError as exc:
            oom_error = exc
        finally:
            oom_removed = _remove_container(executable, root, oom_name)
        if not oom_removed:
            failures.append(
                f"{executable}: active OOM probe container could not be removed"
            )
            continue
        if oom_error is not None:
            failures.append(f"{executable}: active OOM probe failed: {oom_error}")
            continue
        if oom_result is None or inspect is None:
            failures.append(f"{executable}: active OOM probe produced no result")
            continue
        oom_killed = False
        if inspect.returncode == 0:
            try:
                record = json.loads(
                    inspect.stdout, object_pairs_hook=reject_duplicate_keys
                )[0]
                state = record.get("State", {})
                oom_killed = bool(
                    state.get("OOMKilled")
                    or state.get("OomKilled")
                    or state.get("oomKilled")
                )
            except (
                json.JSONDecodeError,
                HarnessError,
                IndexError,
                AttributeError,
            ):
                oom_killed = False
        if not oom_killed:
            candidate = EngineProbe(
                requested=requested,
                kind=kind,
                executable=str(executable),
                usable=True,
                hard=False,
                reason=(
                    "visible limits were correct, but the sacrificial over-limit "
                    f"worker was not classified OOMKilled (exit={oom_result.returncode})"
                ),
                identity=identity,
                image=immutable_image,
                image_identity=image_identity,
                storage_driver=storage,
            )
            if soft_probe is None:
                soft_probe = candidate
            continue
        return EngineProbe(
            requested=requested,
            kind=kind,
            executable=str(executable),
            usable=True,
            hard=True,
            reason=f"{limit_reason}; over-limit allocation was OOMKilled",
            identity=identity,
            image=immutable_image,
            image_identity=image_identity,
            storage_driver=storage,
            limit_mode="worker",
            aggregate=aggregate,
        )
    if soft_probe is not None:
        return soft_probe
    return EngineProbe(
        requested=requested,
        kind=None,
        executable=None,
        usable=False,
        hard=False,
        reason="; ".join(failures) if failures else "no requested container engine found",
        identity={},
        image=image,
        image_identity=None,
        storage_driver=None,
        limit_mode="worker",
        aggregate=aggregate,
    )


def engine_receipt(probe: EngineProbe, host: HostResources) -> dict[str, Any]:
    return {
        "schema": 1,
        "observed_at": utc_now(),
        "kernel": platform.release(),
        "platform": platform.platform(),
        "machine": platform.machine(),
        "host": dataclasses.asdict(host),
        "engine": dataclasses.asdict(probe),
    }


def git_identity(root: pathlib.Path) -> tuple[str, str, bool, list[str]]:
    commit = run_capture(
        ["git", "rev-parse", "--verify", "HEAD^{commit}"], cwd=root
    ).stdout.strip()
    tree = run_capture(["git", "rev-parse", "--verify", "HEAD^{tree}"], cwd=root).stdout.strip()
    status_output = run_capture(
        ["git", "status", "--porcelain=v1", "--untracked-files=all"], cwd=root
    ).stdout
    rows = [row for row in status_output.splitlines() if row]
    if not re.fullmatch(r"[0-9a-f]{40}", commit) or not re.fullmatch(
        r"[0-9a-f]{40}", tree
    ):
        raise FatalRunError("Git returned malformed HEAD identity")
    return commit, tree, not rows, rows


def validate_source_ref_identity(
    root: pathlib.Path, ref: str, expected_commit: str, expected_tree: str
) -> tuple[str, str]:
    """Bind a release source ref to the exact worktree identity used by all gates."""

    if (
        not ref.startswith("refs/")
        or ".." in ref
        or "//" in ref
        or ref.endswith("/")
        or any(ord(character) < 32 for character in ref)
    ):
        raise HarnessError("source_ref must be one safe full refs/... Git ref")
    if (
        run_capture(
            ["git", "check-ref-format", ref], cwd=root, check=False
        ).returncode
        != 0
    ):
        raise HarnessError("source_ref is not a valid full Git ref")
    commit = run_capture(
        ["git", "rev-parse", "--verify", f"{ref}^{{commit}}"], cwd=root
    ).stdout.strip()
    tree = run_capture(
        ["git", "rev-parse", "--verify", f"{ref}^{{tree}}"], cwd=root
    ).stdout.strip()
    head_commit, head_tree, clean, _ = git_identity(root)
    if (
        commit != expected_commit
        or tree != expected_tree
        or head_commit != expected_commit
        or head_tree != expected_tree
        or not clean
    ):
        raise HarnessError(
            "source_ref does not identify the exact clean current HEAD commit/tree: "
            f"{ref} -> {commit}/{tree}, HEAD -> {expected_commit}/{expected_tree}"
        )
    return commit, tree


def resolve_differential_baseline(root: pathlib.Path, ref: str) -> str:
    """Resolve the offline Paradox-1 baseline before cache keys are computed."""

    if not ref or "\x00" in ref or any(ord(character) < 32 for character in ref):
        raise HarnessError("baseline_ref is empty or contains a control character")
    mirror = root / ".local" / "compat" / "differential" / "upstream.git"
    require_plain_directory(mirror, "offline differential mirror")
    commit = run_capture(
        [
            "git",
            f"--git-dir={mirror}",
            "rev-parse",
            "--verify",
            f"{ref}^{{commit}}",
        ],
        cwd=root,
    ).stdout.strip()
    if not re.fullmatch(r"[0-9a-f]{40,64}", commit):
        raise FatalRunError("offline differential baseline resolved malformed identity")
    return commit


def _context_directory(root: pathlib.Path, raw: Any, label: str) -> pathlib.Path:
    if not isinstance(raw, str) or not raw:
        raise HarnessError(f"candidate context {label} must be an absolute path")
    if any(character in raw for character in ("\x00", "\n", "\r", os.pathsep, ",")):
        raise HarnessError(
            f"candidate context {label} cannot be represented safely as an "
            "environment path or bind mount"
        )
    path = pathlib.Path(raw)
    if not path.is_absolute() or path != pathlib.Path(os.path.normpath(raw)):
        raise HarnessError(f"candidate context {label} must be canonical and absolute")
    try:
        path.relative_to(root / ".local")
    except ValueError as exc:
        raise HarnessError(
            f"candidate context {label} must be below {root / '.local'}"
        ) from exc
    require_plain_directory(path, f"candidate context {label}")
    if path.resolve() != path:
        raise HarnessError(f"candidate context {label} resolves elsewhere: {path}")
    return path


def load_candidate_context(
    root: pathlib.Path,
    raw_path: str,
    *,
    required_roles: frozenset[str] = frozenset({"consumer", "documentation"}),
) -> dict[str, str]:
    if not required_roles <= {"consumer", "documentation"}:
        raise HarnessError("candidate context requested an unknown library role")
    path = pathlib.Path(raw_path)
    if not path.is_absolute():
        path = root / path
    path = pathlib.Path(os.path.normpath(str(path)))
    path = require_plain_file(path, "candidate context")
    if path.resolve() != path:
        raise HarnessError("candidate context path resolves through a symbolic link")
    if path.stat().st_size > 1024 * 1024:
        raise HarnessError("candidate context is unexpectedly large")
    raw = path.read_bytes()
    try:
        data = json.loads(raw, object_pairs_hook=reject_duplicate_keys)
    except (UnicodeError, json.JSONDecodeError) as exc:
        raise HarnessError(f"candidate context is malformed: {exc}") from exc
    if not isinstance(data, dict):
        raise HarnessError("candidate context must be one JSON object")
    required = {
        "schema",
        "run_id",
        "ref",
        "commit",
        "tree",
        "source",
        "library",
        "dependency_library",
        "content_sha256",
        "evidence_profile",
        "axis",
    }
    optional = {
        "bridge_library",
        "extra_libraries",
        "documentation_libraries",
    }
    _require_keys(data, required | optional, "candidate context")
    missing = required - data.keys()
    if missing:
        raise HarnessError(
            "candidate context lacks field(s): " + ", ".join(sorted(missing))
        )
    if data["schema"] != 1:
        raise HarnessError("candidate context schema must be 1")
    run_id = data["run_id"]
    ref = data["ref"]
    commit = data["commit"]
    tree = data["tree"]
    content = data["content_sha256"]
    evidence_profile = data["evidence_profile"]
    axis = data["axis"]
    if not isinstance(run_id, str) or not SAFE_NAME.fullmatch(run_id):
        raise HarnessError("candidate context run_id has an unsafe shape")
    if (
        not isinstance(ref, str)
        or not ref.startswith("refs/")
        or ".." in ref
        or "//" in ref
        or ref.endswith("/")
    ):
        raise HarnessError("candidate context ref is not one safe full Git ref")
    if (
        run_capture(
            ["git", "check-ref-format", ref], cwd=root, check=False
        ).returncode
        != 0
    ):
        raise HarnessError("candidate context ref is not a valid full Git ref")
    if not isinstance(commit, str) or not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise HarnessError("candidate context commit is not a SHA-1")
    if not isinstance(tree, str) or not re.fullmatch(r"[0-9a-f]{40}", tree):
        raise HarnessError("candidate context tree is not a SHA-1")
    if not isinstance(content, str) or not re.fullmatch(r"[0-9a-f]{64}", content):
        raise HarnessError("candidate context content_sha256 is not a SHA-256")
    if (
        not isinstance(evidence_profile, str)
        or not SAFE_NAME.fullmatch(evidence_profile)
    ):
        raise HarnessError("candidate context evidence_profile has an unsafe shape")
    if axis not in {"paradox1", "paradox2"}:
        raise HarnessError("candidate context axis must be paradox1 or paradox2")

    source = _context_directory(root, data["source"], "source")
    library = _context_directory(root, data["library"], "library")
    dependency_library = _context_directory(
        root, data["dependency_library"], "dependency_library"
    )
    need_consumer = bool(required_roles & {"consumer", "documentation"})
    bridge_library: pathlib.Path | None = None
    if need_consumer:
        if "bridge_library" not in data:
            raise HarnessError(
                "candidate context lacks bridge_library required by this profile"
            )
        bridge_library = _context_directory(
            root, data["bridge_library"], "bridge_library"
        )
    extra_data = data.get("extra_libraries", [])
    if (
        not isinstance(extra_data, list)
        or any(not isinstance(value, str) for value in extra_data)
        or len(extra_data) != len(set(extra_data))
    ):
        raise HarnessError(
            "candidate context extra_libraries must be a unique string array"
        )
    extras = (
        [
            _context_directory(root, value, f"extra_libraries[{index}]")
            for index, value in enumerate(extra_data)
        ]
        if need_consumer
        else []
    )
    documentation_data = data.get("documentation_libraries", [])
    if (
        not isinstance(documentation_data, list)
        or any(not isinstance(value, str) for value in documentation_data)
        or len(documentation_data) != len(set(documentation_data))
    ):
        raise HarnessError(
            "candidate context documentation_libraries must be a unique "
            "string array"
        )
    documentation_libraries = (
        [
            _context_directory(root, value, f"documentation_libraries[{index}]")
            for index, value in enumerate(documentation_data)
        ]
        if "documentation" in required_roles
        else []
    )
    all_libraries = [
        library,
        dependency_library,
        *([bridge_library] if bridge_library is not None else []),
        *extras,
        *documentation_libraries,
    ]
    if len(all_libraries) != len(set(all_libraries)):
        raise HarnessError(
            "candidate, dependency, bridge, consumer, and documentation libraries "
            "must be distinct"
        )
    expected_library = root / ".local" / "compat" / "runs" / run_id / "library-candidate"
    candidate_run_root = expected_library.parent
    if library != expected_library:
        raise HarnessError(
            f"candidate context library must be the run-owned path {expected_library}"
        )
    expected_source = (
        root / ".local" / "compat" / "candidate-snapshots" / commit
    )
    if source != expected_source:
        raise HarnessError(
            f"candidate context source must be the commit-owned path {expected_source}"
        )
    if bridge_library is not None and (
        bridge_library.parent != candidate_run_root
        or not bridge_library.name.startswith("library-downstream-bridges")
    ):
        raise HarnessError(
            "candidate context bridge_library must be a run-owned downstream "
            f"bridge path below {candidate_run_root}"
        )
    source_commit = run_capture(
        ["git", "-C", str(source), "rev-parse", "--verify", "HEAD^{commit}"],
        cwd=root,
    ).stdout.strip()
    source_tree = run_capture(
        ["git", "-C", str(source), "rev-parse", "--verify", "HEAD^{tree}"],
        cwd=root,
    ).stdout.strip()
    source_status = run_capture(
        [
            "git",
            "-C",
            str(source),
            "status",
            "--porcelain=v1",
            "--untracked-files=all",
        ],
        cwd=root,
    ).stdout
    if source_commit != commit or source_tree != tree or source_status:
        raise HarnessError(
            "candidate context source is dirty or does not match commit/tree"
        )
    resolved_ref = run_capture(
        ["git", "-C", str(source), "rev-parse", "--verify", f"{ref}^{{commit}}"],
        cwd=root,
    ).stdout.strip()
    if resolved_ref != commit:
        raise HarnessError("candidate context ref does not resolve to its commit")

    provenance = require_plain_file(
        library / ".paradox-candidate-provenance.tsv",
        "candidate provenance",
    )
    provenance_seal = require_plain_file(
        library / ".paradox-candidate-provenance.sha256",
        "candidate provenance seal",
    )
    sentinel = require_plain_file(
        library / ".paradox-candidate-content-sha256",
        "candidate content sentinel",
    )
    seal_fields = provenance_seal.read_text(encoding="ascii").strip().split()
    if (
        len(seal_fields) != 2
        or seal_fields[0] != sha256_file(provenance)
        or seal_fields[1] != provenance.name
    ):
        raise HarnessError("candidate provenance seal does not authenticate its TSV")
    if sentinel.read_text(encoding="ascii").strip() != content:
        raise HarnessError("candidate content sentinel differs from candidate context")
    rows: dict[str, str] = {}
    for number, row in enumerate(
        provenance.read_text(encoding="utf-8").splitlines(), 1
    ):
        fields = row.split("\t")
        if number == 1:
            if fields != ["key", "value"]:
                raise HarnessError("candidate provenance has a malformed header")
            continue
        if len(fields) != 2 or fields[0] in rows:
            raise HarnessError(
                f"candidate provenance has malformed or duplicate row {number}"
            )
        rows[fields[0]] = fields[1]
    expected_rows = {
        "candidate_run_id": run_id,
        "candidate_ref": ref,
        "candidate_commit": commit,
        "candidate_tree": tree,
        "candidate_library": str(library),
        "dependency_library": str(dependency_library),
        "candidate_content_sha256": content,
    }
    for name, expected_value in expected_rows.items():
        if rows.get(name) != expected_value:
            raise HarnessError(
                f"candidate provenance {name} differs from candidate context"
            )
    identity_fields = set(required)
    if need_consumer:
        identity_fields.update({"bridge_library", "extra_libraries"})
    if "documentation" in required_roles:
        identity_fields.add("documentation_libraries")
    context_identity = {
        name: data.get(name, [] if name.endswith("_libraries") else "")
        for name in sorted(identity_fields)
    }
    result = {
        "candidate_context_sha256": sha256_bytes(canonical_json(context_identity)),
        "candidate_run_id": run_id,
        "candidate_ref": ref,
        "candidate_commit": commit,
        "candidate_tree": tree,
        "candidate_source": str(source),
        "candidate_library": str(library),
        "dependency_library": str(dependency_library),
        "candidate_content": content,
        "evidence_profile": evidence_profile,
        "paradox_axis": axis,
        "candidate_run_root": str(candidate_run_root),
    }
    if bridge_library is not None:
        result["bridge_library"] = str(bridge_library)
        result["consumer_extra_libs"] = os.pathsep.join(
            [str(bridge_library), *(str(value) for value in extras)]
        )
        if "documentation" in required_roles:
            result["documentation_extra_libs"] = os.pathsep.join(
                [
                    str(bridge_library),
                    *(str(value) for value in extras),
                    *(str(value) for value in documentation_libraries),
                ]
            )
    return result


def changed_files(root: pathlib.Path, since: str | None) -> tuple[list[str], str]:
    commands: list[list[str]] = []
    description: str
    if since:
        base = run_capture(
            ["git", "merge-base", "--", since, "HEAD"], cwd=root
        ).stdout.strip()
        commands.append(
            ["git", "diff", "--name-only", "--diff-filter=ACMRD", f"{base}..HEAD"]
        )
        description = f"committed changes since merge-base({since}, HEAD)={base}"
    else:
        description = "working-tree changes against HEAD"
    commands.extend(
        [
            ["git", "diff", "--name-only", "--diff-filter=ACMRD", "--"],
            ["git", "diff", "--cached", "--name-only", "--diff-filter=ACMRD", "--"],
            ["git", "ls-files", "--others", "--exclude-standard"],
        ]
    )
    result: set[str] = set()
    for command in commands:
        output = run_capture(command, cwd=root).stdout
        for row in output.splitlines():
            parts = pathlib.PurePosixPath(row).parts
            if (
                row
                and "\x00" not in row
                and not any(part in EXCLUDED_INPUT_PARTS for part in parts)
                and not row.endswith((".pyc", ".pyo"))
            ):
                result.add(row)
    if not result and not since:
        parent = run_capture(
            ["git", "rev-parse", "--verify", "HEAD^"], cwd=root, check=False
        )
        if parent.returncode == 0:
            output = run_capture(
                [
                    "git",
                    "diff",
                    "--name-only",
                    "--diff-filter=ACMRD",
                    f"{parent.stdout.strip()}..HEAD",
                ],
                cwd=root,
            ).stdout
            result.update(row for row in output.splitlines() if row)
            description = "clean tree; changes in the current HEAD commit"
    return sorted(result), description


def task_impacted(task: Task, changed: Sequence[str]) -> bool:
    return any(
        fnmatch.fnmatchcase(path, pattern)
        for path in changed
        for pattern in task.impacts
    )


def dependency_closure(tasks: Mapping[str, Task], selected: Iterable[str]) -> set[str]:
    result: set[str] = set()

    def add(task_id: str) -> None:
        if task_id in result:
            return
        result.add(task_id)
        for dependency in tasks[task_id].dependencies:
            add(dependency)

    for task_id in selected:
        add(task_id)
    return result


def select_tasks(
    manifest: Manifest,
    profile: Profile,
    changed: Sequence[str],
    explicit: Sequence[str],
    changed_only_override: bool,
) -> set[str]:
    if explicit:
        unknown = set(explicit) - manifest.tasks.keys()
        if unknown:
            raise HarnessError(f"unknown selected task(s): {', '.join(sorted(unknown))}")
        outside = set(explicit) - set(profile.tasks)
        if outside:
            raise HarnessError(
                f"task(s) not in profile {profile.name}: {', '.join(sorted(outside))}"
            )
        return dependency_closure(manifest.tasks, explicit)
    changed_only = profile.changed_only or changed_only_override
    if not changed_only:
        return dependency_closure(manifest.tasks, profile.tasks)
    selected = set(profile.always)
    selected.update(
        task_id
        for task_id in profile.tasks
        if task_impacted(manifest.tasks[task_id], changed)
    )
    if not selected:
        selected.update(profile.always)
    return dependency_closure(manifest.tasks, selected)


def resolve_parameters(
    manifest: Manifest,
    profile: Profile,
    run_id: str,
    assignments: Sequence[str],
    root: pathlib.Path,
    git_commit: str,
    git_tree: str,
    context: Mapping[str, str] | None = None,
) -> dict[str, str]:
    values = dict(manifest.parameter_defaults)
    context = {} if context is None else dict(context)
    values.update(context)
    for assignment in assignments:
        if "=" not in assignment:
            raise HarnessError("--param requires NAME=VALUE")
        name, value = assignment.split("=", 1)
        if not SAFE_PARAMETER.fullmatch(name):
            raise HarnessError(f"unsafe parameter name: {name}")
        if name in context:
            raise HarnessError(
                f"--param {name} cannot override authenticated candidate context"
            )
        if "\x00" in value or any(ord(character) < 32 for character in value):
            raise HarnessError(f"parameter {name} contains a control character")
        values[name] = value
    values.update(
        {
            "root": str(root),
            "run_id": run_id,
            "profile": profile.name,
            "task_id": "",
            "attempt_id": ATTEMPT_ID_SENTINEL,
            "git_commit": git_commit,
            "git_tree": git_tree,
        }
    )
    return values


def validate_required_parameters(
    tasks: Mapping[str, Task], selected: set[str], parameters: Mapping[str, str]
) -> None:
    missing: dict[str, list[str]] = {}
    for task_id in selected:
        names = [
            name
            for name in tasks[task_id].required_parameters
            if not parameters.get(name)
        ]
        if names:
            missing[task_id] = names
    if missing:
        details = "; ".join(
            f"{task_id}: {', '.join(names)}"
            for task_id, names in sorted(missing.items())
        )
        raise HarnessError(f"selected tasks require missing --param values: {details}")
    violations: list[str] = []
    for task_id in sorted(selected):
        for name, allowed in tasks[task_id].required_values.items():
            value = parameters.get(name, "")
            if value not in allowed:
                violations.append(
                    f"{task_id}: {name}={value!r} (allowed: "
                    + ", ".join(repr(item) for item in allowed)
                    + ")"
                )
    if violations:
        raise HarnessError(
            "selected tasks reject parameter values: " + "; ".join(violations)
        )


def _input_paths(
    root: pathlib.Path, manifest: Manifest, task: Task
) -> list[pathlib.Path]:
    patterns: list[str] = list(task.inputs)
    for group in task.input_groups:
        patterns.extend(manifest.input_groups[group])
    paths: set[pathlib.Path] = set()
    for declared_pattern in patterns:
        optional = declared_pattern.startswith("?")
        pattern = declared_pattern[1:] if optional else declared_pattern
        matches = list(root.glob(pattern))
        if not matches:
            if optional:
                continue
            raise FatalRunError(
                f"task {task.task_id} input pattern matched nothing: {pattern}"
            )
        for match in matches:
            if match.is_symlink():
                raise FatalRunError(
                    f"task {task.task_id} input follows a symbolic link: {match}"
                )
            if match.is_dir():
                for child in match.rglob("*"):
                    relative_parts = child.relative_to(root).parts
                    if any(part in EXCLUDED_INPUT_PARTS for part in relative_parts):
                        continue
                    if child.is_symlink():
                        raise FatalRunError(
                            f"task {task.task_id} input follows a symbolic link: {child}"
                        )
                    if child.is_file():
                        paths.add(child)
            elif match.is_file():
                paths.add(match)
            else:
                raise FatalRunError(
                    f"task {task.task_id} input is not regular: {match}"
                )
    return sorted(paths, key=lambda value: value.relative_to(root).as_posix())


def input_receipt(
    root: pathlib.Path, manifest: Manifest, task: Task
) -> tuple[str, list[dict[str, Any]]]:
    rows: list[dict[str, Any]] = []
    for path in _input_paths(root, manifest, task):
        before = path.lstat()
        if not stat.S_ISREG(before.st_mode):
            raise FatalRunError(
                f"task {task.task_id} input stopped being regular: {path}"
            )
        digest = sha256_file(path)
        after = path.lstat()
        identity_before = (
            before.st_dev,
            before.st_ino,
            before.st_mode,
            before.st_size,
            before.st_mtime_ns,
            before.st_ctime_ns,
        )
        identity_after = (
            after.st_dev,
            after.st_ino,
            after.st_mode,
            after.st_size,
            after.st_mtime_ns,
            after.st_ctime_ns,
        )
        if identity_before != identity_after or not stat.S_ISREG(after.st_mode):
            raise FatalRunError(
                f"task {task.task_id} input changed while it was hashed: {path}"
            )
        rows.append(
            {
                "path": path.relative_to(root).as_posix(),
                "size": after.st_size,
                "mode": stat.S_IMODE(after.st_mode),
                "sha256": digest,
            }
        )
    return sha256_bytes(canonical_json(rows)), rows


def authenticate_planned_inputs(
    root: pathlib.Path, manifest: Manifest | None, item: "PlannedTask"
) -> None:
    """Reject execution or publication under a stale planning digest."""

    if manifest is None:  # Synthetic scheduler fixtures have no filesystem manifest.
        return
    observed, _ = input_receipt(root, manifest, item.task)
    if observed != item.input_digest:
        raise FatalRunError(
            f"task {item.task.task_id} inputs changed after planning"
        )


def tree_content_sha256(path: pathlib.Path, label: str) -> str:
    require_plain_directory(path, label)
    digest = hashlib.sha256()
    for child in sorted(path.rglob("*"), key=lambda value: value.relative_to(path).as_posix()):
        relative = child.relative_to(path).as_posix()
        details = child.lstat()
        if stat.S_ISLNK(details.st_mode):
            raise FatalRunError(f"{label} contains a symbolic link: {child}")
        if stat.S_ISDIR(details.st_mode):
            digest.update(b"D\0")
            digest.update(relative.encode("utf-8"))
            digest.update(b"\0")
            continue
        if not stat.S_ISREG(details.st_mode):
            raise FatalRunError(f"{label} contains a non-regular path: {child}")
        digest.update(b"F\0")
        digest.update(relative.encode("utf-8"))
        digest.update(b"\0")
        digest.update(str(stat.S_IMODE(details.st_mode)).encode("ascii"))
        digest.update(b"\0")
        digest.update(str(details.st_size).encode("ascii"))
        digest.update(b"\0")
        with child.open("rb") as stream:
            while True:
                block = stream.read(1024 * 1024)
                if not block:
                    break
                digest.update(block)
        digest.update(b"\0")
    return digest.hexdigest()


def execution_environment_identity(
    root: pathlib.Path, *, include_r_library: bool
) -> dict[str, Any]:
    prefix = root / ".local" / "toolchain"
    records: list[dict[str, Any]] = []
    for relative in (
        "environment/toolchain-linux-64.lock",
        ".local/toolchain/bin/R",
        ".local/toolchain/bin/Rscript",
        ".local/toolchain/bin/git",
        ".local/toolchain/bin/make",
        ".local/toolchain/bin/python3",
        ".local/toolchain/bin/x86_64-conda-linux-gnu-gcc",
        ".local/toolchain/bin/x86_64-conda-linux-gnu-clang",
    ):
        path = root / relative
        if not path.exists() and not path.is_symlink():
            records.append({"path": relative, "status": "absent"})
            continue
        link_target: str | None = None
        if path.is_symlink():
            link_target = os.readlink(path)
            link_target = link_target.replace(str(prefix), "{TOOLCHAIN}")
            resolved = path.resolve()
            try:
                resolved.relative_to(prefix)
            except ValueError as exc:
                raise FatalRunError(
                    f"toolchain executable link escapes its prefix: {path}"
                ) from exc
            path_for_hash = require_plain_file(resolved, "toolchain executable")
        else:
            path_for_hash = require_plain_file(path, "toolchain identity input")
        payload = path_for_hash.read_bytes()
        if relative == ".local/toolchain/bin/R":
            # Conda's generated R launcher embeds its absolute installation
            # prefix. Normalize only this reviewed wrapper so identical
            # lockfile-built toolchains remain semantically identical when a
            # checkout moves, while arbitrary wrapper changes still rekey.
            payload = payload.replace(
                str(prefix).encode("utf-8"), b"{TOOLCHAIN}"
            )
        records.append(
            {
                "path": relative,
                "link_target": link_target,
                "size": len(payload),
                "sha256": sha256_bytes(payload),
            }
        )
    result: dict[str, Any] = {
        "schema": 1,
        "platform": sys.platform,
        "machine": platform.machine(),
        "inputs": records,
    }
    library = root / ".local" / "R" / "library"
    if include_r_library and library.is_dir() and not library.is_symlink():
        result["r_library_sha256"] = tree_content_sha256(
            library, "repository-local R library"
        )
    else:
        result["r_library_sha256"] = "not-selected"
    result["identity_sha256"] = sha256_bytes(canonical_json(result))
    return result


@dataclasses.dataclass
class PlannedTask:
    task: Task
    command: tuple[str, ...]
    environment: dict[str, str]
    image: str | None
    image_identity: str | None
    writable_paths: tuple[str, ...]
    readonly_paths: tuple[str, ...]
    impacted: bool
    input_digest: str
    cache_key: str
    score: float
    status: str = "pending"
    reason: str = ""
    cached_receipt: dict[str, Any] | None = None


def cache_root(root: pathlib.Path) -> pathlib.Path:
    return root / ".local" / "verify" / "cache" / "results"


def cache_entry(root: pathlib.Path, task_id: str, key: str) -> pathlib.Path:
    return cache_root(root) / task_id / key


def validate_cache_entry(path: pathlib.Path, task_id: str, key: str) -> dict[str, Any]:
    require_plain_directory(path, "verification cache entry")
    entries = sorted(child.name for child in path.iterdir())
    if entries != ["receipt.json", "task.log"]:
        raise FatalRunError(f"cache entry contains unexpected paths: {path}")
    receipt_path = require_plain_file(path / "receipt.json", "cache receipt")
    log_path = require_plain_file(path / "task.log", "cached task log")
    if receipt_path.stat().st_size > 1024 * 1024:
        raise FatalRunError(f"cache receipt is unexpectedly large: {receipt_path}")
    try:
        receipt = json.loads(
            receipt_path.read_text(encoding="utf-8"),
            object_pairs_hook=reject_duplicate_keys,
        )
    except (OSError, UnicodeError, json.JSONDecodeError, HarnessError) as exc:
        raise FatalRunError(f"cache receipt is malformed: {receipt_path}: {exc}") from exc
    if (
        not isinstance(receipt, dict)
        or receipt.get("schema") != CACHE_SCHEMA
        or receipt.get("task_id") != task_id
        or receipt.get("cache_key") != key
        or receipt.get("status") != "passed"
        or receipt.get("log_sha256") != sha256_file(log_path)
    ):
        raise FatalRunError(f"cache entry failed authentication: {path}")
    original = receipt.get("original_result")
    if (
        not isinstance(original, dict)
        or original.get("task_id") != task_id
        or original.get("cache_key") != key
        or original.get("status") != "passed"
    ):
        raise FatalRunError(f"cache entry has malformed original result: {path}")
    return receipt


def history_failures(root: pathlib.Path) -> set[tuple[str, str]]:
    path = root / ".local" / "verify" / "history.jsonl"
    if not path.exists():
        return set()
    require_plain_file(path, "verification history")
    if path.stat().st_size > 64 * 1024 * 1024:
        raise FatalRunError("verification history is unexpectedly large")
    failures: set[tuple[str, str]] = set()
    read_flags = os.O_RDONLY
    if hasattr(os, "O_NOFOLLOW"):
        read_flags |= os.O_NOFOLLOW
    descriptor = os.open(path, read_flags)
    with os.fdopen(descriptor, "r", encoding="utf-8") as stream:
        if not stat.S_ISREG(os.fstat(stream.fileno()).st_mode):
            raise FatalRunError("verification history is not a regular file")
        fcntl.flock(stream.fileno(), fcntl.LOCK_SH)
        try:
            for number, line in enumerate(stream, 1):
                try:
                    row = json.loads(
                        line, object_pairs_hook=reject_duplicate_keys
                    )
                except (json.JSONDecodeError, HarnessError) as exc:
                    raise FatalRunError(
                        f"verification history has malformed row {number}"
                    ) from exc
                if not isinstance(row, dict):
                    raise FatalRunError(
                        f"verification history row {number} is not an object"
                    )
                pair = (row.get("task_id"), row.get("cache_key"))
                if not all(isinstance(value, str) for value in pair):
                    raise FatalRunError(
                        f"verification history row {number} lacks task identity"
                    )
                origin = row.get("origin", "execution")
                if not isinstance(origin, str):
                    raise FatalRunError(
                        f"verification history row {number} has malformed origin"
                    )
                if (
                    row.get("status") in TERMINAL_FAILURE
                    and row.get("status") != "blocked"
                    and origin == "execution"
                ):
                    # A later pass intentionally does not erase priority.
                    failures.add(pair)
        finally:
            fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
    return failures


def plan_tasks(
    root: pathlib.Path,
    manifest: Manifest,
    profile: Profile,
    selected: set[str],
    changed: Sequence[str],
    parameters: Mapping[str, str],
    probe: EngineProbe,
    *,
    reuse_cache: bool,
) -> dict[str, PlannedTask]:
    planned: dict[str, PlannedTask] = {}
    prior_failures = history_failures(root)
    # Resume reuses successful rows even in profiles that disable generic
    # development-cache publication. The ordinary activated R library is
    # therefore always semantic; tying it only to `reuse_cache` would allow a
    # release resume against different dependency bytes.
    runtime_identity = execution_environment_identity(
        root, include_r_library=True
    )

    def plan_one(task_id: str) -> PlannedTask:
        if task_id in planned:
            return planned[task_id]
        task = manifest.tasks[task_id]
        dependency_keys = {
            dependency: plan_one(dependency).cache_key
            for dependency in task.dependencies
            if dependency in selected
        }
        values = dict(parameters)
        values["task_id"] = task_id
        command = tuple(expand(value, values) for value in task.command)
        environment = {
            name: expand(value, values) for name, value in task.environment.items()
        }
        if task.image:
            requested_image = expand(task.image, values)
            if not probe.executable or not probe.kind:
                raise HarnessError(
                    f"task {task_id} requests an image without a usable engine"
                )
            image_identity, image = _image_identity(
                pathlib.Path(probe.executable),
                probe.kind,
                requested_image,
                root,
            )
        else:
            image = probe.image if probe.hard else None
            image_identity = probe.image_identity if probe.hard else None
        writable_paths = tuple(expand(value, values) for value in task.writable_paths)
        readonly_paths = tuple(expand(value, values) for value in task.readonly_paths)
        input_digest, _ = input_receipt(root, manifest, task)
        template_values = (
            list(task.command)
            + list(task.environment.values())
            + list(task.writable_paths)
            + list(task.readonly_paths)
            + ([task.image] if task.image else [])
        )
        relevant_parameters = set(task.required_parameters) | set(task.required_values)
        relevant_parameters.update(
            set().union(*(placeholders(value) for value in template_values))
        )
        semantic_parameters = {
            name: value
            for name, value in parameters.items()
            if name in relevant_parameters
            and name
            not in {"run_id", "task_id", "profile", "root", "attempt_id"}
        }
        backend = {
            "kind": (
                "controller"
                if task.isolation == "controller"
                else (hard_backend_name(probe) if probe.hard else "best-effort-host")
            ),
            "hard": probe.hard if task.isolation == "worker" else False,
            "limit_mode": probe.limit_mode
            if task.isolation == "worker" and probe.hard
            else None,
            "cgroup_layout": semantic_cgroup_layout(probe)
            if task.isolation == "worker"
            else None,
            "image_identity": image_identity
            if task.isolation == "worker" and probe.hard
            else None,
            "platform": sys.platform,
            "machine": platform.machine(),
        }
        if task.isolation == "worker" and not probe.hard:
            # The explicit host fallback is deliberately nonportable evidence.
            # Its exact host/controller runtime remains semantic even though
            # hard workers may move across incidental kernel/engine versions.
            backend["best_effort_host"] = {
                "platform": platform.platform(),
                "python": platform.python_version(),
            }
        key_payload = {
            "schema": 1,
            "task": dataclasses.asdict(task),
            "input_digest": input_digest,
            "dependencies": dependency_keys,
            "parameters": semantic_parameters,
            "backend": backend,
            "execution_environment": runtime_identity,
            "controller_sha256": sha256_file(pathlib.Path(__file__)),
        }
        key = sha256_bytes(canonical_json(key_payload))
        impacted = task_impacted(task, changed)
        score = (
            task.priority
            + (100000 if impacted else 0)
            + (50000 if (task_id, key) in prior_failures else 0)
            + 1000 * task.information / max(1, task.estimated_seconds)
        )
        result = PlannedTask(
            task=task,
            command=command,
            environment=environment,
            image=image,
            image_identity=image_identity,
            writable_paths=writable_paths,
            readonly_paths=readonly_paths,
            impacted=impacted,
            input_digest=input_digest,
            cache_key=key,
            score=score,
        )
        if reuse_cache and task.cache == "success":
            path = cache_entry(root, task_id, key)
            if path.exists():
                result.cached_receipt = validate_cache_entry(path, task_id, key)
                result.status = "cached"
                result.reason = f"authenticated exact-key cache hit: {path}"
        incompatibility = task_platform_incompatibility(task)
        if incompatibility and result.status != "cached":
            result.status = "unsupported"
            result.reason = incompatibility
        planned[task_id] = result
        return result

    for task_id in selected:
        plan_one(task_id)
    return planned


def topology_rows(planned: Mapping[str, PlannedTask]) -> list[PlannedTask]:
    indegree = {task_id: 0 for task_id in planned}
    children: dict[str, list[str]] = defaultdict(list)
    for task_id, item in planned.items():
        for dependency in item.task.dependencies:
            if dependency in planned:
                indegree[task_id] += 1
                children[dependency].append(task_id)
    ready = sorted(task_id for task_id, degree in indegree.items() if degree == 0)
    result: list[PlannedTask] = []
    while ready:
        ready.sort(
            key=lambda task_id: (
                planned[task_id].task.phase,
                -planned[task_id].score,
                task_id,
            )
        )
        task_id = ready.pop(0)
        result.append(planned[task_id])
        for child in children[task_id]:
            indegree[child] -= 1
            if indegree[child] == 0:
                ready.append(child)
    if len(result) != len(planned):
        raise FatalRunError("internal task plan contains a cycle")
    return result


def plan_payload(
    manifest: Manifest,
    profile: Profile,
    policy: str,
    run_id: str,
    changed: Sequence[str],
    changed_description: str,
    parameters: Mapping[str, str],
    probe: EngineProbe,
    host: HostResources,
    planned: Mapping[str, PlannedTask],
) -> dict[str, Any]:
    payload = {
        "schema": PLAN_SCHEMA,
        "manifest": str(manifest.path),
        "manifest_sha256": manifest.digest,
        "profile": profile.name,
        "policy": policy,
        "run_id": run_id,
        "changed_files": list(changed),
        "changed_description": changed_description,
        "parameters": {
            name: value
            for name, value in parameters.items()
            if name not in {"task_id"}
        },
        "engine": dataclasses.asdict(probe),
        "host": dataclasses.asdict(host),
        "tasks": [
            {
                "id": item.task.task_id,
                "description": item.task.description,
                "dependencies": [
                    value for value in item.task.dependencies if value in planned
                ],
                "phase": item.task.phase,
                "score": item.score,
                "impacted": item.impacted,
                "resources": dataclasses.asdict(item.task.resources),
                "failure_class": item.task.failure_class,
                "cache": item.task.cache,
                "cache_key": item.cache_key,
                "initial_status": item.status,
                "initial_reason": item.reason,
                "command": list(item.command),
                "environment": item.environment,
                "image": item.image,
                "image_identity": item.image_identity,
                "writable_paths": list(item.writable_paths),
                "readonly_paths": list(item.readonly_paths),
                "platforms": list(item.task.platforms),
                "machines": list(item.task.machines),
                "revalidate_on_resume": item.task.revalidate_on_resume,
            }
            for item in topology_rows(planned)
        ],
    }
    identity = {
        "schema": payload["schema"],
        "manifest_sha256": payload["manifest_sha256"],
        "profile": payload["profile"],
        "policy": payload["policy"],
        "run_id": payload["run_id"],
        "parameters": payload["parameters"],
        "engine": {
            "kind": hard_backend_name(probe)
            if probe.hard
            else "best-effort-host",
            "hard": probe.hard,
            "limit_mode": probe.limit_mode if probe.hard else None,
            "cgroup_layout": semantic_cgroup_layout(probe),
            "image_identity": probe.image_identity if probe.hard else None,
        },
        "tasks": [
            {
                name: task[name]
                for name in (
                    "id",
                    "dependencies",
                    "phase",
                    "resources",
                    "failure_class",
                    "cache",
                    "cache_key",
                    "command",
                    "environment",
                    "image",
                    "image_identity",
                    "writable_paths",
                    "readonly_paths",
                    "platforms",
                    "machines",
                    "revalidate_on_resume",
                )
            }
            for task in payload["tasks"]
        ],
    }
    payload["plan_fingerprint"] = sha256_bytes(canonical_json(identity))
    return payload


def print_doctor(host: HostResources, probe: EngineProbe) -> None:
    print(f"host_cpus\t{host.cpus}")
    print(f"host_memory_available_mib\t{host.memory_available_mib}")
    print(f"host_memory_source\t{host.memory_source}")
    print(f"host_memory_reserve_mib\t{host.memory_reserve_mib}")
    print(f"host_memory_budget_mib\t{host.memory_budget_mib}")
    print(
        "host_memory_capacity_mib\t"
        f"{host.memory_capacity_mib if host.memory_capacity_mib is not None else host.memory_budget_mib}"
    )
    print(f"host_disk_available_mib\t{host.disk_available_mib}")
    print(f"host_disk_reserve_mib\t{host.disk_reserve_mib}")
    print(f"host_disk_budget_mib\t{host.disk_budget_mib}")
    print(
        "host_disk_capacity_mib\t"
        f"{host.disk_capacity_mib if host.disk_capacity_mib is not None else host.disk_budget_mib}"
    )
    print(f"host_pid_budget\t{host.pid_budget}")
    print(
        "host_pid_capacity\t"
        f"{host.pid_capacity if host.pid_capacity is not None else host.pid_budget}"
    )
    print(f"host_pid_source\t{host.pid_source}")
    print(f"engine\t{probe.kind or 'none'}")
    print(f"engine_executable\t{probe.executable or '-'}")
    print(f"engine_usable\t{str(probe.usable).lower()}")
    print(f"hard_isolation\t{str(probe.hard).lower()}")
    print(
        "hard_isolation_scope\t"
        f"{probe.limit_mode if probe.hard else 'none'}"
    )
    print(f"engine_reason\t{probe.reason}")
    print(f"storage_driver\t{probe.storage_driver or '-'}")
    aggregate = probe.aggregate
    print(
        "aggregate_containment\t"
        f"{str(bool(aggregate and aggregate.hard)).lower()}"
    )
    if aggregate is not None:
        print(f"aggregate_reason\t{aggregate.reason}")
        if aggregate.hard:
            print(
                "aggregate_memory_limit_mib\t"
                f"{int(aggregate.identity['memory_effective_limit_bytes']) // (1024 * 1024)}"
            )
            print(
                "aggregate_cgroup\t"
                f"{aggregate.identity['cgroup_path']}"
            )


def print_plan(payload: Mapping[str, Any]) -> None:
    host = payload["host"]
    engine = payload["engine"]
    print(
        f"profile={payload['profile']} policy={payload['policy']} "
        f"tasks={len(payload['tasks'])} "
        f"hard_isolation={str(engine['hard']).lower()} "
        f"scope={engine.get('limit_mode') or 'none'}"
    )
    print(
        f"budget: cpu={host['cpu_budget']:.2f} "
        f"memory={host['memory_budget_mib']} MiB "
        f"pids={host['pid_budget']} scratch={host['disk_budget_mib']} MiB"
    )
    print(f"changes: {payload['changed_description']}")
    if payload["changed_files"]:
        print("changed files: " + ", ".join(payload["changed_files"]))
    print("")
    print("phase\tstatus\timpact\tcpu_min..max\tmemory_min..max\ttimeout\ttask")
    for task in payload["tasks"]:
        resources = task["resources"]
        print(
            f"{task['phase']}\t{task['initial_status']}\t"
            f"{'yes' if task['impacted'] else 'no'}\t"
            f"{resources['cpu_min']}..{resources['cpu']}\t"
            f"{resources['memory_mib_min']}..{resources['memory_mib']}\t"
            f"{resources['timeout_seconds']}\t{task['id']}"
        )


def clean_parallel_environment(environment: dict[str, str], cpu: float) -> None:
    threads = max(1, int(math.floor(cpu)))
    environment.update(
        {
            "PARADOX_VERIFY_ASSIGNED_CPUS": str(cpu),
            "PARADOX_VERIFY_ASSIGNED_MEMORY_MIB": environment.get(
                "PARADOX_VERIFY_ASSIGNED_MEMORY_MIB", ""
            ),
            "OMP_NUM_THREADS": "1",
            "OMP_THREAD_LIMIT": "1",
            "OPENBLAS_NUM_THREADS": "1",
            "GOTO_NUM_THREADS": "1",
            "MKL_NUM_THREADS": "1",
            "VECLIB_MAXIMUM_THREADS": "1",
            "BLIS_NUM_THREADS": "1",
            "NUMEXPR_NUM_THREADS": "1",
            "TESTTHAT_PARALLEL": "false",
            "TESTTHAT_CPUS": "1",
            "MAKEFLAGS": f"-j{threads}",
        }
    )


def attempt_run_id(run_id: str, task_id: str, attempt: int) -> str:
    suffix = f"-{task_id}-a{attempt:03d}"
    candidate = run_id + suffix
    if len(candidate) <= 128 and SAFE_NAME.fullmatch(candidate):
        return candidate
    digest = sha256_bytes(f"{run_id}\0{task_id}\0{attempt}".encode())[:16]
    candidate = f"{run_id[:78]}-{task_id[:24]}-{digest}-a{attempt:03d}"
    if not SAFE_NAME.fullmatch(candidate):
        raise FatalRunError("could not construct one safe attempt-specific run ID")
    return candidate


def materialize_attempt(value: str, attempt_id: str) -> str:
    return value.replace(ATTEMPT_ID_SENTINEL, attempt_id)


def managed_mount_path(
    root: pathlib.Path, raw: str, *, writable: bool, label: str
) -> pathlib.Path:
    path = pathlib.Path(raw)
    if not path.is_absolute():
        path = root / path
    path = pathlib.Path(os.path.normpath(str(path)))
    try:
        relative = path.relative_to(root)
    except ValueError as exc:
        raise FatalRunError(f"{label} escapes the repository: {path}") from exc
    if (
        not relative.parts
        or relative.parts[0] not in {".local", ".cache"}
        or any(part in {"", ".", ".."} for part in relative.parts)
    ):
        raise FatalRunError(
            f"{label} must be a non-root path below .local or .cache: {path}"
        )
    if "," in str(path) or "\n" in str(path) or "\r" in str(path):
        raise FatalRunError(f"{label} cannot be represented safely as a bind mount")
    if writable:
        return ensure_managed_directory(root, path)
    require_plain_directory(path, label)
    if path.resolve() != path:
        raise FatalRunError(f"{label} resolves through a symbolic link: {path}")
    return path


def container_command(
    root: pathlib.Path,
    probe: EngineProbe,
    item: PlannedTask,
    run_root: pathlib.Path,
    attempt_id: str,
    attempt: int,
    allocation: Allocation | None = None,
) -> tuple[list[str], str]:
    if not probe.hard or not probe.executable or not probe.kind:
        raise FatalRunError("container command requested without proven hard isolation")
    if not item.image:
        raise FatalRunError(f"task {item.task.task_id} has no worker image")
    task = item.task
    assigned = allocation or Allocation(
        cpu=task.resources.cpu,
        memory_mib=task.resources.memory_mib,
        pids=task.resources.pids,
        scratch_mib=task.resources.scratch_mib,
    )
    safe_fragment = re.sub(r"[^A-Za-z0-9_.-]", "-", task.task_id)
    name = f"paradox-v-{os.getpid()}-{safe_fragment}-{time.time_ns()}"
    task_root = run_root / "tasks" / task.task_id
    task_state = task_root / "state" / attempt_id
    home = task_state / "home"
    temporary = task_state / "tmp"
    runtime = task_state / "runtime"
    home.mkdir(parents=True, exist_ok=True)
    temporary.mkdir(parents=True, exist_ok=True)
    runtime.mkdir(parents=True, exist_ok=True)
    command = [
        probe.executable,
        "run",
        "--name",
        name,
        "--pull=never",
    ]
    if probe.limit_mode == "aggregate":
        if probe.kind != "podman" or probe.aggregate is None or not probe.aggregate.hard:
            raise FatalRunError(
                "aggregate worker command lacks proved local Podman containment"
            )
        command.extend(
            [
                "--cgroups=disabled",
                "--cgroupns=host",
                "--oom-score-adj=1000",
            ]
        )
    else:
        command.extend(
            [
                "--memory",
                f"{assigned.memory_mib}m",
                "--memory-swap",
                f"{assigned.memory_mib}m",
                "--cpus",
                str(assigned.cpu),
                "--pids-limit",
                str(assigned.pids),
            ]
        )
    command.extend(
        [
        "--shm-size",
        f"{max(16, min(256, assigned.memory_mib // 8))}m",
        "--read-only",
        "--cap-drop=all",
        "--security-opt=no-new-privileges",
        "--security-opt=label=disable",
        "--label=org.mlr-org.paradox.verify=worker",
        f"--label=org.mlr-org.paradox.verify.uid={os.getuid()}",
        "--log-driver=none",
        "--workdir",
        str(root),
        ]
    )
    if not task.network:
        command.append("--network=none")
    if probe.kind == "podman":
        command.extend(["--userns=keep-id"])
    else:
        command.extend(["--user", f"{os.getuid()}:{os.getgid()}"])
    if "," in str(root):
        raise FatalRunError("repository path cannot be represented as a bind mount")
    command.extend(["--mount", f"type=bind,src={root},dst={root},ro"])
    writable = {
        managed_mount_path(root, str(home), writable=True, label="task home"),
        managed_mount_path(root, str(temporary), writable=True, label="task temporary"),
        managed_mount_path(root, str(runtime), writable=True, label="task runtime"),
    }
    writable.update(
        managed_mount_path(
            root,
            value,
            writable=True,
            label="activation writable path",
        )
        for value in ACTIVATION_WRITABLE_PATHS
    )
    writable.update(
        managed_mount_path(
            root,
            materialize_attempt(value, attempt_id),
            writable=True,
            label=f"task {task.task_id} writable path",
        )
        for value in item.writable_paths
    )
    readonly = {
        managed_mount_path(
            root,
            materialize_attempt(value, attempt_id),
            writable=False,
            label=f"task {task.task_id} read-only path",
        )
        for value in item.readonly_paths
    }
    for environment_name in (
        "PARADOX_CONSUMER_EXTRA_LIBS",
        "PARADOX_DOCUMENTATION_EXTRA_LIBS",
    ):
        extras = item.environment.get(environment_name, "")
        for value in extras.split(os.pathsep):
            if value:
                readonly.add(
                    managed_mount_path(
                        root,
                        value,
                        writable=False,
                        label=f"task {task.task_id} extra library",
                    )
                )
    for path in sorted(writable, key=lambda value: (len(value.parts), str(value))):
        command.extend(
            ["--mount", f"type=bind,src={path},dst={path},rw"]
        )
    command.extend(
        ["--mount", f"type=bind,src={temporary},dst=/tmp,rw"]
    )
    for relative in ACTIVATION_PRIVATE_PATHS:
        destination = managed_mount_path(
            root,
            relative,
            writable=True,
            label="activation private destination",
        )
        source = managed_mount_path(
            root,
            str(task_state / "activation" / relative.lstrip(".")),
            writable=True,
            label="activation private source",
        )
        command.extend(
            ["--mount", f"type=bind,src={source},dst={destination},rw"]
        )
    # Read-only descendants deliberately come last so they remain protected
    # when a task needs a writable evidence parent.
    for path in sorted(readonly, key=lambda value: (len(value.parts), str(value))):
        command.extend(
            ["--mount", f"type=bind,src={path},dst={path},ro"]
        )
    for key, value in item.environment.items():
        command.extend(
            ["--env", f"{key}={materialize_attempt(value, attempt_id)}"]
        )
    command.extend(
        [
            "--env",
            f"PARADOX_VERIFY_ASSIGNED_CPUS={assigned.cpu}",
            "--env",
            f"PARADOX_VERIFY_ASSIGNED_MEMORY_MIB={assigned.memory_mib}",
            "--env",
            f"PARADOX_VERIFY_TASK_ATTEMPT={attempt}",
        ]
    )
    if probe.limit_mode == "aggregate":
        command.extend(["--env", "PARADOX_VERIFY_AGGREGATE_SYSTEMD=1"])
    command.extend(
        [
            item.image,
            "sh",
            str(root / "scripts" / "environment" / "verify-task-entry"),
            str(root),
            str(home),
            str(temporary),
            str(runtime),
            *(materialize_attempt(value, attempt_id) for value in item.command),
        ]
    )
    return command, name


def best_effort_preexec(memory_mib: int) -> Any:
    def configure() -> None:
        os.setsid()
        ceiling = memory_mib * 1024 * 1024
        try:
            resource.setrlimit(resource.RLIMIT_AS, (ceiling, ceiling))
        except (ValueError, OSError):
            pass

    return configure


def process_tree_rss_mib(root_pid: int) -> int:
    if not pathlib.Path("/proc").is_dir():
        return 0
    queue = deque([root_pid])
    seen: set[int] = set()
    total_kib = 0
    while queue:
        pid = queue.popleft()
        if pid in seen:
            continue
        seen.add(pid)
        status = pathlib.Path(f"/proc/{pid}/status")
        try:
            for line in status.read_text(encoding="ascii", errors="ignore").splitlines():
                if line.startswith("VmRSS:"):
                    fields = line.split()
                    if len(fields) >= 2 and fields[1].isdigit():
                        total_kib += int(fields[1])
                    break
        except (FileNotFoundError, PermissionError, ProcessLookupError):
            continue
        children = pathlib.Path(f"/proc/{pid}/task/{pid}/children")
        try:
            queue.extend(int(value) for value in children.read_text().split())
        except (FileNotFoundError, PermissionError, ProcessLookupError, ValueError):
            continue
    return (total_kib + 1023) // 1024


@dataclasses.dataclass
class RunningTask:
    planned: PlannedTask
    process: subprocess.Popen[bytes]
    log: BinaryIO
    log_path: pathlib.Path
    result_path: pathlib.Path
    started_monotonic: float
    started_at: str
    attempt: int
    container_name: str | None
    backend: str = "best-effort-host"
    allocation: Allocation | None = None
    peak_rss_mib: int = 0
    forced_status: str | None = None
    forced_reason: str = ""
    cleanup_failed: bool = False
    cleanup_confirmed: bool = False


def terminate_running(
    running: RunningTask, probe: EngineProbe, root: pathlib.Path, reason: str
) -> None:
    if running.process.poll() is not None and not running.container_name:
        return
    if running.container_name and probe.executable:
        for command, timeout in (
            (
                [probe.executable, "stop", "--time", "5", running.container_name],
                12,
            ),
            ([probe.executable, "kill", running.container_name], 10),
        ):
            try:
                run_capture(command, cwd=root, timeout=timeout, check=False)
            except HarnessError:
                continue
        if _remove_container(
            pathlib.Path(probe.executable), root, running.container_name
        ):
            running.cleanup_confirmed = True
        else:
            running.cleanup_failed = True
        deadline = time.monotonic() + 5
        while running.process.poll() is None and time.monotonic() < deadline:
            time.sleep(0.05)
        if running.process.poll() is None:
            try:
                os.killpg(running.process.pid, signal.SIGKILL)
            except (ProcessLookupError, PermissionError):
                pass
    else:
        try:
            os.killpg(running.process.pid, signal.SIGTERM)
        except (ProcessLookupError, PermissionError):
            pass
        deadline = time.monotonic() + 5
        while running.process.poll() is None and time.monotonic() < deadline:
            time.sleep(0.05)
        if running.process.poll() is None:
            try:
                os.killpg(running.process.pid, signal.SIGKILL)
            except (ProcessLookupError, PermissionError):
                pass
    running.forced_reason = reason


def container_outcome(
    probe: EngineProbe, root: pathlib.Path, name: str
) -> tuple[bool, int | None, bool, bool, str]:
    if not probe.executable:
        return False, None, False, False, "container engine is absent"
    oom = False
    exit_code: int | None = None
    valid = False
    reason = "container state could not be inspected"
    try:
        result = run_capture(
            [probe.executable, "inspect", name], cwd=root, timeout=20, check=False
        )
        if result.returncode == 0:
            try:
                records = json.loads(
                    result.stdout, object_pairs_hook=reject_duplicate_keys
                )
                if (
                    not isinstance(records, list)
                    or len(records) != 1
                    or not isinstance(records[0], dict)
                ):
                    raise ValueError("inspect did not return exactly one object")
                record = records[0]
                state = record.get("State", {})
                if not isinstance(state, dict):
                    raise ValueError("inspect State is not an object")
                running = state.get("Running")
                status = state.get("Status")
                raw_exit = state.get("ExitCode")
                if (
                    not isinstance(running, bool)
                    or not isinstance(status, str)
                    or not isinstance(raw_exit, int)
                ):
                    raise ValueError(
                        "inspect lacks Boolean Running/string Status/integer ExitCode"
                    )
                oom = bool(
                    state.get("OOMKilled")
                    or state.get("OomKilled")
                    or state.get("oomKilled")
                )
                exit_code = raw_exit
                valid = not running and status == "exited"
                reason = (
                    "container reported a completed state"
                    if valid
                    else (
                        "container did not reach the required exited state "
                        f"(Running={running}, Status={status!r})"
                    )
                )
            except (
                json.JSONDecodeError,
                HarnessError,
                AttributeError,
                TypeError,
                ValueError,
            ) as exc:
                reason = f"container inspect state is malformed: {exc}"
        else:
            reason = f"container inspect exited with status {result.returncode}"
    except HarnessError as exc:
        reason = f"container inspect failed: {exc}"
    finally:
        removed = _remove_container(pathlib.Path(probe.executable), root, name)
    return oom, exit_code, removed, valid, reason


def reap_stale_workers(
    root: pathlib.Path,
    probe: EngineProbe,
    *,
    require_hard: bool = True,
) -> None:
    if (require_hard and not probe.hard) or not probe.executable:
        return
    result = run_capture(
        [
            probe.executable,
            "ps",
            "-a",
            "--filter",
            "label=org.mlr-org.paradox.verify=worker",
            "--filter",
            f"label=org.mlr-org.paradox.verify.uid={os.getuid()}",
            "--format",
            "{{.Names}}",
        ],
        cwd=root,
        timeout=30,
    )
    for name in result.stdout.splitlines():
        name = name.strip()
        if not re.fullmatch(
            r"paradox-(?:v|verify-(?:oom-)?probe)-[A-Za-z0-9_.-]+", name
        ):
            raise FatalRunError(
                f"engine returned unsafe labelled worker name: {name!r}"
            )
        if not _remove_container(pathlib.Path(probe.executable), root, name):
            raise FatalRunError(
                f"engine did not confirm removal of stale contained worker {name}"
            )
        check = run_capture(
            [probe.executable, "inspect", name],
            cwd=root,
            timeout=20,
            check=False,
        )
        if check.returncode == 0:
            raise FatalRunError(f"could not remove stale contained worker {name}")


def publish_cache(
    root: pathlib.Path,
    item: PlannedTask,
    result: Mapping[str, Any],
    log_path: pathlib.Path,
) -> pathlib.Path:
    parent = cache_entry(root, item.task.task_id, item.cache_key).parent
    ensure_managed_directory(root, parent)
    destination = parent / item.cache_key
    if destination.exists():
        validate_cache_entry(destination, item.task.task_id, item.cache_key)
        return destination
    temporary = pathlib.Path(
        tempfile.mkdtemp(prefix=f".{item.cache_key}.new.", dir=parent)
    )
    try:
        shutil.copyfile(log_path, temporary / "task.log")
        receipt = {
            "schema": CACHE_SCHEMA,
            "task_id": item.task.task_id,
            "cache_key": item.cache_key,
            "status": "passed",
            "input_digest": item.input_digest,
            "log_sha256": sha256_file(temporary / "task.log"),
            "original_run_id": result["run_id"],
            "original_result": result,
            "published_at": utc_now(),
        }
        atomic_write(temporary / "receipt.json", canonical_json(receipt))
        try:
            os.rename(temporary, destination)
        except FileExistsError:
            validate_cache_entry(destination, item.task.task_id, item.cache_key)
        return destination
    finally:
        if temporary.exists():
            shutil.rmtree(temporary)


def append_history(root: pathlib.Path, result: Mapping[str, Any]) -> None:
    verify_root = root / ".local" / "verify"
    ensure_managed_directory(root, verify_root)
    path = verify_root / "history.jsonl"
    if path.exists():
        require_plain_file(path, "verification history")
    payload = canonical_json(
        {
            "recorded_at": utc_now(),
            "run_id": result["run_id"],
            "task_id": result["task_id"],
            "cache_key": result["cache_key"],
            "status": result["status"],
            "origin": result.get("origin", "execution"),
            "failure_class": result.get("failure_class"),
            "reason": result.get("reason"),
            "elapsed_seconds": result["elapsed_seconds"],
            "peak_rss_mib": result["peak_rss_mib"],
        }
    )
    write_flags = os.O_WRONLY | os.O_CREAT | os.O_APPEND
    if hasattr(os, "O_NOFOLLOW"):
        write_flags |= os.O_NOFOLLOW
    descriptor = os.open(path, write_flags, 0o600)
    try:
        if not stat.S_ISREG(os.fstat(descriptor).st_mode):
            raise FatalRunError("verification history is not a regular file")
        fcntl.flock(descriptor, fcntl.LOCK_EX)
        os.write(descriptor, payload)
        os.fsync(descriptor)
    finally:
        try:
            fcntl.flock(descriptor, fcntl.LOCK_UN)
        except OSError:
            pass
        os.close(descriptor)


def persist_task_result(
    result_path: pathlib.Path,
    result: Mapping[str, Any],
    *,
    retain_attempt: bool,
) -> None:
    """Retain an immutable execution receipt before updating the latest view."""

    if retain_attempt:
        attempt = result.get("attempt")
        if not isinstance(attempt, int) or isinstance(attempt, bool) or attempt <= 0:
            raise FatalRunError("execution result lacks a positive attempt number")
        attempt_path = result_path.with_name(f"attempt-{attempt:03d}.result.json")
        if attempt_path.exists() or attempt_path.is_symlink():
            raise FatalRunError(f"execution result already exists: {attempt_path}")
        atomic_write(attempt_path, canonical_json(dict(result)))
    atomic_write(result_path, canonical_json(dict(result)))


def write_task_result(
    running: RunningTask,
    *,
    run_id: str,
    status: str,
    reason: str,
    return_code: int | None,
    backend: str,
    origin: str,
    invocation_id: str,
    invocation_sha256: str,
) -> dict[str, Any]:
    ended = time.monotonic()
    result = {
        "schema": RESULT_SCHEMA,
        "run_id": run_id,
        "task_id": running.planned.task.task_id,
        "cache_key": running.planned.cache_key,
        "status": status,
        "origin": origin,
        "reason": reason,
        "failure_class": running.planned.task.failure_class,
        "return_code": return_code,
        "attempt": running.attempt,
        "invocation_id": invocation_id,
        "invocation_sha256": invocation_sha256,
        "child_run_id": attempt_run_id(
            run_id, running.planned.task.task_id, running.attempt
        ),
        "backend": backend,
        "resources": dataclasses.asdict(
            running.allocation
            or Allocation(
                cpu=running.planned.task.resources.cpu,
                memory_mib=running.planned.task.resources.memory_mib,
                pids=running.planned.task.resources.pids,
                scratch_mib=running.planned.task.resources.scratch_mib,
            )
        ),
        "started_at": running.started_at,
        "finished_at": utc_now(),
        "elapsed_seconds": round(ended - running.started_monotonic, 6),
        "peak_rss_mib": running.peak_rss_mib,
        "log": str(running.log_path),
        "log_sha256": sha256_file(running.log_path),
    }
    persist_task_result(running.result_path, result, retain_attempt=True)
    return result


def can_fit(
    item: PlannedTask,
    active: Iterable[RunningTask],
    host: HostResources,
    best_effort: bool,
) -> bool:
    active_list = list(active)
    if best_effort and active_list:
        return False
    allocations = [
        value.allocation
        or Allocation(
            cpu=value.planned.task.resources.cpu,
            memory_mib=value.planned.task.resources.memory_mib,
            pids=value.planned.task.resources.pids,
            scratch_mib=value.planned.task.resources.scratch_mib,
        )
        for value in active_list
    ]
    used_cpu = sum(value.cpu for value in allocations)
    used_memory = sum(value.memory_mib for value in allocations)
    used_pids = sum(value.pids for value in allocations)
    used_scratch = sum(value.scratch_mib for value in allocations)
    resources = item.task.resources
    return (
        used_cpu + resources.minimum_cpu <= host.cpu_budget + 1e-9
        and used_memory + resources.minimum_memory_mib <= host.memory_budget_mib
        and used_pids + resources.pids <= host.pid_budget
        and used_scratch + resources.scratch_mib <= host.disk_budget_mib
    )


def allocate_ready_tasks(
    ready: Sequence[PlannedTask],
    active: Iterable[RunningTask],
    host: HostResources,
    best_effort: bool,
) -> list[tuple[PlannedTask, Allocation]]:
    """Admit a high-information batch at minima, then distribute spare capacity."""

    active_allocations = [
        value.allocation
        or Allocation(
            cpu=value.planned.task.resources.cpu,
            memory_mib=value.planned.task.resources.memory_mib,
            pids=value.planned.task.resources.pids,
            scratch_mib=value.planned.task.resources.scratch_mib,
        )
        for value in active
    ]
    available_cpu = host.cpu_budget - sum(value.cpu for value in active_allocations)
    available_memory = host.memory_budget_mib - sum(
        value.memory_mib for value in active_allocations
    )
    available_pids = host.pid_budget - sum(value.pids for value in active_allocations)
    available_scratch = host.disk_budget_mib - sum(
        value.scratch_mib for value in active_allocations
    )
    selected: list[tuple[PlannedTask, Allocation]] = []
    for item in ready:
        resources = item.task.resources
        minimum = Allocation(
            cpu=resources.minimum_cpu,
            memory_mib=resources.minimum_memory_mib,
            pids=resources.pids,
            scratch_mib=resources.scratch_mib,
        )
        if (
            minimum.cpu <= available_cpu + 1e-9
            and minimum.memory_mib <= available_memory
            and minimum.pids <= available_pids
            and minimum.scratch_mib <= available_scratch
        ):
            selected.append((item, minimum))
            available_cpu -= minimum.cpu
            available_memory -= minimum.memory_mib
            available_pids -= minimum.pids
            available_scratch -= minimum.scratch_mib
            if best_effort:
                break

    # Every selected peer keeps its minimum. Higher-priority peers receive
    # spare capacity first, up to their reviewed ceiling.
    allocated: list[tuple[PlannedTask, Allocation]] = []
    for item, minimum in selected:
        resources = item.task.resources
        extra_cpu = min(resources.cpu - minimum.cpu, max(0.0, available_cpu))
        extra_memory = min(
            resources.memory_mib - minimum.memory_mib,
            max(0, available_memory),
        )
        allocation = dataclasses.replace(
            minimum,
            cpu=minimum.cpu + extra_cpu,
            memory_mib=minimum.memory_mib + extra_memory,
        )
        available_cpu -= extra_cpu
        available_memory -= extra_memory
        allocated.append((item, allocation))
    return allocated


def include_active_reservations(
    host: HostResources,
    active: Iterable[RunningTask],
) -> HostResources:
    """Convert live remaining headroom into the scheduler's total budget."""

    allocations = [
        value.allocation
        or Allocation(
            cpu=value.planned.task.resources.cpu,
            memory_mib=value.planned.task.resources.memory_mib,
            pids=value.planned.task.resources.pids,
            scratch_mib=value.planned.task.resources.scratch_mib,
        )
        for value in active
    ]
    memory_capacity = (
        host.memory_capacity_mib
        if host.memory_capacity_mib is not None
        else host.memory_budget_mib
    )
    disk_capacity = (
        host.disk_capacity_mib
        if host.disk_capacity_mib is not None
        else host.disk_budget_mib
    )
    pid_capacity = (
        host.pid_capacity if host.pid_capacity is not None else host.pid_budget
    )
    return dataclasses.replace(
        host,
        memory_budget_mib=min(
            memory_capacity,
            host.memory_budget_mib
            + sum(value.memory_mib for value in allocations),
        ),
        disk_budget_mib=min(
            disk_capacity,
            host.disk_budget_mib
            + sum(value.scratch_mib for value in allocations),
        ),
        pid_budget=min(
            pid_capacity,
            host.pid_budget + sum(value.pids for value in allocations),
        ),
    )


def live_host_budget(
    root: pathlib.Path,
    host: HostResources,
    aggregate: AggregateProbe | None = None,
) -> HostResources:
    """Refresh admission budgets within immutable machine-capacity ceilings."""

    aggregate_hard = aggregate is not None and aggregate.hard
    aggregate_available: int | None = None
    if aggregate_hard:
        current_memory, raw_source = memory_available_mib()
        source = f"{raw_source}+systemd_aggregate"
        current_kernel = _aggregate_kernel_identity(
            proc_root=pathlib.Path(str(aggregate.identity["proc_root"])),
            cgroup_root=pathlib.Path(str(aggregate.identity["cgroup_root"])),
        )
        aggregate_available = (
            int(current_kernel["memory_headroom_bytes"]) // (1024 * 1024)
        )
    else:
        current_memory, source = live_memory_available_mib(root)
    current_disk = shutil.disk_usage(root).free // (1024 * 1024)
    pid_available, pid_source = cgroup_pid_available()
    memory_ceiling = (
        host.memory_capacity_mib
        if host.memory_capacity_mib is not None
        else host.memory_budget_mib
    )
    disk_ceiling = (
        host.disk_capacity_mib
        if host.disk_capacity_mib is not None
        else host.disk_budget_mib
    )
    pid_ceiling = (
        host.pid_capacity if host.pid_capacity is not None else host.pid_budget
    )
    engine_overhead = (
        host.engine_overhead_mib
        if host.memory_capacity_mib is not None
        else max(
            0,
            host.memory_available_mib
            - host.memory_reserve_mib
            - host.memory_budget_mib,
        )
    )
    memory_reserve = host.memory_reserve_mib
    if (
        host.memory_reserve_min_mib is not None
        and host.memory_reserve_fraction is not None
    ):
        memory_reserve = max(
            host.memory_reserve_min_mib,
            int(current_memory * host.memory_reserve_fraction),
        )
    if aggregate_hard:
        assert aggregate_available is not None
        current_memory_budget = max(
            0,
            min(
                max(0, current_memory - memory_reserve),
                aggregate_available,
            )
            - engine_overhead,
        )
    else:
        current_memory_budget = max(
            0, current_memory - memory_reserve - engine_overhead
        )
    return dataclasses.replace(
        host,
        memory_available_mib=current_memory,
        disk_available_mib=current_disk,
        memory_reserve_mib=memory_reserve,
        memory_budget_mib=min(memory_ceiling, current_memory_budget),
        disk_budget_mib=min(
            disk_ceiling,
            max(0, current_disk - host.disk_reserve_mib),
        ),
        memory_source=source,
        aggregate_memory_available_mib=aggregate_available,
        pid_budget=min(
            pid_ceiling,
            max(0, pid_available - 128)
            if pid_available is not None
            else pid_ceiling,
        ),
        pid_source=pid_source,
    )


def validate_task_fits(item: PlannedTask, host: HostResources) -> None:
    resources = item.task.resources
    memory_capacity = (
        host.memory_capacity_mib
        if host.memory_capacity_mib is not None
        else host.memory_budget_mib
    )
    disk_capacity = (
        host.disk_capacity_mib
        if host.disk_capacity_mib is not None
        else host.disk_budget_mib
    )
    pid_capacity = (
        host.pid_capacity if host.pid_capacity is not None else host.pid_budget
    )
    failures = []
    incompatibility = task_platform_incompatibility(item.task)
    if incompatibility:
        failures.append(incompatibility)
    if resources.minimum_cpu > host.cpu_budget:
        failures.append(
            f"minimum cpu {resources.minimum_cpu} > safe budget {host.cpu_budget:.2f}"
        )
    if resources.minimum_memory_mib > memory_capacity:
        failures.append(
            f"minimum memory {resources.minimum_memory_mib} MiB > static safe "
            f"capacity {memory_capacity} MiB"
        )
    if resources.pids > pid_capacity:
        failures.append(f"pids {resources.pids} > static safe capacity {pid_capacity}")
    if resources.scratch_mib > disk_capacity:
        failures.append(
            f"scratch {resources.scratch_mib} MiB > static safe capacity "
            f"{disk_capacity} MiB"
        )
    if failures:
        raise FatalRunError(
            f"task {item.task.task_id} cannot fit safely: {'; '.join(failures)}"
        )


def descendants(planned: Mapping[str, PlannedTask], roots: set[str]) -> set[str]:
    children: dict[str, set[str]] = defaultdict(set)
    for task_id, item in planned.items():
        for dependency in item.task.dependencies:
            if dependency in planned:
                children[dependency].add(task_id)
    result: set[str] = set()
    queue = deque(roots)
    while queue:
        current = queue.popleft()
        for child in children[current]:
            if child not in result:
                result.add(child)
                queue.append(child)
    return result


def next_attempt(task_root: pathlib.Path) -> int:
    maximum = 0
    for path in task_root.glob("attempt-*.log"):
        match = re.fullmatch(r"attempt-([0-9]{3})\.log", path.name)
        if match:
            maximum = max(maximum, int(match.group(1)))
    return maximum + 1


def resume_revalidation_closure(
    planned: Mapping[str, PlannedTask],
) -> set[str]:
    roots = {
        task_id
        for task_id, item in planned.items()
        if item.task.revalidate_on_resume
    }
    return roots | descendants(planned, roots)


def load_resume_successes(
    run_root: pathlib.Path, planned: Mapping[str, PlannedTask]
) -> dict[str, dict[str, Any]]:
    successes: dict[str, dict[str, Any]] = {}
    revalidate = resume_revalidation_closure(planned)
    for task_id, item in planned.items():
        if task_id in revalidate:
            continue
        result_path = run_root / "tasks" / task_id / "result.json"
        if not result_path.exists():
            continue
        require_plain_file(result_path, "resumed task result")
        try:
            result = json.loads(
                result_path.read_text(encoding="utf-8"),
                object_pairs_hook=reject_duplicate_keys,
            )
        except (OSError, UnicodeError, json.JSONDecodeError, HarnessError) as exc:
            raise FatalRunError(f"resumed result is malformed: {result_path}") from exc
        if (
            not isinstance(result, dict)
            or result.get("schema") != RESULT_SCHEMA
            or result.get("run_id") != run_root.name
            or result.get("task_id") != task_id
            or result.get("cache_key") != item.cache_key
        ):
            raise FatalRunError(f"resumed result identity does not match: {result_path}")
        if result.get("status") != "passed":
            continue
        if (
            result.get("origin") != "execution"
            or result.get("return_code") != 0
            or isinstance(result.get("return_code"), bool)
            or result.get("failure_class") != item.task.failure_class
        ):
            raise FatalRunError(
                f"resumed success has an invalid execution contract: {result_path}"
            )
        attempt = result.get("attempt")
        if (
            not isinstance(attempt, int)
            or isinstance(attempt, bool)
            or not 1 <= attempt <= 999
        ):
            raise FatalRunError(
                f"resumed success has an invalid attempt number: {result_path}"
            )
        expected_child_run_id = attempt_run_id(run_root.name, task_id, attempt)
        resources = result.get("resources")
        cpu = resources.get("cpu") if isinstance(resources, dict) else None
        memory = resources.get("memory_mib") if isinstance(resources, dict) else None
        pids = resources.get("pids") if isinstance(resources, dict) else None
        scratch = resources.get("scratch_mib") if isinstance(resources, dict) else None
        elapsed = result.get("elapsed_seconds")
        peak_rss = result.get("peak_rss_mib")
        if (
            result.get("child_run_id") != expected_child_run_id
            or not isinstance(result.get("backend"), str)
            or result.get("backend")
            not in {
                "best-effort-host",
                "podman-hard",
                "docker-hard",
                "podman-aggregate-hard",
            }
            or not isinstance(result.get("reason"), str)
            or not result.get("reason")
            or not isinstance(resources, dict)
            or set(resources) != {"cpu", "memory_mib", "pids", "scratch_mib"}
            or isinstance(cpu, bool)
            or not isinstance(cpu, (int, float))
            or not math.isfinite(cpu)
            or not item.task.resources.minimum_cpu
            <= float(cpu)
            <= item.task.resources.cpu
            or isinstance(memory, bool)
            or not isinstance(memory, int)
            or not item.task.resources.minimum_memory_mib
            <= memory
            <= item.task.resources.memory_mib
            or isinstance(pids, bool)
            or not isinstance(pids, int)
            or pids != item.task.resources.pids
            or isinstance(scratch, bool)
            or not isinstance(scratch, int)
            or scratch != item.task.resources.scratch_mib
            or isinstance(elapsed, bool)
            or not isinstance(elapsed, (int, float))
            or not math.isfinite(elapsed)
            or elapsed < 0
            or isinstance(peak_rss, bool)
            or not isinstance(peak_rss, int)
            or peak_rss < 0
            or not isinstance(result.get("started_at"), str)
            or not isinstance(result.get("finished_at"), str)
        ):
            raise FatalRunError(
                f"resumed success has a malformed execution receipt: {result_path}"
            )
        expected_task_root = run_root / "tasks" / task_id
        immutable_path = expected_task_root / f"attempt-{attempt:03d}.result.json"
        require_plain_file(immutable_path, "immutable resumed task result")
        if sha256_file(result_path) != sha256_file(immutable_path):
            raise FatalRunError(
                f"latest resumed result differs from its immutable attempt: "
                f"{result_path}"
            )
        log = pathlib.Path(str(result.get("log", "")))
        expected_log = expected_task_root / f"attempt-{attempt:03d}.log"
        if log != expected_log:
            raise FatalRunError(
                f"resumed success names an unexpected attempt log: {result_path}"
            )
        require_plain_file(log, "resumed task log")
        if result.get("log_sha256") != sha256_file(log):
            raise FatalRunError(
                f"resumed task log differs from its result: {log}"
            )
        invocation_id = result.get("invocation_id")
        invocation_sha256 = result.get("invocation_sha256")
        if (
            not isinstance(invocation_id, str)
            or not re.fullmatch(r"invocation-[0-9]{3}", invocation_id)
            or not isinstance(invocation_sha256, str)
            or not re.fullmatch(r"[0-9a-f]{64}", invocation_sha256)
        ):
            raise FatalRunError(
                f"resumed success lacks a valid invocation identity: {result_path}"
            )
        invocation_path = run_root / "invocations" / f"{invocation_id}.json"
        require_plain_file(invocation_path, "resumed invocation receipt")
        if sha256_file(invocation_path) != invocation_sha256:
            raise FatalRunError(
                f"resumed invocation receipt changed: {invocation_path}"
            )
        try:
            invocation = json.loads(
                invocation_path.read_text(encoding="utf-8"),
                object_pairs_hook=reject_duplicate_keys,
            )
        except (OSError, UnicodeError, json.JSONDecodeError, HarnessError) as exc:
            raise FatalRunError(
                f"resumed invocation receipt is malformed: {invocation_path}: {exc}"
            ) from exc
        if (
            not isinstance(invocation, dict)
            or invocation.get("schema") != 1
            or invocation.get("invocation_id") != invocation_id
            or not isinstance(invocation.get("resume"), bool)
            or not isinstance(invocation.get("pid"), int)
            or isinstance(invocation.get("pid"), bool)
            or invocation.get("pid", 0) <= 0
            or not isinstance(invocation.get("started_at"), str)
            or not isinstance(invocation.get("engine"), dict)
        ):
            raise FatalRunError(
                f"resumed invocation receipt has an invalid contract: "
                f"{invocation_path}"
            )
        successes[task_id] = result
    return successes


def run_scheduler(
    *,
    root: pathlib.Path,
    manifest: Manifest,
    profile: Profile,
    policy: str,
    run_id: str,
    run_root: pathlib.Path,
    planned: dict[str, PlannedTask],
    probe: EngineProbe,
    host: HostResources,
    best_effort: bool,
    publish_results: bool,
    resume: bool,
    source_ref: str | None = None,
    expected_commit: str | None = None,
    expected_tree: str | None = None,
    admission_timeout_seconds: int = 1800,
    invocation_id: str = "invocation-unknown",
    invocation_sha256: str = "",
) -> tuple[dict[str, dict[str, Any]], bool]:
    if source_ref and (not expected_commit or not expected_tree):
        raise FatalRunError("source-ref authentication lacks commit/tree identity")
    results: dict[str, dict[str, Any]] = {}
    resume_revalidation: set[str] = set()
    if resume:
        resume_revalidation = resume_revalidation_closure(planned)
        for task_id in resume_revalidation:
            item = planned[task_id]
            if item.status == "cached":
                item.status = "pending"
                item.reason = "resume revalidation overrides development cache"
                item.cached_receipt = None
    for task_id, item in planned.items():
        if item.status == "cached":
            authenticate_planned_inputs(root, manifest, item)
            item.cached_receipt = validate_cache_entry(
                cache_entry(root, task_id, item.cache_key),
                task_id,
                item.cache_key,
            )
            results[task_id] = {
                "schema": RESULT_SCHEMA,
                "run_id": run_id,
                "task_id": task_id,
                "cache_key": item.cache_key,
                "status": "cached",
                "origin": "cache",
                "reason": item.reason,
                "failure_class": item.task.failure_class,
                "return_code": 0,
                "attempt": 0,
                "invocation_id": invocation_id,
                "invocation_sha256": invocation_sha256,
                "backend": "cache",
                "started_at": utc_now(),
                "finished_at": utc_now(),
                "elapsed_seconds": 0.0,
                "peak_rss_mib": 0,
                "log": str(cache_entry(root, task_id, item.cache_key) / "task.log"),
                "log_sha256": item.cached_receipt["log_sha256"]
                if item.cached_receipt
                else "",
            }
    if resume:
        results.update(load_resume_successes(run_root, planned))

    pending = set(planned) - results.keys()
    # A release profile may contain independent branches with different
    # machine minima. Preserve useful work on a smaller host: an unfit leaf is
    # a retained blocked result (and blocks only its descendants), not a
    # controller-wide invariant failure. Cached/resumed successes need no
    # current execution allocation and are intentionally checked first.
    for task_id in sorted(pending):
        item = planned[task_id]
        try:
            validate_task_fits(item, host)
        except FatalRunError as exc:
            task_root = run_root / "tasks" / task_id
            ensure_managed_directory(root, task_root)
            result = {
                "schema": RESULT_SCHEMA,
                "run_id": run_id,
                "task_id": task_id,
                "cache_key": item.cache_key,
                "status": "blocked",
                "origin": "host_capacity",
                "reason": str(exc),
                "failure_class": item.task.failure_class,
                "return_code": None,
                "attempt": 0,
                "invocation_id": invocation_id,
                "invocation_sha256": invocation_sha256,
                "backend": "none",
                "started_at": utc_now(),
                "finished_at": utc_now(),
                "elapsed_seconds": 0.0,
                "peak_rss_mib": 0,
                "log": "",
                "log_sha256": "",
            }
            persist_task_result(
                task_root / "result.json", result, retain_attempt=False
            )
            append_history(root, result)
            results[task_id] = result
            pending.remove(task_id)

    running: dict[str, RunningTask] = {}
    fatal = False
    interrupted = False
    blocked_reason: dict[str, tuple[str, str]] = {}
    current_host = include_active_reservations(
        live_host_budget(root, host, probe.aggregate),
        (),
    )
    last_resource_refresh = time.monotonic()
    last_aggregate_check = 0.0
    last_aggregate_systemd_check = 0.0
    resource_wait_started: float | None = None

    old_handlers: dict[int, Any] = {}

    def request_interrupt(signum: int, _frame: Any) -> None:
        nonlocal interrupted
        interrupted = True

    for signum in (signal.SIGINT, signal.SIGTERM):
        old_handlers[signum] = signal.signal(signum, request_interrupt)

    try:
        while pending or running:
            completed_this_iteration = False
            now = time.monotonic()
            if (
                probe.aggregate is not None
                and probe.aggregate.hard
                and (
                    now - last_aggregate_check >= 1
                    or any(value.process.poll() is not None for value in running.values())
                )
            ):
                worker_completed = any(
                    value.process.poll() is not None
                    for value in running.values()
                )
                reauthenticate_systemd = (
                    now - last_aggregate_systemd_check >= 30
                    or worker_completed
                )
                violation = aggregate_violation(
                    probe.aggregate,
                    reauthenticate_systemd=reauthenticate_systemd,
                    root=root,
                )
                last_aggregate_check = now
                if reauthenticate_systemd:
                    last_aggregate_systemd_check = now
                if violation is not None:
                    if not running:
                        raise FatalRunError(violation)
                    fatal = True
                    failure_status = aggregate_violation_status(violation)
                    for value in list(running.values()):
                        if value.forced_status is None:
                            value.forced_status = failure_status
                            terminate_running(value, probe, root, violation)
            completed_failures = {
                task_id
                for task_id, result in results.items()
                if result["status"] in TERMINAL_FAILURE
                and result["status"] != "blocked"
                and planned[task_id].task.failure_class != "advisory"
            }
            for task_id in list(pending):
                failed_dependencies = [
                    dependency
                    for dependency in planned[task_id].task.dependencies
                    if dependency in results
                    and results[dependency]["status"] not in TERMINAL_SUCCESS
                ]
                if failed_dependencies:
                    blocked_reason[task_id] = (
                        "dependency",
                        "failed dependency: " + ", ".join(sorted(failed_dependencies)),
                    )
            if policy == "adaptive" and completed_failures:
                failed_phase = min(planned[value].task.phase for value in completed_failures)
                for task_id in list(pending):
                    if planned[task_id].task.phase > failed_phase:
                        blocked_reason.setdefault(
                            task_id,
                            (
                                "policy",
                                f"adaptive phase gate stopped after failure in phase {failed_phase}",
                            ),
                        )
            if policy == "fail-fast" and completed_failures:
                for task_id in list(pending):
                    blocked_reason.setdefault(
                        task_id, ("policy", "fail-fast policy")
                    )
                for value in list(running.values()):
                    value.forced_status = "cancelled"
                    terminate_running(value, probe, root, "fail-fast policy")
            if fatal or interrupted:
                for value in list(running.values()):
                    if value.forced_status is None:
                        value.forced_status = "cancelled"
                        terminate_running(
                            value,
                            probe,
                            root,
                            "global run aborted" if fatal else "run interrupted",
                        )
                for task_id in list(pending):
                    blocked_reason.setdefault(
                        task_id,
                        (
                            "controller",
                            "global run aborted" if fatal else "run interrupted",
                        ),
                    )
            for task_id, (origin, reason) in list(blocked_reason.items()):
                if task_id not in pending:
                    continue
                item = planned[task_id]
                result = {
                    "schema": RESULT_SCHEMA,
                    "run_id": run_id,
                    "task_id": task_id,
                    "cache_key": item.cache_key,
                    "status": "blocked",
                    "origin": origin,
                    "reason": reason,
                    "failure_class": item.task.failure_class,
                    "return_code": None,
                    "attempt": 0,
                    "invocation_id": invocation_id,
                    "invocation_sha256": invocation_sha256,
                    "backend": "none",
                    "started_at": utc_now(),
                    "finished_at": utc_now(),
                    "elapsed_seconds": 0.0,
                    "peak_rss_mib": 0,
                    "log": "",
                    "log_sha256": "",
                }
                task_root = run_root / "tasks" / task_id
                ensure_managed_directory(root, task_root)
                persist_task_result(
                    task_root / "result.json", result, retain_attempt=False
                )
                append_history(root, result)
                results[task_id] = result
                pending.remove(task_id)
                blocked_reason.pop(task_id, None)

            ready: list[PlannedTask] = []
            open_phases = [
                planned[task_id].task.phase for task_id in pending
            ] + [
                value.planned.task.phase for value in running.values()
            ]
            minimum_open_phase = min(open_phases) if open_phases else None
            for task_id in pending:
                item = planned[task_id]
                if (
                    minimum_open_phase is not None
                    and item.task.phase != minimum_open_phase
                ):
                    continue
                dependencies = [
                    dependency
                    for dependency in item.task.dependencies
                    if dependency in planned
                ]
                if all(
                    dependency in results
                    and results[dependency]["status"] in TERMINAL_SUCCESS
                    for dependency in dependencies
                ):
                    ready.append(item)
            ready.sort(
                key=lambda item: (
                    item.task.phase,
                    -item.score,
                    item.task.task_id,
                )
            )

            now = time.monotonic()
            # A hard worker cannot cross its own cgroup ceiling. Refresh the
            # aggregate host/cgroup budget before each new wave and every two
            # seconds while work is active; spawning a resource-discovery
            # subprocess on every 250-ms scheduler tick adds no useful safety.
            if (not running and ready) or now - last_resource_refresh >= 2:
                current_host = include_active_reservations(
                    live_host_budget(root, host, probe.aggregate),
                    running.values(),
                )
                last_resource_refresh = now
                if (
                    current_host.memory_available_mib
                    < current_host.memory_reserve_mib
                ):
                    if running:
                        fatal = True
                        for value in list(running.values()):
                            value.forced_status = "infrastructure"
                            terminate_running(
                                value,
                                probe,
                                root,
                                "host memory fell below the protected reserve",
                            )
                if current_host.disk_available_mib < host.disk_reserve_mib:
                    if running:
                        fatal = True
                        for value in list(running.values()):
                            value.forced_status = "infrastructure"
                            terminate_running(
                                value,
                                probe,
                                root,
                                "host disk fell below the protected reserve",
                            )

            allocations = (
                []
                if fatal or interrupted
                else allocate_ready_tasks(
                    ready, running.values(), current_host, best_effort
                )
            )
            launched = False
            for item, allocation in allocations:
                task_id = item.task.task_id
                task_root = run_root / "tasks" / task_id
                ensure_managed_directory(root, task_root)
                attempt = next_attempt(task_root)
                if attempt > 999:
                    raise FatalRunError(f"too many retained attempts for {task_id}")
                authenticate_planned_inputs(root, manifest, item)
                if source_ref:
                    validate_source_ref_identity(
                        root, source_ref, expected_commit or "", expected_tree or ""
                    )
                attempt_id = attempt_run_id(run_id, task_id, attempt)
                # Materialize and authenticate the declared filesystem
                # contract for both backends before opening an attempt. Hard
                # workers repeat these checks while constructing bind mounts;
                # best-effort workers still need reserved output leaves.
                for value in (*ACTIVATION_WRITABLE_PATHS, *item.writable_paths):
                    managed_mount_path(
                        root,
                        materialize_attempt(value, attempt_id),
                        writable=True,
                        label=f"task {task_id} writable path",
                    )
                for value in item.readonly_paths:
                    managed_mount_path(
                        root,
                        materialize_attempt(value, attempt_id),
                        writable=False,
                        label=f"task {task_id} read-only path",
                    )
                for environment_name in (
                    "PARADOX_CONSUMER_EXTRA_LIBS",
                    "PARADOX_DOCUMENTATION_EXTRA_LIBS",
                ):
                    extras = item.environment.get(environment_name, "")
                    for value in extras.split(os.pathsep):
                        if value:
                            managed_mount_path(
                                root,
                                value,
                                writable=False,
                                label=f"task {task_id} extra library",
                            )
                log_path = task_root / f"attempt-{attempt:03d}.log"
                result_path = task_root / "result.json"
                log = log_path.open("xb")
                environment = dict(os.environ)
                environment.update(
                    {
                        name: materialize_attempt(value, attempt_id)
                        for name, value in item.environment.items()
                    }
                )
                environment["PARADOX_VERIFY_ASSIGNED_MEMORY_MIB"] = str(
                    allocation.memory_mib
                )
                environment["PARADOX_VERIFY_TASK_ATTEMPT"] = str(attempt)
                clean_parallel_environment(environment, allocation.cpu)
                container_name: str | None = None
                if best_effort or item.task.isolation == "controller":
                    task_state = task_root / "state" / attempt_id
                    home = ensure_managed_directory(root, task_state / "home")
                    temporary = ensure_managed_directory(root, task_state / "tmp")
                    runtime = ensure_managed_directory(root, task_state / "runtime")
                    environment.update(
                        {
                            "HOME": str(home),
                            "TMPDIR": str(temporary),
                            "TMP": str(temporary),
                            "TEMP": str(temporary),
                            "XDG_RUNTIME_DIR": str(runtime),
                        }
                    )
                    task_command = [
                        materialize_attempt(value, attempt_id)
                        for value in item.command
                    ]
                    command = (
                        task_command
                        if manifest is None
                        else [
                            "sh",
                            str(
                                root
                                / "scripts"
                                / "environment"
                                / "verify-task-entry"
                            ),
                            str(root),
                            str(home),
                            str(temporary),
                            str(runtime),
                            *task_command,
                        ]
                    )
                    preexec = best_effort_preexec(allocation.memory_mib)
                    backend = "best-effort-host"
                else:
                    command, container_name = container_command(
                        root,
                        probe,
                        item,
                        run_root,
                        attempt_id,
                        attempt,
                        allocation,
                    )
                    preexec = os.setsid
                    environment = dict(os.environ)
                    backend = hard_backend_name(probe)
                header = (
                    f"task={task_id}\n"
                    f"attempt={attempt}\n"
                    f"backend={backend}\n"
                    f"started_at={utc_now()}\n"
                    f"command={json.dumps(command)}\n\n"
                ).encode()
                log.write(header)
                log.flush()
                try:
                    process = subprocess.Popen(
                        command,
                        cwd=root,
                        env=environment,
                        stdin=subprocess.DEVNULL,
                        stdout=log,
                        stderr=subprocess.STDOUT,
                        start_new_session=False,
                        preexec_fn=preexec,
                    )
                except OSError as exc:
                    log.write(f"\nlaunch failed: {exc}\n".encode())
                    log.close()
                    result = {
                        "schema": RESULT_SCHEMA,
                        "run_id": run_id,
                        "task_id": task_id,
                        "cache_key": item.cache_key,
                        "status": "infrastructure",
                        "origin": "infrastructure",
                        "reason": f"launch failed: {exc}",
                        "failure_class": item.task.failure_class,
                        "return_code": None,
                        "attempt": attempt,
                        "invocation_id": invocation_id,
                        "invocation_sha256": invocation_sha256,
                        "child_run_id": attempt_id,
                        "backend": backend,
                        "resources": dataclasses.asdict(allocation),
                        "started_at": utc_now(),
                        "finished_at": utc_now(),
                        "elapsed_seconds": 0.0,
                        "peak_rss_mib": 0,
                        "log": str(log_path),
                        "log_sha256": sha256_file(log_path),
                    }
                    persist_task_result(
                        result_path, result, retain_attempt=True
                    )
                    results[task_id] = result
                    append_history(root, result)
                    pending.remove(task_id)
                    if item.task.failure_class == "fatal":
                        fatal = True
                        for other in list(running.values()):
                            other.forced_status = "cancelled"
                            terminate_running(
                                other,
                                probe,
                                root,
                                f"fatal task {task_id} could not launch",
                            )
                    continue
                running[task_id] = RunningTask(
                    planned=item,
                    process=process,
                    log=log,
                    log_path=log_path,
                    result_path=result_path,
                    started_monotonic=time.monotonic(),
                    started_at=utc_now(),
                    attempt=attempt,
                    container_name=container_name,
                    backend=backend,
                    allocation=allocation,
                )
                pending.remove(task_id)
                launched = True
                print(
                    f"START phase={item.task.phase} task={task_id} "
                    f"cpu={allocation.cpu} "
                    f"memory={allocation.memory_mib}MiB "
                    f"log={log_path}",
                    flush=True,
                )

            for task_id, value in list(running.items()):
                if interrupted and value.forced_status is None:
                    value.forced_status = "cancelled"
                    terminate_running(value, probe, root, "run interrupted")
                elapsed = time.monotonic() - value.started_monotonic
                if best_effort:
                    rss = process_tree_rss_mib(value.process.pid)
                    value.peak_rss_mib = max(value.peak_rss_mib, rss)
                    assigned_memory = (
                        value.allocation.memory_mib
                        if value.allocation
                        else value.planned.task.resources.memory_mib
                    )
                    if rss > assigned_memory:
                        value.forced_status = "oom"
                        terminate_running(
                            value,
                            probe,
                            root,
                            "best-effort aggregate RSS watchdog crossed task ceiling",
                        )
                if (
                    value.process.poll() is None
                    and elapsed > value.planned.task.resources.timeout_seconds
                ):
                    value.forced_status = "timeout"
                    terminate_running(value, probe, root, "task deadline exceeded")
                return_code = value.process.poll()
                if return_code is None:
                    continue
                value.log.write(
                    (
                        f"\nfinished_at={utc_now()}\n"
                        f"controller_return_code={return_code}\n"
                    ).encode()
                )
                value.log.close()
                oom = False
                container_exit: int | None = None
                container_removed = value.cleanup_confirmed or not value.container_name
                container_state_valid = not value.container_name
                container_state_reason = ""
                if value.container_name and not value.cleanup_confirmed:
                    (
                        oom,
                        container_exit,
                        container_removed,
                        container_state_valid,
                        container_state_reason,
                    ) = container_outcome(probe, root, value.container_name)
                elif value.container_name and value.cleanup_confirmed:
                    # Forced termination has already supplied the authoritative
                    # status; successful completion is never inferred here.
                    container_state_valid = value.forced_status is not None
                    container_state_reason = "worker was forcibly terminated"
                if probe.aggregate is not None and probe.aggregate.hard:
                    violation = aggregate_violation(
                        probe.aggregate,
                        reauthenticate_systemd=True,
                        root=root,
                    )
                    last_aggregate_check = time.monotonic()
                    last_aggregate_systemd_check = last_aggregate_check
                    if violation is not None:
                        fatal = True
                        failure_status = aggregate_violation_status(violation)
                        if value.forced_status is None:
                            value.forced_status = failure_status
                            value.forced_reason = violation
                        for other in list(running.values()):
                            if other is not value and other.forced_status is None:
                                other.forced_status = failure_status
                                terminate_running(other, probe, root, violation)
                effective_return = (
                    container_exit if container_exit is not None else return_code
                )
                if value.cleanup_failed or not container_removed:
                    status = "infrastructure"
                    reason = (
                        "container engine did not confirm removal of the worker; "
                        "aggregate admission is no longer trustworthy"
                    )
                    fatal = True
                    for other in list(running.values()):
                        if other is not value:
                            other.forced_status = "cancelled"
                            terminate_running(
                                other,
                                probe,
                                root,
                                f"container cleanup failed for {task_id}",
                            )
                elif not container_state_valid:
                    status = "infrastructure"
                    reason = container_state_reason
                elif (
                    value.container_name
                    and value.forced_status is None
                    and container_exit != return_code
                ):
                    status = "infrastructure"
                    reason = (
                        "container client/worker exit statuses disagree "
                        f"({return_code} != {container_exit})"
                    )
                elif value.forced_status:
                    status = value.forced_status
                    reason = value.forced_reason
                elif oom:
                    status = "oom"
                    reason = "container engine classified the task OOMKilled"
                elif effective_return == 0:
                    status = "passed"
                    reason = "command completed successfully"
                else:
                    status = "failed"
                    reason = f"command exited with status {effective_return}"
                try:
                    authenticate_planned_inputs(root, manifest, value.planned)
                except FatalRunError as exc:
                    status = "infrastructure"
                    reason = str(exc)
                    fatal = True
                    for other in list(running.values()):
                        if other is not value:
                            other.forced_status = "cancelled"
                            terminate_running(
                                other,
                                probe,
                                root,
                                f"source authentication failed in {task_id}",
                            )
                result = write_task_result(
                    value,
                    run_id=run_id,
                    status=status,
                    reason=reason,
                    return_code=effective_return,
                    backend=value.backend,
                    origin=(
                        "infrastructure"
                        if status == "infrastructure"
                        else "execution"
                    ),
                    invocation_id=invocation_id,
                    invocation_sha256=invocation_sha256,
                )
                results[task_id] = result
                append_history(root, result)
                if (
                    status == "passed"
                    and publish_results
                    and value.planned.task.cache == "success"
                ):
                    publish_cache(root, value.planned, result, value.log_path)
                if (
                    status not in TERMINAL_SUCCESS
                    and value.planned.task.failure_class == "fatal"
                ):
                    fatal = True
                    for other in list(running.values()):
                        if other is not value:
                            other.forced_status = "cancelled"
                            terminate_running(
                                other,
                                probe,
                                root,
                                f"fatal task {task_id} failed",
                            )
                print(
                    f"DONE status={status} task={task_id} "
                    f"elapsed={result['elapsed_seconds']:.1f}s "
                    f"log={value.log_path}",
                    flush=True,
                )
                del running[task_id]
                completed_this_iteration = True

            if not launched and running:
                time.sleep(0.25)
            elif (
                not launched
                and not completed_this_iteration
                and pending
                and not running
            ):
                if not ready:
                    raise FatalRunError(
                        "scheduler made no progress and has no dependency-ready task"
                    )
                if resource_wait_started is None:
                    resource_wait_started = time.monotonic()
                    print(
                        "WAIT resource pressure prevents safe task admission; "
                        f"retrying for up to {admission_timeout_seconds}s",
                        flush=True,
                    )
                if (
                    time.monotonic() - resource_wait_started
                    > admission_timeout_seconds
                ):
                    raise FatalRunError(
                        "resource pressure did not clear before the admission timeout"
                    )
                time.sleep(5)
            elif launched or completed_this_iteration:
                resource_wait_started = None
    finally:
        for signum, handler in old_handlers.items():
            signal.signal(signum, handler)
        for value in list(running.values()):
            value.forced_status = "cancelled"
            terminate_running(value, probe, root, "controller shutdown")
            try:
                value.process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                value.process.kill()
            value.log.close()
    if probe.aggregate is not None and probe.aggregate.hard:
        try:
            require_stable_aggregate(
                probe.aggregate,
                phase="before finalizing the run",
                root=root,
                reauthenticate_systemd=True,
            )
        except HarnessError as exc:
            raise FatalRunError(str(exc)) from exc
    for item in planned.values():
        authenticate_planned_inputs(root, manifest, item)
        if item.status == "cached":
            validate_cache_entry(
                cache_entry(root, item.task.task_id, item.cache_key),
                item.task.task_id,
                item.cache_key,
            )
    if source_ref:
        validate_source_ref_identity(
            root, source_ref, expected_commit or "", expected_tree or ""
        )
    return results, interrupted


def write_summary(
    run_root: pathlib.Path,
    run_id: str,
    profile: Profile,
    policy: str,
    results: Mapping[str, Mapping[str, Any]],
    planned: Mapping[str, PlannedTask],
    started: float,
    interrupted: bool,
    invocation_id: str,
    invocation_sha256: str,
) -> tuple[dict[str, Any], int]:
    counts: dict[str, int] = defaultdict(int)
    for result in results.values():
        counts[str(result["status"])] += 1
    nonadvisory_failures = [
        result
        for task_id, result in results.items()
        if result["status"] not in TERMINAL_SUCCESS
        and planned[task_id].task.failure_class != "advisory"
    ]
    advisory_failures = [
        result
        for task_id, result in results.items()
        if result["status"] not in TERMINAL_SUCCESS
        and planned[task_id].task.failure_class == "advisory"
    ]
    status = (
        "interrupted"
        if interrupted
        else (
            "failed"
            if nonadvisory_failures
            else ("passed_with_advisories" if advisory_failures else "passed")
        )
    )
    summary = {
        "schema": 1,
        "run_id": run_id,
        "profile": profile.name,
        "policy": policy,
        "invocation_id": invocation_id,
        "invocation_sha256": invocation_sha256,
        "status": status,
        "started_monotonic": started,
        "finished_at": utc_now(),
        "elapsed_seconds": round(time.monotonic() - started, 6),
        "counts": dict(sorted(counts.items())),
        "tasks": [results[task_id] for task_id in sorted(results)],
    }
    atomic_write(run_root / "summary.json", canonical_json(summary))
    lines = [
        "task\tstatus\tfailure_class\telapsed_seconds\tpeak_rss_mib\tcache_key\tlog\treason"
    ]
    for task_id in sorted(results):
        result = results[task_id]
        values = [
            task_id,
            str(result["status"]),
            planned[task_id].task.failure_class,
            str(result["elapsed_seconds"]),
            str(result["peak_rss_mib"]),
            str(result["cache_key"]),
            str(result["log"] or "-"),
            str(result["reason"]).replace("\t", " ").replace("\n", " "),
        ]
        lines.append("\t".join(values))
    atomic_write(run_root / "summary.tsv", ("\n".join(lines) + "\n").encode())
    completion = {
        "schema": 1,
        "run_id": run_id,
        "status": status,
        "invocation_id": invocation_id,
        "invocation_sha256": invocation_sha256,
        "summary_json_sha256": sha256_file(run_root / "summary.json"),
        "summary_tsv_sha256": sha256_file(run_root / "summary.tsv"),
    }
    atomic_write(run_root / "completion.json", canonical_json(completion))
    print("")
    print(
        f"verification {status}: run={run_id} elapsed={summary['elapsed_seconds']:.1f}s "
        + " ".join(f"{name}={value}" for name, value in sorted(counts.items()))
    )
    print(f"summary: {run_root / 'summary.tsv'}")
    return summary, 0 if status in {"passed", "passed_with_advisories"} else 1


def acquire_file_lock(
    path: pathlib.Path, *, contention_message: str, receipt: Mapping[str, Any]
) -> BinaryIO:
    flags = os.O_RDWR | os.O_CREAT
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    try:
        descriptor = os.open(path, flags, 0o600)
    except OSError as exc:
        raise HarnessError(f"cannot open verification lock {path}: {exc}") from exc
    stream = os.fdopen(descriptor, "a+b", buffering=0)
    try:
        details = os.fstat(stream.fileno())
        if not stat.S_ISREG(details.st_mode):
            raise HarnessError("verification run lock is not a regular file")
        if details.st_uid != os.getuid():
            raise HarnessError("verification lock is not owned by the current user")
        fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError as exc:
        stream.close()
        raise HarnessError(contention_message) from exc
    except BaseException:
        stream.close()
        raise
    stream.seek(0)
    stream.truncate()
    stream.write(canonical_json(dict(receipt)))
    os.fsync(stream.fileno())
    return stream


def acquire_run_lock(run_root: pathlib.Path) -> BinaryIO:
    return acquire_file_lock(
        run_root / ".coordinator.lock",
        contention_message=(
            f"another coordinator owns verification run {run_root.name}"
        ),
        receipt={
            "schema": 1,
            "pid": os.getpid(),
            "started_at": utc_now(),
            "run": run_root.name,
        },
    )


def acquire_global_execution_lock(lock_root: pathlib.Path, run_id: str) -> BinaryIO:
    return acquire_file_lock(
        lock_root / f".paradox-verify-{os.getuid()}.execution.lock",
        contention_message=(
            "another verification coordinator for this user is already admitting "
            "work on this machine; "
            "wait for it to finish so aggregate host limits remain valid"
        ),
        receipt={
            "schema": 1,
            "pid": os.getpid(),
            "started_at": utc_now(),
            "run": run_id,
        },
    )


def record_invocation(
    root: pathlib.Path,
    run_root: pathlib.Path,
    *,
    host: HostResources,
    probe: EngineProbe,
    resume: bool,
) -> tuple[str, str]:
    """Write the current host/engine observation without changing plan identity."""

    directory = ensure_managed_directory(root, run_root / "invocations")
    maximum = 0
    for path in directory.iterdir():
        match = re.fullmatch(r"invocation-([0-9]{3})\.json", path.name)
        if not match:
            abandoned = re.fullmatch(
                r"\.invocation-[0-9]{3}\.json\.new\.[0-9]+\.[0-9]+",
                path.name,
            )
            details = path.lstat()
            if (
                abandoned
                and stat.S_ISREG(details.st_mode)
                and details.st_uid == os.getuid()
            ):
                path.unlink()
                continue
            raise FatalRunError(f"unexpected invocation receipt path: {path}")
        require_plain_file(path, "retained invocation receipt")
        maximum = max(maximum, int(match.group(1)))
    number = maximum + 1
    if number > 999:
        raise FatalRunError("too many retained coordinator invocations")
    invocation_id = f"invocation-{number:03d}"
    payload = {
        "schema": 1,
        "invocation_id": invocation_id,
        "resume": resume,
        "pid": os.getpid(),
        "started_at": utc_now(),
        "engine": engine_receipt(probe, host),
    }
    path = directory / f"{invocation_id}.json"
    atomic_write(path, canonical_json(payload))
    return invocation_id, sha256_file(path)


def parse_parameters(values: Sequence[str]) -> list[str]:
    result: list[str] = []
    seen: set[str] = set()
    for value in values:
        name = value.split("=", 1)[0]
        if name in seen:
            raise HarnessError(f"--param {name} was supplied more than once")
        seen.add(name)
        result.append(value)
    return result


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Resource-contained, change-aware Paradox verification DAG"
    )
    parser.add_argument("--root", required=True, help=argparse.SUPPRESS)
    parser.add_argument("--manifest", required=True, help=argparse.SUPPRESS)
    subparsers = parser.add_subparsers(dest="command", required=True)

    def engine_arguments(child: argparse.ArgumentParser) -> None:
        child.add_argument(
            "--engine",
            default=os.environ.get("PARADOX_CONTAINER_ENGINE", "auto"),
            help="auto, podman, docker, none, or an explicit engine path",
        )
        child.add_argument(
            "--worker-image",
            default=os.environ.get("PARADOX_VERIFY_WORKER_IMAGE"),
            help="pinned, already-local worker image used for active limit proof",
        )
        child.add_argument(
            "--containment",
            choices=sorted(ALLOWED_CONTAINMENT),
            default=os.environ.get("PARADOX_VERIFY_CONTAINMENT", "auto"),
            help=(
                "auto selects proved per-worker limits or a dedicated aggregate "
                "systemd envelope; worker/aggregate require that exact mode"
            ),
        )
        child.add_argument(
            "--best-effort",
            action="store_true",
            help=(
                "development-only serial host fallback with RLIMIT_AS/RSS watchdog; "
                "not hard containment or release evidence"
            ),
        )

    doctor = subparsers.add_parser("doctor", help="probe host and containment")
    engine_arguments(doctor)
    doctor.add_argument("--json", action="store_true", help="emit JSON receipt")

    for name in ("plan", "run"):
        child = subparsers.add_parser(name, help=f"{name} a verification profile")
        engine_arguments(child)
        child.add_argument("--profile", default="focused")
        child.add_argument("--since")
        child.add_argument("--task", action="append", default=[])
        child.add_argument("--param", action="append", default=[])
        child.add_argument(
            "--candidate-context",
            help="authenticated schema-1 candidate context JSON for downstream profiles",
        )
        child.add_argument("--run-id")
        child.add_argument("--changed-only", action="store_true")
        child.add_argument("--policy", choices=sorted(ALLOWED_POLICIES))
        child.add_argument("--no-cache", action="store_true")
        if name == "run":
            child.add_argument(
                "--resume",
                action="store_true",
                help="resume the exact existing --run-id plan",
            )

    self_test = subparsers.add_parser(
        "self-test", help="run the synthetic controller regression suite"
    )
    self_test.add_argument(
        "--internal",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    root = pathlib.Path(args.root).resolve()
    require_plain_directory(root, "repository root")
    manifest_path = pathlib.Path(args.manifest)
    if not manifest_path.is_absolute():
        manifest_path = root / manifest_path
    manifest_path = pathlib.Path(os.path.normpath(str(manifest_path)))
    if manifest_path.resolve() != manifest_path:
        raise HarnessError("verification manifest path resolves through a symlink")

    if args.command == "self-test":
        test = root / "scripts" / "environment" / "test-verify-harness"
        if not test.is_file() or test.is_symlink():
            raise HarnessError(f"synthetic harness test is absent: {test}")
        return subprocess.call([str(test)], cwd=root)

    manifest = load_manifest(root, manifest_path)
    marker_requests_aggregate = (
        os.environ.get("PARADOX_VERIFY_AGGREGATE_SYSTEMD") == "1"
    )
    # The marker only requests observation; the actual cgroup and systemd
    # properties remain authoritative. Even an explicit per-worker mode still
    # records/monitors a real outer envelope supplied by the launcher.
    aggregate_requested = (
        args.containment == "aggregate" or marker_requests_aggregate
    )
    aggregate = probe_aggregate_containment(
        root,
        manifest.defaults,
        requested=aggregate_requested,
    )
    host = discover_host_resources(
        root, manifest.defaults, aggregate if aggregate.hard else None
    )
    active_probe = args.command in {"doctor", "plan", "run"} and not args.best_effort
    probe_lock: BinaryIO | None = None
    if active_probe:
        probe_lock = acquire_global_execution_lock(
            pathlib.Path("/tmp"), f"probe-{os.getpid()}"
        )
    try:
        probe = probe_engine(
            root,
            args.engine,
            args.worker_image,
            active=active_probe,
            containment=args.containment,
            aggregate=aggregate,
        )
    finally:
        if probe_lock is not None:
            probe_lock.close()
    if aggregate.hard:
        require_stable_aggregate(
            aggregate,
            phase="during engine startup",
            root=root,
            reauthenticate_systemd=True,
        )
        refreshed_aggregate = probe_aggregate_containment(
            root, manifest.defaults, requested=True
        )
        if not refreshed_aggregate.hard:
            raise HarnessError(
                "aggregate containment changed during startup: "
                f"{refreshed_aggregate.reason}"
            )
        aggregate = refreshed_aggregate
        probe = dataclasses.replace(probe, aggregate=aggregate)
        host = discover_host_resources(root, manifest.defaults, aggregate)

    if args.command == "doctor":
        receipt = engine_receipt(probe, host)
        if args.json:
            sys.stdout.buffer.write(canonical_json(receipt))
        else:
            print_doctor(host, probe)
        if probe.hard or args.best_effort:
            return 0
        return 2

    if args.profile not in manifest.profiles:
        raise HarnessError(
            f"unknown profile {args.profile!r}; available: "
            + ", ".join(sorted(manifest.profiles))
        )
    profile = manifest.profiles[args.profile]
    run_id = args.run_id or generated_run_id(profile.name)
    if not SAFE_NAME.fullmatch(run_id):
        raise HarnessError("--run-id must be one safe name of at most 128 characters")
    if getattr(args, "resume", False) and not args.run_id:
        raise HarnessError("--resume requires an explicit --run-id")
    commit, tree, clean, dirty_rows = git_identity(root)
    if profile.require_clean and not clean:
        sample = "; ".join(dirty_rows[:5])
        raise HarnessError(
            f"profile {profile.name} requires a clean Git worktree"
            + (f": {sample}" if sample else "")
        )
    changed, changed_description = changed_files(root, args.since)
    selected = select_tasks(
        manifest, profile, changed, args.task, args.changed_only
    )
    selected_parameters = set().union(
        *(set(manifest.tasks[task_id].required_parameters) for task_id in selected)
    )
    context_roles: set[str] = set()
    if selected_parameters & {"bridge_library", "consumer_extra_libs"}:
        context_roles.add("consumer")
    if "documentation_extra_libs" in selected_parameters:
        context_roles.update({"consumer", "documentation"})
    context = (
        load_candidate_context(
            root,
            args.candidate_context,
            required_roles=frozenset(context_roles),
        )
        if args.candidate_context
        else None
    )
    parameters = resolve_parameters(
        manifest,
        profile,
        run_id,
        parse_parameters(args.param),
        root,
        commit,
        tree,
        context,
    )
    validate_required_parameters(manifest.tasks, selected, parameters)
    if "runtime-supported" in selected:
        validate_source_ref_identity(
            root, parameters["source_ref"], commit, tree
        )
    if "differential" in selected:
        parameters["baseline_ref"] = resolve_differential_baseline(
            root, parameters["baseline_ref"]
        )
    policy = args.policy or profile.policy
    reuse_cache = profile.reuse_results and not args.no_cache
    if args.best_effort:
        probe = dataclasses.replace(
            probe,
            hard=False,
            reason=(
                probe.reason
                + "; explicit best-effort host mode selected (serial, non-release)"
            ),
        )
    planned = plan_tasks(
        root,
        manifest,
        profile,
        selected,
        changed,
        parameters,
        probe,
        reuse_cache=reuse_cache,
    )
    for item in planned.values():
        if item.status == "cached":
            continue
        try:
            validate_task_fits(item, host)
        except FatalRunError as exc:
            item.status = "unsupported"
            item.reason = str(exc)
    payload = plan_payload(
        manifest,
        profile,
        policy,
        run_id,
        changed,
        changed_description,
        parameters,
        probe,
        host,
        planned,
    )
    if args.command == "plan":
        print_plan(payload)
        not_runnable = False
        unsupported = [
            item.task.task_id
            for item in planned.values()
            if item.status == "unsupported"
        ]
        if unsupported:
            print(
                "\nNOT FULLY RUNNABLE: static capacity/platform constraints "
                "block: " + ", ".join(sorted(unsupported)),
                file=sys.stderr,
            )
            not_runnable = True
        if profile.require_hard_isolation and not probe.hard and not args.best_effort:
            print(
                "\nNOT RUNNABLE: hard resource isolation was not proven: "
                + probe.reason,
                file=sys.stderr,
            )
            not_runnable = True
        return 2 if not_runnable else 0

    if profile.require_hard_isolation and not probe.hard and not args.best_effort:
        raise HarnessError(
            "hard resource isolation is required but was not proven: "
            f"{probe.reason}. Fix the container/cgroup setup, or use "
            "--best-effort only for a conservative development run."
        )
    if not args.best_effort:
        worker_tasks = [
            item for item in planned.values() if item.task.isolation == "worker"
        ]
        if worker_tasks and not probe.hard:
            raise HarnessError("selected worker tasks require proven hard isolation")
        for item in worker_tasks:
            if not item.image:
                raise HarnessError(
                    f"task {item.task.task_id} requires --worker-image or image"
                )
    verify_root = root / ".local" / "verify"
    ensure_managed_directory(root, verify_root)
    runs_root = verify_root / "runs"
    ensure_managed_directory(root, runs_root)
    run_root = runs_root / run_id
    resume = getattr(args, "resume", False)
    execution_lock = acquire_global_execution_lock(pathlib.Path("/tmp"), run_id)
    try:
        reap_stale_workers(root, probe, require_hard=False)
        if resume:
            require_plain_directory(run_root, "resumed verification run")
        else:
            if run_root.exists() or run_root.is_symlink():
                raise HarnessError(f"verification run already exists: {run_root}")
            ensure_managed_directory(root, run_root)
        coordinator_lock = acquire_run_lock(run_root)
    except BaseException:
        fcntl.flock(execution_lock.fileno(), fcntl.LOCK_UN)
        execution_lock.close()
        raise
    try:
        if resume:
            retained_plan = require_plain_file(
                run_root / "plan.json", "retained plan"
            )
            try:
                retained = json.loads(
                    retained_plan.read_text(encoding="utf-8"),
                    object_pairs_hook=reject_duplicate_keys,
                )
            except (OSError, UnicodeError, json.JSONDecodeError, HarnessError) as exc:
                raise HarnessError(f"retained plan is malformed: {exc}") from exc
            if (
                not isinstance(retained, dict)
                or retained.get("plan_fingerprint") != payload["plan_fingerprint"]
            ):
                raise HarnessError(
                    "the resumed plan differs from current inputs, parameters, "
                    "backend, or cache state; use a new run ID"
                )
            completion_path = run_root / "completion.json"
            if completion_path.exists():
                completion_path = require_plain_file(
                    completion_path, "retained completion"
                )
                try:
                    completion = json.loads(
                        completion_path.read_text(encoding="utf-8"),
                        object_pairs_hook=reject_duplicate_keys,
                    )
                except (
                    OSError,
                    UnicodeError,
                    json.JSONDecodeError,
                    HarnessError,
                ) as exc:
                    raise HarnessError(
                        f"retained completion is malformed: {exc}"
                    ) from exc
                summary_json = require_plain_file(
                    run_root / "summary.json", "retained summary JSON"
                )
                summary_tsv = require_plain_file(
                    run_root / "summary.tsv", "retained summary TSV"
                )
                if (
                    not isinstance(completion, dict)
                    or completion.get("schema") != 1
                    or completion.get("run_id") != run_id
                    or completion.get("summary_json_sha256")
                    != sha256_file(summary_json)
                    or completion.get("summary_tsv_sha256")
                    != sha256_file(summary_tsv)
                ):
                    raise HarnessError(
                        "retained completion does not authenticate its summaries"
                    )
                if completion.get("status") in {
                    "passed",
                    "passed_with_advisories",
                }:
                    print(
                        f"verification was complete: run={run_id} "
                        f"status={completion['status']}; authenticating retained "
                        "successes and revalidating opted-in tasks"
                    )
        else:
            atomic_write(run_root / "plan.json", canonical_json(payload))
            atomic_write(
                run_root / "engine.json",
                canonical_json(engine_receipt(probe, host)),
            )
        invocation_id, invocation_sha256 = record_invocation(
            root,
            run_root,
            host=host,
            probe=probe,
            resume=resume,
        )
        started = time.monotonic()
        results, interrupted = run_scheduler(
            root=root,
            manifest=manifest,
            profile=profile,
            policy=policy,
            run_id=run_id,
            run_root=run_root,
            planned=planned,
            probe=probe,
            host=host,
            best_effort=args.best_effort,
            publish_results=profile.publish_results and not args.no_cache,
            resume=resume,
            source_ref=parameters["source_ref"]
            if "runtime-supported" in selected
            else None,
            expected_commit=commit,
            expected_tree=tree,
            admission_timeout_seconds=_positive_int(
                manifest.defaults.get("admission_timeout_seconds", 1800),
                "defaults.admission_timeout_seconds",
                24 * 3600,
            ),
            invocation_id=invocation_id,
            invocation_sha256=invocation_sha256,
        )
        invocation_path = (
            run_root / "invocations" / f"{invocation_id}.json"
        )
        if sha256_file(
            require_plain_file(invocation_path, "current invocation receipt")
        ) != invocation_sha256:
            raise FatalRunError("current invocation receipt changed during execution")
        _, status = write_summary(
            run_root,
            run_id,
            profile,
            policy,
            results,
            planned,
            started,
            interrupted,
            invocation_id,
            invocation_sha256,
        )
        return status
    finally:
        fcntl.flock(coordinator_lock.fileno(), fcntl.LOCK_UN)
        coordinator_lock.close()
        fcntl.flock(execution_lock.fileno(), fcntl.LOCK_UN)
        execution_lock.close()


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except HarnessError as error:
        print(f"verify: {error}", file=sys.stderr)
        raise SystemExit(2)
