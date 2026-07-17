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

## Frozen 2.0.0 handoff

The local custom candidate ref cannot be selected by GitHub Actions until the
same commit is available as a remote branch or tag. For 2.0.0, publish only the
exact frozen commit as the CI-only tag below, and only after an explicit push
authorization. Do not push local `main`, the later benchmark companion, every
local tag, or the `refs/paradox-release/` namespace. A tag is preferable to a
temporary branch because it is immutable by convention and does not match any
configured `push.branches` trigger. Run every block in this section from the
repository root in the same shell; later blocks reuse the bound variables.

```sh
candidate_ref=refs/paradox-release/candidate-20260716T180137Z
candidate_commit=afa56689e4037ee14a75b32811686f563f95effe
ci_tag=paradox-2.0.0-ci-afa5668

set -eu
test "$(git remote get-url origin)" = \
  "https://github.com/mlr-org/paradox.git"
test "$(git rev-parse "$candidate_ref^{commit}")" = "$candidate_commit"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
test -z "$(git ls-remote --refs --tags origin "refs/tags/$ci_tag")"

git push --porcelain origin \
  "$candidate_ref:refs/tags/$ci_tag"
test "$(git ls-remote --refs origin "refs/tags/$ci_tag" | cut -f 1)" = \
  "$candidate_commit"

gh workflow run r-cmd-check.yml \
  --repo mlr-org/paradox \
  --ref "$ci_tag"
```

The workflow exists on the default branch with `workflow_dispatch`, while the
selected tag supplies the exact workflow and package bytes used by the run.
GitHub's dispatch may take a few seconds to appear. The bounded loop below
performs only read-only discovery; it does not dispatch a second run. Requiring
one numeric result rejects both an absent run and duplicate matching runs.

```sh
attempt=0
run_id=
while test -z "$run_id"; do
  run_id=$(
    gh run list \
      --repo mlr-org/paradox \
      --workflow r-cmd-check.yml \
      --event workflow_dispatch \
      --commit "$candidate_commit" \
      --limit 20 \
      --json databaseId,event,headBranch,headSha \
      --jq '.[] | select(
        .event == "workflow_dispatch" and
        .headBranch == "'"$ci_tag"'" and
        .headSha == "'"$candidate_commit"'"
      ) | .databaseId'
  )
  case "$run_id" in
    *[!0-9]*) echo "duplicate or invalid matching run IDs" >&2; exit 1 ;;
    ?*) break ;;
  esac
  attempt=$((attempt + 1))
  test "$attempt" -lt 30 || {
    echo "workflow dispatch did not appear within 60 seconds" >&2
    exit 1
  }
  sleep 2
done

watch_status=0
gh run watch "$run_id" \
  --repo mlr-org/paradox \
  --exit-status || watch_status=$?
```

Retain the authoritative metadata, complete logs, exact workflow, and their
hashes locally. This is one CI run with six matrix jobs; do not rerun Linux
release evidence merely because the remote matrix also contains Linux jobs.

```sh
evidence=".local/ci/r-cmd-check-$run_id"
mkdir -p .local/ci
test ! -e "$evidence"
mkdir "$evidence"

gh run view "$run_id" \
  --repo mlr-org/paradox \
  --json databaseId,url,event,headBranch,headSha,conclusion,jobs \
  > "$evidence/run.json"
gh run view "$run_id" \
  --repo mlr-org/paradox \
  --log > "$evidence/run.log"
git show "$candidate_commit:.github/workflows/r-cmd-check.yml" \
  > "$evidence/r-cmd-check.yml"

verify_status=0
# The single-quoted text is R, not shell.
# shellcheck disable=SC2016
.local/toolchain/bin/Rscript --vanilla -e '
  args = commandArgs(TRUE)
  x = jsonlite::read_json(args[[1L]])
  job_names = vapply(x$jobs, `[[`, "", "name")
  job_results = vapply(x$jobs, `[[`, "", "conclusion")
  stopifnot(
    identical(x$headSha, args[[2L]]),
    identical(x$headBranch, args[[3L]]),
    identical(x$event, "workflow_dispatch"),
    identical(x$conclusion, "success"),
    length(x$jobs) == 6L,
    all(job_results == "success"),
    sum(job_names == "macos-15 / arm64 (release)") == 1L,
    sum(job_names == "windows-latest / x86_64 (release)") == 1L
  )
' "$evidence/run.json" "$candidate_commit" "$ci_tag" || verify_status=$?

sha256sum "$evidence/run.json" "$evidence/run.log" \
  "$evidence/r-cmd-check.yml" > "$evidence/SHA256SUMS"

test "$watch_status" -eq 0
test "$verify_status" -eq 0
```

Before publication, `run.json` must identify the exact candidate SHA, the
`workflow_dispatch` event, an overall `success`, and exactly six successful
matrix jobs. Those jobs must include `macos-15 / arm64 (release)` and
`windows-latest / x86_64 (release)`. Record the run ID, URL, both named job
conclusions, and the three retained hashes in the release ledger. Never
force-update the CI tag.
