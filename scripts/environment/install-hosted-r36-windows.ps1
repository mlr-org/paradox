[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$RepositoryRoot,

    [Parameter(Mandatory = $true)]
    [string]$WorkRoot,

    [Parameter(Mandatory = $true)]
    [string]$EvidenceRoot
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"
if (Get-Variable `
        -Name PSNativeCommandUseErrorActionPreference `
        -ErrorAction SilentlyContinue) {
    # curl failures are inspected explicitly below so an authenticated
    # fallback URL remains reachable on every supported pwsh release.
    Set-Variable -Name PSNativeCommandUseErrorActionPreference -Value $false
}
$Utf8NoBom = New-Object Text.UTF8Encoding($false)

function Invoke-Native {
    param(
        [Parameter(Mandatory = $true)]
        [string]$FilePath,

        [Parameter(Mandatory = $true)]
        [string[]]$Arguments,

        [Parameter(Mandatory = $true)]
        [string]$Label
    )

    & $FilePath @Arguments
    $status = $LASTEXITCODE
    if ($status -ne 0) {
        throw "$Label exited with status $status"
    }
}

if ($env:RUNNER_OS -and $env:RUNNER_OS -ne "Windows") {
    throw "the hosted R 3.6 dependency installer is Windows-only"
}

$RepositoryRoot = (Resolve-Path -LiteralPath $RepositoryRoot).Path
$WorkRoot = [IO.Path]::GetFullPath($WorkRoot)
$EvidenceRoot = [IO.Path]::GetFullPath($EvidenceRoot)
$ExpectedLibraryRoot = Join-Path $WorkRoot "library"
if ($env:PARADOX_R36_ISOLATION_SCHEMA -cne "hosted-r36-isolation-v1" -or
    $env:PARADOX_R36_ISOLATION_PHASE -cne "closure" -or
    $env:R_KEEP_PKG_SOURCE -cne "yes" -or
    -not [String]::Equals(
        [IO.Path]::GetFullPath($env:R_LIBS_USER),
        [IO.Path]::GetFullPath($ExpectedLibraryRoot),
        [StringComparison]::OrdinalIgnoreCase
    )) {
    throw "hosted R 3.6 dependency installation is not in its isolated closure phase"
}
$LockPath = Join-Path $RepositoryRoot "environment\runtime-r-3.6.3-packages.lock"
if (-not (Test-Path -LiteralPath $LockPath -PathType Leaf)) {
    throw "missing runtime source lock: $LockPath"
}
if (Test-Path -LiteralPath $WorkRoot) {
    throw "hosted R 3.6 work root must be fresh: $WorkRoot"
}
if (Test-Path -LiteralPath $EvidenceRoot) {
    throw "hosted R 3.6 evidence root must be fresh: $EvidenceRoot"
}

$null = New-Item -ItemType Directory -Path $WorkRoot
$null = New-Item -ItemType Directory -Path $EvidenceRoot
$SourceRoot = Join-Path $WorkRoot "sources"
$LibraryRoot = Join-Path $WorkRoot "library"
$ClosureEvidence = Join-Path $EvidenceRoot "runtime-closure"
$null = New-Item -ItemType Directory -Path $SourceRoot
$null = New-Item -ItemType Directory -Path $LibraryRoot
$null = New-Item -ItemType Directory -Path $ClosureEvidence

$Expected = [ordered]@{
    "backports" = @("1.5.1", "runtime-import")
    "checkmate" = @("2.3.4", "runtime-import")
    "data.table" = @("1.18.4", "runtime-import")
    "R6" = @("2.6.1", "runtime-import")
    "cli" = @("3.6.6", "runtime-dependency")
    "digest" = @("0.6.39", "runtime-dependency")
    "mlr3misc" = @("0.22.0", "runtime-import")
}
$InstallOrder = @(
    "backports",
    "checkmate",
    "data.table",
    "R6",
    "cli",
    "digest",
    "mlr3misc"
)

$Rows = @(Import-Csv -LiteralPath $LockPath -Delimiter "`t")
if ($Rows.Count -lt $Expected.Count) {
    throw "runtime source lock has fewer rows than the selected closure"
}
$Selected = @{}
foreach ($Package in $InstallOrder) {
    $Matches = @($Rows | Where-Object { $_.Package -ceq $Package })
    if ($Matches.Count -ne 1) {
        throw "runtime source lock must contain exactly one row for $Package"
    }
    $Row = $Matches[0]
    $Version = $Expected[$Package][0]
    $Role = $Expected[$Package][1]
    if ($Row.Version -cne $Version -or $Row.Role -cne $Role) {
        throw "unexpected locked identity for $Package"
    }
    if ($Row.SHA256 -cnotmatch "^[0-9a-f]{64}$") {
        throw "invalid SHA-256 for $Package"
    }
    $ArchiveName = "${Package}_${Version}.tar.gz"
    if ($Row.URL -cnotmatch "^https://" -or
        -not $Row.URL.EndsWith("/$ArchiveName", [StringComparison]::Ordinal)) {
        throw "invalid primary source URL for $Package"
    }
    if ($Row.FallbackURL -cne "-" -and
        ($Row.FallbackURL -cnotmatch "^https://" -or
         -not $Row.FallbackURL.EndsWith(
             "/$ArchiveName",
             [StringComparison]::Ordinal
         ))) {
        throw "invalid fallback source URL for $Package"
    }
    $Selected[$Package] = $Row
}

$RExe = (Get-Command "R.exe" -ErrorAction Stop).Source
$RScriptExe = (Get-Command "Rscript.exe" -ErrorAction Stop).Source
$CurlExe = (Get-Command "curl.exe" -ErrorAction Stop).Source
$ToolIdentities = @(
    [pscustomobject]@{
        Name = "gcc"
        Command = "gcc.exe"
        ExpectedPath = "C:\Rtools\mingw_64\bin\gcc.exe"
        SHA256 = "2d415b0fd5eacb43268e2ddf080b50f706d9fa2465b1e32d04f54ce936fac3da"
        Path = $null
    },
    [pscustomobject]@{
        Name = "g++"
        Command = "g++.exe"
        ExpectedPath = "C:\Rtools\mingw_64\bin\g++.exe"
        SHA256 = "0d3d581bca702c777fc045a2fe69696e5979d86e819efe2350e2ac43f33f2b7f"
        Path = $null
    },
    [pscustomobject]@{
        Name = "objdump"
        Command = "objdump.exe"
        ExpectedPath = "C:\Rtools\mingw_64\bin\objdump.exe"
        SHA256 = "cbf5f996ef759be73502387c9d1296176f8bb7b6320b63cfb61371f7a98e7b59"
        Path = $null
    },
    [pscustomobject]@{
        Name = "make"
        Command = "make.exe"
        ExpectedPath = "C:\Rtools\bin\make.exe"
        SHA256 = "ce462e4ca812718a077ae4b67ebec0bd2df0e7a3bc1e31897e40895023e13c72"
        Path = $null
    }
)
foreach ($Tool in $ToolIdentities) {
    $Tool.Path = (
        Get-Command `
            $Tool.Command `
            -CommandType Application `
            -ErrorAction Stop
    ).Source
    if (-not [String]::Equals(
        [IO.Path]::GetFullPath($Tool.Path),
        [IO.Path]::GetFullPath($Tool.ExpectedPath),
        [StringComparison]::OrdinalIgnoreCase
    )) {
        throw "dependency installer resolved an unexpected Rtools35 $($Tool.Name)"
    }
    $ObservedToolHash = (
        Get-FileHash -LiteralPath $Tool.Path -Algorithm SHA256
    ).Hash.ToLowerInvariant()
    if ($ObservedToolHash -cne $Tool.SHA256) {
        throw "dependency installer found non-official Rtools35 $($Tool.Name) bytes"
    }
}
$GxxExe = (
    $ToolIdentities | Where-Object { $_.Name -ceq "g++" }
).Path
$GxxVersion = & $GxxExe --version
$GxxFirstLine = $GxxVersion | Select-Object -First 1
if ($LASTEXITCODE -ne 0 -or
    $GxxFirstLine -cne (
        "g++.exe (x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3"
    )) {
    throw "dependency installer did not resolve exact Rtools35 G++ 4.9.3"
}
$RetainedGxxExe = [IO.Path]::GetFullPath($GxxExe).Replace("\", "/")
if ($RetainedGxxExe -cne "C:/Rtools/mingw_64/bin/g++.exe") {
    throw "normalized Rtools35 G++ path differs from the reviewed path"
}
$ToolEvidence = @("Tool`tPath`tSHA256")
foreach ($Tool in $ToolIdentities) {
    $RetainedToolPath = [IO.Path]::GetFullPath($Tool.Path).Replace("\", "/")
    $ToolEvidence += (
        "{0}`t{1}`t{2}" -f
        $Tool.Name, $RetainedToolPath, $Tool.SHA256
    )
}
[IO.File]::WriteAllLines(
    (Join-Path $ClosureEvidence "rtools35.tsv"),
    $ToolEvidence,
    $Utf8NoBom
)
$env:R_LIBS_USER = $LibraryRoot

$Ledger = @(
    "Package`tVersion`tRole`tSHA256`tSourceURL`tArchive"
)
$DigestSourceFiles = @(
    "SpookyV2.cpp",
    "crc32c.cpp",
    "crc32c_portable.cpp",
    "spooky_serialize.cpp"
)
$HadPkgCxxStd = Test-Path -LiteralPath "Env:R_PKG_CXX_STD"
$OriginalPkgCxxStd = if ($HadPkgCxxStd) {
    $env:R_PKG_CXX_STD
} else {
    $null
}
Remove-Item -LiteralPath "Env:R_PKG_CXX_STD" -ErrorAction SilentlyContinue
try {
foreach ($Package in $InstallOrder) {
    $Row = $Selected[$Package]
    $ArchiveName = "${Package}_$($Row.Version).tar.gz"
    $ArchivePath = Join-Path $SourceRoot $ArchiveName
    $URLs = @($Row.URL)
    if ($Row.FallbackURL -cne "-") {
        $URLs += $Row.FallbackURL
    }

    $UsedURL = $null
    foreach ($URL in $URLs) {
        if (Test-Path -LiteralPath $ArchivePath) {
            Remove-Item -LiteralPath $ArchivePath -Force
        }
        & $CurlExe `
            "--fail" `
            "--location" `
            "--silent" `
            "--show-error" `
            "--retry" "3" `
            "--retry-all-errors" `
            "--output" $ArchivePath `
            $URL
        $status = $LASTEXITCODE
        if ($status -eq 0) {
            $UsedURL = $URL
            break
        }
    }
    if ($null -eq $UsedURL) {
        throw "both locked source URLs failed for $Package"
    }

    $ObservedHash = (
        Get-FileHash -LiteralPath $ArchivePath -Algorithm SHA256
    ).Hash.ToLowerInvariant()
    if ($ObservedHash -cne $Row.SHA256) {
        throw (
            "SHA-256 mismatch for {0}: expected {1}, observed {2}" -f
            $Package, $Row.SHA256, $ObservedHash
        )
    }

    $InstallArguments = @(
        "CMD",
        "INSTALL",
        "--preclean",
        "--clean",
        "--no-multiarch",
        "--library=$LibraryRoot",
        $ArchivePath
    )
    $InstallOutput = @(& $RExe @InstallArguments 2>&1)
    $InstallStatus = $LASTEXITCODE
    $InstallLines = @($InstallOutput | ForEach-Object { "$_" })
    if ($Package -ceq "digest") {
        [IO.File]::WriteAllLines(
            (Join-Path $ClosureEvidence "digest-install.log"),
            $InstallLines,
            $Utf8NoBom
        )
    }
    if ($InstallLines.Count -gt 0) {
        Write-Host ($InstallLines -join [Environment]::NewLine)
    }
    if ($InstallStatus -ne 0) {
        throw (
            "source installation of {0} {1} exited with status {2}" -f
            $Package, $Row.Version, $InstallStatus
        )
    }
    if ($Package -ceq "digest") {
        $CompileLineIndices = @()
        foreach ($SourceFile in $DigestSourceFiles) {
            $EscapedSource = [Regex]::Escape($SourceFile)
            $MatchingIndices = @(
                0..($InstallLines.Count - 1) | Where-Object {
                    $InstallLines[$_] -match (
                        "(^|[\\/\s`"']){0}(\s|[`"']|$)" -f $EscapedSource
                    )
                }
            )
            if ($MatchingIndices.Count -ne 1) {
                throw (
                    "digest install log must contain one compiler command for {0}" -f
                    $SourceFile
                )
            }
            $CompileLine = $InstallLines[$MatchingIndices[0]]
            if ($CompileLine -notmatch (
                "(^|[\\/\s`"'])g\+\+(\.exe)?(\s|[`"']|$)"
            )) {
                throw "digest compile command does not select Rtools35 g++"
            }
            $Standards = @(
                [Regex]::Matches($CompileLine, "-std=[^\s]+") |
                    ForEach-Object { $_.Value }
            )
            if ($Standards.Count -ne 1 -or
                $Standards[0] -cne "-std=gnu++11") {
                throw (
                    "digest compile command must contain only exact -std=gnu++11"
                )
            }
            $CompileLineIndices += $MatchingIndices[0]
        }
        if (@($CompileLineIndices | Select-Object -Unique).Count -ne
            $DigestSourceFiles.Count) {
            throw "digest translation units do not have distinct compiler commands"
        }
        $DigestEvidence = @(
            "Field`tValue",
            "gxx_path`t$RetainedGxxExe",
            "gxx_first_line`t$GxxFirstLine",
            "standard`t-std=gnu++11"
        )
        foreach ($SourceFile in $DigestSourceFiles) {
            $DigestEvidence += "source`t$SourceFile"
        }
        [IO.File]::WriteAllLines(
            (Join-Path $ClosureEvidence "digest-cxx11.tsv"),
            $DigestEvidence,
            $Utf8NoBom
        )
    }

    $VerifyCode = @'
args <- commandArgs(TRUE)
stopifnot(
    length(args) == 3L,
    identical(
        as.character(utils::packageVersion(args[[2L]], lib.loc = args[[1L]])),
        args[[3L]]
    )
)
'@
    Invoke-Native `
        -FilePath $RScriptExe `
        -Arguments @(
            "--vanilla",
            "-e",
            $VerifyCode,
            $LibraryRoot,
            $Package,
            $Row.Version
        ) `
        -Label "installed-version verification for $Package"

    $Ledger += (
        "{0}`t{1}`t{2}`t{3}`t{4}`t{5}" -f
        $Package,
        $Row.Version,
        $Row.Role,
        $ObservedHash,
        $UsedURL,
        $ArchiveName
    )
}
} finally {
    if ($HadPkgCxxStd) {
        $env:R_PKG_CXX_STD = $OriginalPkgCxxStd
    } else {
        Remove-Item `
            -LiteralPath "Env:R_PKG_CXX_STD" `
            -ErrorAction SilentlyContinue
    }
}

$VerifyClosureCode = @'
args <- commandArgs(TRUE)
lib <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
expected <- c(
    backports = "1.5.1",
    checkmate = "2.3.4",
    "data.table" = "1.18.4",
    R6 = "2.6.1",
    cli = "3.6.6",
    digest = "0.6.39",
    mlr3misc = "0.22.0"
)
observed <- sort(list.files(lib))
stopifnot(identical(observed, sort(names(expected))))
for (package in names(expected)) {
    stopifnot(identical(
        as.character(utils::packageVersion(package, lib.loc = lib)),
        unname(expected[[package]])
    ))
}
.libPaths(c(lib, .Library))
loadNamespace("data.table", lib.loc = lib)
dll <- getLoadedDLLs()[["data_table"]]
stopifnot(
    !is.null(dll),
    file.exists(dll[["path"]]),
    grepl("\\.dll$", dll[["path"]], ignore.case = TRUE)
)
'@
Invoke-Native `
    -FilePath $RScriptExe `
    -Arguments @(
        "--vanilla",
        "-e",
        $VerifyClosureCode,
        $LibraryRoot
    ) `
    -Label "complete runtime-closure verification"

[IO.File]::WriteAllLines(
    (Join-Path $ClosureEvidence "sources.tsv"),
    $Ledger,
    $Utf8NoBom
)
Copy-Item -LiteralPath $LockPath -Destination (
    Join-Path $ClosureEvidence "runtime-r-3.6.3-packages.lock"
)
$LockHash = (
    Get-FileHash -LiteralPath $LockPath -Algorithm SHA256
).Hash.ToLowerInvariant()
[IO.File]::WriteAllText(
    (Join-Path $ClosureEvidence "lock-sha256.txt"),
    "$LockHash`n",
    $Utf8NoBom
)
[IO.File]::WriteAllText(
    (Join-Path $ClosureEvidence "library-path.txt"),
    "$LibraryRoot`n",
    $Utf8NoBom
)

Write-Host "Installed and authenticated the exact seven-package R 3.6 closure."
