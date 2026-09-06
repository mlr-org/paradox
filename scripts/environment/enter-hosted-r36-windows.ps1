[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$IsolationRoot,

    [Parameter(Mandatory = $true)]
    [string]$WorkRoot,

    [Parameter(Mandatory = $true)]
    [ValidateSet("toolchain", "closure", "candidate")]
    [string]$Phase,

    [switch]$Initialize
)

# This script is deliberately dot-sourced by each old-Windows execution step.
# Environment changes therefore apply to the exact R/R CMD children launched
# by that step, while every step starts from the same checked-in policy.

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"
if (Get-Variable `
        -Name PSNativeCommandUseErrorActionPreference `
        -ErrorAction SilentlyContinue) {
    # The dependency installer owns native exit-status handling so a failed
    # primary URL can reach its SHA-authenticated fallback.
    Set-Variable -Name PSNativeCommandUseErrorActionPreference -Value $false
}

$IsolationSchema = "hosted-r36-isolation-v1"
$EmptySHA256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
$Utf8NoBom = New-Object Text.UTF8Encoding($false)

function Assert-PlainDirectory {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,

        [Parameter(Mandatory = $true)]
        [string]$Label
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Container)) {
        throw "$Label is absent or is not a directory: $Path"
    }
    $Item = Get-Item -LiteralPath $Path -Force
    if (($Item.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
        throw "$Label is a reparse point: $Path"
    }
}

function Assert-PlainFile {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,

        [Parameter(Mandatory = $true)]
        [string]$Label
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        throw "$Label is absent or is not a regular file: $Path"
    }
    $Item = Get-Item -LiteralPath $Path -Force
    if (($Item.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
        throw "$Label is a reparse point: $Path"
    }
}

function Convert-ToRetainedPath {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    [IO.Path]::GetFullPath($Path).Replace("\", "/")
}

$IsolationRoot = [IO.Path]::GetFullPath($IsolationRoot)
$WorkRoot = [IO.Path]::GetFullPath($WorkRoot)
if ([String]::Equals(
        $IsolationRoot,
        $WorkRoot,
        [StringComparison]::OrdinalIgnoreCase
    )) {
    throw "hosted R 3.6 isolation and work roots must be distinct"
}

$FilesRoot = Join-Path $IsolationRoot "files"
$HomeRoot = Join-Path $IsolationRoot "home"
$TempRoot = Join-Path $IsolationRoot "tmp"
$LibrariesRoot = Join-Path $IsolationRoot "libraries"
$ReceiptsRoot = Join-Path $IsolationRoot "receipts"
$SiteLibrary = Join-Path $LibrariesRoot "site"
$ToolchainLibrary = Join-Path $LibrariesRoot "toolchain"

$EmptyFiles = [ordered]@{
    R_ENVIRON = "Renviron.site"
    R_ENVIRON_USER = "Renviron.user"
    R_PROFILE = "Rprofile.site"
    R_PROFILE_USER = "Rprofile.user"
    R_MAKEVARS_SITE = "Makevars.site"
    R_MAKEVARS_USER = "Makevars.user"
}

if ($Initialize) {
    if ($Phase -cne "toolchain") {
        throw "only the toolchain phase may initialize old-Windows isolation"
    }
    if (Test-Path -LiteralPath $IsolationRoot) {
        throw "hosted R 3.6 isolation root must be fresh: $IsolationRoot"
    }
    $null = New-Item -ItemType Directory -Path $IsolationRoot
    foreach ($Directory in @(
        $FilesRoot,
        $HomeRoot,
        $TempRoot,
        $LibrariesRoot,
        $ReceiptsRoot,
        $SiteLibrary,
        $ToolchainLibrary
    )) {
        $null = New-Item -ItemType Directory -Path $Directory
    }
    foreach ($FileName in $EmptyFiles.Values) {
        $Path = Join-Path $FilesRoot $FileName
        [IO.File]::WriteAllBytes($Path, [byte[]]@())
        [IO.File]::SetAttributes($Path, [IO.FileAttributes]::ReadOnly)
    }
} else {
    Assert-PlainDirectory $IsolationRoot "hosted R 3.6 isolation root"
    foreach ($Directory in @(
        $FilesRoot,
        $HomeRoot,
        $TempRoot,
        $LibrariesRoot,
        $ReceiptsRoot,
        $SiteLibrary,
        $ToolchainLibrary
    )) {
        Assert-PlainDirectory $Directory "hosted R 3.6 isolation directory"
    }
}

foreach ($Variable in $EmptyFiles.Keys) {
    $Path = Join-Path $FilesRoot $EmptyFiles[$Variable]
    Assert-PlainFile $Path "hosted R 3.6 empty $Variable file"
    $Item = Get-Item -LiteralPath $Path -Force
    $ObservedHash = (
        Get-FileHash -LiteralPath $Path -Algorithm SHA256
    ).Hash.ToLowerInvariant()
    if ($Item.Length -ne 0 -or
        $ObservedHash -cne $EmptySHA256 -or
        ($Item.Attributes -band [IO.FileAttributes]::ReadOnly) -eq 0) {
        throw "hosted R 3.6 empty $Variable file changed"
    }
}
if (@(Get-ChildItem -LiteralPath $SiteLibrary -Force).Count -ne 0) {
    throw "hosted R 3.6 isolated site library is not empty"
}

$PhaseHome = Join-Path $HomeRoot $Phase
$PhaseTemp = Join-Path $TempRoot $Phase
foreach ($Directory in @($PhaseHome, $PhaseTemp)) {
    if (Test-Path -LiteralPath $Directory) {
        throw "hosted R 3.6 phase directory must be fresh: $Directory"
    }
    $null = New-Item -ItemType Directory -Path $Directory
}

$Receipt = Join-Path $ReceiptsRoot "$Phase.tsv"
if (Test-Path -LiteralPath $Receipt) {
    throw "hosted R 3.6 phase receipt already exists: $Receipt"
}

# Keep this as one exact policy array. The portability fixture extracts it
# structurally and rejects additions, omissions, reordering, or duplicates.
$ResetVariables = @(
    "R_HOME",
    "R_LIBS",
    "R_LIBS_USER",
    "R_LIBS_SITE",
    "R_DEFAULT_PACKAGES",
    "R_ENVIRON",
    "R_ENVIRON_USER",
    "R_PROFILE",
    "R_PROFILE_USER",
    "R_BUILD_ENVIRON",
    "R_CHECK_ENVIRON",
    "R_INSTALL_ENVIRON",
    "R_MAKEVARS_SITE",
    "R_MAKEVARS_USER",
    "R_USER",
    "R_HISTFILE",
    "R_ARCH",
    "R_INSTALL_TAR",
    "R_PKG_CFLAGS",
    "R_PKG_CPPFLAGS",
    "R_PKG_CXXFLAGS",
    "R_PKG_CXX_STD",
    "R_PKG_FFLAGS",
    "R_PKG_FCFLAGS",
    "R_PKG_LIBS",
    "CC",
    "CPP",
    "CXX",
    "CXX11",
    "CXX14",
    "CXX17",
    "CXX20",
    "CXX23",
    "FC",
    "F77",
    "F90",
    "F95",
    "OBJC",
    "OBJCXX",
    "CC_FOR_BUILD",
    "CPP_FOR_BUILD",
    "CXX_FOR_BUILD",
    "FC_FOR_BUILD",
    "AR",
    "AS",
    "LD",
    "NM",
    "OBJCOPY",
    "OBJDUMP",
    "RANLIB",
    "READELF",
    "SIZE",
    "STRINGS",
    "STRIP",
    "WINDRES",
    "DLLTOOL",
    "BINPREF",
    "BINPREF64",
    "M_ARCH",
    "CFLAGS",
    "CPPFLAGS",
    "CXXFLAGS",
    "CXX11FLAGS",
    "CXX14FLAGS",
    "CXX17FLAGS",
    "CXX20FLAGS",
    "CXX23FLAGS",
    "FCFLAGS",
    "FFLAGS",
    "FORTRANFLAGS",
    "LDFLAGS",
    "CPATH",
    "C_INCLUDE_PATH",
    "CPLUS_INCLUDE_PATH",
    "OBJC_INCLUDE_PATH",
    "LIBRARY_PATH",
    "COMPILER_PATH",
    "GCC_EXEC_PREFIX",
    "MAKE",
    "MAKEFLAGS",
    "MFLAGS",
    "GNUMAKEFLAGS",
    "MAKEFILES",
    "CONFIG_SITE",
    "PKG_CONFIG",
    "PKG_CONFIG_PATH",
    "PKG_CONFIG_LIBDIR",
    "PKG_CONFIG_SYSROOT_DIR"
)
if (@($ResetVariables | Select-Object -Unique).Count -ne
    $ResetVariables.Count) {
    throw "hosted R 3.6 reset-variable policy contains a duplicate"
}
foreach ($Variable in $ResetVariables) {
    # Passing `$null` through PowerShell's method binder to the .NET string
    # overload can become `String.Empty`.  Starting with .NET 9 an empty
    # process value is retained rather than deleted, which leaves hostile
    # inputs such as R_HOME present on current hosted Windows runners.  The
    # environment provider has an unambiguous delete operation; the audit
    # immediately below still fails closed if any deletion does not take.
    Remove-Item `
        -LiteralPath "Env:\$Variable" `
        -ErrorAction SilentlyContinue
}

$UserLibrary = if ($Phase -ceq "toolchain") {
    $ToolchainLibrary
} else {
    Join-Path $WorkRoot "library"
}
if ($Phase -ceq "candidate") {
    Assert-PlainDirectory $UserLibrary "hosted R 3.6 dependency library"
}
$ControlledVariables = [ordered]@{
    R_ENVIRON = Join-Path $FilesRoot $EmptyFiles.R_ENVIRON
    R_ENVIRON_USER = Join-Path $FilesRoot $EmptyFiles.R_ENVIRON_USER
    R_PROFILE = Join-Path $FilesRoot $EmptyFiles.R_PROFILE
    R_PROFILE_USER = Join-Path $FilesRoot $EmptyFiles.R_PROFILE_USER
    R_BUILD_ENVIRON = Join-Path $FilesRoot $EmptyFiles.R_ENVIRON
    R_CHECK_ENVIRON = Join-Path $FilesRoot $EmptyFiles.R_ENVIRON
    R_INSTALL_ENVIRON = Join-Path $FilesRoot $EmptyFiles.R_ENVIRON
    R_MAKEVARS_SITE = Join-Path $FilesRoot $EmptyFiles.R_MAKEVARS_SITE
    R_MAKEVARS_USER = Join-Path $FilesRoot $EmptyFiles.R_MAKEVARS_USER
    R_USER = $PhaseHome
    HOME = $PhaseHome
    USERPROFILE = $PhaseHome
    TMP = $PhaseTemp
    TEMP = $PhaseTemp
    TMPDIR = $PhaseTemp
    R_HISTFILE = Join-Path $PhaseHome "Rhistory"
    R_LIBS_USER = $UserLibrary
    R_LIBS_SITE = $SiteLibrary
    R_KEEP_PKG_SOURCE = "yes"
    PARADOX_R36_ISOLATION_SCHEMA = $IsolationSchema
    PARADOX_R36_ISOLATION_ROOT = $IsolationRoot
    PARADOX_R36_ISOLATION_PHASE = $Phase
    PARADOX_R36_ISOLATION_RECEIPT = $Receipt
}
foreach ($Variable in $ControlledVariables.Keys) {
    [Environment]::SetEnvironmentVariable(
        $Variable,
        $ControlledVariables[$Variable],
        [EnvironmentVariableTarget]::Process
    )
}
foreach ($Variable in $ResetVariables) {
    if (-not $ControlledVariables.Contains($Variable) -and
        $null -ne [Environment]::GetEnvironmentVariable(
            $Variable,
            [EnvironmentVariableTarget]::Process
        )) {
        throw "hosted R 3.6 inherited build input survived reset: $Variable"
    }
}
foreach ($Variable in $ControlledVariables.Keys) {
    $Observed = [Environment]::GetEnvironmentVariable(
        $Variable,
        [EnvironmentVariableTarget]::Process
    )
    if ($Observed -cne $ControlledVariables[$Variable]) {
        throw "hosted R 3.6 controlled environment differs: $Variable"
    }
}

$Rows = New-Object Collections.Generic.List[string]
$null = $Rows.Add("Kind`tName`tValue`tSHA256")
$null = $Rows.Add("metadata`tschema`t$IsolationSchema`t-")
$null = $Rows.Add("metadata`tphase`t$Phase`t-")
$null = $Rows.Add(
    "directory`thome`t$(Convert-ToRetainedPath $PhaseHome)`t-"
)
$null = $Rows.Add(
    "directory`ttmp`t$(Convert-ToRetainedPath $PhaseTemp)`t-"
)
$null = $Rows.Add(
    "directory`tuser_library`t$(Convert-ToRetainedPath $UserLibrary)`t-"
)
$null = $Rows.Add(
    "directory`tsite_library`t$(Convert-ToRetainedPath $SiteLibrary)`t-"
)
foreach ($Variable in $EmptyFiles.Keys) {
    $Path = Join-Path $FilesRoot $EmptyFiles[$Variable]
    $null = $Rows.Add(
        "file`t$Variable`t$(Convert-ToRetainedPath $Path)`t$EmptySHA256"
    )
}
foreach ($Variable in $ControlledVariables.Keys) {
    $Value = $ControlledVariables[$Variable]
    if ($Variable -notin @(
        "R_KEEP_PKG_SOURCE",
        "PARADOX_R36_ISOLATION_SCHEMA",
        "PARADOX_R36_ISOLATION_PHASE"
    )) {
        $Value = Convert-ToRetainedPath $Value
    }
    $null = $Rows.Add("controlled`t$Variable`t$Value`t-")
}
foreach ($Variable in $ResetVariables) {
    if (-not $ControlledVariables.Contains($Variable)) {
        $null = $Rows.Add("cleared`t$Variable`tabsent`t-")
    }
}
[IO.File]::WriteAllLines($Receipt, $Rows, $Utf8NoBom)
[IO.File]::SetAttributes($Receipt, [IO.FileAttributes]::ReadOnly)
Assert-PlainFile $Receipt "hosted R 3.6 phase receipt"

Write-Host "Entered isolated hosted R 3.6 environment for phase $Phase."
