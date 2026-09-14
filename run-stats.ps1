<#
.SYNOPSIS
  WLM/WLE statistics runner -- no sbt required at run time (needs Java 11+).

.DESCRIPTION
  Runs org.scalawiki.wlx.stat.Statistics from the self-contained fat jar built
  by `sbt scalawiki-wlx/assembly`. If the jar is missing it is built once; after
  that only a JRE is needed. All arguments pass straight through to the CLI.

.EXAMPLE
  .\run-stats.ps1 --campaign wlm-ua --year 2024 --regional-stat
.EXAMPLE
  .\run-stats.ps1 -c wlm-ua --year 2023 2024 --fill-lists-rating
.EXAMPLE
  .\run-stats.ps1 --help

.NOTES
  Environment:
    JAVA_HOME   JDK to use (must be 11+). Otherwise `java` from PATH.
    SW_JAR      Explicit path to the fat jar (skips autodiscovery + build).
    JAVA_OPTS   Extra JVM options, e.g. "-Xmx6g".
#>
param(
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $Passthrough
)

# 'Continue', not 'Stop': native tools here (java -version, sbt) write to stderr
# on success, which PowerShell 5.1 would otherwise turn into a terminating error.
# Failures are checked explicitly below via $LASTEXITCODE / null results.
$ErrorActionPreference = 'Continue'

# Cyrillic in the reports prints as '?' unless both ends speak UTF-8:
#  - the JVM's stdout encoding (set via the -D flags below), and
#  - the console PowerShell uses to decode the child process output.
$OutputEncoding = [System.Text.UTF8Encoding]::new($false)
try { [Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false) } catch { }

$repo = $PSScriptRoot
$jarDir = Join-Path $repo 'scalawiki-wlx\target\scala-2.13'

function Find-Jar {
  Get-ChildItem -Path $jarDir -Filter 'scalawiki-wlx-*.jar' -ErrorAction SilentlyContinue |
    Where-Object { $_.Name -notmatch '(-javadoc|-sources|_2\.1)' } |
    Sort-Object LastWriteTime -Descending |
    Select-Object -First 1 -ExpandProperty FullName
}

# --- locate java ---------------------------------------------------------
if ($env:JAVA_HOME -and (Test-Path (Join-Path $env:JAVA_HOME 'bin\java.exe'))) {
  $java = Join-Path $env:JAVA_HOME 'bin\java.exe'
} else {
  $java = (Get-Command java -ErrorAction SilentlyContinue).Source
}
if (-not $java) { throw "run-stats: no java found (set JAVA_HOME or put java on PATH)" }

$verLine = (& $java -version 2>&1 | Select-Object -First 1 | Out-String)
if ($verLine -match 'version "(?:1\.)?(\d+)') {
  $major = [int]$Matches[1]
  if ($major -lt 11) { throw "run-stats: Java 11+ required, found major version $major ($java)" }
}

# --- locate (or build once) the fat jar --------------------------------
$jar = $env:SW_JAR
if (-not $jar) { $jar = Find-Jar }
if (-not $jar -or -not (Test-Path $jar)) {
  Write-Host "run-stats: fat jar not found -- building it with sbt (one-time)..."
  Push-Location $repo
  try {
    & sbt "scalawiki-wlx/assembly"
    if ($LASTEXITCODE -ne 0) { throw "sbt scalawiki-wlx/assembly failed" }
  } finally { Pop-Location }
  $jar = Find-Jar
}
if (-not $jar) { throw "run-stats: no fat jar in $jarDir (run: sbt scalawiki-wlx/assembly)" }

# UTF-8 stdout/stderr: file.encoding drives it on JDK <=17, the stdout/stderr
# props on JDK 18+. Setting all of them is harmless on every version.
$enc = @(
  '-Dfile.encoding=UTF-8'
  '-Dsun.stdout.encoding=UTF-8'
  '-Dsun.stderr.encoding=UTF-8'
  '-Dstdout.encoding=UTF-8'
  '-Dstderr.encoding=UTF-8'
)

# Most of the heap is strings, many of them repeated (authors, categories...);
# let G1 share their character data. JAVA_OPTS comes after, so it can override.
$mem = @('-XX:+UseStringDeduplication')

$extra = @()
if ($env:JAVA_OPTS) { $extra = $env:JAVA_OPTS -split '\s+' | Where-Object { $_ } }

& $java @enc @mem @extra -jar $jar @Passthrough
exit $LASTEXITCODE
