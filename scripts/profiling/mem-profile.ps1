<#
.SYNOPSIS
  Profile the memory and CPU use of one stats run (Windows, PowerShell 5.1+).

.DESCRIPTION
  Runs org.scalawiki.wlx.stat.Statistics from the fat jar with --dry-run (nothing is
  published) and --no-progress, under Java Flight Recorder, and writes to
  <OutDir>\mem-<Tag>\:
    stats.jfr            CPU / allocation profile (JDK Mission Control, or `jfr print`)
    gc.log               GC log: heap before/after every collection
    stringdedup.log      string deduplication statistics
    loaded-*, reports-*  class histogram (live objects; forces a full GC), heap info and
                         native memory summary, taken when the log shows
                         "START Generating reports" (data loaded) and
                         "START Saving dry-run output" (reports built)
    rss.csv              process working set / private memory every 2 s
    run.out, run.err     console output, including the final "Memory:" line
    scalawiki-run.log    this run's part of logs\scalawiki.log
    summary.txt          snapshot times, exit code, wall time
  Runs share csv-cache\, so run one at a time.

.EXAMPLE
  scripts\profiling\mem-profile.ps1
.EXAMPLE
  scripts\profiling\mem-profile.ps1 -Xmx -Xmx1g
.EXAMPLE
  scripts\profiling\mem-profile.ps1 -Jdk 'C:\Program Files\Eclipse Adoptium\jdk-21.0.0.35-hotspot' -NoDedup -Tag j21-nodedup
.EXAMPLE
  scripts\profiling\mem-profile.ps1 -StatsArgs '-c','wle-ua','-y','2025','--regional-stat'

.NOTES
  Needs the fat jar (sbt scalawiki-wlx/assembly) and a full JDK for jcmd: -Jdk, JAVA_HOME,
  or java on PATH. Exits with the stats run's exit code.
#>
param(
  [string] $Xmx = '',          # e.g. '-Xmx1g' for a heap-capped run
  [string] $Tag = '',          # results folder suffix; defaults from -Xmx, else 'default'
  [string] $Jdk = $env:JAVA_HOME,
  [switch] $NoDedup,           # leave out -XX:+UseStringDeduplication (the run-stats scripts pass it)
  [string[]] $StatsArgs = @('-c', 'wlm-ua', '-s', '2012', '-y', '2025', '--regional-details', '--regional-stat'),
  [string] $OutDir = ''        # defaults to <repo>\profiling-out
)
$ErrorActionPreference = 'Continue'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path

if (-not $Jdk) {
  $javaCmd = Get-Command java -ErrorAction SilentlyContinue
  if ($javaCmd) { $Jdk = Split-Path (Split-Path $javaCmd.Source) }
}
$jdkBin = if ($Jdk) { Join-Path $Jdk 'bin' } else { '' }
if (-not $jdkBin -or -not (Test-Path (Join-Path $jdkBin 'jcmd.exe'))) {
  throw 'mem-profile: no JDK with jcmd found (pass -Jdk or set JAVA_HOME)'
}

$jar = Get-ChildItem (Join-Path $repo 'scalawiki-wlx\target\scala-2.13') -Filter 'scalawiki-wlx-*.jar' -ErrorAction SilentlyContinue |
  Where-Object { $_.Name -notmatch '(-javadoc|-sources|_2\.1)' } |
  Sort-Object LastWriteTime -Descending | Select-Object -First 1 -ExpandProperty FullName
if (-not $jar) { throw 'mem-profile: no fat jar (run: sbt scalawiki-wlx/assembly)' }

if (-not $OutDir) { $OutDir = Join-Path $repo 'profiling-out' }
$tagName = if ($Tag) { $Tag } elseif ($Xmx) { $Xmx.TrimStart('-') } else { 'default' }
$out = Join-Path $OutDir "mem-$tagName"
New-Item -ItemType Directory -Force $out | Out-Null
Get-ChildItem $out -File -ErrorAction SilentlyContinue | Remove-Item -Force

$log = Join-Path $repo 'logs\scalawiki.log'
$baseline = if (Test-Path $log) { (Get-Content $log | Measure-Object -Line).Lines } else { 0 }

# -Xlog's file= can't take a drive-letter path (the colon), so these logs are written
# to the repo root (the working directory) and moved into $out afterwards.
$gcName = "gc-profile-$tagName.log"
$sdName = "stringdedup-profile-$tagName.log"

# Same JVM flags as run-stats.ps1, plus profiling. No bot login: a dry run only reads.
$javaArgs = @(
  '-Dfile.encoding=UTF-8', '-Dsun.stdout.encoding=UTF-8', '-Dsun.stderr.encoding=UTF-8',
  '-Dstdout.encoding=UTF-8', '-Dstderr.encoding=UTF-8'
)
if (-not $NoDedup) { $javaArgs += '-XX:+UseStringDeduplication' }
if ($Xmx) { $javaArgs += $Xmx }
$javaArgs += @(
  "-XX:StartFlightRecording=filename=$out\stats.jfr,settings=profile,dumponexit=true",
  "-Xlog:gc*:file=${gcName}:uptime,level,tags",
  "-Xlog:stringdedup*=debug:file=${sdName}:uptime,level,tags",
  '-XX:NativeMemoryTracking=summary',
  '-jar', $jar
) + $StatsArgs + @('--dry-run', '--no-progress')
# Start-Process joins arguments with spaces and doesn't quote them
$quotedArgs = $javaArgs | ForEach-Object { if ($_ -match '\s') { '"' + $_ + '"' } else { $_ } }

$start = Get-Date
$p = Start-Process -FilePath (Join-Path $jdkBin 'java.exe') -ArgumentList $quotedArgs -WorkingDirectory $repo -PassThru `
  -WindowStyle Hidden -RedirectStandardOutput "$out\run.out" -RedirectStandardError "$out\run.err"
$null = $p.Handle   # keep a handle so ExitCode is available after exit
$javaPid = $p.Id
"java pid: $javaPid, jar: $jar, jdk: $Jdk" | Out-File "$out\summary.txt"

function Snap($name) {
  $t = [math]::Round(((Get-Date) - $start).TotalSeconds)
  "$name at ${t}s" | Out-File -Append "$out\summary.txt"
  $jcmd = Join-Path $jdkBin 'jcmd.exe'
  & $jcmd $javaPid GC.heap_info             > "$out\$name-heap-before.txt" 2>&1
  & $jcmd $javaPid GC.class_histogram       > "$out\$name-histo.txt" 2>&1   # full GC: live objects only
  & $jcmd $javaPid GC.heap_info             > "$out\$name-heap-live.txt" 2>&1
  & $jcmd $javaPid VM.native_memory summary > "$out\$name-nmt.txt" 2>&1
}

$snapped = @{}
$samples = New-Object System.Collections.Generic.List[string]
$samples.Add('seconds,workingSetMB,privateMB')
while (-not $p.HasExited) {
  Start-Sleep -Seconds 2
  try {
    $jp = Get-Process -Id $javaPid -ErrorAction Stop
    $samples.Add(('{0},{1},{2}' -f [math]::Round(((Get-Date) - $start).TotalSeconds), [math]::Round($jp.WorkingSet64 / 1MB), [math]::Round($jp.PrivateMemorySize64 / 1MB)))
  } catch { }
  $new = if (Test-Path $log) { Get-Content $log | Select-Object -Skip $baseline } else { @() }
  if (-not $snapped['loaded'] -and ($new -match 'START Generating reports')) { Snap 'loaded'; $snapped['loaded'] = $true }
  if (-not $snapped['reports'] -and ($new -match 'START Saving dry-run output')) { Snap 'reports'; $snapped['reports'] = $true }
}
$samples | Out-File "$out\rss.csv"
"exit code: $($p.ExitCode), wall: $([math]::Round(((Get-Date) - $start).TotalSeconds))s" | Out-File -Append "$out\summary.txt"
foreach ($pair in @(@($gcName, 'gc.log'), @($sdName, 'stringdedup.log'))) {
  $f = Join-Path $repo $pair[0]
  if (Test-Path $f) { Move-Item -Force $f (Join-Path $out $pair[1]) }
}
if (Test-Path $log) { Get-Content $log | Select-Object -Skip $baseline | Out-File "$out\scalawiki-run.log" }
Get-Content "$out\summary.txt"
exit $p.ExitCode
