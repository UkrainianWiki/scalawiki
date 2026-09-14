<#
.SYNOPSIS
  Profile the stats run with and without -XX:+UseStringDeduplication on one or more JDKs.

.DESCRIPTION
  Calls mem-profile.ps1 twice per JDK (string dedup on, then off), one run at a time,
  with results in profiling-out\mem-<JDK folder name>-dedup and -nodedup. Compare the
  "Memory:" line in each run.err, the loaded-histo.txt class histograms, and stringdedup.log.

.EXAMPLE
  scripts\profiling\run-jdk-matrix.ps1 -Jdks 'C:\Program Files\Eclipse Adoptium\jdk-17.0.9.9-hotspot','C:\Program Files\Eclipse Adoptium\jdk-21.0.0.35-hotspot'
#>
param(
  [string[]] $Jdks = @($env:JAVA_HOME),
  [string[]] $StatsArgs    # passed through to mem-profile.ps1; its default otherwise
)
$ErrorActionPreference = 'Continue'
$profiler = Join-Path $PSScriptRoot 'mem-profile.ps1'
$common = @{}
if ($StatsArgs) { $common['StatsArgs'] = $StatsArgs }

foreach ($jdk in $Jdks) {
  $name = Split-Path $jdk.TrimEnd('\') -Leaf
  & $profiler -Jdk $jdk -Tag "$name-dedup" @common
  & $profiler -Jdk $jdk -Tag "$name-nodedup" -NoDedup @common
}
