#!/usr/bin/env bash
#
# WLM/WLE statistics runner -- no sbt required at run time (needs Java 11+).
#
# Runs org.scalawiki.wlx.stat.Statistics from the self-contained fat jar built
# by `sbt scalawiki-wlx/assembly`. If the jar is missing it is built once; after
# that this script only needs a JRE. All arguments pass straight through to the
# CLI (see `./run-stats.sh --help`), e.g.:
#
#   ./run-stats.sh --campaign wlm-ua --year 2024 --regional-stat
#   ./run-stats.sh -c wlm-ua --year 2023 2024 --fill-lists-rating
#   ./run-stats.sh -c wle-ua -y 2025 --gallery --regional-gallery
#
# Environment:
#   JAVA_HOME   JDK to use (must be 11+). Otherwise `java` from PATH.
#   SW_JAR      Explicit path to the fat jar (skips autodiscovery + build).
#   JAVA_OPTS   Extra JVM options, e.g. JAVA_OPTS="-Xmx6g".
#
set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
jar_dir="$repo_dir/scalawiki-wlx/target/scala-2.13"

find_jar() {
  ls -t "$jar_dir"/scalawiki-wlx-*.jar 2>/dev/null \
    | grep -Ev -- '-javadoc|-sources|_2\.1' \
    | head -1 || true
}

# --- locate java -----------------------------------------------------------
if [[ -n "${JAVA_HOME:-}" && -x "$JAVA_HOME/bin/java" ]]; then
  java_bin="$JAVA_HOME/bin/java"
else
  java_bin="$(command -v java || true)"
fi
[[ -n "$java_bin" ]] || {
  echo "run-stats: no java found (set JAVA_HOME or put java on PATH)" >&2
  exit 1
}

ver_raw="$("$java_bin" -version 2>&1 | head -1 | sed -E 's/.*version "([0-9._]+).*/\1/')"
major="${ver_raw%%.*}"
[[ "$major" == "1" ]] && major="$(printf '%s' "$ver_raw" | cut -d. -f2)"
if [[ "$major" =~ ^[0-9]+$ && "$major" -lt 11 ]]; then
  echo "run-stats: Java 11+ required, found major version $major ($java_bin)" >&2
  exit 1
fi

# --- locate (or build once) the fat jar ----------------------------------
jar="${SW_JAR:-$(find_jar)}"
if [[ -z "$jar" || ! -f "$jar" ]]; then
  echo "run-stats: fat jar not found -- building it with sbt (one-time)..." >&2
  ( cd "$repo_dir" && sbt "scalawiki-wlx/assembly" )
  jar="$(find_jar)"
fi
[[ -n "$jar" && -f "$jar" ]] || {
  echo "run-stats: no fat jar in $jar_dir (run: sbt scalawiki-wlx/assembly)" >&2
  exit 1
}

# UTF-8 stdout/stderr so Cyrillic in the reports isn't mangled to '?'.
enc_opts=(
  -Dfile.encoding=UTF-8
  -Dsun.stdout.encoding=UTF-8
  -Dsun.stderr.encoding=UTF-8
  -Dstdout.encoding=UTF-8
  -Dstderr.encoding=UTF-8
)

# Most of the heap is strings, many of them repeated (authors, categories...);
# let G1 share their character data. JAVA_OPTS comes after, so it can override.
mem_opts=(-XX:+UseStringDeduplication)

exec "$java_bin" "${enc_opts[@]}" "${mem_opts[@]}" ${JAVA_OPTS:-} -jar "$jar" "$@"
