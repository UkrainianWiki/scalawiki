#!/usr/bin/env bash
#
# Daily WLM Ukraine statistics run, for cron on a server (wlm.org.ua).
#
# One run publishes every report that is kept up to date:
#   - Regional statistics (+ per-oblast subpages, hromada breakdown, newly pictured)
#   - <year> Special nominations statistics
#   - <year>/RecentlyTaken
#   - <year>/Number of objects pictured by uploader
#   - Most photographed objects (all years + by region)
#   - <year>/Images with bad ids and /Images with missing ids, for every year
#     (--csv-cache-resync picks up ids fixed in older years' files)
#
# Expects, next to this script (see "Running on a server" in RUNNING.md):
#   run-stats.sh         from the repo root
#   scalawiki-wlx.jar    the fat jar (sbt scalawiki-wlx/assembly), or set SW_JAR
#   secrets.env          SCALAWIKI_LOGIN=... / SCALAWIKI_PASSWORD=... (chmod 600)
#   csv-cache/           optional, copied from a machine that already has it
#
# Prints nothing on success, so cron mails only failures. Extra arguments are
# passed on to the stats CLI, e.g. `wlx-daily.sh --dry-run` for a trial run.
#
set -uo pipefail

base="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$base" || exit 1

# One run at a time: they share the CSV caches.
exec 9>"$base/wlx-daily.lock"
if ! flock -n 9; then
  echo "wlx-daily: the previous run is still going, skipped this one" >&2
  exit 1
fi

# If the server runs out of memory, let the kernel kill this run rather than
# the jury tool or the database (raising one's own score needs no root).
echo 1000 > "/proc/$$/oom_score_adj" 2>/dev/null || true

if [[ -f "$base/secrets.env" ]]; then
  set -a
  # shellcheck disable=SC1091
  . "$base/secrets.env"
  set +a
fi
# Without a login the edits would go out anonymously, under the server's IP.
if [[ -z "${SCALAWIKI_LOGIN:-}" || -z "${SCALAWIKI_PASSWORD:-}" ]] && [[ " $* " != *" --dry-run "* ]]; then
  echo "wlx-daily: no SCALAWIKI_LOGIN / SCALAWIKI_PASSWORD (in $base/secrets.env); refusing to publish" >&2
  exit 1
fi

# The Ukrainian contest uploads run through October: from October report on
# this year's contest, before it on last year's (no empty next-year pages).
year=$(date +%Y)
(( $(date +%-m) < 10 )) && year=$((year - 1))

mkdir -p "$base/logs"
log="$base/logs/daily-$(date +%F).txt"

# The CSV caches make the raw request cache unnecessary after a run or two.
find "$base/http-cache" -type f -mtime +14 -delete 2>/dev/null
find "$base/logs" -name 'daily-*.txt' -mtime +30 -delete 2>/dev/null

export SW_JAR="${SW_JAR:-$base/scalawiki-wlx.jar}"
# 1 GB heap: regional stat alone measured ~400 MB live (fits -Xmx700m); this run
# does much more. Check the "Memory:" line in the log and adjust via JAVA_OPTS.
# On an OutOfMemoryError exit (and so fail loudly) instead of limping on.
export JAVA_OPTS="${JAVA_OPTS:--Xmx1g} -XX:+ExitOnOutOfMemoryError"

nice -n 10 ionice -c3 "$base/run-stats.sh" \
  --campaign wlm-ua --start-year 2012 --year "$year" \
  --regional-stat --regional-details \
  --special-nominations \
  --authors-stat \
  --most-popular-monuments \
  --wrong-ids --missing-ids \
  --csv-cache-resync \
  --no-progress \
  "$@" > "$log" 2>&1
status=$?

if (( status != 0 )); then
  echo "wlx-daily: the stats run failed (exit $status); full output in $log"
  echo
  tail -n 60 "$log"
fi
exit "$status"
