# Running WLM/WLE statistics without sbt

The stats engine (`org.scalawiki.wlx.stat.Statistics`) can be run from a single
self-contained fat jar. Only a **JDK/JRE 11+** is required at run time — sbt is
needed once to build the jar, and never again unless the code changes.

## 1. Build the fat jar (once, needs sbt)

```
sbt scalawiki-wlx/assembly
```

Output: `scalawiki-wlx/target/scala-2.13/scalawiki-wlx-<version>.jar` (~contains
all dependencies). Rebuild it only after changing the code.

## 2. Run it (no sbt)

Use the wrapper scripts at the repo root — they locate a JDK, find (or, if
missing, build) the jar, set UTF-8 output encoding, and pass every argument
straight through to the CLI.

**Linux / macOS / Git Bash**

```
./run-stats.sh --campaign wlm-ua --year 2024 --regional-stat
```

**Windows**

```
.\run-stats.cmd --campaign wlm-ua --year 2024 --regional-stat
```

`run-stats.cmd` is a thin shim that runs `run-stats.ps1` with
`-ExecutionPolicy Bypass`, so it works no matter the machine's script policy.
To call `run-stats.ps1` directly instead, either run
`powershell -ExecutionPolicy Bypass -File .\run-stats.ps1 ...` or, once,
`Set-ExecutionPolicy -Scope CurrentUser RemoteSigned`.

Or invoke `java` directly:

```
java \
  -Dfile.encoding=UTF-8 -Dstdout.encoding=UTF-8 -Dstderr.encoding=UTF-8 \
  -jar scalawiki-wlx/target/scala-2.13/scalawiki-wlx-*.jar \
  --campaign wlm-ua --year 2024 --regional-stat
```

No `--add-exports` / `--add-opens` flags are needed — the engine runs on any
JDK 11+ (including 17, 21) out of the box.

## Console output and logs

The console stays quiet: it shows a short progress line per stage (fetching
monument lists, fetching images, generating reports, publishing edits) with a
live bar — items done / total, speed, elapsed, ETA — plus any warnings/errors
and the final `=== Publish summary ===`.

The full detail (every API request, every page batch, every wiki edit) goes to a
rolling **`logs/scalawiki.log`** in the working directory (`logs/scalawiki.*.log.gz`
once it rolls). Nothing to configure — it's created on first run.

| flag           | effect                                                                 |
|----------------|-----------------------------------------------------------------------|
| `--verbose`    | echo the per-request INFO logging to the console (it always goes to the file) and record extra DEBUG detail in `logs/scalawiki.log` |
| `--no-progress`| turn off the live bar; progress is still written to `logs/scalawiki.log` as periodic lines. Useful when piping output. |
| `--dry-run`    | build every report but publish nothing: each page edit's text is saved to `dry-run/<host>/<title>.wiki` instead (`/`, `:` etc. in titles become `_`) and uploads are skipped. Wiki reads and the local caches work as usual. |
| `--dry-run-dir DIR` | save `--dry-run` output under `DIR` instead of `dry-run` |
| `--allow-past-year-rating` | let `--fill-lists-rating` run for a `--year` whose rating rules (`rates.<year>` in the campaign config) a later configured year already replaces. Without it such a run stops before fetching anything (exit code 2), so the lists aren't overwritten with outdated points. |

Report text and CSVs are written to **stdout**; the progress bar, the phase
lines and the `=== Publish summary ===` all go to **stderr** — so
`... > report.txt` captures only the report.

The bar is shown when a terminal is attached (even if stdout is redirected). In
CI or when both streams are redirected it degrades to periodic progress lines in
`logs/scalawiki.log`.

## Image CSV cache

By default the stats engine keeps a second-tier cache of contest images as CSV
files under a **`csv-cache/`** directory in the working directory (above the
lower-level `http-cache/` request cache):

```
csv-cache/wlm-ua-2015-images.csv   # one per past contest year
csv-cache/wlm-ua-2026-images.csv   # current year (incrementally synced)
csv-cache/wlm-ua-all-images.csv    # all-time DB's images in no per-year CSV (the all-time DB = per-year images + these), when a year range / rating run needs it
```

* **First run** builds them from the wiki queries (whose raw responses land in
  `http-cache/`).
* **Later runs** read the CSVs directly and skip the slow sequential JSON parse
  of the cached API responses.
* **The current contest year** is *incrementally synced* on every run: a cheap
  sweep of the category returns each file's id **and its latest revision id +
  timestamp**. New ids have their metadata pulled and appended; ids whose
  revision changed since caching (page edited, file reuploaded — e.g. a monument
  id corrected or an ineligible-submission category added) are re-fetched; ids no
  longer in the category are dropped.
* **Past contest years and the all-images CSV** are frozen (read verbatim) unless
  `--csv-cache-resync` is given, which runs that same new/changed/deleted sweep
  against them. Deleting a CSV still forces a full refetch.
* **Past contest years are loaded slim** to save memory: only the fields their
  reports, ratings and eligibility checks read (title, author, monument ids,
  sizes, special nominations, EXIF date — or the upload date when there is none —
  and the ineligible/interior categories). URLs, camera, other categories and
  revision ids are left out. They are loaded in full with `--csv-cache-resync`
  (which rewrites their CSVs) or `--export-images-csv`. The CSV files themselves
  always keep every column.
* The CSV carries two extra columns, `last_revid` and `last_revision_ts`. Rows
  written before those columns existed fall back to a per-row timestamp: on a
  resync a change counts only if the live revision post-dates the contest's end
  for that year, so the first resync of an old cache stays cheap.

Flags:

| flag                   | effect                                                            |
|------------------------|------------------------------------------------------------------|
| `--no-csv-cache`       | disable entirely; always fetch/parse from the wiki (`http-cache/` request cache still applies) |
| `--csv-cache-dir DIR`  | use `DIR` instead of `csv-cache`                                  |
| `--csv-cache-refresh`  | ignore existing CSV caches this run and overwrite them            |
| `--csv-cache-resync`   | re-check past years + all-images against the wiki (id+revision sweep); refetch only changed rows, drop deleted |

`--images-from-csv DIR` still works as before (strict: the per-year files must
already exist in `DIR`; `--csv-cache-resync` does not touch them).

## Monument list cache

The monument lists are cached the same way, one CSV per campaign:

```
csv-cache/wlm-ua-monuments.csv     # every parsed monument + its source page's last revision
```

* **First run** fetches and parses every list page from the wiki (the long pole
  of a cold run), then writes the CSV plus one cheap revision sweep to tag each
  page.
* **Later runs** do only the sweep — an `embeddedin` id + revision query with no
  page content — diff it against the CSV, refetch and re-parse just the list
  pages whose revision changed, drop the ones the sweep confirms are gone, and
  reuse the rest. A run where nothing changed is a few seconds.
* A short/failed sweep never wipes the cache: deletions are acted on only when
  the sweep still covers most of the cached pages; any sync error falls back to
  a full refetch.

| flag                       | effect                                                              |
|----------------------------|-------------------------------------------------------------------|
| `--no-csv-cache`           | also disables this cache (shared toggle)                            |
| `--monument-cache-refresh` | ignore the cached lists this run: refetch every list page and overwrite the CSV (`--csv-cache-refresh` does this too) |

## Script environment variables

| var         | meaning                                                        |
|-------------|---------------------------------------------------------------|
| `JAVA_HOME` | JDK to use (must be 11+); otherwise `java` from `PATH`         |
| `SW_JAR`    | explicit path to the fat jar (skips autodiscovery and build)  |
| `JAVA_OPTS` | extra JVM options, e.g. `JAVA_OPTS="-Xmx6g"`                   |

## Memory

Every run ends with a line like

```
Memory: heap peak 2860 MB, max after GC 2587 MB, limit 4060 MB (committed 3588 MB); GC: 94 collections, 6.1 s, process peak RSS 3900 MB
```

* **heap peak** — highest heap use seen (just before a garbage collection).
* **max after GC** — most memory still in use right after a collection: an upper
  bound on what the run really needs. Size `-Xmx` from this plus headroom.
* **limit / committed** — the heap cap and what the JVM actually reserved.
* **process peak RSS** — whole-process peak (heap + JVM overhead, typically
  +350–400 MB); Linux only.

Set `-Xmx` explicitly on a server (`JAVA_OPTS="-Xmx3g"`): by default the JVM
takes only ¼ of physical RAM, which on a small machine can be too little. The
run scripts add `-XX:+UseStringDeduplication`; it only takes effect with the G1
collector, which the JVM picks by default except on machines with fewer than 2
CPUs or under ~1.8 GB of RAM — add `-XX:+UseG1GC` there.

To see where memory and time go in detail, `scripts/profiling/mem-profile.ps1`
(Windows) runs a `--dry-run` under Java Flight Recorder and saves a GC log,
class histograms and process memory samples to `profiling-out/`;
`run-jdk-matrix.ps1` repeats it with and without string deduplication per JDK.
The `.jsh` scripts there (run with `jshell`) break the image CSV cache down by
field. Each script's header explains its options.

## Exit behaviour and publishing

The stats engine now **waits for every wiki edit/upload to finish and then
exits** (previously it hung after the reports, and edits still in flight when it
was killed just didn't publish). On completion it prints a `Publish summary`:

* `OK (N wiki writes)` — every report step ran and every edit landed.
* `INCOMPLETE` — lists the report steps that threw and the wiki writes that
  errored, and the process exits with a non-zero status. One failing report no
  longer aborts the rest.

Wiki writes are throttled (default 4 concurrent) so a report that fans out many
edits no longer overruns the 2-connection pool and silently drops the surplus.
Override with `JAVA_OPTS="-Dscalawiki.write.maxConcurrent=8"` if needed.

## Common invocations

```
# see all options
./run-stats.sh --help

# regional statistics table for one year
./run-stats.sh -c wlm-ua -y 2024 --regional-stat

# galleries
./run-stats.sh -c wlm-ua -y 2024 --gallery --regional-gallery

# fill the rating (бали) field in monument lists over a year range
# (note: the start-year flag is --start-year / -s, not --start; --year must be the
# year whose rates apply - an older year is refused unless --allow-past-year-rating)
./run-stats.sh -c wlm-ua --start-year 2012 --year 2026 --fill-lists-rating

# Wiki Loves Earth
./run-stats.sh -c wle-ua -y 2025 --regional-stat
```

## Cyrillic prints as `?` on Windows

Two things have to speak UTF-8: the JVM's stdout and the console.

* **JVM** — `run-stats.ps1` / `run-stats.sh` now pass `-Dfile.encoding=UTF-8`
  (plus the `stdout`/`stderr` variants for JDK 18+), and `.jvmopts` does the same
  for plain `sbt` runs. This alone removes the literal `?` characters (they come
  from the JVM's default `Cp1252` encoder dropping every non-Latin glyph).
* **Console** — the script also sets PowerShell's `[Console]::OutputEncoding` to
  UTF-8. If you run `java` by hand from `cmd.exe`, first do `chcp 65001`; in a raw
  PowerShell session, `[Console]::OutputEncoding = [Text.UTF8Encoding]::new()`.
  Use a TrueType console font (Consolas, Cascadia Mono) — the legacy raster font
  has no Cyrillic glyphs and shows boxes.

Config files (`wlm_ua.conf`, `wle_ua.conf`, KOATUU/KATOTTH data, etc.) are
bundled inside the jar. Output `*.wiki` / `*.png` files and the `csv-cache/` /
`http-cache/` directories are written to the current working directory, so run
from wherever you want the artifacts to land.
