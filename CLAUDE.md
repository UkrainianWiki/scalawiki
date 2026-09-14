# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

scalawiki is a Scala MediaWiki API client, built for batch/parallel data fetching (using generators, unlike most Java clients). It also hosts a substantial application on top: statistics, gallery generation, and list management for Wikimedia photography contests like Wiki Loves Monuments (WLM) and Wiki Loves Earth (WLE), primarily for Wikimedia Ukraine.

## Build system

SBT multi-module project, Scala 2.13 only, emits JVM 8-compatible bytecode (`-target:jvm-1.8`), requires Java 11+ to build and run (CI is JDK 11; `build.sbt`'s `initialize` asserts it).

Modules (`build.sbt`):
- `scalawiki-core` — MediaWiki API client: `MwBot` (the bot/client), DSL for building API queries (`dto/cmd`), JSON parsing (`json`), HTTP layer over Pekko HTTP (`http`), wikitext parsing (`wikitext`), page/query abstractions (`query`).
- `http-extensions` — standalone Pekko HTTP helpers (currently a cookie jar / TLD list implementation), no dependency on core.
- `scalawiki-dumps` — reading MediaWiki XML dumps.
- `scalawiki-wlx` — contest statistics engine (WLM/WLE): monument lists, image databases, rating, and report generation. Depends on `core`.
- `scalawiki-bots` — CLI bots/utilities built on `core` and `wlx` (education, finance, museum, voting, copyvio bots, Twirl-templated output). Depends on `core` and `wlx`.
- `scalawiki-sql` exists as a directory but is **not** wired into `build.sbt` — treat it as inactive/legacy, don't assume it builds.

### Common commands

```
sbt compile              # compile all modules
sbt +test                # run all tests, cross-Scala-version (CI does this: sbt -v +test)
sbt test                 # run all tests against the default Scala version
sbt scalawiki-wlx/test   # run tests for one module
sbt "scalawiki-wlx/testOnly org.scalawiki.wlx.MonumentDbSpec"   # run a single spec
sbt assembly             # build fat jars (scalawiki-wlx's assembly main class is org.scalawiki.wlx.stat.Statistics)
sbt scalawiki-wlx/assembly   # just the stats fat jar -> scalawiki-wlx/target/scala-2.13/scalawiki-wlx-<ver>.jar
```

To run the WLM/WLE stats engine **without sbt** (JDK 11+ only), build the fat jar
once and use the `run-stats.sh` / `run-stats.cmd` (Windows) / `run-stats.ps1`
wrappers at the repo root — see `RUNNING.md`.

Tests use specs2 (`org.specs2.mutable.Specification`), not ScalaTest. `Test / fork := true` is set globally, and `assembly / test := {}` skips tests during assembly builds.

CI (`.github/workflows/ci.yml`) runs on JDK 11 via `sbt -v +test`. `appveyor.yml` is a legacy/parallel Windows CI config building with `sbt clean compile test` on JDK 11 — both exist, ci.yml (GitHub Actions) is the primary one.

## Architecture

### Core MediaWiki client (`scalawiki-core`)

- `MwBot` (trait) / `MwBotImpl` — the central API client. Created via `MwBot.fromHost(...)` or `MwBot.create(...)`; instances are cached per-site domain (`MwBot.cache`). Wraps a Pekko `ActorSystem` and an `HttpClient`.
- Requests are modeled as a typed DSL under `dto/cmd` (`Action`, `Query`, `ListParam`, `Prop`, etc.) rather than raw string params — e.g. `dto/cmd/query/list/CategoryMembers.scala`, `dto/cmd/query/prop/ImageInfo.scala`. `DslQuery` (`query/DslQuery.scala`) turns an `Action` into paginated HTTP calls and accumulates results into `Page`/`PageList` objects.
- JSON responses are parsed with Play JSON `Reads` defined in `json/MwReads.scala`.
- `query/PageQuery` / `SinglePageQuery` give a fluent, higher-level API on top of `DslQuery` (`bot.page("title").edit(...)`, etc.) — see `ActionLibrary` for usage examples like `message()`/`email()`.
- `cache/CachedBot` — an `MwBotImpl` subclass that memoizes API responses to disk as a plain per-key file cache under `http-cache/<name>/` (one file per request, named by the SHA-256 of the params; `<name>` is the campaign, e.g. `wlm-UA-2024`). Contest stat runs use this to avoid refetching the same Commons queries across repeated local runs. Only JSON-looking bodies are persisted; on a `MwException` it evicts the offending cache entry and retries once. (Replaced an earlier ChronicleMap-backed store, which needed `--add-opens` JVM flags on JDK 17+.)
- `LoginInfo.fromEnv()` reads bot credentials from environment variables for authenticated sessions.
- **Logging**: Pekko is wired to slf4j (`application.conf`), and `scalawiki-core/src/main/resources/logback.xml` sends everything at INFO to a rolling `logs/scalawiki.log` while the console (stderr) only shows WARN+. Each module with tests ships a `logback-test.xml` (console only) so test output is unaffected. `MwBot.log` is the `ActorSystem` log, so it logs under `org.apache.pekko.actor.ActorSystemImpl` — don't add an `org.apache.pekko` logger rule expecting it to filter only Pekko internals.

### Contest statistics engine (`scalawiki-wlx`)

This is the most actively developed part of the codebase. Rough data flow:

1. **Monument lists**: `MonumentQuery` fetches/parses monument list pages from wiki (via `WlxTemplateParser`/`WlxTableParser`) into `Monument` DTOs; `MonumentDB` indexes them by id/region/type for a given `Contest`.
   - **Monument CSV cache** (on by default, same `--no-csv-cache` toggle): `MonumentDbCache` serializes parsed monuments to `csv-cache/<campaign>-monuments.csv`, each row tagged with its source page + that page's `page_revid`/`page_ts` (`otherParams` as a JSON column, `listConfig` re-attached on read). `stat/cache/MonumentDbProvider` (`.gather()`) drives it: with a cache present it runs a cheap `MonumentQuery.listPageRevs` (`embeddedin` id+timestamp sweep, no content), refetches only pages whose revid changed via `MonumentQuery.monumentsByPages`, drops pages a complete sweep confirms are gone (a short sweep never mass-deletes), reuses the rest, rewrites the cache; any sync error falls back to a full fetch. `--monument-cache-refresh` (or `--csv-cache-refresh`) forces the full path.
2. **Images**: `ImageQuery` fetches uploaded contest images from Commons; `ImageDB` (`ImageDB.scala`) wraps them, applying eligibility filters — monument id matching, minimum resolution (`minMpx`), "taken after deadline" checks, ineligible-submission categories, etc. `sansIneligible`/`ineligible` are the key derived collections other reports read from.
   - **Image CSV cache** (on by default, `stat/cache/ImageDbProvider` + `ImageCsvExporter`/`ImageCsvImporter`): a second tier over the `http-cache/` request cache. Built `Image` DBs are serialized to `csv-cache/<campaign>-<year>-images.csv` (+ `-all-images.csv`, holding only the all-time images that are in no per-year CSV — the all-time DB is read as the per-year images plus those; an older full-format file is rewritten on first read); later runs read those and skip the sequential JSON parse. Each row stores the file's `last_revid` / `last_revision_ts`. The current year is always incrementally synced (`ImageDbProvider.syncImageDb`): a cheap `ImageQuery.imageIdsFromCategory` sweep returns id + latest revision per file, so new / changed (revid differs) / deleted files are reconciled. Past years and the all-images CSV are frozen unless `--csv-cache-resync` runs the same sweep against them (rows with no `last_revid` fall back to `contestEndInstant(year)`). Past years are loaded as slim `Image`s (`ImageCsvImporter.slim`: only the fields past-year reports/ratings/eligibility read) unless `--csv-cache-resync` or `--export-images-csv` needs them whole — never write a slim image back to a CSV. All CSV reads in a run share one `ImageCsvImporter.ValuePool`, so repeated authors/categories/monument ids are held once. `--no-csv-cache` / `--csv-cache-refresh` / `--csv-cache-resync` / `--csv-cache-dir` control it. See `RUNNING.md`.
3. **Coordination**: `stat/Statistics.scala` ties monument and image queries together per year into a `ContestStat` (current-year DB, all-time DB, and per-year DBs), asynchronously via `gatherData` — a thin wirer that delegates to `stat/cache/MonumentDbProvider` and `stat/cache/ImageDbProvider` (which own the CSV caches + wiki resync) and to `stat/reports/ReportRunner` (which runs the reports and waits for publishing).
4. **Reports**: `stat/reports/` (e.g. `Output.scala`, `ReporterRegistry`) turns a `ContestStat` into wiki-markup tables, galleries, and rating outputs. `stat/rating/` computes per-participant/per-place scoring (`RateConfig`, `PerPlaceStat`).
5. **CLI entry point**: `stat/Statistics` (companion `object`, `def main`) is invoked via `StatParams`/`StatConfig` (Scallop-based CLI, see `StatParams.scala`) — flags like `--campaign`, `--year`, `--region`, `--gallery`, `--regional-stat`, `--min-mpx` select which reports run. `Statistics.run` is synchronous (`Await` on `gatherData`, then hands the `ContestStat` to `ReportRunner`, which runs the reports, drives `WriteWatcher.awaitQuiescence`, and prints a `=== Publish summary ===`).
   - **Console progress**: `stat/progress/Progress` (a thin wrapper over `me.tongfei:progressbar`) is the only thing that writes progress to the screen (stderr). `Statistics`, the `stat/cache` providers, `ReportRunner` and `ReporterRegistry` call `Progress.phase` / `Progress.bar` around the fetch and report stages. With no TTY or `--no-progress` it degrades to throttled INFO lines in the log. `--verbose` lifts the console log threshold to INFO.

`dto/Contest`, `dto/Country`, and the KOATUU/KATOTTH parsers (`dto/Katotth*`, `dto/Koatuu*`) encode Ukraine's administrative-division hierarchies used to bucket monuments/images by region.

### Bots (`scalawiki-bots`)

Standalone CLI utilities grouped by domain under `bots/{edu,finance,museum,np,stat,vote}` and a `copyvio` checker, generally combining `core` (wiki I/O) with `wlx` (contest data) and Apache POI (`Poi.*` deps) for spreadsheet I/O. Uses Twirl templates (`SbtTwirl` plugin) for text templating.

## Repo hygiene note

The working tree in this checkout tends to accumulate generated/output artifacts at the repo root from manual stats runs — the `http-cache/` and `csv-cache/` directories, stale `*.cache` files (from the old ChronicleMap cache), gallery images (`*.png`), report files (`most.wiki`, etc.), JVM crash logs (`hs_err_pid*.log`), heap/thread dumps. These are run artifacts, not source; don't assume untracked files at the root are meaningful to a given task unless the user points at them.
