## Why

Contest organizers currently see participation only down to the oblast level on the `Regional statistics` wiki page. To target outreach at under-represented areas ahead of the next contest, they need to see, as of now, how many monuments are pictured within each raion and hromada (Ukraine's administrative subdivisions below oblast) — not a per-year history, just current totals.

## What Changes

- Add a raion/hromada breakdown covering the full administrative hierarchy below oblast (oblast → raion → hromada), resolved primarily from each monument's own current, hromada-organized wiki list-page title, with the existing numeric KOATUU→KATOTTH mapping as a fallback.
- Raion and hromada breakdowns render as **nested tables inside the existing oblast detail subpage** (`Commons:<Contest>/Monuments pictured by region in <Oblast>`) — no new wiki page is created per raion or per hromada.
- Nested raion/hromada tables show **current cumulative totals only**: objects in lists, objects pictured, percentage pictured. They do not include the per-year historical columns the oblast-level table has.
- The top-level oblast-by-oblast `Regional statistics` table and its per-year columns are unchanged.
- **Not in this change** (explicitly deferred, called out here for later): a "changed since last check" delta column and an "all-time + 2026" column, planned for use once the 2026 contest is underway.

## Capabilities

### New Capabilities
- `regional-stat-detail`: recursive, current-totals-only breakdown of monument counts by raion and hromada, nested under each oblast's regional-detail wiki page.

### Modified Capabilities
(none — the existing oblast-level `Regional statistics` table and its per-year columns are unchanged)

## Impact

- `scalawiki-wlx/src/main/scala/org/scalawiki/wlx/stat/reports/MonumentsPicturedByRegion.scala`: oblast-subpage wiki output shape (`asText`/`communityBreakdown`), sharing one KATOTTH resolution map across oblasts.
- `scalawiki-wlx/src/main/scala/org/scalawiki/wlx/KatotthResolver.scala` (new): shared monument→hromada resolution, now primarily from each monument's own list-page title (already hromada-organized on-wiki — see design.md), falling back to the existing numeric KOATUU→KATOTTH mapping (`katotthFor`) only when the page doesn't resolve one.
- `scalawiki-wlx/src/main/scala/org/scalawiki/wlx/stat/reports/RegionalCommunityBreakdown.scala` (new): builds the nested current-totals wikitext and the Unresolved rows.
- `scalawiki-wlx/src/main/scala/org/scalawiki/wlx/KatotthMonumentListCreator.scala`: its original purpose (drafting hromada-organized list pages) is superseded by reality — live lists are already organized that way — so it now shares `KatotthResolver`'s KATOTTH tree/mapping instead of building its own; the tool itself is otherwise unchanged (still available for any future one-off re-migration).
- `scalawiki-wlx/src/main/resources/katotth.csv` (and a compatibility check on `katotth_koatuu.csv`): refreshed from the current official КАТОТТГ codifier (see design.md for source and cadence) — the bundled snapshot was from a single 2021-06-06 commit.
- `scalawiki-wlx/src/main/scala/org/scalawiki/wlx/stat/StatParams.scala`: `--regional-details` flag description updated to describe the new behavior. No change to the flag's name or default.
- Affects only Commons wiki pages written under `Commons:<Contest>/Monuments pictured by region in <Oblast>` — the main `Regional statistics` page is untouched.
