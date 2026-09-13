## Purpose

Gives contest organizers a current-snapshot view of monument-photography participation broken down by raion and hromada within each oblast, nested inside the existing per-oblast regional-detail wiki page, so under-represented communities can be targeted for outreach.

## ADDED Requirements

### Requirement: Raion breakdown nested in the oblast detail page
When regional details are enabled, each oblast's detail wiki page SHALL include a nested table listing every raion within that oblast, using Ukraine's current (KATOTTH) administrative division rather than the pre-2020 raion boundaries used elsewhere in this report.

#### Scenario: Oblast page lists its current raions
- **WHEN** the regional-details report generates the detail page for an oblast
- **THEN** that page contains a table with one row per raion currently within that oblast

#### Scenario: No separate wiki page is created per raion
- **WHEN** the regional-details report generates raion rows for an oblast
- **THEN** no new wiki page is created for any individual raion — the raion table is embedded in the oblast's existing detail page

### Requirement: Hromada breakdown nested under each raion
Each raion's row or section SHALL expand into a nested table listing the hromadas within that raion, using the same current administrative division.

#### Scenario: Raion section lists its hromadas
- **WHEN** the regional-details report generates the raion table for an oblast
- **THEN** each raion's section contains a nested table with one row per hromada currently within that raion

#### Scenario: No separate wiki page is created per hromada
- **WHEN** the regional-details report generates hromada rows for a raion
- **THEN** no new wiki page is created for any individual hromada — the hromada table is embedded in the same oblast detail page

#### Scenario: Breakdown does not descend below hromada
- **WHEN** the regional-details report reaches hromada level
- **THEN** it does not generate rows for settlements or any level below hromada

### Requirement: Raion and hromada rows show current totals only
Raion and hromada table rows SHALL show only current cumulative totals — objects in lists, objects pictured, and percentage pictured — and SHALL NOT include the per-year historical columns present in the oblast-level table.

#### Scenario: Hromada row omits per-year columns
- **WHEN** a hromada row is rendered in a raion's nested table
- **THEN** the row shows the region name, objects in lists, objects pictured, and pictured percentage, and does not show any per-year (e.g. "2024 Objects", "2024 Pictures") columns

#### Scenario: Raion row omits per-year columns
- **WHEN** a raion row is rendered in an oblast's nested table
- **THEN** the row shows the region name, objects in lists, objects pictured, and pictured percentage, and does not show any per-year columns

### Requirement: Monuments unresolved to a hromada are surfaced, not dropped
A monument that cannot be resolved to a current raion/hromada under the KATOTTH mapping SHALL still be counted in its oblast's existing totals, and SHALL be reflected in a distinct "Unresolved" row in that oblast's nested breakdown rather than being silently omitted or attributed to an unrelated raion/hromada.

#### Scenario: Unresolvable monument appears in the Unresolved row
- **WHEN** an oblast contains one or more monuments that cannot be mapped to a current raion or hromada
- **THEN** that oblast's nested breakdown includes an "Unresolved" row whose count includes those monuments

#### Scenario: Unresolvable monument still counts at oblast level
- **WHEN** an oblast contains one or more monuments that cannot be mapped to a current raion or hromada
- **THEN** the oblast-level totals (in the existing `Regional statistics` table) still include those monuments, unchanged from current behavior

### Requirement: Areas with no hromada level group by raion or city district
A monument whose page names a raion but no hromada — because that area structurally has no hromada level (Kyiv, Sevastopol, occupied Crimea) or because that raion's monument list hasn't yet been split into hromada pages — SHALL be grouped under that raion's (or city district's) own row, not treated as unresolved.

#### Scenario: Kyiv or Sevastopol monuments group by city district
- **WHEN** a monument's page names a Kyiv or Sevastopol city district and no hromada
- **THEN** that monument is counted under a row for that city district, not the oblast-level Unresolved row

#### Scenario: Crimean monuments group by raion
- **WHEN** a monument's page names a Crimean raion and no hromada
- **THEN** that monument is counted under a row for that raion, not the oblast-level Unresolved row

### Requirement: A monument whose id doesn't belong to its assigned oblast is excluded from normal counts
A monument that appears in an oblast's candidate set only because of a coincidental id-parsing fallback — not because its id actually starts with that oblast's code — SHALL NOT be counted as a normal or Unresolved monument there. The same treatment applies when a monument's id does start with the oblast's code, but its own list page resolves it to a different oblast entirely.

#### Scenario: A misattributed id is not counted as normal or Unresolved
- **WHEN** an oblast's candidate monuments include one whose id does not literally start with that oblast's code, or one resolved to a different oblast entirely
- **THEN** that monument does not appear in any raion, hromada, or district row, and does not appear in the oblast-level "Unresolved" row

#### Scenario: A monument resolved to a different oblast's raion or hromada does not create that oblast's section here
- **WHEN** an oblast's candidate monuments include one whose id starts with that oblast's code, but whose own list page resolves it to a raion or hromada belonging to a different oblast
- **THEN** this oblast's breakdown does not create a section for the other oblast's raion or hromada

### Requirement: Every problematic monument is itemized in one explained list, replacing separate unexplained sections
Each oblast's breakdown SHALL include a single list itemizing every monument that needed a judgment call — one whose id doesn't belong to this oblast, one resolved to a different oblast, one that couldn't be resolved to any current raion/hromada/district (or only down to raion level), and one whose id is shared by more than one monument — each entry naming the monument and explaining what is wrong with it. This list SHALL replace the separate, unexplained "Bad IDs" (count only) and "Wrong region ids" (ids only) sections.

#### Scenario: A bad id is itemized with an explanation
- **WHEN** an oblast's candidate monuments include one whose id does not literally start with that oblast's code
- **THEN** the oblast's issues list includes an entry for that monument's id explaining that it does not belong to this oblast

#### Scenario: A monument resolved to a different oblast is itemized with an explanation naming where it resolved to
- **WHEN** an oblast's candidate monuments include one whose id starts with that oblast's code but whose page resolves it to a different oblast
- **THEN** the oblast's issues list includes an entry for that monument's id explaining that it resolves to the other oblast, naming that oblast/raion/hromada

#### Scenario: An unresolved monument is itemized with an explanation
- **WHEN** an oblast contains one or more monuments that cannot be mapped to a current raion, hromada, or district (or only down to raion level, not to a specific hromada)
- **THEN** the oblast's issues list includes an entry for each, explaining that no current raion/hromada/district (or no specific hromada) could be determined

#### Scenario: A duplicated id is itemized with an explanation naming every page it appears on
- **WHEN** an id is shared by more than one monument
- **THEN** the oblast's issues list includes one entry for that id, explaining that it is shared and naming every page on which a monument uses it

#### Scenario: No separate Bad IDs or Wrong region ids sections remain
- **WHEN** an oblast's breakdown includes any of the above problem monuments
- **THEN** it does not include a separate "Bad IDs" section or a separate "Wrong region ids" section — only the one itemized, explained list

### Requirement: Raion and hromada names link to their monument list page
Each raion or hromada name in the nested breakdown SHALL be a link to its monument list wiki page. When a region's monuments are split across several list pages, the link SHALL point to the first page (in alphabetical order), which carries a navbar to reach the rest.

#### Scenario: A hromada with one list page links to it
- **WHEN** a hromada row is rendered and all of its monuments share one list page
- **THEN** the hromada's name in that row links to that page

#### Scenario: A hromada split across several list pages links to the first
- **WHEN** a hromada's monuments are split across several list pages (e.g. alphabetically)
- **THEN** the hromada's name links to the alphabetically first of those pages

#### Scenario: A raion with no hromada split links to its own page
- **WHEN** a raion's monuments are not split into hromadas (e.g. Kyiv, Sevastopol, occupied Crimea, or a raion not yet split) and all share one list page
- **THEN** the raion's name (used as that section's heading) links to that page

### Requirement: Tables include a Total row
Each raion's nested hromada table, and the oblast's overall raion/hromada breakdown, SHALL include a Total row summing the current-totals columns (objects in lists, objects pictured, pictured percentage) across all rows in that table.

#### Scenario: A raion's hromada table has a Total row
- **WHEN** a raion's nested table lists its hromadas (and, if present, that raion's own Unresolved row)
- **THEN** the table includes a final "Total" row whose counts equal the sum of all other rows in that table

#### Scenario: The oblast breakdown has an overall Total section
- **WHEN** an oblast's nested breakdown is rendered
- **THEN** it includes a final "Total" row or section summing the oblast's genuine (non-bad-id) monument counts

### Requirement: A raion with no hromada breakdown is listed in a shared raions table, not its own redundant nested table
A raion with no hromada rows at all (because its area has no hromada level, or its list hasn't been split into hromada pages yet) SHALL be listed as a single row in one shared table for such raions, rather than getting its own nested table whose only rows would be an "Unresolved" row identical to its "Total" row.

#### Scenario: A raion with zero hromadas appears in the shared raions table
- **WHEN** an oblast contains a raion with no monuments resolved to any hromada within it
- **THEN** that raion appears as one row in a shared table for raions with no hromada breakdown, not as its own section with a nested table

#### Scenario: A raion with at least one hromada still gets its own nested table
- **WHEN** an oblast contains a raion with at least one monument resolved to a hromada within it
- **THEN** that raion still gets its own section with a nested hromada table, unaffected by other raions in the same oblast having no hromada breakdown

### Requirement: A region's link is unaffected by other monuments sharing its id with a different region
When two different monuments share the same id due to a data-entry error on their source list pages, resolving one region's link SHALL NOT be affected by which of the two monuments happens to win that id's slot elsewhere in the resolution.

#### Scenario: Two regions sharing a duplicated id each link to their own page
- **WHEN** an id is duplicated across two monuments whose pages name two different regions
- **THEN** each region's own row or heading links to its own page, never to the other region's page

### Requirement: A region's link prefers a page that actually names it
When a region's monuments include some resolved into it from a page that does not itself name the region (e.g. a thematic special-nomination list unrelated to geography), the region's link SHALL prefer a page that does name it over one that merely sorts first alphabetically.

#### Scenario: An unrelated thematic page does not win the link
- **WHEN** a region's monuments include some listed on an unrelated, non-geographic page alongside others listed on the region's own geographic page
- **THEN** the region's link points to its own geographic page, not the unrelated page, regardless of alphabetical order

### Requirement: The Chornobyl Exclusion Zone is its own named group
The monument list page for the Chornobyl Exclusion Zone (nominally filed under Вишгородський raion) SHALL be grouped under its own name, "Зона відчуження ЧАЕС", rather than being treated as unresolved or folded into a generic raion-only bucket.

#### Scenario: The exclusion zone page gets its own row
- **WHEN** a monument's page is the Chornobyl Exclusion Zone's list page
- **THEN** that monument is counted under a row named "Зона відчуження ЧАЕС", nested within Вишгородський raion's breakdown

#### Scenario: The exclusion zone is not reported as unresolved
- **WHEN** a monument's page is the Chornobyl Exclusion Zone's list page
- **THEN** that monument does not appear in any "Unresolved" row or in the "Data issues" list

### Requirement: A single-table oblast breakdown has no separate Total table
When an oblast's breakdown produces exactly one table overall, the Total row SHALL be part of that table rather than a separate table repeating the same numbers.

#### Scenario: A lone Raions table gets its Total row merged in
- **WHEN** an oblast's breakdown consists of only the shared "Raions" table (no split-raion sections, no oblast-level Unresolved table)
- **THEN** that table includes a Total row, and no separate Total section is rendered

#### Scenario: A lone raion or Unresolved table needs no additional row
- **WHEN** an oblast's breakdown consists of only one split raion's table, or only the oblast-level Unresolved table
- **THEN** no separate Total section is rendered, since that one table's own row already gives the oblast total

#### Scenario: A multi-table oblast still gets a separate Total section
- **WHEN** an oblast's breakdown produces more than one table
- **THEN** a separate Total section is rendered summarizing the whole oblast, as before

### Requirement: A multi-table Total section lists each raion's own total before the grand total
When an oblast's breakdown produces more than one table, the separate Total section SHALL list every raion's own total (linked the same way as its own section or row) before the oblast-level Unresolved bucket (if any) and the final grand-total row, rather than only the grand total.

#### Scenario: The Total section breaks totals down by raion
- **WHEN** an oblast has several raion tables (split or shared "Raions" rows)
- **THEN** the Total section includes one row per raion, each summing that raion's own monuments and linked to its own page, followed by the oblast-level Unresolved row (if any) and a final row summing the whole oblast

### Requirement: Data issues list entries use ukwiki-interwiki links
Every wiki page or article mentioned in a "Data issues" entry SHALL be a clickable interwiki link to uk.wikipedia.org, not plain text or a link that targets Commons.

#### Scenario: A page named in a Duplicated id explanation is a link
- **WHEN** a "Duplicated id" entry names the pages a duplicated id appears on
- **THEN** each page is rendered as an interwiki link to uk.wikipedia.org

#### Scenario: A monument name's own embedded wikilink targets ukwiki
- **WHEN** a monument's name (shown in an issue entry) contains an embedded `[[wikilink]]`
- **THEN** that wikilink is rendered as an interwiki link to uk.wikipedia.org, not a same-titled Commons link

#### Scenario: A Wrong region entry's resolved location is a link
- **WHEN** a "Wrong region" entry explains where a monument actually resolved to
- **THEN** that location is rendered as an interwiki link to the monument's own list page on uk.wikipedia.org

### Requirement: Oblast-level report is unaffected
Enabling the raion/hromada breakdown SHALL NOT change the existing oblast-level `Regional statistics` table's rows, columns, or per-year history.

#### Scenario: Top-level regional statistics table is unchanged
- **WHEN** the regional-details report runs with raion/hromada breakdown enabled
- **THEN** the `Commons:<Contest>/Regional statistics` page's oblast-level table still shows the same rows and per-year columns as before this change
