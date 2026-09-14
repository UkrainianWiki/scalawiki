package org.scalawiki.wlx.stat.reports

import org.scalawiki.dto.markup.Table
import org.scalawiki.wlx.dto.AdmDivision
import org.scalawiki.wlx.{ImageDB, KatotthResolver, MonumentDB}

/** Current-totals-only breakdown of monument counts by raion and hromada
  * within one oblast, using Ukraine's present-day (KATOTTH) administrative
  * division rather than the pre-2020 raion boundaries the rest of this
  * report uses. Nested inside the oblast's existing detail wiki page - no
  * per-raion or per-hromada pages are created.
  */
object RegionalCommunityBreakdown {

  val unresolvedName = "Unresolved"
  val totalName = "Total"
  val raionsName = "Raions"
  val issuesName = "Data issues"

  val columns: Seq[String] =
    Seq("Region (KATOTTH)", "Objects in lists", "Pictured", "Pictured percentage")

  /** One monument whose id needs an editor's attention, for the "Data
    * issues" list: what's wrong with it and why, in place of the old
    * separate "Bad IDs" (count only) and "Wrong region ids" (ids only, no
    * explanation) sections.
    */
  case class Issue(id: String, name: String, category: String, explanation: String)

  def totals(
      monumentIds: Set[String],
      monumentDb: MonumentDB,
      totalImageDb: ImageDB
  ): (Int, Int, Int) = {
    val inLists = monumentIds.size
    val pictured =
      (monumentIds intersect (totalImageDb.ids ++ monumentDb.picturedIds)).size
    val percentage = if (inLists != 0) 100 * pictured / inLists else 0
    (inLists, pictured, percentage)
  }

  def row(
      name: String,
      monumentIds: Set[String],
      monumentDb: MonumentDB,
      totalImageDb: ImageDB
  ): Seq[String] = {
    val (inLists, pictured, percentage) = totals(monumentIds, monumentDb, totalImageDb)
    Seq(name, inLists.toString, pictured.toString, percentage.toString)
  }

  /** `name` linked to the earliest (alphabetically first) monument list page
    * among `monumentIds`, so a region split across several list pages (e.g.
    * "Львівська громада (А–В)", "(Г)", ...) links to the first one - those
    * pages carry a navbar to reach the rest. Falls back to plain `name` if
    * none of the monuments have a page recorded. Monument list pages live
    * on uk.wikipedia.org, while this report is written to Commons, so the
    * link needs the `:uk:` interwiki prefix (as used elsewhere in this
    * package, e.g. `Output.regionalStat`) rather than a plain `[[page]]`
    * link, which would otherwise point at a same-titled Commons page.
    *
    * `pageById` (from `KatotthResolver.resolveDetailed`) is used rather
    * than a separate `MonumentDB.byId` lookup: a handful of WLM ids are
    * duplicated by data-entry error across two different list pages, and
    * `byId` resolves such a duplicate independently of - and can therefore
    * disagree with - however the id's admin-division match was resolved,
    * pointing the link at the wrong one's page.
    *
    * Pages whose own title actually names this region (per
    * `KatotthResolver.parsePage`) are preferred over any others found among
    * `monumentIds`. A hromada's monuments aren't necessarily all listed
    * under its own geographic page: some may only be resolved into it via
    * the numeric fallback from an unrelated page - e.g. a thematic special
    * nomination like "Єврейська спадщина" lists monuments by topic, not
    * geography, but a monument on it can still genuinely belong to a real
    * hromada. Sorting *all* candidate pages alphabetically would let such a
    * page (which may sort earlier than the real one) win the link outright;
    * restricting to pages that actually name this region first avoids that.
    */
  def linkedName(name: String, monumentIds: Set[String], pageById: Map[String, String]): String = {
    val pages = monumentIds.flatMap(pageById.get).filter(_.nonEmpty)
    val ownPages = pages.filter(namesRegion(_, name))
    val candidates = if (ownPages.nonEmpty) ownPages else pages
    candidates.toSeq.sorted.headOption.fold(name)(page => s"[[:uk:$page|$name]]")
  }

  private def namesRegion(page: String, name: String): Boolean = {
    val parsed = KatotthResolver.parsePage(page)
    parsed.hromadaStem.exists(name.startsWith) || parsed.raionStem.exists(name.startsWith)
  }

  /** Whether any monument in `raionMonumentIds` resolves down to hromada
    * level. False for a raion with no hromada level at all (Kyiv/Sevastopol
    * districts, occupied Crimea) or one not yet split into hromada lists -
    * such a raion gets a single row in the shared raions table (see
    * [[render]]) instead of its own nested table, which would otherwise
    * show only a redundant "Unresolved" row identical to its "Total" row.
    */
  def hasHromadaBreakdown(raionMonumentIds: Set[String], resolved: Map[String, AdmDivision]): Boolean =
    raionMonumentIds.exists(id => resolved.get(id).flatMap(KatotthResolver.hromadaAncestor).isDefined)

  /** A monument's fully-qualified location, oblast down to whatever level it
    * resolved to (e.g. "Київська область / Бучанський район / Ірпінська
    * громада"), for explaining where a "wrong region" id actually landed.
    * Hromadas are named as in their table rows (see
    * `KatotthResolver.hromadaDisplayName`).
    */
  private def locationDescription(node: AdmDivision): String = {
    def chain(n: AdmDivision): List[AdmDivision] = n.parent().fold(List(n))(p => chain(p) :+ n)
    chain(node)
      .drop(1)
      .map(n => if (n.regionType.exists(_.code == "H")) KatotthResolver.hromadaDisplayName(n) else n.fullName)
      .mkString(" / ")
  }

  /** A monument's name, with any embedded `[[wikilink]]`s pointed at
    * uk.wikipedia.org (as `Output.scala` does elsewhere for the same
    * reason): monument names come from uk.wikipedia.org list pages and
    * their links target ukwiki articles, but the "Data issues" list is
    * written to Commons, where a plain `[[link]]` would target a
    * same-titled (likely nonexistent) Commons page instead.
    */
  private def wikifyName(name: String): String = name.replace("[[", "[[:uk:")

  /** A monument list page, wrapped as an interwiki link to uk.wikipedia.org
    * (see `linkedName` for why the `:uk:` prefix is needed here).
    */
  private def wikifyPage(page: String): String = s"[[:uk:$page]]"

  private def monumentName(id: String, monumentDb: MonumentDB): String =
    monumentDb.byId(id).map(m => wikifyName(m.name)).getOrElse("")

  /** Renders one raion's nested hromada table, and separately returns the
    * ids resolved to this raion but not further to any specific hromada -
    * the caller collects these into the oblast's "Data issues" list.
    */
  def renderRaion(
      raion: AdmDivision,
      raionMonumentIds: Set[String],
      resolved: Map[String, AdmDivision],
      pageById: Map[String, String],
      monumentDb: MonumentDB,
      totalImageDb: ImageDB
  ): (String, Set[String]) = {
    val byHromada: Map[Option[AdmDivision], Set[String]] =
      raionMonumentIds.groupBy(id => resolved.get(id).flatMap(KatotthResolver.hromadaAncestor))

    // Collect over a Seq, not the Map: two distinct hromadas can share a
    // name within one raion (e.g. two "Миколаївська" in Сумський raion), and
    // re-keying by name would silently drop one of them. Such hromadas are
    // labeled with their seat-type adjective (see hromadaDisplayName).
    val hromadaRows = byHromada.toSeq.collect { case (Some(hromada), ids) =>
      val name = KatotthResolver.hromadaDisplayName(hromada)
      (name, hromada.code) -> row(linkedName(name, ids, pageById), ids, monumentDb, totalImageDb)
    }.sortBy(_._1).map(_._2)

    val raionUnresolved = byHromada.getOrElse(None, Set.empty)
    val rows = hromadaRows ++
      (if (raionUnresolved.nonEmpty)
         Seq(row(unresolvedName, raionUnresolved, monumentDb, totalImageDb))
       else Nil) ++
      Seq(row(totalName, raionMonumentIds, monumentDb, totalImageDb))

    val header = linkedName(raion.fullName, raionMonumentIds, pageById)
    val text = s"\n==== $header ====\n" + Table(columns, rows, raion.fullName).asWiki + "\n"
    (text, raionUnresolved)
  }

  /** Every id sharing a WLM id with at least one other monument, among
    * `ids` - a data-entry error (ids are meant to be unique), reported as
    * its own issue rather than silently letting one arbitrarily "win" a
    * page or region lookup.
    */
  def duplicateIdIssues(ids: Set[String], monumentDb: MonumentDB): Seq[Issue] = {
    val matching = monumentDb.allMonuments.filter(m => ids.contains(m.id))
    matching
      .groupBy(_.id)
      .collect { case (id, ms) if ms.size > 1 =>
        val pages = ms.map(_.page).distinct.map(wikifyPage).mkString("; ")
        Issue(
          id,
          ms.map(m => wikifyName(m.name)).mkString(" / "),
          "Duplicated id",
          s"this id is used by ${ms.size} monuments, across page(s): $pages"
        )
      }
      .toSeq
  }

  /** Nested wikitext for one oblast's raion/hromada breakdown, plus a final
    * "Data issues" list itemizing every monument that needed a judgment
    * call: an id that doesn't belong to this oblast at all ("Bad id"), an
    * id that belongs here but whose page resolves it to a different oblast
    * ("Wrong region"), a monument with no current raion/hromada/district
    * found at all or only down to raion level ("Unresolved"), and an id
    * shared by more than one monument ("Duplicated id"). This replaces the
    * old separate count-only "Bad IDs" section and the plain, unexplained
    * "Wrong region ids" list with one itemized, explained list.
    */
  def render(
      oblastRegionId: String,
      oblastMonumentIds: Set[String],
      badIdsByPrefix: Set[String],
      badIdsByResolution: Set[String],
      resolved: Map[String, AdmDivision],
      pageById: Map[String, String],
      monumentDb: MonumentDB,
      totalImageDb: ImageDB
  ): String = {
    val byRaion: Map[Option[AdmDivision], Set[String]] =
      oblastMonumentIds.groupBy(id => resolved.get(id).flatMap(KatotthResolver.raionAncestor))

    val raionEntries = byRaion.toSeq.collect { case (Some(raion), ids) => (raion, ids) }
    val (splitRaions, unsplitRaions) =
      raionEntries.partition { case (_, ids) => hasHromadaBreakdown(ids, resolved) }

    val raionResults = splitRaions.sortBy(_._1.fullName).map { case (raion, raionMonumentIds) =>
      renderRaion(raion, raionMonumentIds, resolved, pageById, monumentDb, totalImageDb)
    }
    val raionSections = raionResults.map(_._1)
    val raionOnlyUnresolved = raionResults.flatMap(_._2).toSet

    val oblastUnresolved = byRaion.getOrElse(None, Set.empty)

    // When the whole breakdown comes down to exactly one table, a separate
    // "Total" table right after it would just repeat the same numbers in a
    // second table of its own. A lone split raion's own table already ends
    // with its Total row (equal to the oblast total, since it's the only
    // raion); a lone oblast-level Unresolved table's single row already *is*
    // the oblast total. Only the "Raions" table has no Total row of its own,
    // so it gets one added directly instead of a separate table.
    val tableCount =
      splitRaions.size + (if (unsplitRaions.nonEmpty) 1 else 0) + (if (oblastUnresolved.nonEmpty) 1 else 0)
    val isSingleTable = tableCount == 1

    val raionsTableSection =
      if (unsplitRaions.nonEmpty) {
        val rows = unsplitRaions.sortBy(_._1.fullName).map { case (raion, ids) =>
          row(linkedName(raion.fullName, ids, pageById), ids, monumentDb, totalImageDb)
        }
        val allRows =
          if (isSingleTable) rows :+ row(totalName, oblastMonumentIds, monumentDb, totalImageDb) else rows
        s"\n==== $raionsName ====\n" + Table(columns, allRows, raionsName).asWiki + "\n"
      } else ""

    val unresolvedSection =
      if (oblastUnresolved.nonEmpty)
        s"\n==== $unresolvedName ====\n" +
          Table(columns, Seq(row(unresolvedName, oblastUnresolved, monumentDb, totalImageDb)), unresolvedName).asWiki +
          "\n"
      else ""

    val totalSection =
      if (isSingleTable) ""
      else {
        // With several raion tables already shown above in detail, a "Total"
        // table with just one grand-total row loses the at-a-glance
        // per-raion comparison. List every raion's own totals first (split
        // and unsplit alike, linked the same way as their own sections),
        // then the oblast-level Unresolved bucket if any, then the grand
        // total - one table that summarizes the whole oblast.
        val raionRows = (splitRaions ++ unsplitRaions).sortBy(_._1.fullName).map { case (raion, ids) =>
          row(linkedName(raion.fullName, ids, pageById), ids, monumentDb, totalImageDb)
        }
        val unresolvedRow =
          if (oblastUnresolved.nonEmpty) Seq(row(unresolvedName, oblastUnresolved, monumentDb, totalImageDb))
          else Nil
        val rows = (raionRows ++ unresolvedRow) :+ row(totalName, oblastMonumentIds, monumentDb, totalImageDb)
        s"\n==== $totalName ====\n" + Table(columns, rows, totalName).asWiki + "\n"
      }

    val duplicated = duplicateIdIssues(
      oblastMonumentIds ++ badIdsByPrefix ++ badIdsByResolution,
      monumentDb
    )
    val duplicatedIds = duplicated.map(_.id).toSet

    val badIdIssues = (badIdsByPrefix -- duplicatedIds).toSeq.map { id =>
      Issue(
        id,
        monumentName(id, monumentDb),
        "Bad id",
        "this monument's id does not start with this oblast's own numeric code, so it doesn't genuinely belong here"
      )
    }
    val wrongRegionIssues = (badIdsByResolution -- duplicatedIds).toSeq.map { id =>
      val where = resolved.get(id).map(locationDescription).getOrElse("an unresolved place")
      val linkedWhere = pageById.get(id).fold(where)(page => s"[[:uk:$page|$where]]")
      Issue(
        id,
        monumentName(id, monumentDb),
        "Wrong region",
        s"this monument's id looks like it belongs to this oblast, but its own list page actually places it in $linkedWhere"
      )
    }
    val fullyUnresolvedIssues = (oblastUnresolved -- duplicatedIds).toSeq.map { id =>
      Issue(
        id,
        monumentName(id, monumentDb),
        "Unresolved",
        "no current raion, hromada, or district could be determined from this monument's page or recorded place"
      )
    }
    val raionOnlyIssues = (raionOnlyUnresolved -- duplicatedIds).toSeq.map { id =>
      Issue(
        id,
        monumentName(id, monumentDb),
        "Unresolved",
        "resolved to a raion but not to any specific hromada within it"
      )
    }

    val issues =
      (badIdIssues ++ wrongRegionIssues ++ fullyUnresolvedIssues ++ raionOnlyIssues ++ duplicated)
        .sortBy(i => (i.category, i.id))
    val issuesSection =
      if (issues.isEmpty) ""
      else
        s"\n==== $issuesName ====\n" +
          issues.map(i => s"* ${i.id} (${i.name}): ${i.category} — ${i.explanation}").mkString("\n") +
          "\n"

    raionSections.mkString("") + raionsTableSection + unresolvedSection + totalSection + issuesSection
  }
}
