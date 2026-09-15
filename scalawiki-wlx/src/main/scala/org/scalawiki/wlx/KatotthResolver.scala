package org.scalawiki.wlx

import org.scalawiki.wlx.dto.{AdmDivision, Country, Katotth, Region, RegionType}

/** Resolves monuments (geocoded against the pre-2020 KOATUU settlement codes,
  * see [[MonumentDB.placeByMonumentId]]) to their current-day (KATOTTH)
  * administrative location. Shared between [[KatotthMonumentListCreator]]
  * and the regional-details report so both use the same mapping.
  */
object KatotthResolver {

  val country: Country = new Country(
    "UA",
    "Ukraine",
    Seq("uk"),
    Katotth.regions(() => Some(country))
  )

  val katotthMap: Map[String, AdmDivision] = country.mapByCode

  /** Deepest KATOTTH node matching a padded KOATUU settlement code, if any. */
  def katotthFor(koatuu: String): Option[AdmDivision] = {
    val paddedKoatuu = koatuu.padTo(10, "0").mkString
    val candidates =
      Katotth.toKatotth.getOrElse(paddedKoatuu, Nil).flatMap(nodeForCode)
    if (candidates.nonEmpty) Some(candidates.maxBy(_.level)) else None
  }

  // Settlement nodes (місто/смт/село/селище), keyed by lowercased current name,
  // each paired with its KOATUU code from the KATOTTH->KOATUU mapping.
  private lazy val koatuuByCurrentName: Map[String, Seq[String]] =
    katotthMap.values.toSeq
      .filter(_.regionType.exists(rt => Set("M", "T", "C", "X").contains(rt.code)))
      .flatMap(node => Katotth.toKoatuu.get(node.code).map(normalizeName(node.name) -> _))
      .groupBy(_._1)
      .map { case (name, pairs) => name -> pairs.map(_._2).distinct }

  private def normalizeName(name: String): String = name.replace('’', '\'').toLowerCase

  /** KOATUU code of the settlement currently (per KATOTTH) named `name` whose
    * KOATUU code starts with `koatuuPrefix` - the digits of a monument id's
    * first two segments ("12-119" -> "12119"), i.e. its oblast and KOATUU raion
    * or city council. KOATUU is frozen, so a settlement renamed since then
    * (Новомосковськ -> Самар, Червоноград -> Шептицький) can only be found under
    * its new name here; its code didn't change, so this is the same place a
    * KOATUU lookup by the old name finds. None unless exactly one settlement
    * matches.
    */
  def koatuuForCurrentName(name: String, koatuuPrefix: String): Option[String] =
    koatuuByCurrentName.getOrElse(normalizeName(name), Nil).filter(_.startsWith(koatuuPrefix)) match {
      case Seq(single) => Some(single)
      case _           => None
    }

  // Hromada nodes keyed by the first 10 digits of their 17-digit KATOTTH code
  // (oblast 2 + raion 2 + hromada 3 + settlement "000").
  private lazy val hromadaByPrefix: Map[String, AdmDivision] =
    katotthMap.values.toSeq
      .filter(a => a.regionType.exists(_.code == "H") && a.code.length == 17)
      .groupBy(_.code.take(10))
      .collect { case (prefix, Seq(single)) => prefix -> single }

  /** The KATOTTH node for a code from `katotth_koatuu.csv`, or - when the
    * bundled codifier edition no longer lists it (settlements are dropped or
    * renumbered between editions, while the KOATUU mapping is frozen) - its
    * nearest surviving ancestor, derived from the code's own hierarchical
    * digits: its hromada, else its raion.
    */
  private[wlx] def nodeForCode(code: String): Option[AdmDivision] =
    katotthMap
      .get(code)
      .orElse(if (code.length == 17) hromadaByPrefix.get(code.take(7) + "000") else None)
      .orElse(if (code.length == 17) katotthMap.get(code.take(5)) else None)

  private val hromadaSuffix = " громада"
  private val raionSuffix = " район"
  // Settlement-type adjective a hromada's page title carries when its bare
  // name collides with another hromada in the same raion (e.g. two distinct
  // "Миколаївська" hromadas in Сумський raion) - maps the adjective to the
  // regionType code(s) of the hromada's administrative-seat settlement.
  // "селищна" covers both T (селище міського типу) and X (плain селище) -
  // Wikipedia page titles don't distinguish the two sub-categories.
  private val seatTypeAdjectives: Map[String, Set[String]] =
    Map("міська" -> Set("M"), "сільська" -> Set("C"), "селищна" -> Set("T", "X"))

  private[wlx] case class ParsedPage(
      oblastSeg: Option[String],
      raionStem: Option[String],
      hromadaStem: Option[String],
      hromadaAdjective: Option[String]
  )

  private def stripParenthetical(s: String): String = {
    val t = s.trim
    if (t.endsWith(")")) {
      val open = t.lastIndexOf('(')
      if (open > 0) t.substring(0, open).trim else t
    } else t
  }

  /** Parses a monument's list-page title (e.g. `Вікіпедія:Вікі любить
    * пам'ятки/Київська область/Білоцерківський район/Ковалівська громада`)
    * into its oblast/raion/hromada name segments. Live monument lists are
    * already organized this way; a page with no hromada segment (Kyiv,
    * Sevastopol, occupied Crimea, or a raion not yet split into hromada
    * lists) yields `hromadaStem = None`.
    */
  private[wlx] def parsePage(page: String): ParsedPage = {
    val segs = page.split("/").toList.map(_.trim)
    if (segs.size < 3) ParsedPage(None, None, None, None)
    else {
      val lastRaw = stripParenthetical(segs.last)
      val hromadaPart =
        if (lastRaw.endsWith(hromadaSuffix.trim)) {
          var stem = lastRaw.dropRight(hromadaSuffix.trim.length).trim
          val adjective = seatTypeAdjectives.keys.find(adj => stem.endsWith(" " + adj))
          adjective.foreach(adj => stem = stem.dropRight(adj.length + 1).trim)
          Some((stem, adjective))
        } else None
      val raionStem = segs
        .lift(2)
        .map(stripParenthetical)
        .filter(_.endsWith(raionSuffix.trim))
        .map(s => s.dropRight(raionSuffix.trim.length).trim)
      ParsedPage(segs.lift(1), raionStem, hromadaPart.map(_._1), hromadaPart.flatMap(_._2))
    }
  }

  private def matchOblast(oblastSeg: String): Seq[AdmDivision] =
    country.regions.filter(r =>
      oblastSeg.startsWith(r.name) || r.name.startsWith(oblastSeg.replace(" область", ""))
    )

  // "B" (район в місті) covers Kyiv/Sevastopol's city districts, which are
  // the raion-equivalent level for cities with no hromadas of their own.
  private def matchRaion(raionStem: String, within: Seq[AdmDivision]): Seq[AdmDivision] =
    within.flatMap(_.regions).filter(r =>
      r.regionType.exists(rt => Set("P", "B").contains(rt.code)) && r.name.equalsIgnoreCase(raionStem)
    )

  // Hromada nodes under a division, keyed by its code. `allSubregions` rebuilds
  // the whole subtree (down to every settlement) on each call, so without this
  // each resolved page re-walked its entire oblast.
  private val hromadasUnderCache =
    scala.collection.concurrent.TrieMap.empty[String, Seq[AdmDivision]]

  private def hromadasUnder(division: AdmDivision): Seq[AdmDivision] =
    hromadasUnderCache.getOrElseUpdate(
      division.code,
      division.allSubregions.filter(_.regionType.exists(_.code == "H"))
    )

  private def hromadaCandidates(stem: String, within: Seq[AdmDivision]): Seq[AdmDivision] =
    within
      .flatMap(hromadasUnder)
      .filter(_.name.equalsIgnoreCase(stem))
      .distinct

  /** The regionType code of a hromada's administrative-seat settlement.
    * KATOTTH numbers the seat's sub-code lowest (conventionally "010"), so
    * it sorts first among the hromada's children.
    */
  private def seatType(hromada: AdmDivision): Option[String] =
    hromada.regions.headOption.flatMap(_.regionType).map(_.code)

  private lazy val seatAdjectiveByType: Map[String, String] =
    seatTypeAdjectives.toSeq.flatMap { case (adj, types) => types.map(_ -> adj) }.toMap

  /** A hromada's row label: its plain `fullName` ("Ковалівська громада"),
    * or - when another hromada in the same raion shares its name - the
    * seat-type-qualified form the on-wiki list pages use for exactly that
    * case ("Миколаївська селищна громада" / "Миколаївська сільська
    * громада"), so the two rows can be told apart.
    */
  def hromadaDisplayName(hromada: AdmDivision): String = {
    val collides = hromada.parent().exists(_.regions.exists(other =>
      other.code != hromada.code && other.regionType.exists(_.code == "H") && other.name.equalsIgnoreCase(hromada.name)
    ))
    val adjective = if (collides) seatType(hromada).flatMap(seatAdjectiveByType.get) else None
    adjective.fold(hromada.fullName)(adj => s"${hromada.name} $adj$hromadaSuffix")
  }

  private def disambiguateBySeatType(
      candidates: Seq[AdmDivision],
      adjective: Option[String]
  ): Option[AdmDivision] =
    adjective.flatMap(seatTypeAdjectives.get).flatMap { wantedTypes =>
      candidates.filter(c => seatType(c).exists(wantedTypes.contains)) match {
        case Seq(single) => Some(single)
        case _           => None
      }
    }

  // The Chornobyl Exclusion Zone ("ЧЗВ" - Чорнобильська зона відчуження) is
  // nominally listed under Вишгородський raion's page, but it isn't a real
  // hromada and shouldn't be lumped into that raion's generic "resolved to
  // raion, not to a hromada" Unresolved bucket - it's a distinct, well-known
  // area worth its own named group. Modeled as a synthetic hromada-level
  // (regionType "H") node so it renders exactly like a real hromada row,
  // nested under the real Вишгородський raion.
  private val exclusionZonePage =
    "Вікіпедія:Вікі любить пам'ятки/Київська область/Вишгородський район/ЧЗВ"
  private val exclusionZoneName = "Зона відчуження ЧАЕС"

  private lazy val exclusionZone: Option[AdmDivision] =
    for {
      oblast <- matchOblast("Київська область").headOption
      raion <- matchRaion("Вишгородський", Seq(oblast)).headOption
    } yield Region(
      raion.code + "-ЧЗВ",
      exclusionZoneName,
      regionType = Some(RegionType("H", Seq("зона відчуження"))),
      parent = () => Some(raion)
    )

  /** Resolves a monument's location from its own list-page title: a hromada
    * when the page names one, otherwise the raion (or, for Kyiv/Sevastopol,
    * the city district) itself - which is the most specific level available
    * for areas with no hromada level (Kyiv, Sevastopol, occupied Crimea, or
    * a raion not yet split into hromada lists) rather than leaving them
    * unresolved (`resolveDetailed` may still refine such a raion to a
    * hromada via the numeric mapping). Scoped to at most the parsed oblast (never country-wide):
    * an unscoped, purely name-based match anywhere in the country is more
    * often a coincidental same-named region in an unrelated oblast than the
    * intended one.
    */
  def resolveFromPage(page: String): Option[AdmDivision] =
    if (page == exclusionZonePage) exclusionZone
    else {
      val parsed = parsePage(page)
      val oblasts = parsed.oblastSeg.map(matchOblast).getOrElse(Nil)
      if (oblasts.isEmpty) None
      else
        parsed.hromadaStem match {
          case Some(stem) =>
            val raionScoped =
              parsed.raionStem.map(matchRaion(_, oblasts)).map(hromadaCandidates(stem, _)).getOrElse(Nil)
            val scoped = if (raionScoped.nonEmpty) raionScoped else hromadaCandidates(stem, oblasts)
            scoped match {
              case Seq(single) => Some(single)
              case Seq()        => None
              case many         => disambiguateBySeatType(many, parsed.hromadaAdjective)
            }
          case None =>
            parsed.raionStem.flatMap(matchRaion(_, oblasts) match {
              case Seq(single) => Some(single)
              case _           => None
            })
        }
    }

  /** A monument's resolved location, paired with the page that produced it.
    * A small number of WLM ids are duplicated by data-entry error across two
    * different monument list pages (e.g. the same id appearing once each in
    * two different Sevastopol district lists); when that happens, only one
    * of the two can occupy that id's slot in the resulting map, since ids
    * are otherwise expected to be unique. Carrying `page` alongside the
    * resolved division - both derived from the same winning `Monument`, in
    * the same pass - keeps them consistent with each other, rather than
    * looking the page up separately afterwards (e.g. via
    * `MonumentDB.byId`, which resolves such a duplicate independently and
    * can therefore disagree about which of the two monuments "wins").
    */
  case class Resolution(admDivision: AdmDivision, page: String)

  /** Monument id -> current-day KATOTTH location, with the page that
    * produced it. Resolves primarily from each monument's own list-page
    * title (already hromada-organized on-wiki for current lists), using the
    * numeric KOATUU->KATOTTH mapping (`katotthFor`):
    *  - instead, when the page doesn't resolve at all (no recognizable
    *    segments, an unmatched name, or a same-raion name collision that
    *    seat type can't settle);
    *  - to refine it, when the page resolves only to a raion/city district
    *    (no hromada segment) and the mapping places the monument somewhere
    *    inside that same raion - a mapping result elsewhere is ignored, the
    *    page's raion being the more authoritative signal.
    * Monuments unresolved by both are absent from the map.
    */
  def resolveDetailed(monumentDb: MonumentDB): Map[String, Resolution] = {
    val placeByMonumentId = monumentDb.placeByMonumentId
    // Many monuments share a list page; resolve each distinct page once.
    val fromPageByPage: Map[String, Option[AdmDivision]] =
      monumentDb.monuments.iterator.map(_.page).distinct.map(p => p -> resolveFromPage(p)).toMap
    monumentDb.monuments.flatMap { m =>
      lazy val fromMapping = placeByMonumentId.get(m.id).flatMap(katotthFor)
      val resolved = fromPageByPage(m.page) match {
        case Some(raion) if isRaionLevel(raion) =>
          fromMapping
            .filter(adm => raionAncestor(adm).exists(_.code == raion.code))
            .orElse(Some(raion))
        case fromPage @ Some(_) => fromPage
        case None               => fromMapping
      }
      resolved.map(adm => m.id -> Resolution(adm, m.page))
    }.toMap
  }

  private def isRaionLevel(node: AdmDivision): Boolean =
    node.regionType.exists(rt => Set("P", "B").contains(rt.code))

  /** Monument id -> current-day KATOTTH location. See `resolveDetailed` for
    * the resolution rules; this is that result with the page dropped.
    */
  def resolve(monumentDb: MonumentDB): Map[String, AdmDivision] =
    resolveDetailed(monumentDb).view.mapValues(_.admDivision).toMap

  /** Nearest ancestor of `node` (or `node` itself) that is a raion or a
    * city district ("B" - район в місті, for cities with special status
    * like Kyiv/Sevastopol, which have no hromadas of their own).
    */
  def raionAncestor(node: AdmDivision): Option[AdmDivision] =
    if (node.regionType.exists(rt => Set("P", "B").contains(rt.code))) Some(node)
    else node.parent().flatMap(raionAncestor)

  /** Nearest ancestor of `node` (or `node` itself) that is a hromada. */
  def hromadaAncestor(node: AdmDivision): Option[AdmDivision] =
    if (node.regionType.exists(_.code == "H")) Some(node)
    else node.parent().flatMap(hromadaAncestor)
}
