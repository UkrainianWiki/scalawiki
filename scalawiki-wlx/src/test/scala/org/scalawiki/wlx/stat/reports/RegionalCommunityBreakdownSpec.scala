package org.scalawiki.wlx.stat.reports

import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa
import org.scalawiki.wlx.dto.{AdmDivision, Contest, Monument, RegionType, Region}
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

class RegionalCommunityBreakdownSpec extends Specification {

  val contest = Contest.WLMUkraine(2020)

  val raionType = Some(RegionType("P", Seq("район"), Some(" район")))
  val hromadaType = Some(RegionType("H", Seq("громада"), Some(" громада")))

  val raion: Region = Region("01001", "Раіон", regionType = raionType)
  val hromadaA: Region =
    Region("01001010", "Громада А", regionType = hromadaType, parent = () => Some(raion))
  val hromadaB: Region =
    Region("01001020", "Громада Б", regionType = hromadaType, parent = () => Some(raion))

  def monument(id: String, name: String) =
    new Monument(id = id, name = name, listConfig = Some(WlmUa))

  val monuments = Seq(
    monument("01-xxx-0001", "M1"),
    monument("01-xxx-0002", "M2"),
    monument("01-xxx-0003", "M3"),
    monument("01-xxx-0004", "M4"),
    monument("01-xxx-0005", "M5")
  )

  val monumentDb = new MonumentDB(contest, monuments)
  val monumentIds = monuments.map(_.id).toSet

  val picturedImage =
    Image("File:Img1.jpg", monumentIds = List("01-xxx-0001"), pageId = Some(1L))
  val totalImageDb = new ImageDB(contest, Seq(picturedImage), monumentDb)

  // M1, M2 resolve to hromada A; M3 to hromada B; M4 resolves only to the
  // raion itself (no hromada); M5 has no KATOTTH resolution at all.
  val resolved: Map[String, AdmDivision] = Map(
    "01-xxx-0001" -> hromadaA,
    "01-xxx-0002" -> hromadaA,
    "01-xxx-0003" -> hromadaB,
    "01-xxx-0004" -> raion
  )

  "RegionalCommunityBreakdown.render" should {
    val text = RegionalCommunityBreakdown.render(
      "01",
      monumentIds,
      Set.empty,
      Set.empty,
      resolved,
      Map.empty,
      monumentDb,
      totalImageDb
    )

    "show one row per hromada with current totals only" in {
      text must contain(s"| ${hromadaA.fullName} || 2 || 1 || 50")
      text must contain(s"| ${hromadaB.fullName} || 1 || 0 || 0")
    }

    "not include any per-year columns" in {
      text must not(contain("2020 Objects"))
      text must not(contain("newly pictured"))
    }

    "surface a monument resolved only to the raion as an Unresolved row under that raion" in {
      text must contain(s"==== ${raion.fullName} ====")
      text must contain("| Unresolved || 1 || 0 || 0")
    }

    "surface a monument with no KATOTTH resolution as an oblast-level Unresolved row" in {
      text must contain("==== Unresolved ====")
    }

    "include a Total row summing each raion's table" in {
      // Раіон's table covers hromadaA (M1,M2) + hromadaB (M3) + the raion's
      // own Unresolved bucket (M4) = 4 monuments, 1 pictured.
      text must contain("| Total || 4 || 1 || 25")
    }

    "include a final oblast-level Total section" in {
      text must contain("==== Total ====")
      text must contain(s"| ${RegionalCommunityBreakdown.totalName} || 5 || 1 || 20")
    }

    "list each raion's own total, then Unresolved, then the grand total, in that order" in {
      val totalSection = text.substring(text.indexOf("==== Total ===="))
      val raionRowPos = totalSection.indexOf(s"| ${raion.fullName} || 4 || 1 || 25")
      val unresolvedRowPos = totalSection.indexOf("| Unresolved || 1 || 0 || 0")
      val grandTotalRowPos = totalSection.indexOf(s"| ${RegionalCommunityBreakdown.totalName} || 5 || 1 || 20")

      raionRowPos must be_>=(0)
      unresolvedRowPos must be_>(raionRowPos)
      grandTotalRowPos must be_>(unresolvedRowPos)
    }

    "list the raion-only-resolved and fully-unresolved monuments in the Data issues list, with an explanation" in {
      text must contain("==== Data issues ====")
      text must contain("01-xxx-0004 (M4): Unresolved — resolved to a raion but not to any specific hromada")
      text must contain(
        "01-xxx-0005 (M5): Unresolved — no current raion, hromada, or district could be determined"
      )
    }
  }

  "RegionalCommunityBreakdown.renderRaion with two same-named hromadas" should {
    val twinA: Region =
      Region("01001030", "Громада В", regionType = hromadaType, parent = () => Some(raion))
    val twinB: Region =
      Region("01001040", "Громада В", regionType = hromadaType, parent = () => Some(raion))

    val (text, _) = RegionalCommunityBreakdown.renderRaion(
      raion,
      Set("01-xxx-0001", "01-xxx-0002", "01-xxx-0003"),
      Map("01-xxx-0001" -> twinA, "01-xxx-0002" -> twinA, "01-xxx-0003" -> twinB),
      Map.empty,
      monumentDb,
      totalImageDb
    )

    "keep a separate row for each hromada" in {
      text must contain(s"| ${twinA.fullName} || 2 || 1 || 50")
      text must contain(s"| ${twinB.fullName} || 1 || 0 || 0")
      text must contain("| Total || 3 || 1 || 33")
    }
  }

  "RegionalCommunityBreakdown.render with bad ids" should {
    "list a wrong-prefix id as Bad id and a wrong-oblast-resolution id as Wrong region, with an explanation" in {
      val text = RegionalCommunityBreakdown.render(
        "01",
        monumentIds,
        Set("01-xxx-0006"),
        Set("01-xxx-0007"),
        resolved + ("01-xxx-0007" -> hromadaA),
        Map.empty,
        monumentDb,
        totalImageDb
      )

      text must contain("==== Data issues ====")
      text must contain("01-xxx-0006")
      text must contain("Bad id — this monument's id does not start with this oblast's own numeric code")
      text must contain("01-xxx-0007")
      text must contain(
        "Wrong region — this monument's id looks like it belongs to this oblast, but its own list page actually places it in"
      )
    }

    "link the Wrong region location to the monument's own page, and wikify an embedded monument-name link" in {
      val page = "Real/Page"
      val db = new MonumentDB(
        contest,
        monuments :+ Monument(
          id = "01-xxx-0007",
          name = "[[Some article|Some monument]]",
          page = page,
          listConfig = Some(WlmUa)
        )
      )
      val text = RegionalCommunityBreakdown.render(
        "01",
        monumentIds,
        Set.empty,
        Set("01-xxx-0007"),
        resolved + ("01-xxx-0007" -> hromadaA),
        Map("01-xxx-0007" -> page),
        db,
        totalImageDb
      )

      text must contain(s"[[:uk:$page|${hromadaA.fullName}]]")
      text must contain("[[:uk:Some article|Some monument]]")
    }
  }

  "RegionalCommunityBreakdown.duplicateIdIssues" should {
    "report an id used by more than one monument, listing every page it appears on" in {
      val pageA = "Page/A"
      val pageB = "Page/B"
      val db = new MonumentDB(
        contest,
        Seq(
          Monument(id = "01-xxx-0099", name = "First", page = pageA, listConfig = Some(WlmUa)),
          Monument(id = "01-xxx-0099", name = "Second", page = pageB, listConfig = Some(WlmUa))
        )
      )

      val issues = RegionalCommunityBreakdown.duplicateIdIssues(Set("01-xxx-0099"), db)
      issues must haveSize(1)
      issues.head.id === "01-xxx-0099"
      issues.head.category === "Duplicated id"
      issues.head.explanation must contain(pageA)
      issues.head.explanation must contain(pageB)
    }

    "report nothing for an id used by only one monument" in {
      RegionalCommunityBreakdown.duplicateIdIssues(Set("01-xxx-0001"), monumentDb) must beEmpty
    }
  }

  "RegionalCommunityBreakdown.render with a raion that has no hromada breakdown" should {
    val raionNoHromada: Region = Region("01002", "Раіон без громад", regionType = raionType)
    val m6 = monument("01-xxx-0006", "M6")
    val dbWithNoHromadaRaion = new MonumentDB(contest, monuments :+ m6)
    val idsWithNoHromadaRaion = monumentIds + m6.id
    val resolvedWithNoHromadaRaion = resolved + (m6.id -> raionNoHromada)

    val text = RegionalCommunityBreakdown.render(
      "01",
      idsWithNoHromadaRaion,
      Set.empty,
      Set.empty,
      resolvedWithNoHromadaRaion,
      Map.empty,
      dbWithNoHromadaRaion,
      totalImageDb
    )

    "list it as a row in a shared Raions table, not its own nested section" in {
      text must contain("==== Raions ====")
      text must contain(s"| ${raionNoHromada.fullName} || 1 || 0 || 0")
      text must not(contain(s"==== ${raionNoHromada.fullName} ===="))
    }

    "still render the split raion's own nested section unaffected" in {
      text must contain(s"==== ${raion.fullName} ====")
      text must contain(s"| ${hromadaA.fullName} || 2 || 1 || 50")
    }
  }

  "RegionalCommunityBreakdown.render with exactly one table" should {
    "add the Total row directly into the Raions table, with no separate Total section, when Raions is the only table" in {
      val raionNoHromada: Region = Region("01002", "Раіон без громад", regionType = raionType)
      val ids = Set("01-xxx-0001", "01-xxx-0002")
      val db = new MonumentDB(contest, monuments.filter(m => ids.contains(m.id)))
      val text = RegionalCommunityBreakdown.render(
        "01",
        ids,
        Set.empty,
        Set.empty,
        ids.map(_ -> raionNoHromada).toMap,
        Map.empty,
        db,
        totalImageDb
      )

      text must contain("==== Raions ====")
      text must contain(s"| ${raionNoHromada.fullName} || 2 || 1 || 50")
      text must contain("| Total || 2 || 1 || 50")
      text must not(contain("==== Total ===="))
    }

    "not add a separate Total section when a single split raion's own table already has one" in {
      val ids = Set("01-xxx-0001", "01-xxx-0002", "01-xxx-0003")
      val db = new MonumentDB(contest, monuments.filter(m => ids.contains(m.id)))
      val singleRaionResolved: Map[String, AdmDivision] =
        Map("01-xxx-0001" -> hromadaA, "01-xxx-0002" -> hromadaA, "01-xxx-0003" -> hromadaB)
      val text = RegionalCommunityBreakdown.render(
        "01",
        ids,
        Set.empty,
        Set.empty,
        singleRaionResolved,
        Map.empty,
        db,
        totalImageDb
      )

      text must contain(s"| Total || 3 || 1 || 33")
      text must not(contain("==== Total ===="))
    }
  }

  "RegionalCommunityBreakdown.totals" should {
    "compute in-list count, pictured count and percentage" in {
      RegionalCommunityBreakdown.totals(monumentIds, monumentDb, totalImageDb) === ((5, 1, 20))
    }

    "return 0 percentage for an empty set" in {
      RegionalCommunityBreakdown.totals(Set.empty, monumentDb, totalImageDb) === ((0, 0, 0))
    }
  }

  "RegionalCommunityBreakdown.linkedName" should {
    val page1 = "Page/A"
    val page2 = "Page/B"

    "link to the monument's page when exactly one page is present" in {
      RegionalCommunityBreakdown.linkedName("Name", Set("01-xxx-0010"), Map("01-xxx-0010" -> page1)) ===
        s"[[:uk:$page1|Name]]"
    }

    "link to the alphabetically first page when several are present" in {
      val pageById = Map("01-xxx-0011" -> page2, "01-xxx-0012" -> page1)
      RegionalCommunityBreakdown.linkedName("Name", Set("01-xxx-0011", "01-xxx-0012"), pageById) ===
        s"[[:uk:$page1|Name]]"
    }

    "prefer a page that actually names the region over an unrelated one that sorts first" in {
      // Real case: a hromada's monuments can include some only resolved
      // into it via the numeric fallback from an unrelated thematic page
      // (e.g. "Єврейська спадщина", which sorts alphabetically before most
      // oblast names) - that page must not win the link just by sorting
      // first; the page that actually names the hromada must be preferred.
      val realPage =
        "Вікіпедія:Вікі любить пам'ятки/Чернівецька область/Чернівецький район/Новоселицька громада"
      val thematicPage = "Вікіпедія:Вікі любить пам'ятки/Єврейська спадщина/Чернівецька область"
      val pageById = Map("73-xxx-0001" -> realPage, "73-xxx-0002" -> thematicPage)
      RegionalCommunityBreakdown.linkedName(
        "Новоселицька громада",
        Set("73-xxx-0001", "73-xxx-0002"),
        pageById
      ) === s"[[:uk:$realPage|Новоселицька громада]]"
    }

    "fall back to the plain name when no monument has a page" in {
      RegionalCommunityBreakdown.linkedName("Name", Set("01-xxx-0013"), Map.empty) === "Name"
    }
  }
}
