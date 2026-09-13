package org.scalawiki.wlx.stat.reports

import org.scalawiki.util.TestUtils.resourceAsString
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

/** Exercises [[MonumentsPicturedByRegion.communityBreakdown]] - the text
  * written to an oblast's regional-detail wiki page - against real KATOTTH
  * resolution data, spanning two oblasts and a monument with no resolvable
  * place. This is the same string [[MonumentsPicturedByRegion.asText]]
  * would send to the wiki; it is tested directly here rather than through
  * `Output.regionalStat`'s live `bot.page(...).edit(...)` call, since this
  * suite has no wiki-write mocking to intercept that with.
  */
class MonumentsPicturedByRegionCommunityBreakdownSpec extends Specification {

  val contest = Contest.WLMUkraine(2020)

  def burshtynMonuments: Seq[Monument] = {
    val wiki = resourceAsString("/org/scalawiki/wlx/Burshtyn.wiki")
    val page = "Вікіпедія:Вікі любить пам'ятки/Івано-Франківська область/Бурштин"
    Monument.monumentsFromText(wiki, page, WlmUa.templateName, WlmUa).toSeq
  }

  val unresolvableMonument = Monument(
    id = "01-xxx-0001",
    name = "Unresolvable",
    city = Some("Неіснуюче село"),
    listConfig = Some(WlmUa)
  )

  val monumentDb = new MonumentDB(contest, burshtynMonuments :+ unresolvableMonument)
  val totalImageDb = new ImageDB(contest, Seq.empty, monumentDb)

  val stat =
    ContestStat(contest, contest.year, Some(monumentDb), totalImageDb, totalImageDb, Nil)

  "MonumentsPicturedByRegion.communityBreakdown" should {
    "show the current-day hromada for a real oblast, with no per-year columns" in {
      val ivanoFrankivsk =
        contest.country.regions.find(_.name.startsWith("Івано-Франківська")).get
      val report = new MonumentsPicturedByRegion(stat, regionParam = Some(ivanoFrankivsk))

      val text = report.communityBreakdown
      text must contain("Бурштин")
      text must not(contain("2020 Objects"))
    }

    "surface an oblast-level Unresolved row for a monument with no resolvable place" in {
      val crimea = contest.country.regions.find(_.code == "01").get
      val report = new MonumentsPicturedByRegion(stat, regionParam = Some(crimea))

      val text = report.communityBreakdown
      text must contain("==== Unresolved ====")
      text must contain("| Unresolved || 1 || 0 || 0")
    }

    "group a Crimean monument by its raion instead of the oblast-level Unresolved bucket" in {
      // Crimea has no hromada level in KATOTTH; a monument whose page names
      // a real raion should be grouped there, not dumped in Unresolved.
      val crimeaRaionMonument = Monument(
        id = "01-xxx-0002",
        name = "In Bakhchysarai raion",
        page = "Вікіпедія:Вікі любить пам'ятки/Автономна Республіка Крим/Бахчисарайський район",
        listConfig = Some(WlmUa)
      )
      val mdb = new MonumentDB(contest, Seq(crimeaRaionMonument))
      val imgDb = new ImageDB(contest, Seq.empty, mdb)
      val localStat = ContestStat(contest, contest.year, Some(mdb), imgDb, imgDb, Nil)

      val crimea = contest.country.regions.find(_.code == "01").get
      val report = new MonumentsPicturedByRegion(localStat, regionParam = Some(crimea))

      val text = report.communityBreakdown
      text must contain("==== Raions ====")
      text must contain(
        "| [[:uk:Вікіпедія:Вікі любить пам'ятки/Автономна Республіка Крим/Бахчисарайський район|Бахчисарайський район]] || 1 || 0 || 0"
      )
      text must not(contain("==== Unresolved ===="))
      text must contain("| Total || 1 || 0 || 0")
    }

    "group a Kyiv monument by its city district instead of the oblast-level Unresolved bucket" in {
      val kyivDistrictMonument = Monument(
        id = "80-xxx-0001",
        name = "In Darnytskyi district",
        page = "Вікіпедія:Вікі любить пам'ятки/Київ/Дарницький район",
        listConfig = Some(WlmUa)
      )
      val mdb = new MonumentDB(contest, Seq(kyivDistrictMonument))
      val imgDb = new ImageDB(contest, Seq.empty, mdb)
      val localStat = ContestStat(contest, contest.year, Some(mdb), imgDb, imgDb, Nil)

      val kyiv = contest.country.regions.find(_.name == "Київ").get
      val report = new MonumentsPicturedByRegion(localStat, regionParam = Some(kyiv))

      val text = report.communityBreakdown
      text must contain("==== Raions ====")
      text must contain(
        "| [[:uk:Вікіпедія:Вікі любить пам'ятки/Київ/Дарницький район|Дарницький район]] || 1 || 0 || 0"
      )
      text must not(contain("==== Unresolved ===="))
    }

    "report a bad-id monument as Bad IDs, not as belonging to the oblast Monument.getRegionId falls back to" in {
      // Real case found in csv-cache/wlm-UA-monuments.csv: a "99-…" special-
      // nomination id (Jewish Heritage sub-list) whose id doesn't start with
      // a real oblast code, but whose middle segment ("230") coincidentally
      // starts with "23" (Zaporizhzhia) - Monument.getRegionId's fallback
      // then misattributes it there, even though the monument is actually
      // in Chernivtsi oblast. It must show up as a Bad ID under whichever
      // oblast byRegion happened to bucket it into, not as a normal or
      // Unresolved entry.
      val badIdMonument = Monument(
        id = "99-230-0001",
        name = "Bad id special nomination monument",
        page = "Вікіпедія:Вікі любить пам'ятки/Єврейська спадщина/Чернівецька область",
        city = Some("[[Бояни]]"),
        listConfig = Some(WlmUa)
      )
      val mdb = new MonumentDB(contest, Seq(badIdMonument))
      Monument.getRegionId(badIdMonument.id) === "23"
      mdb.isIdCorrect(badIdMonument.id) must beTrue

      val imgDb = new ImageDB(contest, Seq.empty, mdb)
      val localStat = ContestStat(contest, contest.year, Some(mdb), imgDb, imgDb, Nil)

      val zaporizhzhia = contest.country.regions.find(_.code == "23").get
      val report = new MonumentsPicturedByRegion(localStat, regionParam = Some(zaporizhzhia))

      val text = report.communityBreakdown
      text must contain("==== Data issues ====")
      text must contain("99-230-0001 (Bad id special nomination monument): Bad id")
      text must not(contain("==== Unresolved ===="))
    }

    "report a monument whose id-prefix matches this oblast but whose page names a different oblast as a Bad ID" in {
      // Real case found in csv-cache/wlm-UA-monuments.csv: a handful of WWII
      // fortification-point ids ("80-386-…", Kyiv-city-style numbering)
      // are listed on a Kyiv-*oblast* raion/hromada page (Бучанський
      // район/Ірпінська громада). Their id's own prefix ("80") passes the
      // oblast-membership check for Kyiv city, but their page resolves to a
      // Kyiv-oblast (code "32") hromada - a different oblast entirely. That
      // must surface as a Bad ID under Kyiv city, not as a bogus "Бучанський
      // район" section inside Kyiv city's own breakdown.
      val strayFortificationPoint = Monument(
        id = "80-386-0105",
        name = "ДОТ № 428",
        page = "Вікіпедія:Вікі любить пам'ятки/Київська область/Бучанський район/Ірпінська громада",
        listConfig = Some(WlmUa)
      )
      val kyivDistrictMonument = Monument(
        id = "80-386-0001",
        name = "A real Kyiv district monument",
        page = "Вікіпедія:Вікі любить пам'ятки/Київ/Печерський район",
        listConfig = Some(WlmUa)
      )
      val mdb = new MonumentDB(contest, Seq(strayFortificationPoint, kyivDistrictMonument))
      val imgDb = new ImageDB(contest, Seq.empty, mdb)
      val localStat = ContestStat(contest, contest.year, Some(mdb), imgDb, imgDb, Nil)

      val kyiv = contest.country.regions.find(_.name == "Київ").get
      val report = new MonumentsPicturedByRegion(localStat, regionParam = Some(kyiv))

      val text = report.communityBreakdown
      text must contain("==== Data issues ====")
      text must contain("80-386-0105 (ДОТ № 428): Wrong region")
      text must contain("Ірпінська громада")
      text must not(contain("Бучанський район ===="))
      text must contain("Печерський район")
    }

    "link each Sevastopol district to its own page even when a duplicated id is shared with another district" in {
      // Real case: WLM id 85-369-0198 is duplicated across Sevastopol's
      // Нахімовський and Балаклавський district lists. Whichever monument
      // wins that id's slot in the resolution map, both districts' *other*,
      // unambiguous monuments must still link to their own correct page -
      // Nakhimov's link must never point at Balaklava's page or vice versa.
      val nakhimovPage = "Вікіпедія:Вікі любить пам'ятки/Севастополь/Нахімовський район"
      val balaklavaPage = "Вікіпедія:Вікі любить пам'ятки/Севастополь/Балаклавський район"
      def monumentOn(id: String, page: String) =
        Monument(id = id, name = id, page = page, listConfig = Some(WlmUa))

      val monuments =
        (1 to 3).map(i => monumentOn(f"85-369-0$i%03d", nakhimovPage)) ++
          (1 to 3).map(i => monumentOn(f"85-369-1$i%03d", balaklavaPage)) ++
          Seq(monumentOn("85-369-0198", nakhimovPage), monumentOn("85-369-0198", balaklavaPage))

      val mdb = new MonumentDB(contest, monuments)
      val imgDb = new ImageDB(contest, Seq.empty, mdb)
      val localStat = ContestStat(contest, contest.year, Some(mdb), imgDb, imgDb, Nil)

      val sevastopol = contest.country.regions.find(_.name == "Севастополь").get
      val report = new MonumentsPicturedByRegion(localStat, regionParam = Some(sevastopol))

      // Whichever monument wins the duplicated id, each district's row must
      // link to its own page - never to the other's - regardless of the
      // exact resulting count.
      val text = report.communityBreakdown
      text must contain(s"[[:uk:$nakhimovPage|Нахімовський район]] || ")
      text must contain(s"[[:uk:$balaklavaPage|Балаклавський район]] || ")
      text must not(contain(s"[[:uk:$balaklavaPage|Нахімовський район]]"))
      text must not(contain(s"[[:uk:$nakhimovPage|Балаклавський район]]"))

      // The duplicated id itself is also surfaced as an actionable issue.
      text must contain("==== Data issues ====")
      text must contain("85-369-0198")
      text must contain("Duplicated id")
    }

    "group the Chornobyl Exclusion Zone under its own name, nested in Вишгородський raion, not Unresolved" in {
      val exclusionZoneMonument = Monument(
        id = "32-100-0001",
        name = "In the exclusion zone",
        page = "Вікіпедія:Вікі любить пам'ятки/Київська область/Вишгородський район/ЧЗВ",
        listConfig = Some(WlmUa)
      )
      val realHromadaMonument = Monument(
        id = "32-100-0002",
        name = "In a real hromada",
        page = "Вікіпедія:Вікі любить пам'ятки/Київська область/Вишгородський район/Димерська громада",
        listConfig = Some(WlmUa)
      )
      val mdb = new MonumentDB(contest, Seq(exclusionZoneMonument, realHromadaMonument))
      val imgDb = new ImageDB(contest, Seq.empty, mdb)
      val localStat = ContestStat(contest, contest.year, Some(mdb), imgDb, imgDb, Nil)

      val kyivOblast = contest.country.regions.find(_.name.startsWith("Київська")).get
      val report = new MonumentsPicturedByRegion(localStat, regionParam = Some(kyivOblast))

      val text = report.communityBreakdown
      text must contain("|Вишгородський район]] ====")
      text must contain(
        "[[:uk:Вікіпедія:Вікі любить пам'ятки/Київська область/Вишгородський район/ЧЗВ|Зона відчуження ЧАЕС]] || 1 || 0 || 0"
      )
      text must not(contain("==== Unresolved ===="))
      text must not(contain("==== Data issues ===="))
    }
  }
}
