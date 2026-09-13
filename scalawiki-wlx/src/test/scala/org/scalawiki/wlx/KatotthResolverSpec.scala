package org.scalawiki.wlx

import org.scalawiki.util.TestUtils.resourceAsString
import org.scalawiki.wlx.dto.Contest
import org.scalawiki.wlx.dto.Monument
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa
import org.specs2.mutable.Specification

class KatotthResolverSpec extends Specification {

  val contest = Contest.WLMUkraine(2020)

  def burshtynMonumentDb: MonumentDB = {
    val burshtynWiki = resourceAsString("/org/scalawiki/wlx/Burshtyn.wiki")
    val page =
      "Вікіпедія:Вікі любить пам'ятки/Івано-Франківська область/Бурштин"
    val monuments = Monument
      .monumentsFromText(burshtynWiki, page, WlmUa.templateName, WlmUa)
      .toSeq
    new MonumentDB(contest, monuments)
  }

  "KatotthResolver.country" should {
    "expose an oblast -> raion -> hromada hierarchy" in {
      val oblast =
        KatotthResolver.country.regions.find(_.name.startsWith("Івано-Франківська")).get
      val raions = oblast.regions.filter(_.regionType.exists(_.code == "P"))
      raions must not(beEmpty)

      val hromadas = raions.flatMap(_.regions).filter(_.regionType.exists(_.code == "H"))
      hromadas must not(beEmpty)
    }
  }

  "KatotthResolver.resolve" should {
    "match KatotthMonumentListCreator.getMapping for a fixture list" in {
      val mdb = burshtynMonumentDb
      val mapping = KatotthMonumentListCreator.getMapping(mdb)
      val expected = mapping.flatMap(k2k => k2k.katotth.map(k2k.monumentIds.head -> _)).toMap

      val resolved = KatotthResolver.resolve(mdb)

      resolved.keySet === expected.keySet
      resolved.forall { case (id, adm) => expected(id).code == adm.code } must beTrue
    }

    "leave unresolvable monuments out of the map" in {
      val mdb = burshtynMonumentDb
      val resolved = KatotthResolver.resolve(mdb)
      val unknownId = "99-999-9999"
      resolved.contains(unknownId) must beFalse
    }
  }

  "KatotthResolver raion/hromada ancestors" should {
    "find the raion and hromada containing a resolved monument" in {
      val mdb = burshtynMonumentDb
      val resolved = KatotthResolver.resolve(mdb)
      resolved must not(beEmpty)

      val node = resolved.values.head
      val raion = KatotthResolver.raionAncestor(node)
      val hromada = KatotthResolver.hromadaAncestor(node)

      raion must beSome
      hromada must beSome
      raion.get.regionType.map(_.code) === Some("P")
      hromada.get.regionType.map(_.code) === Some("H")
    }
  }

  "KatotthResolver.parsePage" should {
    "recognize a plain hromada page" in {
      val parsed = KatotthResolver.parsePage(
        "Вікіпедія:Вікі любить пам'ятки/Київська область/Білоцерківський район/Ковалівська громада"
      )
      parsed.oblastSeg === Some("Київська область")
      parsed.raionStem === Some("Білоцерківський")
      parsed.hromadaStem === Some("Ковалівська")
      parsed.hromadaAdjective === None
    }

    "strip a trailing alphabetic-split parenthetical and keep the type adjective" in {
      val parsed = KatotthResolver.parsePage(
        "Вікіпедія:Вікі любить пам'ятки/Сумська область/Сумський район/Миколаївська сільська громада"
      )
      parsed.hromadaStem === Some("Миколаївська")
      parsed.hromadaAdjective === Some("сільська")

      val split = KatotthResolver.parsePage(
        "Вікіпедія:Вікі любить пам'ятки/Одеська область/Одеський район/Одеська громада (А—Г)"
      )
      split.hromadaStem === Some("Одеська")
    }

    "recognize a page with no hromada segment" in {
      KatotthResolver.parsePage("Вікіпедія:Вікі любить пам'ятки/Київ/Дарницький район").hromadaStem === None
      KatotthResolver.parsePage("Вікіпедія:Вікі любить пам'ятки/Завантажувач").hromadaStem === None
    }
  }

  "KatotthResolver.resolveFromPage" should {
    "resolve a real hromada from its page title" in {
      val resolved = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Київська область/Білоцерківський район/Ковалівська громада"
      )
      resolved must beSome
      resolved.get.name === "Ковалівська"
      resolved.get.regionType.map(_.code) === Some("H")
    }

    "scope by raion so a same-named hromada in a different raion doesn't cause a false ambiguity" in {
      // Дніпропетровська has "Миколаївська" hromadas in both Дніпровський and
      // Синельниківський raions - the raion segment must pick the right one.
      val resolved = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Дніпропетровська область/Дніпровський район/Миколаївська громада"
      )
      resolved must beSome
      resolved.get.code === "12020090000044548"
    }

    "disambiguate a genuine same-raion name collision using the seat-settlement type" in {
      // Real collision: Сумський raion has two distinct "Миколаївська"
      // hromadas, one selyshche-seated, one village-seated.
      val selyshchna = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Сумська область/Сумський район/Миколаївська селищна громада"
      )
      val silska = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Сумська область/Сумський район/Миколаївська сільська громада"
      )
      selyshchna must beSome
      silska must beSome
      selyshchna.get.code === "59080130000022249"
      silska.get.code === "59080150000013842"
      selyshchna.get.code !== silska.get.code
    }

    "give up rather than guess when the collision has no disambiguating adjective" in {
      val resolved = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Сумська область/Сумський район/Миколаївська громада"
      )
      resolved must beNone
    }

    "never match a same-named hromada in an unrelated oblast" in {
      // "Ковалівська" only exists as a hromada under Київська oblast; a page
      // naming it under a different, unrelated oblast must not be guessed
      // at by matching the name anywhere in the country.
      val resolved = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Одеська область/Одеський район/Ковалівська громада"
      )
      resolved must beNone
    }

    "resolve Kyiv/Sevastopol pages to their city district, not Unresolved" in {
      // Kyiv and Sevastopol have no hromadas of their own - a "район" page
      // there names a city district ("B"), which is the most specific
      // level available and should not be treated as unresolved.
      val kyiv = KatotthResolver.resolveFromPage("Вікіпедія:Вікі любить пам'ятки/Київ/Дарницький район")
      val sevastopol =
        KatotthResolver.resolveFromPage("Вікіпедія:Вікі любить пам'ятки/Севастополь/Балаклавський район")
      kyiv must beSome
      sevastopol must beSome
      kyiv.get.name === "Дарницький"
      kyiv.get.regionType.map(_.code) === Some("B")
      sevastopol.get.name === "Балаклавський"
      sevastopol.get.regionType.map(_.code) === Some("B")
    }

    "resolve a Crimean or not-yet-hromada-split raion page to its raion" in {
      // Occupied Crimea's admin division has no hromada level in KATOTTH;
      // a couple of mainland raions (e.g. Berehove, Khmelnytskyi) also
      // haven't had their monument lists split into hromada pages yet.
      // Both should group by raion rather than fall to Unresolved.
      val crimea =
        KatotthResolver.resolveFromPage("Вікіпедія:Вікі любить пам'ятки/Автономна Республіка Крим/Бахчисарайський район")
      val notYetSplit =
        KatotthResolver.resolveFromPage("Вікіпедія:Вікі любить пам'ятки/Закарпатська область/Берегівський район")
      crimea must beSome
      notYetSplit must beSome
      crimea.get.name === "Бахчисарайський"
      crimea.get.regionType.map(_.code) === Some("P")
      notYetSplit.get.name === "Берегівський"
      notYetSplit.get.regionType.map(_.code) === Some("P")
    }

    "still return None for a page naming no raion at all" in {
      KatotthResolver.resolveFromPage("Вікіпедія:Вікі любить пам'ятки/Завантажувач") must beNone
    }

    "resolve the Chornobyl Exclusion Zone page to its own named group, not the raion generically" in {
      // ЧЗВ (Чорнобильська зона відчуження) is nominally listed under
      // Вишгородський raion's page, but it isn't a real hromada - it's a
      // distinct, well-known area that deserves its own group rather than
      // being folded into that raion's generic "resolved to raion only"
      // Unresolved bucket.
      val resolved = KatotthResolver.resolveFromPage(
        "Вікіпедія:Вікі любить пам'ятки/Київська область/Вишгородський район/ЧЗВ"
      )
      resolved must beSome
      resolved.get.fullName === "Зона відчуження ЧАЕС"
      resolved.get.regionType.map(_.code) === Some("H")
      KatotthResolver.raionAncestor(resolved.get).map(_.name) === Some("Вишгородський")
      KatotthResolver.hromadaAncestor(resolved.get) === resolved
    }
  }

  "KatotthResolver.resolve wiring" should {
    "prefer the page-title resolution over the numeric fallback when both could apply" in {
      val monuments = Seq(
        new Monument(
          id = "32-214-0030",
          name = "Test",
          page = "Вікіпедія:Вікі любить пам'ятки/Київська область/Білоцерківський район/Ковалівська громада",
          listConfig = Some(WlmUa)
        )
      )
      val mdb = new MonumentDB(contest, monuments)
      val resolved = KatotthResolver.resolve(mdb)
      resolved.get("32-214-0030").map(_.name) === Some("Ковалівська")
    }

    "fall back to the numeric mapping when the page has no hromada segment" in {
      // The Burshtyn fixture's page names only the oblast and city, not a
      // hromada - resolution must still succeed via the numeric fallback.
      val mdb = burshtynMonumentDb
      val resolved = KatotthResolver.resolve(mdb)
      resolved must not(beEmpty)
      resolved.values.forall(_.name == "Бурштин") must beTrue
    }
  }

  "KatotthResolver.resolveDetailed" should {
    "keep the resolved page consistent with the resolved division when two monuments share one id" in {
      // Real case: WLM id 85-369-0198 is duplicated by a data-entry error
      // across Sevastopol's Нахімовський and Балаклавський district lists.
      // Only one can occupy that id's slot; resolveDetailed must resolve
      // both its division and its page from the *same* winning Monument,
      // so a page lookup for that id never points at the other district.
      val duplicateId = "85-369-0001"
      val nakhimovMonument = new Monument(
        id = duplicateId,
        name = "Nakhimov",
        page = "Вікіпедія:Вікі любить пам'ятки/Севастополь/Нахімовський район",
        listConfig = Some(WlmUa)
      )
      val balaklavaMonument = new Monument(
        id = duplicateId,
        name = "Balaklava",
        page = "Вікіпедія:Вікі любить пам'ятки/Севастополь/Балаклавський район",
        listConfig = Some(WlmUa)
      )
      val mdb = new MonumentDB(contest, Seq(nakhimovMonument, balaklavaMonument))

      val detailed = KatotthResolver.resolveDetailed(mdb)
      val resolution = detailed(duplicateId)

      // Whichever monument won, its own page resolves back to the same
      // division - never a mix of one monument's division and the other's
      // page.
      KatotthResolver.resolveFromPage(resolution.page) === Some(resolution.admDivision)
    }
  }
}
