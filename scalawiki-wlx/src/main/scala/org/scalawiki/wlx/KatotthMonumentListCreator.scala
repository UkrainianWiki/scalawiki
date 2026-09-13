package org.scalawiki.wlx

import org.scalawiki.MwBot
import org.scalawiki.dto.markup.Table
import org.scalawiki.wlx.dto.{
  AdmDivision,
  Contest,
  Country,
  Katotth,
  Koatuu,
  Monument,
  RegionType
}
import org.scalawiki.wlx.query.MonumentQuery
import org.scalawiki.wlx.stat.reports.Output

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Koatuu2Katotth(
    koatuu: Option[String],
    katotth: Option[AdmDivision],
    monumentIds: Seq[String] /*, candidates: Candidate*/
)

object KatotthMonumentListCreator {

  val UkraineKatotth: Country = KatotthResolver.country
  val regionsKatotth = UkraineKatotth.regions
  val katotthMap = KatotthResolver.katotthMap

  val UkraineKoatuu: Country = new Country(
    "UA",
    "Ukraine",
    Seq("uk"),
    Koatuu.regionsNew(() => Some(UkraineKoatuu))
  )
  val regionsKoatuu = UkraineKoatuu.regions
  val koatuuMap = UkraineKoatuu.mapByCode

  val parentCodes = Set("H", "P", "K", "O")

  def main(args: Array[String]): Unit = {
    val ukWiki = MwBot.fromHost(MwBot.ukWiki)
    val contest = Contest.WLMUkraine(2021)
    val query = MonumentQuery.create(contest)
    val monumentDB = MonumentDB.getMonumentDbBlocking(contest, query)
    val sequence: Seq[Koatuu2Katotth] = getMapping(monumentDB)

    val (mapped, unmapped) = sequence.partition(_.katotth.nonEmpty)
    val grouped = groupByAdm(mapped)
    reportUnmapped(
      monumentDB,
      unmapped
        .filter(_.koatuu.nonEmpty)
        .filterNot(_.koatuu.contains("80"))
        .filterNot(_.koatuu.contains("85"))
    )

    val citiRegions = Set("51100370000040590", "12080090000039979")
    val village1Regions = Set("51100390000087217", "59080130000022249")
    val village2Regions = Set("12080110000055958", "59080150000013842")

    Future.sequence(grouped.map { case (adm, k2k) =>
      if (
        !Set("Автономна Республіка Крим", "Київ", "Севастополь")
          .contains(adm.namesList.tail.head)
      ) {
        val pageNameGuess =
          s"Вікіпедія:Вікі любить пам'ятки/новий АТУ/${adm.namesList.tail.mkString("/")}"

        val newPageName = if (citiRegions.contains(adm.code)) {
          pageNameGuess.replace("громада", "міська громада")
        } else if (village1Regions.contains(adm.code)) {
          pageNameGuess.replace("громада", "селищна громада")
        } else if (village2Regions.contains(adm.code)) {
          pageNameGuess.replace("громада", "сільська громада")
        } else {
          pageNameGuess
        }

        val (cities, other) =
          k2k.partition(_.katotth.exists(_.regionType.exists(_.code == "М")))
        val cityMonumentIds = cities.flatMap(_.monumentIds).toSet
        val otherMonumentIds = other.flatMap(_.monumentIds).toSet

        val header = "{{ВЛП нові списки}}\n" +
          s"{{Вікіпедія:Вікі любить пам'ятки/новий АТУ/${adm.namesList.tail.head}/navbar}}\n" +
          "{{WLM-шапка}}\n"
        val monuments =
          monumentDB.monuments.filter(m => cityMonumentIds.contains(m.id)) ++
            monumentDB.monuments.filter(m => otherMonumentIds.contains(m.id))

        def monumentPage(monument: Monument) = {
          val oldPage = monument.page
          if (oldPage.last == ')') {
            val suffix = oldPage.substring(oldPage.lastIndexOf("("))
            val koatuu =
              Katotth.toKoatuu.get(adm.code).map(_.take(5)).getOrElse("")
            if (
              Set(
                "63101",
                "12101",
                "51101",
                "48101",
                "35101",
                "46101",
                "26101",
                "73101",
                "53240"
              ).contains(koatuu)
            ) {
              newPageName + s" $suffix"
            } else {
              newPageName
            }
          } else newPageName
        }
        val bySubPage = monuments.groupBy(monumentPage)

        val subPageFutures = bySubPage.map { case (page, pageMonuments) =>
          val koatuuPages = pageMonuments.map(_.page).distinct.sorted
          val pageText = header + pageMonuments
            .map(_.asWiki(Some("WLM-рядок")))
            .mkString("") + "\n|}" +
            "\n== Взято з ==\n" + koatuuPages
              .map(page => s"* [[$page]]")
              .mkString("\n") +
            "\n== Примітки ==\n{{reflist}}"

          if (pageText.length > 1000 * 1000) {
            println(s" $page ${pageText.length}")
          }
          // Future.successful()
          ukWiki.page(page).edit(pageText)
        }
        Future.sequence(subPageFutures)
      } else Future.sequence(Seq(Future.successful()))
    })
  }

  def groupByAdm(
      sequence: Seq[Koatuu2Katotth]
  ): Map[AdmDivision, Seq[Koatuu2Katotth]] = {
    sequence.groupBy { k2k =>
      val k = k2k.katotth.get
      var groupK = k

      while (
        !groupK.regionType.exists(rt => parentCodes.contains(rt.code)) && groupK
          .parent()
          .nonEmpty
      ) {
        groupK = groupK.parent().get
      }
      groupK
    }
  }

  def reportUnmapped(mDb: MonumentDB, sequence: Seq[Koatuu2Katotth]) = {
    Output.unknownPlaces(mDb)
    val byKoatuu = sequence
      .map { k2k =>
        k2k.koatuu.get -> k2k.monumentIds
      }
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))
      .toMap

    val table = Table(
      Seq("koatuu", "monumentIds"),
      byKoatuu.toSeq.sortBy(_._1).map { case (koatuu, ids) =>
        Seq(koatuu, ids.mkString(", "))
      }
    )

    val text = table.asWiki
    val pageName = s"Вікіпедія:${mDb.contest.contestType.name}/unmappedPlaces"
    MwBot.fromHost(MwBot.ukWiki).page(pageName).edit(text, Some("updating"))
  }

  def getMapping(monumentDB: MonumentDB): Seq[Koatuu2Katotth] = {
    val placeByMonumentId = monumentDB.placeByMonumentId

    monumentDB.monuments.map { m =>
      val koatuuOpt = placeByMonumentId.get(m.id)
      val katotthOpt = koatuuOpt.flatMap(KatotthResolver.katotthFor)

      Koatuu2Katotth(koatuuOpt, katotthOpt, List(m.id))
    }
  }
}
