package org.scalawiki.wlx

import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.query.MonumentQuery
import org.scalawiki.MwBot

import scala.collection.immutable.SortedSet

object SubsetCreator {

  import scala.concurrent.ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {

    val specialNomination = "дерев'яна архітектура"

    val contest = Contest.WLMUkraine(2025)
    val query = MonumentQuery.create(contest)
    query.byMonumentTemplate(contest.listTemplate.get).map { monuments =>
      createSubset(
        monuments,
        contest,
        specialNomination,
        m => {
          val lowerCaseName = m.name.toLowerCase
          val lowerCaseNameDetail = m.nameDetail.getOrElse("").toLowerCase
          Seq("дер").exists(p => lowerCaseName.contains(p) || lowerCaseNameDetail.contains(p))
        }
      )
    }
  }

  def createSubset(
      monuments: Iterable[Monument],
      contest: Contest,
      specialNomination: String,
      monumentFilter: Monument => Boolean
  ): Unit = {
    val ukWiki = MwBot.fromHost("uk.wikipedia.org")

    val subset = monuments.filter(monumentFilter)

    val byRegion = subset.groupBy(m => Monument.getRegionId(m.id))

    val regionIds = SortedSet(byRegion.keys.toSeq: _*)

    val regionOnPage = false

    try {
      regionPerPage(ukWiki, specialNomination, contest, byRegion, regionIds)
      //          regionsOnPage(ukWiki, specialNomination, contest, byRegion, regionIds)
    } catch {
      case t: Throwable =>
        println(t)
        throw t
    }

  }

  def regionsOnPage(
      ukWiki: MwBot,
      specialNomination: String,
      contest: Contest,
      byRegion: Map[String, Seq[Monument]],
      regionIds: SortedSet[String]
  ): Unit = {
    val buf = new StringBuffer
    buf.append("__TOC__\n")

    for (regionId <- regionIds) {

      val regionTitle = contest.country.regionName(regionId)
      val regionLink = "Вікіпедія:Вікі любить пам'ятки/" + regionTitle

      buf.append(s"\n== $regionTitle ==\n")

      buf.append("{{WLM-шапка}}")
      val regionMonuments =
        byRegion(regionId).filterNot(_.page.contains(specialNomination))

      val byPage = regionMonuments.groupBy(_.page)
      val pages = SortedSet(byPage.keys.toSeq: _*)

      for (page <- pages) {
        val title = page.replace("Вікіпедія:Вікі любить пам'ятки/", "")
        buf.append(
          s"|-\n|colspan=9 bgcolor=lightyellow|\n=== [[$page|$title]] ===\n|-\n"
        )
        byPage(page).foreach { monument =>
          val text = monument
            .asWiki()
            .split("\\|\\}")(0)
            .replace("{{ВЛП-рядок", "{{WLM-рядок")
          buf.append(text)
        }
      }
      buf.append("\n|}")

    }

    val s = buf.toString

    // ukWiki.page(regionLink +" дерев'яна архітектура").edit(s, s"$regionTitle - дерев'яна архітектура")
    ukWiki
      .page(s"Вікіпедія:Вікі любить пам'ятки $specialNomination")
      .edit(s, Some(specialNomination))

  }

  def regionPerPage(
      ukWiki: MwBot,
      specialNomination: String,
      contest: Contest,
      byRegion: Map[String, Iterable[Monument]],
      regionIds: SortedSet[String]
  ): Unit = {
    for (regionId <- regionIds) {

      val regionTitle = contest.country.regionName(regionId)
      val regionLink =
        "Вікіпедія:Вікі любить пам'ятки/Пам'ятки дерев'яної архітектури України/" + regionTitle

      val regionMonuments =
        byRegion(regionId).filterNot(_.page.contains(specialNomination))

      val byPage = regionMonuments.groupBy(_.page)
      val pages = SortedSet(byPage.keys.toSeq: _*)

      val buf = new StringBuffer
      buf.append(s"{{WLM $specialNomination}}\n__TOC__\n")
      buf.append("{{WLM-шапка}}")

      for (page <- pages) {
        val title = page.replace("Вікіпедія:Вікі любить пам'ятки/", "")
        buf.append(
          s"|-\n|colspan=9 bgcolor=lightyellow|\n=== [[$page|$title]] ===\n|-\n"
        )
        byPage(page).foreach { monument =>
          val text = monument
            .asWiki()
            .split("\\|\\}")(0)
            .replace("{{ВЛП-рядок", "{{WLM-рядок")
          buf.append(text)
        }
      }
      buf.append("\n|}")
      val s = buf.toString

      // ukWiki.page(regionLink +" дерев'яна архітектура").edit(s, s"$regionTitle - дерев'яна архітектура")
      ukWiki
        .page(s"$regionLink")
        .edit(s, Some(s"$regionTitle - $specialNomination"))

    }
  }
}
