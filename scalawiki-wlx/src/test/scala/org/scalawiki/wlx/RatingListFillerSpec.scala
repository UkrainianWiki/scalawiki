package org.scalawiki.wlx

import org.scalawiki.MwBot
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.stat.rating.Rater
import org.specs2.mutable.Specification

class RatingListFillerSpec extends Specification {

  private val contest = Contest.WLMUkraine(2015)
  private val listConfig = contest.uploadConfigs.head.listConfig
  private val host = MwBot.ukWiki

  private val id1 = "01-101-0001"
  private val id2 = "01-101-0002"

  private def monuments(ids: String*): Seq[Monument] =
    ids.map(id => Monument(id = id, name = "name", listConfig = Some(listConfig)))

  private def stat(ms: Seq[Monument]): ContestStat = {
    val monumentDb = new MonumentDB(contest, ms)
    val imageDb = new ImageDB(contest, Nil, Some(monumentDb))
    ContestStat(contest, 2013, Some(monumentDb), imageDb, imageDb)
  }

  private class StubRater(val stat: ContestStat, rates: Map[String, Double]) extends Rater {
    def rate(monumentId: String, author: String): Double = rates.getOrElse(monumentId, 0.0)
    def explain(monumentId: String, author: String): String = s"rate = ${rate(monumentId, author)}"
  }

  private def task(ms: Seq[Monument], rates: Map[String, Double]) = {
    val s = stat(ms)
    val db = s.monumentDb.get
    val updater =
      new RatingUpdater(RatingListFiller.ratings(db, new StubRater(s, rates)))
    new ListUpdaterTask(host, db, updater)
  }

  private def row(id: String, bali: Option[String]): String = {
    val baliLine = bali.fold("")(v => s"\n| бали = $v")
    s"""{{ВЛП-рядок
| ID = $id
| назва = name
| фото = $baliLine
}}"""
  }

  "RatingListFiller" should {

    "fill an empty rating field" in {
      val (text, comment) =
        task(monuments(id1), Map(id1 -> 12.0)).updatePage("p", row(id1, Some("")))
      text === row(id1, Some("12"))
      comment === "updated 1 monument(s)"
    }

    "update a stale rating field" in {
      val (text, comment) =
        task(monuments(id1), Map(id1 -> 17.0)).updatePage("p", row(id1, Some("5")))
      text === row(id1, Some("17"))
      comment === "updated 1 monument(s)"
    }

    "leave a correct rating field untouched" in {
      val (text, comment) =
        task(monuments(id1), Map(id1 -> 12.0)).updatePage("p", row(id1, Some("12")))
      text === row(id1, Some("12"))
      comment === "updated 0 monument(s)"
    }

    "append the rating field when the row does not have one" in {
      val (text, comment) =
        task(monuments(id1), Map(id1 -> 12.0)).updatePage("p", row(id1, None))
      text === row(id1, Some("12"))
      comment === "updated 1 monument(s)"
    }

    "append the rating field without gluing it onto a single-line row" in {
      val singleLine = s"{{ВЛП-рядок|ID=$id1|назва=name|фото=}}"
      val (text, comment) =
        task(monuments(id1), Map(id1 -> 12.0)).updatePage("p", singleLine)

      text.contains("| бали = 12") must beTrue
      text.contains("=| бали") must beFalse
      new WlxTemplateParser(listConfig, "p")
        .parse(text)
        .head
        .otherParams
        .get("бали")
        .map(_.trim) === Some("12")
      comment === "updated 1 monument(s)"
    }

    "not write a zero rating" in {
      val (text, comment) =
        task(monuments(id1), Map(id1 -> 0.0)).updatePage("p", row(id1, None))
      text === row(id1, None)
      comment === "updated 0 monument(s)"
    }

    "skip monuments that are not in the database" in {
      val (text, comment) =
        task(monuments(id1), Map(id2 -> 12.0)).updatePage("p", row(id2, Some("")))
      text === row(id2, Some(""))
      comment === "updated 0 monument(s)"
    }

    "drop the trailing .0 from whole-number ratings" in {
      RatingUpdater.format(12.0) === "12"
      RatingUpdater.format(12.5) === "12.5"
    }
  }

  "RatingListFiller.pastYearRatingError" should {
    val latest = RatingListFiller.latestRatesYear(Contest.WLMUkraine(2026)).get

    "refuse a year whose rating rules a later configured year replaces" in {
      val error = RatingListFiller.pastYearRatingError(Contest.WLMUkraine(latest - 1), allowPastYear = false)
      error must beSome
      error.get must contain(s"--year $latest")
      error.get must contain("--allow-past-year-rating")
    }

    "allow the newest configured year, and a later year that has no rules of its own yet" in {
      RatingListFiller.pastYearRatingError(Contest.WLMUkraine(latest), allowPastYear = false) must beNone
      RatingListFiller.pastYearRatingError(Contest.WLMUkraine(latest + 1), allowPastYear = false) must beNone
    }

    "allow a past year when explicitly approved" in {
      RatingListFiller.pastYearRatingError(Contest.WLMUkraine(latest - 1), allowPastYear = true) must beNone
    }

    "not refuse a campaign without per-year rating rules" in {
      val noConfig = Contest.WLMUkraine(latest - 1).copy(config = None)
      RatingListFiller.pastYearRatingError(noConfig, allowPastYear = false) must beNone
    }
  }
}
