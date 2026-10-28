package org.scalawiki.wlx.stat.rating

import com.typesafe.config.ConfigFactory
import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.{Contest, ContestType, Country, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB, RatingListFiller}
import org.specs2.mutable.Specification

import java.time.ZonedDateTime

/** End-to-end check of the WLM UA 2026 rating built from wlm_ua.conf (Регламент п. 7.3-7.4, 9.4).
  * Place-based bonus (п. 7.3.3) stays 0 here because the fabricated monuments have no resolvable
  * settlement.
  */
class Rater2026Spec extends Specification {

  val contest = Contest(ContestType.WLM, Country.Ukraine, 2026)

  val freshNonWar = "05-101-0001" // Vinnytsia, no prior photos
  val freshWar = "01-101-0001" // Crimea, no prior photos
  val oldPhotosMon = "05-101-0002" // Vinnytsia, only pre-2020 photos by 2 authors

  val monuments =
    Seq(freshNonWar, freshWar, oldPhotosMon).map(id => Monument(id = id, name = id))
  val monumentDb = Some(new MonumentDB(contest, monuments))

  private var pageId = 0L
  def img(monumentId: String, author: String, date: String): Image = {
    pageId += 1
    Image(s"File:$pageId.jpg", pageId = Some(pageId))
      .withAuthor(author)
      .withMonument(monumentId)
      .copy(date = Some(ZonedDateTime.parse(s"${date}T00:00:00Z")))
  }

  val current = Seq(
    img(freshNonWar, "participant", "2026-10-05"),
    img(freshWar, "participant", "2026-10-05"),
    img(oldPhotosMon, "participant", "2026-10-05")
  )

  val old = Seq(
    img(oldPhotosMon, "old author 1", "2018-05-01"),
    img(oldPhotosMon, "old author 2", "2019-09-01")
  )

  val stat = ContestStat(
    contest = contest,
    startYear = 2013,
    monumentDb = monumentDb,
    currentYearImageDb = new ImageDB(contest, current, monumentDb),
    totalImageDb = new ImageDB(contest, current ++ old, monumentDb)
  )

  val rater = Rater.fromConfig(stat, ConfigFactory.load("wlm_ua.conf"))

  "WLM UA 2026 rater" should {

    "be a sum of base rate and the four bonuses" in {
      rater must beAnInstanceOf[RateSum]
      rater.asInstanceOf[RateSum].raters.map(_.getClass.getSimpleName) === Seq(
        "NumberOfMonuments",
        "NumberOfAuthorsBonus",
        "NumberOfImagesInPlaceBonus",
        "NumberOfInteriorImagesBonus",
        "OldPhotosBonus"
      )
    }

    "expose one rating-table column label per rater" in {
      rater.asInstanceOf[RateSum].raters.map(_.label) === Seq(
        "base",
        "authors <br> bonus",
        "images <br> bonus",
        "interior <br> bonus",
        "old photos <br> bonus"
      )
    }

    "rate a fresh monument in a non-occupied region: 1 + 12 + 0 + 5 + 0" in {
      rater.rate(freshNonWar, "participant") === 18.0
    }

    "rate a fresh monument in an occupied region: 10 + 12 + 0 + 5 + 0" in {
      rater.rate(freshWar, "participant") === 27.0
    }

    "rate a monument with only pre-2020 photos by 2 authors: 1 + 6 + 0 + 5 + 5" in {
      rater.rate(oldPhotosMon, "participant") === 17.0
    }

    "leave the interior bonus (п. 7.3.5) out of the list rating hint" in {
      // base + authors + place + old-photos only, no interior +5/+3
      RatingListFiller.ratings(monumentDb.get, rater) === Map(
        freshNonWar -> "13", // 1 + 12 + 0 + 0
        freshWar -> "22", //    10 + 12 + 0 + 0
        oldPhotosMon -> "12" //   1 +  6 + 0 + 5
      )
    }

    "explain the score breakdown" in {
      rater.explain(freshWar, "participant") must contain("Base rate for region 01 = 10.0")
      rater.explain(freshWar, "participant") must contain(
        "Pictured before by 0 (0-0) authors = 12.0"
      )
      rater.explain(freshWar, "participant") must contain("interior images existed, bonus = 5.0")
    }
  }
}
