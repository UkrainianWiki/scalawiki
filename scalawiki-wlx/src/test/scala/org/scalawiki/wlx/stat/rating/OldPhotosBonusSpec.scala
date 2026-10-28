package org.scalawiki.wlx.stat.rating

import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.{Contest, ContestType, Country, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

import java.time.{LocalDate, ZonedDateTime}

/** WLM UA 2026 rule 7.3.4: +5 points if every already-known photo of the monument is dated before
  * 2020-01-01.
  */
class OldPhotosBonusSpec extends Specification {

  val contest = Contest(ContestType.WLM, Country.Ukraine, 2026)

  val allOld = "01-101-0001" // all existing photos are old
  val someNew = "01-101-0002" // has a post-2020 photo
  val noOld = "01-101-0003" // no existing photos at all
  val noDate = "01-101-0004" // an existing photo without a date

  val monuments = Seq(allOld, someNew, noOld, noDate).map(id => Monument(id = id, name = id))
  val monumentDb = Some(new MonumentDB(contest, monuments))

  def img(pageId: Long, monumentId: String, date: Option[String]): Image =
    Image(s"File:$pageId.jpg", pageId = Some(pageId))
      .withAuthor("someone")
      .withMonument(monumentId)
      .copy(date = date.map(d => ZonedDateTime.parse(s"${d}T00:00:00Z")))

  // "new" (current year) uploads
  val current = Seq(
    img(101, allOld, Some("2026-10-05")),
    img(102, someNew, Some("2026-10-05")),
    img(103, noOld, Some("2026-10-05")),
    img(104, noDate, Some("2026-10-05"))
  )

  // photos that already existed before the contest
  val old = Seq(
    img(1, allOld, Some("2015-06-01")),
    img(2, allOld, Some("2019-12-31")),
    img(3, someNew, Some("2018-01-01")),
    img(4, someNew, Some("2021-03-01")),
    img(5, noDate, None)
  )

  val stat = ContestStat(
    contest = contest,
    startYear = 2013,
    monumentDb = monumentDb,
    currentYearImageDb = new ImageDB(contest, current, monumentDb),
    totalImageDb = new ImageDB(contest, current ++ old, monumentDb)
  )

  val rater = OldPhotosBonus(stat, bonus = 5, beforeDate = LocalDate.of(2020, 1, 1))

  "OldPhotosBonus" should {
    "give the bonus when all existing photos predate the cutoff" in {
      rater.rate(allOld, "any") === 5.0
      rater.explain(allOld, "any") === "All existing photos are dated before 2020-01-01 = 5.0"
    }

    "give nothing when at least one existing photo is newer" in {
      rater.rate(someNew, "any") === 0.0
    }

    "give nothing when there are no existing photos" in {
      rater.rate(noOld, "any") === 0.0
    }

    "give nothing when an existing photo has no known date" in {
      rater.rate(noDate, "any") === 0.0
    }
  }
}
