package org.scalawiki.wlx.stat.rating

import com.typesafe.config.ConfigFactory
import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.{Contest, ContestType, Country, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

/** WLM UA 2026 rule 7.3.5: bonus by the number of already-known interior photos of the monument: 0
  * -> 5, 1..5 -> 3, 6+ -> 0.
  */
class NumberOfInteriorImagesBonusSpec extends Specification {

  val contest = Contest(ContestType.WLM, Country.Ukraine, 2026)

  val noInterior = "01-101-0001"
  val fewInterior = "01-101-0002"
  val manyInterior = "01-101-0003"

  val monuments =
    Seq(noInterior, fewInterior, manyInterior).map(id => Monument(id = id, name = id))
  val monumentDb = Some(new MonumentDB(contest, monuments))

  private var pageId = 0L
  def img(monumentId: String, interior: Boolean): Image = {
    pageId += 1
    Image(s"File:$pageId.jpg", pageId = Some(pageId))
      .withAuthor("someone")
      .withMonument(monumentId)
      .copy(categories = if (interior) Set(s"Interior of $monumentId") else Set(s"$monumentId"))
  }

  val current = monuments.map(m => img(m.id, interior = false))

  val old =
    Seq.fill(3)(img(fewInterior, interior = true)) ++
      Seq(img(fewInterior, interior = false)) ++
      Seq.fill(6)(img(manyInterior, interior = true)) ++
      Seq(img(noInterior, interior = false))

  val stat = ContestStat(
    contest = contest,
    startYear = 2013,
    monumentDb = monumentDb,
    currentYearImageDb = new ImageDB(contest, current, monumentDb),
    totalImageDb = new ImageDB(contest, current ++ old, monumentDb)
  )

  val ranges = RateRanges(ConfigFactory.parseString("""{"0-0": 5, "1-5": 3}"""))
  val rater = NumberOfInteriorImagesBonus(stat, ranges)

  "NumberOfInteriorImagesBonus" should {
    "give 5 when no interior photos exist yet" in {
      rater.rate(noInterior, "any") === 5.0
      rater.explain(noInterior, "any") === "0 (0-0) interior images existed, bonus = 5.0"
    }

    "give 3 when 1..5 interior photos exist" in {
      rater.rate(fewInterior, "any") === 3.0
      rater.explain(fewInterior, "any") === "3 (1-5) interior images existed, bonus = 3.0"
    }

    "give 0 when 6 or more interior photos exist" in {
      rater.rate(manyInterior, "any") === 0.0
    }

    "not apply to a regular photo (only interior shots earn it, п. 7.3.5)" in {
      rater.appliesToRegularPhoto must beFalse
    }
  }

  "NumberOfInteriorImagesBonus.isInterior" should {
    def image(nominations: Set[String] = Set.empty, categories: Set[String] = Set.empty) =
      Image("File:x.jpg").copy(specialNominations = nominations, categories = categories)

    "recognise an interior special nomination of any year" in {
      NumberOfInteriorImagesBonus.isInterior(
        image(nominations = Set("WLM2023-UA-interior"))
      ) must beTrue
      NumberOfInteriorImagesBonus.isInterior(
        image(nominations = Set("WLM2026-UA-interior"))
      ) must beTrue
      NumberOfInteriorImagesBonus.isInterior(
        image(nominations = Set("WLM2026-UA-інтер'єр"))
      ) must beTrue
    }

    "not treat an exterior nomination as interior" in {
      NumberOfInteriorImagesBonus.isInterior(
        image(nominations = Set("WLM2026-UA-exterior"))
      ) must beFalse
    }

    "recognise interior categories in English and Ukrainian" in {
      NumberOfInteriorImagesBonus.isInterior(
        image(categories = Set("Interior of St Sophia"))
      ) must beTrue
      NumberOfInteriorImagesBonus.isInterior(
        image(categories = Set("Інтер'єр Софійського собору"))
      ) must beTrue
    }
  }
}
