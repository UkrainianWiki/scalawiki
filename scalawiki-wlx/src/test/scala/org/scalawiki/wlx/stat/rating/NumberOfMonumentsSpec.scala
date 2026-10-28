package org.scalawiki.wlx.stat.rating

import org.scalawiki.wlx.dto.{Contest, ContestType, Country, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

/** WLM UA 2026 rule 9.4: base rate is 10 for monuments in the mostly-occupied regions (AR Crimea
  * and Sevastopol, Donetsk, Zaporizhzhia, Luhansk and Kherson oblasts), 1 elsewhere.
  */
class NumberOfMonumentsSpec extends Specification {

  val contest = Contest(ContestType.WLM, Country.Ukraine, 2026)

  val crimea = "01-101-0001"
  val donetsk = "14-101-0001"
  val kherson = "65-101-0001"
  val vinnytsia = "05-101-0001"

  val monuments =
    Seq(crimea, donetsk, kherson, vinnytsia).map(id => Monument(id = id, name = id))
  val monumentDb = Some(new MonumentDB(contest, monuments))
  val imageDb = new ImageDB(contest, Nil, monumentDb)

  val stat = ContestStat(
    contest = contest,
    startYear = 2013,
    monumentDb = monumentDb,
    currentYearImageDb = imageDb,
    totalImageDb = imageDb
  )

  val warRegions = Seq("01", "14", "23", "44", "65", "85")
  val rater = NumberOfMonuments(stat, baseRate = 1, regionRates = warRegions.map(_ -> 10.0).toMap)

  "NumberOfMonuments" should {
    "use base rate 10 for monuments in occupied regions" in {
      rater.rate(crimea, "any") === 10.0
      rater.rate(donetsk, "any") === 10.0
      rater.rate(kherson, "any") === 10.0
      rater.explain(crimea, "any") === "Base rate for region 01 = 10.0"
    }

    "use base rate 1 elsewhere" in {
      rater.rate(vinnytsia, "any") === 1.0
      rater.explain(vinnytsia, "any") === "Base rate = 1.0"
    }

    "give 0 for an unknown monument" in {
      rater.rate("77-777-7777", "any") === 0.0
    }
  }
}
