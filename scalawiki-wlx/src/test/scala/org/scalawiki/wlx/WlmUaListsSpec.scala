package org.scalawiki.wlx

import org.scalawiki.cache.CachedBot
import org.scalawiki.dto.Site
import org.scalawiki.wlx.dto.Contest
import org.scalawiki.wlx.query.MonumentQuery
import org.specs2.mutable.Specification

/** Reads every WLM-UA monument list from the live uk.wikipedia.org (hundreds of API
  * requests without a local http-cache/): only with -Dscalawiki.live=true.
  */
class WlmUaListsSpec extends Specification {

  sequential

  skipAllUnless(sys.props.get("scalawiki.live").contains("true"))

  val campaign = "wlm-ua"
  val cacheName = s"$campaign-2019"

  val contest = Contest.byCampaign(campaign).get.copy(year = 2019)
  val country = contest.country

  // lazy: built only when an example runs, so a skipped spec makes no request
  lazy val bot =
    new CachedBot(Site.ukWiki, cacheName + "-wiki", true)
  lazy val monumentQuery = MonumentQuery.create(contest)(bot)
  lazy val monumentDb = MonumentDB.getMonumentDbBlocking(contest, monumentQuery)
  lazy val all = monumentDb.allMonuments

  "places" should {
    "be mostly detected" in {
      all must not(beEmpty)

      val notFound = monumentDb.unknownPlaces()

      val percentage = notFound.map(_.monuments.size).sum * 100 / all.size
      percentage should be < 5
    }

    "not be just high level region" in {
      val updater = new RegionFixerUpdater(monumentDb)
      updater.raions.size === 490

      val highLevel = all.filter(m =>
        updater.raionNames.contains(m.cityName) && m.place.exists(
          _.trim.nonEmpty
        )
      )

      val percentage = highLevel.size * 100 / all.size
      percentage should be <= 5
    }
  }
}
