package org.scalawiki.wlx

import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.stat.rating.{RateSum, Rater}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Fills the "rating" field of WLM/WLE monument list rows with the number of points
  * a new photo of that monument would score, per the contest's rating rules
  * (`Rater`). WLM UA 2026 lists gained a `бали` field for this; the list template
  * renders it as a "(+ N балів)" hint next to the upload link.
  *
  * Meant to be run before the contest with a multi-year range (e.g.
  * `--start-year 2013 --year 2026`) so the bonuses are computed against photos that
  * already existed.
  */
object RatingListFiller {

  /** Template parameter that holds the rating, overridable via the `ratingListParam`
    * key in the campaign config (`wlm_ua.conf` etc.). */
  val DefaultParam = "бали"

  def paramName(stat: ContestStat): String =
    stat.contest.config
      .flatMap(c => Try(c.getString("ratingListParam")).toOption)
      .getOrElse(DefaultParam)

  /** Points an ordinary new photo would score, as the wiki string to write, keyed
    * by monument id; monuments that would score nothing are absent.
    *
    * Raters that only a specific kind of upload can earn (the interior-photo
    * bonus, п. 7.3.5 — see [[Rater.appliesToRegularPhoto]]) are dropped: the list
    * figure is a hint for a generic photo, and the interior bonus is both
    * conditional on an interior shot and meaningless for monuments with no
    * interior.
    *
    * Computed once, up front and single-threaded: `Rater` implementations keep
    * non-synchronized mutable state (e.g. `NumberOfImagesInPlaceBonus`), and
    * [[ListUpdater]] runs page updates — and therefore the updater callbacks that
    * would otherwise call `Rater.rate` — concurrently.
    */
  def ratings(monumentDb: MonumentDB, rater: Rater): Map[String, String] = {
    val regularPhotoRater: Option[Rater] = (rater match {
      case sum: RateSum => sum.raters
      case single       => Seq(single)
    }).filter(_.appliesToRegularPhoto) match {
      case Seq()       => None
      case Seq(single) => Some(single)
      case many        => Some(RateSum(rater.stat, many))
    }

    regularPhotoRater.fold(Map.empty[String, String]) { r =>
      monumentDb.monuments.iterator.flatMap { m =>
        val rate = r.rate(m.id, "")
        if (rate > 0) Some(m.id -> RatingUpdater.format(rate)) else None
      }.toMap
    }
  }

  /** The newest year that has its own `rates.<year>` block in the campaign config. */
  def latestRatesYear(contest: Contest): Option[Int] =
    contest.config
      .filter(_.hasPath("rates"))
      .map(_.getConfig("rates").root().keySet().asScala.flatMap(key => Try(key.toInt).toOption))
      .filter(_.nonEmpty)
      .map(_.max)

  /** Why the lists must not be filled with `contest`'s year's rating rules, if
    * they must not: the campaign config already has `rates` for a later year, so
    * the lists - which show what a photo would score in the upcoming contest -
    * would get points by outdated rules. `allowPastYear`
    * (`--allow-past-year-rating`) approves such a run explicitly.
    */
  def pastYearRatingError(contest: Contest, allowPastYear: Boolean): Option[String] =
    latestRatesYear(contest).filter(latest => latest > contest.year && !allowPastYear).map { latest =>
      s"--fill-lists-rating: refusing to fill the lists with the ${contest.year} rating rules while " +
        s"rates.$latest is configured for ${contest.campaign}. Run with --year $latest, or add " +
        s"--allow-past-year-rating to use the ${contest.year} rules anyway."
    }

  def fillLists(stat: ContestStat): Future[Unit] = {
    val monumentDb = stat.monumentDb.getOrElse {
      throw new IllegalStateException("RatingListFiller needs a monument database")
    }
    val updater =
      new RatingUpdater(ratings(monumentDb, Rater.create(stat)), paramName(stat))
    for {
      _ <- ListUpdater.updateLists(monumentDb, updater)
      // thematic nominations live on their own list pages, like ImageFiller does
      _ <- ListUpdater.updateSpecialNominationLists(stat, updater)
    } yield ()
  }
}

class RatingUpdater(
    ratings: Map[String, String],
    paramName: String = RatingListFiller.DefaultParam
) extends MonumentUpdater {

  override def updatedParams(monument: Monument): Map[String, String] =
    ratings.get(monument.id).map(paramName -> _).toMap

  override def needsUpdate(monument: Monument): Boolean =
    ratings.get(monument.id).exists { value =>
      !monument.otherParams.get(paramName).map(_.trim).contains(value)
    }
}

object RatingUpdater {

  /** Whole numbers are written without a trailing ".0" so the wiki value stays
    * `12` rather than `12.0`. */
  def format(rate: Double): String =
    if (rate == Math.rint(rate) && !rate.isInfinite) rate.toLong.toString
    else rate.toString
}
