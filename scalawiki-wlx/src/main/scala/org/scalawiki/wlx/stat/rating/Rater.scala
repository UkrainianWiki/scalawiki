package org.scalawiki.wlx.stat.rating

import com.typesafe.config.Config
import org.scalawiki.dto.Image
import org.scalawiki.wlx.ImageDB
import org.scalawiki.wlx.stat.ContestStat

import java.time.LocalDate
import scala.collection.JavaConverters._
import scala.util.Try

trait Rater {

  def stat: ContestStat

  def imageDb: ImageDB = stat.currentYearImageDb

  def rate(monumentId: String, author: String): Double

  def explain(monumentId: String, author: String): String

  /** Short column header for this rater's contribution in the rating breakdown table (see
    * [[org.scalawiki.wlx.stat.reports.Output.galleryByRegionAndId]]).
    */
  def label: String = getClass.getSimpleName

  def disqualify(monumentId: String, author: String): Boolean = false

  def rateMonumentIds(monumentIds: Set[String], author: String): Double = {
    monumentIds.toSeq.map(rate(_, author)).sum
  }

  def rateRegion(regionId: String, author: String): Double = {
    rateMonumentIds(
      imageDb._byAuthorAndRegion
        .by(author, regionId)
        .flatMap(_.monumentId)
        .toSet,
      author
    )
  }

  val oldImages = stat.oldImages

  lazy val oldMonumentIds: Set[String] = oldImages.flatMap(_.monumentId).toSet

  /** `oldImages` grouped by the monument they depict. Images without a monument id are dropped.
    * Computed once and shared by the per-monument bonus raters.
    *
    * NB: `oldImages` only holds images that carry the contest monument template (prior-year contest
    * uploads on Commons / uk.wikipedia). Photos of a monument that exist on Commons or Wikipedia
    * but were never entered into the contest are invisible here — an approximation of the
    * регламент's "усі наявні на Вікісховищі чи у Вікіпедії фотографії".
    */
  lazy val oldImagesByMonumentId: Map[String, Seq[Image]] =
    oldImages.toSeq
      .filter(_.monumentId.isDefined)
      .groupBy(_.monumentId.get)

  def withRating: Boolean = true

  /** Whether this rater's points apply to an ordinary (exterior) photo of the monument.
    *
    * `false` for bonuses that only a particular kind of upload can earn — currently the
    * interior-photo bonus (Регламент 2026, п. 7.3.5), which a regular photo never gets and which is
    * meaningless for a monument that physically has no interior. Such raters are left out of the
    * single per-monument figure [[org.scalawiki.wlx.RatingListFiller]] writes into the `бали` list
    * field, since that figure is a hint for a generic upload.
    */
  def appliesToRegularPhoto: Boolean = true

}

object Rater {

  def create(stat: ContestStat): Rater = {
    stat.contest.config.map(fromConfig(stat, _)).getOrElse {
      val config = stat.contest.rateConfig

      val raters = Seq(NumberOfMonuments(stat, config.baseRate)) ++
        config.newAuthorObjectRating
          .map(r =>
            NewlyPicturedPerAuthorBonus(
              stat,
              config.newObjectRating.getOrElse(1),
              r
            )
          )
          .orElse(
            config.newObjectRating.map(NewlyPicturedBonus(stat, _))
          )

      if (raters.tail.isEmpty) {
        raters.head
      } else {
        RateSum(stat, raters)
      }
    }
  }

  def fromConfig(stat: ContestStat, config: Config): Rater = {
    val rateCfg = config.getConfig(s"rates.${stat.contest.year}")

    val regionRates: Map[String, Double] =
      if (rateCfg.hasPath("war-region-base-rate")) {
        val warCfg = rateCfg.getConfig("war-region-base-rate")
        val rate = warCfg.getDouble("rate")
        warCfg.getStringList("regions").asScala.map(_ -> rate).toMap
      } else Map.empty

    val raters = Seq(
      NumberOfMonuments(
        stat,
        Try(rateCfg.getDouble("base-rate")).toOption.getOrElse(1),
        regionRates
      )
    ) ++
      (if (rateCfg.hasPath("number-of-authors-bonus")) {
         Seq(
           NumberOfAuthorsBonus(
             stat,
             RateRanges(rateCfg.getConfig("number-of-authors-bonus"))
           )
         )
       } else Nil) ++
      (if (rateCfg.hasPath("number-of-images-bonus")) {
         Seq(
           NumberOfImagesInPlaceBonus(
             stat,
             RateRanges(rateCfg.getConfig("number-of-images-bonus"))
           )
         )
       } else Nil) ++
      (if (rateCfg.hasPath("number-of-interior-images-bonus")) {
         Seq(
           NumberOfInteriorImagesBonus(
             stat,
             RateRanges(rateCfg.getConfig("number-of-interior-images-bonus"))
           )
         )
       } else Nil) ++
      (if (rateCfg.hasPath("old-photos-bonus")) {
         val oldPhotosCfg = rateCfg.getConfig("old-photos-bonus")
         Seq(
           OldPhotosBonus(
             stat,
             oldPhotosCfg.getDouble("bonus"),
             LocalDate.parse(oldPhotosCfg.getString("before-date"))
           )
         )
       } else Nil)

    if (raters.tail.isEmpty) {
      raters.head
    } else {
      RateSum(stat, raters)
    }

  }
}
