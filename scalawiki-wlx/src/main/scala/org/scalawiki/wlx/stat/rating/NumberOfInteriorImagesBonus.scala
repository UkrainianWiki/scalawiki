package org.scalawiki.wlx.stat.rating

import org.scalawiki.dto.Image
import org.scalawiki.wlx.stat.ContestStat

/** WLM UA 2026 rule 7.3.5: a monument photo gets a bonus depending on how many
  * interior photos of that monument were already known before the contest started:
  *   - 0 interior photos: 5 points
  *   - 1–5 interior photos: 3 points
  *   - 6 and more: 0 points
  *
  * The bonus is added once per (monument, author) pair — as with the other
  * per-monument bonuses — regardless of how many photos the author uploaded.
  *
  * Per п. 7.3.5 this bonus is earned only by an *interior* photo, so it is not
  * part of the generic per-monument rating hint written to the lists (see
  * [[Rater.appliesToRegularPhoto]] / [[org.scalawiki.wlx.RatingListFiller]]) —
  * that hint stands for an ordinary upload, and many monuments (crosses, graves,
  * kurhany, archaeological sites, free-standing monuments, …) have no interior at
  * all.
  *
  * "Already known" here means prior-year contest uploads that carry an interior
  * marker (see [[NumberOfInteriorImagesBonus.isInterior]]); interior photos on
  * Commons or Wikipedia that were never entered into the contest are not counted
  * (see [[Rater.oldImagesByMonumentId]]).
  */
case class NumberOfInteriorImagesBonus(
    stat: ContestStat,
    rateRanges: RateRanges
) extends Rater {

  val interiorImagesByMonument: Map[String, Int] =
    oldImagesByMonumentId.mapValues(_.count(NumberOfInteriorImagesBonus.isInterior)).toMap

  override def rate(monumentId: String, author: String): Double =
    rateRanges.rate(interiorImagesByMonument.getOrElse(monumentId, 0))

  override def explain(monumentId: String, author: String): String = {
    val number = interiorImagesByMonument.getOrElse(monumentId, 0)
    val (rate, start, end) = rateRanges.rateWithRange(number)
    s"$number ($start-${end.getOrElse("")}) interior images existed, bonus = $rate"
  }

  override def label: String = "interior <br> bonus"

  override def appliesToRegularPhoto: Boolean = false
}

object NumberOfInteriorImagesBonus {

  /** WLM interior special-nomination templates, any year: `WLM2023-UA-interior`,
    * `WLM2024-UA-інтер'єр`, etc.
    */
  private val interiorTemplate = """(?i)WLM\d{4}-UA-(interior|інтер)""".r

  /** An image is treated as an interior shot when it carries an interior
    * special-nomination template, or is filed in an "interior" / "інтер'єр"
    * category on Commons. The category check is a broad substring match and can
    * both over- and under-count; the template check is the reliable signal.
    */
  def isInterior(image: Image): Boolean =
    image.specialNominations.exists(interiorTemplate.findFirstIn(_).isDefined) ||
      image.categories.exists(isInteriorCategory)

  /** The category half of [[isInterior]]. Also decides which categories slim
    * (past-year) images keep, see `ImageCsvImporter.slim`. */
  def isInteriorCategory(category: String): Boolean = {
    val lc = category.toLowerCase
    lc.contains("interior") || lc.contains("інтер'єр") || lc.contains("інтер’єр")
  }
}
