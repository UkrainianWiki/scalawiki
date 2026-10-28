package org.scalawiki.wlx.stat.rating

import org.scalawiki.wlx.dto.Monument
import org.scalawiki.wlx.stat.ContestStat

/** Base rate rater.
  *
  * @param baseRate
  *   points every eligible monument photo gets
  * @param regionRates
  *   per-region base rate overrides keyed by region id (first two digits of the monument id). WLM
  *   UA 2026 rule 9.4 raises the base rate to 10 for monuments in the mostly-occupied regions (AR
  *   Crimea and Sevastopol, Donetsk, Zaporizhzhia, Luhansk and Kherson oblasts).
  */
case class NumberOfMonuments(
    stat: ContestStat,
    baseRate: Double,
    regionRates: Map[String, Double] = Map.empty
) extends Rater {
  val monumentIds = stat.monumentDb.map(_.ids).getOrElse(Set.empty)

  def baseRateOf(monumentId: String): Double =
    regionRates.getOrElse(Monument.getRegionId(monumentId), baseRate)

  override def rate(monumentId: String, author: String): Double = {
    if (monumentIds.contains(monumentId)) baseRateOf(monumentId) else 0
  }

  override def explain(monumentId: String, author: String): String = {
    if (monumentIds.contains(monumentId)) {
      val regionId = Monument.getRegionId(monumentId)
      if (regionRates.contains(regionId))
        s"Base rate for region $regionId = ${baseRateOf(monumentId)}"
      else s"Base rate = $baseRate"
    } else "Not a known monument = 0"
  }

  override def label: String = "base"

  override def withRating: Boolean = false
}

case class NewlyPicturedBonus(stat: ContestStat, newlyPicturedRate: Double) extends Rater {

  override def rate(monumentId: String, author: String): Double = {
    if (!oldMonumentIds.contains(monumentId))
      newlyPicturedRate - 1
    else
      0
  }

  override def explain(monumentId: String, author: String): String = {
    if (!oldMonumentIds.contains(monumentId))
      s"Newly pictured rate bonus = ${newlyPicturedRate - 1}"
    else
      "Not newly pictured = 0"
  }

  override def label: String = "newly pictured <br> bonus"
}
