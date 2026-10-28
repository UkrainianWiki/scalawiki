package org.scalawiki.wlx.stat.rating

import org.scalawiki.MwBot
import org.scalawiki.wlx.ImageDB
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.stat.reports.RateInputDistribution

import scala.collection.mutable

case class NumberOfImagesInPlaceBonus(
    stat: ContestStat,
    rateRanges: RateRanges
) extends Rater {

  val oldImagesDb = new ImageDB(stat.contest, oldImages, stat.monumentDb)
  val perPlaceStat = PerPlaceStat(oldImagesDb)
  val unknownPlaceMonumentsByAuthor = mutable.Map[String, Set[String]]()
  val authorsByMonument: Map[String, Set[String]] = oldImagesByMonumentId.mapValues { images =>
    images.map(_.author.getOrElse("")).toSet
  }.toMap

  val distribution: Map[Int, Int] = perPlaceStat.distribution

  if (stat.config.exists(_.rateInputDistribution)) {
    new RateInputDistribution(
      stat,
      distribution,
      "Number of images in place distribution",
      Seq("Number of images in place", "Number of monuments")
    ).updateWiki(MwBot.fromHost(MwBot.commons))
  }

  override def rate(monumentId: String, author: String): Double = {
    if (
      rateRanges.sameAuthorZeroBonus && authorsByMonument
        .getOrElse(monumentId, Set.empty)
        .contains(author)
    ) {
      0.0
    } else {
      perPlaceStat.placeByMonument
        .get(monumentId)
        .map { place =>
          rateRanges.rate(perPlaceStat.imagesPerPlace.getOrElse(place, 0))
        }
        .getOrElse {
          val monumentIds =
            unknownPlaceMonumentsByAuthor.getOrElse(author, Set.empty[String])
          if (!monumentIds.contains(monumentId)) {
            unknownPlaceMonumentsByAuthor(author) = monumentIds + monumentId
          }
          0.0
        }
    }
  }

  override def explain(monumentId: String, author: String): String = {
    if (
      rateRanges.sameAuthorZeroBonus && authorsByMonument
        .getOrElse(monumentId, Set.empty)
        .contains(author)
    ) {
      s"Pictured by same author before = 0"
    } else {
      perPlaceStat.placeByMonument
        .get(monumentId)
        .map { place =>
          val perPlace = perPlaceStat.imagesPerPlace.getOrElse(place, 0)
          val (rate, start, end) = rateRanges.rateWithRange(perPlace)
          s"$perPlace ($start-${end.getOrElse("")}) images were of $place, bonus = $rate"
        }
        .getOrElse {
          val monumentIds =
            unknownPlaceMonumentsByAuthor.getOrElse(author, Set.empty[String])
          if (!monumentIds.contains(monumentId)) {
            unknownPlaceMonumentsByAuthor(author) = monumentIds + monumentId
          }
          s"unknown place of monument $monumentId, bonus = 0"
        }
    }
  }

  override def label: String = "images <br> bonus"
}
