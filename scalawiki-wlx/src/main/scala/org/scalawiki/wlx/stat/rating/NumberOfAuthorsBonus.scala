package org.scalawiki.wlx.stat.rating

import org.scalawiki.MwBot
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.stat.reports.RateInputDistribution

case class NumberOfAuthorsBonus(stat: ContestStat, rateRanges: RateRanges) extends Rater {
  val authorsByMonument: Map[String, Set[String]] = oldImagesByMonumentId.mapValues { images =>
    images.map(_.author.getOrElse("")).toSet
  }.toMap

  val authorsNumberByMonument: Map[String, Int] =
    authorsByMonument.mapValues(_.size).toMap

  val distribution: Map[Int, Int] =
    authorsNumberByMonument.values.groupBy(identity).mapValues(_.size).toMap

  if (stat.config.exists(_.rateInputDistribution)) {
    new RateInputDistribution(
      stat,
      distribution,
      "Number of authors distribution",
      Seq("Number of authors", "Number of monuments")
    ).updateWiki(MwBot.fromHost(MwBot.commons))
  }

  override def rate(monumentId: String, author: String): Double = {
    if (disqualify(monumentId, author)) 0
    else if (
      rateRanges.sameAuthorZeroBonus && authorsByMonument
        .getOrElse(monumentId, Set.empty)
        .contains(author)
    ) {
      0
    } else {
      rateRanges.rate(authorsNumberByMonument.getOrElse(monumentId, 0))
    }
  }

  override def explain(monumentId: String, author: String): String = {
    val number = rate(monumentId, author)
    if (disqualify(monumentId, author)) {
      "Disqualified for reuploading similar images = 0"
    } else if (
      rateRanges.sameAuthorZeroBonus && authorsByMonument
        .getOrElse(monumentId, Set.empty)
        .contains(author)
    ) {
      s"Pictured by same author before = $number"
    } else {
      val picturedBy = authorsNumberByMonument.getOrElse(monumentId, 0)
      val (rate, start, end) = rateRanges.rateWithRange(picturedBy)
      s"Pictured before by $picturedBy ($start-${end.getOrElse("")}) authors = $rate"
    }
  }

  override def disqualify(monumentId: String, author: String): Boolean = {
    false
    //    Set("Петро Халява", "SnizhokAM").contains(author) &&
    //      authorsByMonument.getOrElse(monumentId, Set.empty).contains(author)
  }

  override def label: String = "authors <br> bonus"
}
