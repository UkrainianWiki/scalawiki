package org.scalawiki.wlx.stat.reports

import org.scalawiki.MwBot
import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.SpecialNomination
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}

import scala.concurrent.{ExecutionContext, Future}

/** Which images of a contest year have a monument id that matches no monument
  * ("bad ids"), and which have none at all ("missing ids").
  *
  * Left out of both: images in an obviously-ineligible category, and special
  * nomination images - those the file itself places in a nomination (its
  * nomination template, a video) and, from the bad ids, those whose id comes
  * from a nomination's own list (`nominationIds`) rather than the monument
  * lists. Those are checked by the special nomination reports instead.
  */
class ImageIdProblems(
    monumentDb: MonumentDB,
    nominations: Seq[SpecialNomination],
    nominationIds: Set[String]
) {

  def badIds(imageDb: ImageDB): Seq[Image] =
    reported(imageDb).filter(_.monumentIds.exists(id => !isKnownId(id)))

  def missingIds(imageDb: ImageDB): Seq[Image] =
    reported(imageDb).filter(_.monumentIds.isEmpty)

  private def isKnownId(id: String): Boolean =
    monumentDb.ids.contains(id) ||
      nominationIds.contains(id) ||
      ImageIdProblems.nominationIdPrefixes.exists(id.startsWith)

  private def reported(imageDb: ImageDB): Seq[Image] = {
    val year = imageDb.contest.year
    val fileNominations = nominations.filter(_.years.contains(year))
    imageDb.images.toSeq.filterNot { image =>
      image.categories.exists(_.startsWith("Obviously ineligible")) ||
      fileNominations.exists(_.matchesFile(image))
    }
  }
}

object ImageIdProblems {

  /** Region prefixes of ids from special nomination lists of past years that
    * are no longer configured, so their ids can't be fetched. */
  val nominationIdPrefixes: Seq[String] = Seq("88", "93", "95", "97", "98", "99")

  /** Write `Commons:<contest>/Images with bad ids` and/or `.../Images with
    * missing ids` for every year of the run, so ids fixed in older files drop
    * off their year's page too. The index pages linking the years are kept by
    * hand.
    */
  def updateWiki(stat: ContestStat, badIds: Boolean, missingIds: Boolean, bot: MwBot)(implicit
      ec: ExecutionContext
  ): Future[Unit] = {
    val imageDbs = if (stat.dbsByYear.nonEmpty) stat.dbsByYear else Seq(stat.currentYearImageDb)
    val years = imageDbs.map(_.contest.year).toSet
    val nominations =
      SpecialNomination.nominations.filter(n => n.years.isEmpty || n.years.exists(years.contains))

    SpecialNomination.getMonumentsMap(nominations, stat).flatMap { monumentsMap =>
      val problems = new ImageIdProblems(
        stat.monumentDb.get,
        nominations,
        monumentsMap.values.flatten.map(_.id).toSet
      )

      def publish(imageDb: ImageDB, title: String, images: Seq[Image]): Future[Any] = {
        val text = images.map(_.title).mkString("<gallery>", "\n", "</gallery>")
        bot.page(s"Commons:${imageDb.contest.name}/$title").edit(text, Some("updating"))
      }

      val edits = imageDbs.flatMap { imageDb =>
        Seq(
          if (badIds) Some(publish(imageDb, "Images with bad ids", problems.badIds(imageDb))) else None,
          if (missingIds) Some(publish(imageDb, "Images with missing ids", problems.missingIds(imageDb)))
          else None
        ).flatten
      }
      Future.sequence(edits).map(_ => ())
    }
  }
}
