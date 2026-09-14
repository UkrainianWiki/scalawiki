package org.scalawiki.wlx.stat.reports

import org.scalawiki.MwBot
import org.scalawiki.dto.markup.Table
import org.scalawiki.wlx.dto.SpecialNomination
import org.scalawiki.wlx.stat.rating.Rater
import org.scalawiki.wlx.stat.{ContestStat, StatConfig, Stats}
import org.scalawiki.wlx.{
  ImageCsvExporter,
  ImageDB,
  ImageFiller,
  ImageFillerUpdater,
  ListUpdater,
  MonumentDB,
  RatingListFiller
}

import org.scalawiki.wlx.stat.progress.ProgressTask
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

class ReporterRegistry(
    stat: ContestStat,
    cfg: StatConfig,
    progress: Option[ProgressTask] = None
)(implicit
    ec: ExecutionContext
) {

  import org.scalawiki.wlx.stat.reports.{ReporterRegistry => RR}

  private val logger = LoggerFactory.getLogger(classOf[ReporterRegistry])

  private val contest = stat.contest
  private val monumentDb = stat.monumentDb
  private val currentYearImageDb = stat.currentYearImageDb
  private val totalImageDb = stat.totalImageDb
  private val commons = MwBot.fromHost(MwBot.commons)

  def monumentDbStat: Option[String] = stat.monumentDb.map(RR.monumentDbStat)

  //  def authorsMonuments: String =
  //    RR.authorsMonuments(stat.currentYearImageDb.get)

  def authorsImages: String =
    RR.authorsImages(currentYearImageDb, monumentDb)

  def authorsContributed: String =
    RR.authorsContributed(stat.dbsByYear, totalImageDb, monumentDb)

  def mostPopularMonuments: String = new MostPopularMonuments(stat).asText

  def monumentsPictured: String =
    new MonumentsPicturedByRegion(stat, gallery = true).asText

  def withArticles: Option[String] = RR.withArticles(monumentDb)

  /** Report steps that threw while building or dispatching their output.
    * Populated by [[step]]; drained by the CLI to report a partial run. */
  private val stepErrors = scala.collection.mutable.Buffer.empty[(String, Throwable)]

  /** Run one report step in isolation: a failure in it is recorded and printed
    * with its stack trace, but never aborts the remaining steps. Only catches
    * exceptions thrown synchronously — asynchronous edit/upload failures are
    * tracked separately by [[org.scalawiki.util.WriteWatcher]]. */
  private def step(name: String)(body: => Any): Unit = {
    try {
      logger.info(s"[report] $name")
      progress.foreach { t => t.step(); t.msg(name) }
      // a step that returns a Future is awaited here so its failure is recorded
      // like a synchronous one; the individual wiki writes it fired are still
      // tracked (and waited on) separately by WriteWatcher.
      body match {
        case f: Future[_] => Await.result(f, Duration.Inf)
        case _            => ()
      }
    } catch {
      case NonFatal(e) =>
        stepErrors += (name -> e)
        logger.warn(s"[report] FAILED: $name: $e", e)
    }
  }

  /** Outputs current year reports.
    */
  def currentYear(): Unit = {
    val imageDb = currentYearImageDb
    step("RecentlyTaken")(new RecentlyTaken(stat).updateWiki(commons))

    if (cfg.specialNominations) {
      step("specialNominations")(new SpecialNominations(stat, imageDb).statistics())
    }

    if (cfg.lowRes) {
      step("lessThan2MpGallery")(Output.lessThan2MpGallery(contest, imageDb))
    }

    monumentDb.foreach { mDb =>
      if (cfg.wrongIds) {
        step("wrongIds")(Output.wrongIds(imageDb, mDb))
      }

      if (cfg.missingIds) {
        step("missingIds")(Output.missingIds(imageDb, mDb))
      }

      if (cfg.multipleIds) {
        step("multipleIds")(Output.multipleIds(imageDb, mDb))
      }

      if (cfg.fillLists && cfg.years.size == 1) {
        step("fillLists")(ImageFiller.fillLists(mDb, imageDb))
      }

      if (cfg.missingGallery) {
        step("missingGallery")(Output.missingGallery(mDb))
      }

      if (cfg.placeDetection) {
        step("placeDetection") {
          Output.unknownPlaces(mDb, imageDb)
          Output.unknownPlaces(mDb)
        }
      }

      if (cfg.mostPopularMonuments) {
        step("mostPopularMonuments") {
          new MostPopularMonuments(stat).updateWiki(MwBot.fromHost(MwBot.commons))
        }
      }
    }
  }

  def allYears(): Unit = {
    val imageDb = totalImageDb
    if (cfg.fillLists) {
      step("fillLists (all years)") {
        for {
          _ <- ImageFiller.fillLists(monumentDb.get, imageDb)
          _ <- fillSpecialNominationLists(imageDb)
        } yield ()
      }
    }

    if (cfg.fillListsRating) {
      step("fillListsRating")(RatingListFiller.fillLists(stat))
    }

    if (cfg.regionalStat) {
      step("regionalStat")(Output.regionalStat(stat))
    }

    if (cfg.newMonuments) {
      step("newMonuments")(Output.newMonuments(stat))
    }

    if (cfg.authorsStat) {
      step("authorsStat")(new AuthorsStat().authorsStat(stat, commons, cfg.gallery))
    } else if (cfg.rateInputDistribution) {
      step("rateInputDistribution")(Rater.create(stat))
    }

    if (cfg.regionalGallery) {
      step("regionalGallery")(Output.byRegion(monumentDb.get, imageDb))
    }

    if (cfg.numberOfMonumentsByNumberOfPictures) {
      // new NumberOfMonumentsByNumberOfPictures(stat, imageDb).updateWiki(commons)
      step("numberOfMonumentsByNumberOfPictures")(Gallery.gallery(imageDb, monumentDb.get))
    }

  }

  /** Runs every configured report step. Returns the steps that threw
    * synchronously; asynchronous wiki-write failures are reported separately by
    * [[org.scalawiki.util.WriteWatcher]]. */
  def output(): Seq[(String, Throwable)] = {
    stepErrors.clear()
    currentYear()
    allYears()

    cfg.exportImagesCsv.foreach { dir =>
      step("exportImagesCsv") {
        val contestYear = stat.contest.year
        stat.dbsByYear.foreach { imageDb =>
          ImageCsvExporter.export(
            imageDb,
            stat.contest.campaign,
            isCurrent = imageDb.contest.year == contestYear,
            outputDir = dir
          )
        }
        ImageCsvExporter.exportTotal(totalImageDb, stat.contest.campaign, outputDir = dir, perYear = stat.dbsByYear)
      }
    }
    stepErrors.toSeq
  }

  /** Special nomination monument lists (e.g. thematic lists like "Музичні пам'ятки в
    * Україні") often live on separate wiki pages from the regular per-region monument
    * lists that `monumentDb` is built from, so `ImageFiller.fillLists` never visits
    * them. Fill each nomination's own list pages too, reusing the same image data.
    */
  private def fillSpecialNominationLists(imageDb: ImageDB): Future[Unit] =
    ListUpdater.updateSpecialNominationLists(
      stat,
      new ImageFillerUpdater(imageDb.copy(ignoreRecentlyTaken = true))
    )

}

class NumberOfMonumentsByNumberOfPictures(
    val stat: ContestStat,
    val imageDb: ImageDB
) extends Reporter {
  val picturesPerMonument =
    imageDb.images.flatMap(_.monumentIds).groupBy(identity).values.map(_.size)
  val numberOfMonumentsByNumberOfPictures = picturesPerMonument
    .groupBy(identity)
    .mapValues(_.size)
    .toSeq
    .sortBy { case (pictures, monuments) => -pictures }

  override val table =
    Table(
      Seq("pictures", "monuments"),
      numberOfMonumentsByNumberOfPictures.map { case (pictures, monuments) =>
        Seq(pictures.toString, monuments.toString)
      }
    )

  override def name: String = "Number Of monuments by number of pictures"
}

object ReporterRegistry {

  def monumentDbStat(db: MonumentDB): String =
    new MonumentDbStat().getStat(Seq(db))

  //  def authorsMonuments(imageDb: ImageDB, newObjectRating: Option[Int] = None): String =
  //    new AuthorMonuments(imageDb, newObjectRating).asText

  def authorsImages(imageDb: ImageDB, monumentDb: Option[MonumentDB]): String =
    new AuthorsStat().authorsImages(imageDb._byAuthor.grouped, monumentDb)

  def authorsContributed(
      imageDbs: Seq[ImageDB],
      totalImageDb: ImageDB,
      monumentDb: Option[MonumentDB]
  ): String = new AuthorsStat().authorsContributed(imageDbs, totalImageDb, monumentDb)

  def withArticles(monumentDb: Option[MonumentDB]): Option[String] =
    monumentDb.map(db => Stats.withArticles(db).asWiki("").asWiki)
}
