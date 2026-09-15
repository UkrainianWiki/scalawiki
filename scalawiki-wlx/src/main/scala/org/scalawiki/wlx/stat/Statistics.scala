package org.scalawiki.wlx.stat

import org.scalawiki.MwBot
import org.scalawiki.cache.CachedBot
import org.scalawiki.dto.{Image, Site}
import org.scalawiki.wlx.dto.Contest
import org.scalawiki.wlx.query.{ImageQuery, MonumentQuery}
import org.scalawiki.wlx.stat.cache.{ImageDbProvider, MonumentDbProvider}
import org.scalawiki.wlx.stat.progress.Progress
import org.scalawiki.wlx.stat.reports.ReportRunner
import org.scalawiki.util.WriteWatcher
import org.scalawiki.wlx.{ImageDB, MonumentCsvExporter, MonumentDB}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.control.NonFatal

/** Holds fetched contest data
  *
  * @param contest
  *   contest: contest type (WLM/WLE), country, year, etc.
  * @param startYear
  *   the first year contest was held, or the first year that we are interested in
  * @param monumentDb
  *   cultural/natural monuments database for the contest
  * @param currentYearImageDb
  *   image database for current year's contest
  * @param totalImageDb
  *   image database that holds images of all monuments from the contest, regardless of when they
  *   where uploaded
  * @param dbsByYear
  *   image databases split by contest year
  */
case class ContestStat(
    contest: Contest,
    startYear: Int,
    monumentDb: Option[MonumentDB] = None,
    currentYearImageDb: ImageDB,
    totalImageDb: ImageDB,
    dbsByYear: Seq[ImageDB] = Nil,
    config: Option[StatConfig] = None
) {

  val imageDbsByYear: Map[Int, Seq[ImageDB]] = dbsByYear.groupBy(_.contest.year)
  val yearSeq: Seq[Int] = imageDbsByYear.keys.toSeq.sorted

  lazy val oldImages: Iterable[Image] = {
    val currentImageIds = currentYearImageDb.images.flatMap(_.pageId).toSet
    totalImageDb.images.filterNot(_.pageId.exists(currentImageIds.contains))
  }

  def imageDbByYear(year: Int): Option[ImageDB] = imageDbsByYear.get(year).map(_.head)

  def mapYears[T](f: ImageDB => T): Seq[T] =
    for {
      year <- yearSeq
      imageDb <- imageDbByYear(year)
    } yield f(imageDb)
}

/** Coordinates fetching contest statistics and creating reports/galleries etc.
  *
  * The heavy lifting lives in focused collaborators:
  *   - [[MonumentDbProvider]] — the monument DB and its CSV cache / revision sync
  *   - [[ImageDbProvider]] — the per-year and all-time image DBs and their CSV cache
  *   - [[ReportRunner]] — running reports and waiting for wiki writes to settle
  *
  * @param contest
  *   contest: contest type (WLM/WLE), country, year, etc.
  * @param startYear
  *   the first year contest was held, or the first year that we are interested in
  * @param monumentQuery
  *   monuments fetcher
  * @param imageQuery
  *   images fetcher
  * @param bot
  *   scalawiki bot instance
  */
class Statistics(
    contest: Contest,
    startYear: Option[Int],
    monumentQuery: MonumentQuery,
    imageQuery: Option[ImageQuery],
    imageQueryWiki: Option[ImageQuery],
    bot: MwBot,
    config: StatConfig
) {

  def this(
      contest: Contest,
      startYear: Option[Int] = None,
      monumentQuery: MonumentQuery,
      imageQuery: Option[ImageQuery] = Some(ImageQuery.create),
      imageQueryWiki: Option[ImageQuery] = None,
      bot: MwBot = MwBot.fromHost(MwBot.commons),
      config: Option[StatConfig] = None
  ) =
    this(
      contest,
      startYear,
      monumentQuery,
      imageQuery,
      imageQueryWiki,
      bot,
      config.getOrElse(StatConfig(contest.campaign))
    )

  private val currentYear = contest.year

  private val contests =
    (startYear.getOrElse(currentYear) to currentYear).map(y => contest.copy(year = y))

  private lazy val monumentProvider =
    new MonumentDbProvider(contest, monumentQuery, config)

  private lazy val imageProvider =
    new ImageDbProvider(contest, imageQuery, imageQueryWiki, config)

  /** Fetches contest data
    *
    * @param total
    *   whether to fetch image database that holds images of all monuments from the contest,
    *   regardless of when they where uploaded
    * @return
    *   asynchronously returned contest data
    */
  def gatherData(total: Boolean): Future[ContestStat] = {
    // the monument-list fetch and the cheap all-time page-rev sweep now overlap
    // (both start here); the per-year image fetches still wait for the monument
    // DB since perYear needs it as input
    val monumentDbF = monumentProvider.gather().map(Some(_))

    val totalPageRevsFuture = imageProvider.prefetchTotalPageRevs(total)

    val byYearLabel =
      if (contests.sizeIs > 1) s"Fetching images ${contests.head.year}-${contests.last.year}"
      else s"Fetching images ${contests.head.year}"
    val byYearF =
      monumentDbF.flatMap { monumentDb =>
        Progress.barF(byYearLabel, contests.size.toLong) { task =>
          Future.sequence(contests.map { yearContest =>
            imageProvider.perYear(monumentDb)(yearContest).map { db =>
              task.step()
              db
            }
          })
        }
      }

    for {
      monumentDb <- monumentDbF
      byYear <- byYearF
      totalPageRevs <- totalPageRevsFuture
      totalImages <- imageProvider.total(monumentDb, byYear, totalPageRevs, total)
    } yield {
      ContestStat(
        contest,
        startYear.getOrElse(contest.year),
        monumentDb,
        byYear.last,
        totalImages,
        byYear,
        Some(config)
      )
    }
  }

  /** Fetch contest data, run every configured report, and block until every
    * wiki write has settled.
    *
    * @return the number of failures (report steps that threw + wiki writes that
    *         errored). 0 means a clean run.
    */
  def run(total: Boolean): Int = {
    Progress.configure(config.progress)
    try {
      // the one sync/async boundary of a stats run: data gathering is fully
      // async, the report pipeline that consumes it is synchronous. No arbitrary
      // timeout — a stuck fetch is the HTTP layer's problem, not ours.
      val stat = Await.result(gatherData(total = total), Duration.Inf)
      new ReportRunner(stat, config).run()
    } finally Progress.close()
  }

  def init(total: Boolean): Unit = {
    run(total)
    ()
  }

  def articleStatistics(monumentDb: MonumentDB): Unit = {
    println(Stats.withArticles(monumentDb).asWiki("Article Statistics").asWiki)
  }

  def toMassMessage(users: Iterable[String]): Iterable[String] = {
    users.map(name => s"{{#target:User talk:$name}}")
  }

  def message(bot: MwBot, user: String, msg: String => String): Unit = {
    bot.page("User_talk:" + user).edit(msg(user), section = Some("new"))
  }
}

object Statistics {

  def main(args: Array[String]): Unit = {
    // logback reads these system properties when it first initialises (about to
    // happen, on the first LoggerFactory call below). `--verbose` lifts the
    // console appender from WARN to INFO (the per-request detail that always goes
    // to logs/scalawiki.log now shows on screen too) and the root logger from
    // INFO to DEBUG (so DEBUG detail reaches the file). Checked directly (not via
    // StatParams) to run before any logger is created; the flag is also declared
    // in StatParams for --help.
    if (args.contains("--verbose") || args.contains("-v")) {
      System.setProperty("sw.console.level", "INFO")
      System.setProperty("sw.root.level", "DEBUG")
    }

    // Track every wiki edit/upload so failures are logged and `main` can wait
    // for them all before shutting the process down.
    WriteWatcher.enable(MwBot.system.log)
    // Record heap/GC use so the final summary shows how much memory the run needed.
    MemoryStats.install()

    var exitCode = 0
    try {
      val cfg = StatParams.parse(args)
      if (cfg.dryRun) {
        org.scalawiki.util.DryRun.enable(cfg.dryRunDir)
        Console.err.println(s"Dry run: nothing will be published; edit text is saved under ${cfg.dryRunDir}")
      }
      val contest = Contest.byCampaign(cfg.campaign, cfg.years.last, cfg.rateConfig)

      // Checked before anything is fetched: filling the lists with a year's rating
      // rules that a later year's already replace would overwrite every list's
      // бали with outdated points.
      val refusal =
        if (cfg.fillListsRating)
          org.scalawiki.wlx.RatingListFiller.pastYearRatingError(contest, cfg.allowPastYearRating)
        else None
      refusal.foreach { error =>
        Console.err.println(error)
        exitCode = 2
      }

      if (refusal.isEmpty && cfg.exportCsv.isDefined) {
        MonumentCsvExporter.exportFromWiki(MonumentQuery.create(contest), cfg.campaign, cfg.exportCsv)
      }

      // Run the full statistics pipeline when either:
      // - no monument CSV export was requested (normal run), or
      // - image CSV export was requested (needs stats pipeline to populate dbsByYear)
      if (refusal.isEmpty && (cfg.exportCsv.isEmpty || cfg.exportImagesCsv.isDefined)) {
        val cacheName = s"${cfg.campaign}-${contest.year}"
        val imageQueryWiki = ImageQuery.create(
          new CachedBot(Site.ukWiki, cacheName + "-wiki", true)
        )

        val stat = new Statistics(
          contest,
          startYear = Some(cfg.years.head),
          monumentQuery = MonumentQuery.create(contest, reportDifferentRegionIds = true),
          config = Some(cfg),
          imageQuery = None,
          imageQueryWiki = Some(imageQueryWiki)
        )

        // rating fill needs the all-time image DB to know which monuments already
        // have photos, even when a single year is requested
        exitCode = stat.run(total = cfg.years.size > 1 || cfg.fillListsRating)
      }
    } catch {
      case NonFatal(e) =>
        println(s"Statistics run failed: $e")
        e.printStackTrace()
        exitCode = 1
    } finally {
      MemoryStats.report()
      // Stop the Pekko ActorSystem so its non-daemon threads no longer keep the
      // JVM alive; without this the process hangs after all reports are done.
      try Await.result(MwBot.system.terminate(), 30.seconds)
      catch { case NonFatal(_) => }
    }

    System.exit(exitCode)
  }
}
