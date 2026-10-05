package org.scalawiki.wlx.stat.cache

import org.scalawiki.cache.CachedBot
import org.scalawiki.dto.{Image, Site}
import org.scalawiki.wlx.dto.Contest
import org.scalawiki.wlx.query.ImageQuery
import org.scalawiki.wlx.query.ImageQuery.PageRevInfo
import org.scalawiki.wlx.stat.StatConfig
import org.scalawiki.wlx.stat.progress.Progress
import org.scalawiki.wlx.ImageCsvImporter.CachedRevision
import org.scalawiki.wlx.{ImageCsvExporter, ImageCsvImporter, ImageDB, MonumentDB}
import org.slf4j.LoggerFactory

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{LocalDate, ZoneOffset, ZonedDateTime}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/** Builds the per-year and all-time [[ImageDB]]s for a contest, going through the
  * image CSV cache when it is enabled.
  *
  * A second-tier cache above the `http-cache/` request cache: once an
  * `ImageDB` has been built it is serialized to `<csvDir>/<campaign>-<year>-images.csv`
  * (and `<campaign>-all-images.csv` with the all-time DB's images that are in
  * no per-year CSV - the all-time DB is those plus the per-year images). Later runs read those
  * CSVs directly and skip the sequential JSON parse of the raw API responses.
  *
  * - `--images-from-csv <dir>` keeps its strict semantics (files must exist);
  *   `--csv-cache-refresh` does not apply there (those files are user-managed
  *   via `--export-images-csv`).
  * - otherwise the cache lives under `csv-cache/` and is filled on demand.
  * - the current contest year is always incrementally synced against the wiki:
  *   a cheap id + latest-revision sweep of the category tells us which files are
  *   new (fetch metadata), which changed since caching (revid differs -> refetch)
  *   and which are gone (dropped). Its CSV is written to the same
  *   `<campaign>-<year>-images.csv` path that next year's run reads as the frozen
  *   past-year copy -- so the last mid-contest sync of year N becomes the
  *   permanent record of year N.
  * - past contest years and the all-images CSV are frozen (read verbatim) unless
  *   `--csv-cache-resync` is given, which runs the same new/changed/deleted sweep
  *   against them (with `--csv-cache-resync-interval N`, at most every N
  *   days). For rows written before the `last_revid` column existed we
  *   have no revid to compare, so a change is assumed only when the live
  *   revision post-dates the moment the CSV was last written (its file mtime).
  *   A resynced past year stays slim: a year the sweep finds unchanged is not
  *   rewritten, and only a changed one is re-read in full to rewrite its CSV, so
  *   a daily resync costs about the memory of a run without it.
  * - deletions are only trusted when the sweep looks complete: the number of
  *   ids it returned is checked against `categoryinfo.files` (and, failing that,
  *   against the cached row count). A short sweep (truncated pagination, a
  *   transient API hiccup) keeps every cached row rather than wiping the CSV.
  * - delete a CSV to force a full refetch (clearing only `http-cache/` does
  *   nothing, the CSV short-circuits before the request cache is consulted).
  * - a CSV missing a column the exporter now writes (e.g. `media_type`) was
  *   written by an older version: it is refetched and rewritten as if absent,
  *   past years included.
  * - `--csv-cache-refresh` ignores existing CSVs and overwrites them.
  */
class ImageDbProvider(
    contest: Contest,
    imageQuery: Option[ImageQuery],
    imageQueryWiki: Option[ImageQuery],
    config: StatConfig
) {

  private val logger = LoggerFactory.getLogger(classOf[ImageDbProvider])

  private val currentYear = contest.year

  private lazy val totalImageQuery: ImageQuery = imageQuery.getOrElse(getImageQuery())

  private lazy val liveImageQuery: ImageQuery = ImageQuery.create

  def getImageQuery(year: Option[Int] = None): ImageQuery = {
    val cacheName = s"${contest.campaign}-${year.getOrElse("all")}"
    ImageQuery.create(new CachedBot(Site.commons, cacheName, true))
  }

  private val csvStrictDir: Option[String] = config.imagesFromCsv
  private val csvAutoCache: Boolean = config.csvCache && csvStrictDir.isEmpty
  private val csvDir: String = config.effectiveCsvCacheDir
  private val csvRefresh: Boolean = config.csvCacheRefresh && csvAutoCache

  /** Where the date of the last completed `--csv-cache-resync` is kept, for
    * `--csv-cache-resync-interval`. */
  private val resyncDateFile = Paths.get(csvDir, s"${contest.campaign}-resync.date")

  private val today = LocalDate.now

  private def lastResync: Option[LocalDate] =
    Try(LocalDate.parse(new String(Files.readAllBytes(resyncDateFile), StandardCharsets.UTF_8).trim)).toOption

  /** With `--csv-cache-resync-interval N`, a resync is due when none has
    * completed in the last N days (by calendar date, so a daily run at a fixed
    * hour resyncs every Nth day). */
  private def resyncDue: Boolean =
    config.csvCacheResyncIntervalDays.forall { days =>
      val last = lastResync
      val due = last.forall(date => !date.isAfter(today.minusDays(days.toLong)))
      if (!due)
        logger.info(
          s"[csv-cache] past years resynced on ${last.get}, less than $days day(s) ago; not resyncing them this run"
        )
      due
    }

  private val csvResync: Boolean =
    config.csvCacheResync && csvAutoCache && !csvRefresh && resyncDue

  /** Record that this run's resync completed, so `--csv-cache-resync-interval`
    * counts from today. Call once every image DB has been built. */
  def resyncCompleted(): Unit =
    if (csvResync)
      Try {
        Files.createDirectories(resyncDateFile.getParent)
        Files.write(resyncDateFile, today.toString.getBytes(StandardCharsets.UTF_8))
      }.failed.foreach(e => logger.warn(s"[csv-cache] could not record the resync date in $resyncDateFile: $e"))

  /** Shared by every CSV read this run, so a value repeated across files (an
    * author, a category) is held once. */
  private val valuePool = new ImageCsvImporter.ValuePool

  /** Past years' images are loaded slim (see `ImageCsvImporter.slim`) unless
    * `--export-images-csv` exports them. A `--csv-cache-resync` that changes a
    * year re-reads that year's CSV in full to rewrite it (see [[SlimRows]]). */
  private val slimPastYears: Boolean = config.exportImagesCsv.isEmpty

  /** Slim cached rows being synced: their revisions (which slim images drop),
    * and how to re-read them in full when the CSV has to be rewritten. */
  private case class SlimRows(revisions: Map[Long, CachedRevision], readFull: () => Seq[Image])

  /** When the CSV at `path` was last written — the instant the cache was known
    * accurate. Used as the "changed since" cut-off for rows that predate the
    * `last_revid` column (no revid to diff). Falls back to "now" (nothing looks
    * changed) if the mtime can't be read, keeping the first resync cheap. */
  private def cacheWrittenAt(path: String): ZonedDateTime =
    Try(Files.getLastModifiedTime(Paths.get(path)).toInstant.atZone(ZoneOffset.UTC))
      .getOrElse(ZonedDateTime.now(ZoneOffset.UTC))

  /** Whether an id sweep of `swept` entries can be trusted to be exhaustive
    * enough to act on deletions. `categoryinfo.files` is the reference when
    * available (it lags reality by a job-queue cycle, hence the 10% slack);
    * without it, only a sweep that still covers most of the cached rows is
    * trusted. An empty sweep against a non-empty cache never is. */
  private def sweepLooksComplete(swept: Int, cachedCount: Int, expectedFiles: Option[Long]): Boolean =
    if (swept == 0 && cachedCount > 0) false
    else
      expectedFiles match {
        case Some(expected) => swept >= expected * 0.9
        case None           => swept >= cachedCount * 0.5
      }

  private def yearCsvPath(year: Int): String =
    ImageCsvExporter.filename(contest.campaign, year, isCurrent = false, csvDir)

  /** Whether the auto-cache CSV at `path` can be read: it exists and has every
    * current column (see the class doc). */
  private def usableCacheCsv(path: String): Boolean =
    new File(path).exists() && {
      val current = ImageCsvImporter.hasCurrentColumns(path)
      if (!current)
        logger.info(s"[csv-cache] $path predates the current CSV columns; refetching it")
      current
    }

  private def totalCsvReadPath: Option[String] =
    csvStrictDir
      .map(dir => ImageCsvExporter.totalFilename(contest.campaign, dir))
      .orElse(if (csvAutoCache) Some(ImageCsvExporter.totalFilename(contest.campaign, csvDir)) else None)

  /** The all-images CSV to read this run, or `None` when it should be (re)fetched
    * (`--csv-cache-refresh`, no usable file, or the cache is off). */
  private lazy val existingTotalCsvPath: Option[String] =
    if (csvRefresh) None
    else if (csvStrictDir.isDefined) totalCsvReadPath.filter(new File(_).exists())
    else totalCsvReadPath.filter(usableCacheCsv)

  private def writeCsvCache(imageDb: ImageDB): Unit =
    if (csvAutoCache)
      ImageCsvExporter.export(imageDb, contest.campaign, isCurrent = false, csvDir)

  private def writeTotalCsvCache(imageDb: ImageDB): Unit =
    if (csvAutoCache)
      ImageCsvExporter.exportTotal(imageDb, contest.campaign, csvDir)

  /** Per-year image DB: current-year incremental sync, past-year read/resync, or
    * a full fetch when there is no cache. */
  def perYear(monumentDb: Some[MonumentDB])(yearContest: Contest): Future[ImageDB] =
    if (yearContest.year != currentYear) pastYearImages(monumentDb)(yearContest)
    else currentYearImages(monumentDb)(yearContest)

  /** Cheap page-id + revision sweep for the all-time template, started before the
    * per-year fetches so the two overlap. `Nil` when the all-time DB is not
    * wanted or an existing all-images CSV will be used instead. */
  def prefetchTotalPageRevs(wantTotal: Boolean): Future[Seq[PageRevInfo]] =
    if (wantTotal && existingTotalCsvPath.isEmpty) imageRevsByTemplate()
    else Future.successful(Nil)

  /** The all-time image DB: resync an existing all-images CSV, read it verbatim,
    * or fetch by template and cache it. When `wantTotal` is false this is just
    * the current year's DB (`dbsByYear.last`). */
  def total(
      monumentDb: Some[MonumentDB],
      dbsByYear: Seq[ImageDB],
      totalPageRevs: Seq[PageRevInfo],
      wantTotal: Boolean
  ): Future[ImageDB] = {
    val currentYearImages = dbsByYear.last
    if (!wantTotal) Future.successful(currentYearImages)
    else
      existingTotalCsvPath match {
        case Some(path) if csvResync => resyncTotalCsv(monumentDb, dbsByYear, path)
        case Some(path)              => Future.successful(totalFromCsv(monumentDb, dbsByYear, path))
        case None                    => imagesByTemplate(monumentDb, dbsByYear, totalPageRevs)
      }
  }

  /** The all-time DB from the all-images CSV: the per-year images (the same
    * objects, not copies) plus the CSV's other rows. A CSV written before it
    * stopped repeating per-year images still reads correctly - those rows are
    * skipped, the per-year copy being the more recently synced one - and is
    * rewritten without them so later runs don't parse them again. */
  private def totalFromCsv(monumentDb: Some[MonumentDB], dbsByYear: Seq[ImageDB], path: String): ImageDB = {
    val perYearIds = ImageCsvExporter.perYearPageIds(dbsByYear)
    var skipped = 0
    val extras = ImageCsvImporter.imagesFromCsv(
      path,
      image => {
        val duplicate = ImageCsvExporter.inPerYear(perYearIds)(image)
        if (duplicate) skipped += 1
        !duplicate
      },
      pool = valuePool
    )
    if (skipped > 0) {
      logger.info(
        s"[csv-cache] all-images: skipped $skipped rows already in the per-year CSVs; " +
          s"rewriting it with the other ${extras.size}"
      )
      writeTotalCsvCache(new ImageDB(contest, extras, monumentDb))
    }
    new ImageDB(contest, dbsByYear.flatMap(_.images) ++ extras, monumentDb, config.minMpx)
  }

  private def pastYearImages(monumentDb: Some[MonumentDB])(yearContest: Contest): Future[ImageDB] = {
    val year = yearContest.year
    val path = yearCsvPath(year)
    csvStrictDir match {
      case Some(_) =>
        // strict, user-managed CSVs: read verbatim (throws if missing)
        Future.successful(
          new ImageDB(yearContest, imagesFromCsvOpt(year).getOrElse(Nil), monumentDb, config.minMpx)
        )
      case None if csvAutoCache && !csvRefresh && usableCacheCsv(path) =>
        if (!csvResync) {
          val cached = ImageCsvImporter.imagesFromCsv(path, pool = valuePool, slim = slimPastYears)
          Future.successful(new ImageDB(yearContest, cached, monumentDb, config.minMpx))
        } else if (slimPastYears) {
          val (cached, revisions) =
            ImageCsvImporter.imagesWithRevisionsFromCsv(path, valuePool, slim = true)
          // a fresh pool: the shared one would keep every full category set alive
          val readFull = () => ImageCsvImporter.imagesFromCsv(path, pool = new ImageCsvImporter.ValuePool)
          syncYearFromCategory(yearContest, monumentDb, cached, path, Some(SlimRows(revisions, readFull)))
        } else
          syncYearFromCategory(
            yearContest,
            monumentDb,
            ImageCsvImporter.imagesFromCsv(path, pool = valuePool),
            path
          )
      case None =>
        fetchImageDb(yearContest, monumentDb).map { db =>
          writeCsvCache(db)
          if (slimPastYears) db.copy(images = db.images.map(ImageCsvImporter.slim(_, valuePool)).toVector)
          else db
        }
    }
  }

  private def currentYearImages(monumentDb: Some[MonumentDB])(yearContest: Contest): Future[ImageDB] = {
    val path = yearCsvPath(yearContest.year)
    if (csvAutoCache && !csvRefresh && usableCacheCsv(path))
      syncYearFromCategory(yearContest, monumentDb, ImageCsvImporter.imagesFromCsv(path, pool = valuePool), path)
    else
      // Live, not through the `http-cache/` request cache: the current year's
      // category is still growing, and a cached response (e.g. the empty listing
      // from before the first upload) would be replayed forever - an empty DB
      // writes no CSV, so the next run would come straight back here.
      fetchImageDb(yearContest, monumentDb, Some(imageQuery.getOrElse(liveImageQuery))).map { db =>
        writeCsvCache(db)
        db
      }
  }

  /** Reconcile a per-year CSV cache against a fresh category id + revision sweep.
    * Shared by the always-on current-year sync and the `--csv-cache-resync`
    * past-year sync. */
  private def syncYearFromCategory(
      yearContest: Contest,
      monumentDb: Some[MonumentDB],
      cached: Seq[Image],
      path: String,
      slimRows: Option[SlimRows] = None
  ): Future[ImageDB] = {
    val query = imageQuery.getOrElse(liveImageQuery)
    // "changed since" cut-off for pre-last_revid rows: once a past year's upload
    // window has closed nothing legitimate changes after it, so it is the exact
    // instant the cache became authoritative. Mid-contest (window end still in
    // the future) fall back to when the CSV was last written.
    val now = ZonedDateTime.now(ZoneOffset.UTC)
    val cutoff = yearContest
      .dates()
      .flatMap(_.uploadEndInstant)
      .filter(_.isBefore(now))
      .getOrElse(cacheWrittenAt(path))
    for {
      liveRevs <- query.imageIdsFromCategory(yearContest)
      expectedFiles <- query.categoryFileCount(yearContest)
      db <- syncImageDb(
        yearContest,
        monumentDb,
        cached,
        writeCsvCache,
        liveRevs,
        cutoff,
        ids => query.imagesWithTemplateByIds(yearContest, ids),
        sweepComplete =
          sweepLooksComplete(liveRevs.size, cached.count(_.pageId.isDefined), expectedFiles),
        slimRows = slimRows
      )
    } yield db
  }

  /** Incrementally reconcile a cached image set against a fresh id + latest-revision
    * sweep of the wiki:
    *   - ids in the sweep but not the cache  -> fetched (new uploads)
    *   - ids in both whose revision changed  -> refetched (page edited / reuploaded)
    *   - ids in the cache but not the sweep  -> dropped, *only* when `sweepComplete`
    *   - everything else kept as-is (revid/timestamp backfilled from the sweep)
    *
    * "changed" is a `revId` mismatch when both the cached row and the sweep entry
    * expose one; for rows written before the column existed (no cached revid) it
    * is the live revision timestamp being after the row's own timestamp, or
    * `fallbackTs` when it has none. A sweep entry with no revid
    * (revision-deleted current revision) is treated as unchanged.
    *
    * `extraImages` are appended unconditionally (e.g. uk.wikipedia-hosted images
    * for the all-images CSV, which live in a different page-id space).
    *
    * With `slimRows`, `cached` are slim images: the diff runs on
    * `slimRows.revisions`, and when it finds nothing to change (no new, changed or
    * gone row, no revid to backfill) the slim rows are returned and the CSV is not
    * rewritten. Otherwise the rows are re-read in full, synced and written as
    * usual, and the result is slimmed.
    */
  private def syncImageDb(
      yearContest: Contest,
      monumentDb: Option[MonumentDB],
      cached: Seq[Image],
      writeCache: ImageDB => Unit,
      liveRevs: Seq[PageRevInfo],
      fallbackTs: ZonedDateTime,
      fetch: Set[Long] => Future[Iterable[Image]],
      extraImages: Iterable[Image] = Nil,
      sweepComplete: Boolean = true,
      slimRows: Option[SlimRows] = None
  ): Future[ImageDB] = {
    val liveById = liveRevs.iterator.map(r => r.pageId -> r).toMap
    val cachedRevs: Map[Long, CachedRevision] = slimRows
      .map(_.revisions)
      .getOrElse(cached.iterator.flatMap(i => i.pageId.map(_ -> CachedRevision(i.revId, i.revTs))).toMap)

    val newIds = liveById.keySet -- cachedRevs.keySet
    val changedIds = (liveById.keySet intersect cachedRevs.keySet).filter { id =>
      val live = liveById(id)
      val row = cachedRevs(id)
      (row.revId, live.revId) match {
        case (Some(cachedRev), Some(liveRev)) => cachedRev != liveRev
        case (Some(_), None)                  => false // revdel'd sweep entry: can't tell, keep
        case (None, _) =>
          live.timestamp.exists(_.isAfter(row.revTs.getOrElse(fallbackTs)))
      }
    }
    val refetch = newIds ++ changedIds

    val goneIds =
      if (sweepComplete) cachedRevs.keySet -- liveById.keySet
      else {
        val missing = cachedRevs.keySet -- liveById.keySet
        if (missing.nonEmpty)
          logger.warn(
            s"[csv-cache] ${yearContest.year}: sweep returned ${liveById.size} ids for " +
              s"${cachedRevs.size} cached rows — treating it as incomplete, keeping " +
              s"${missing.size} unmatched row(s) instead of deleting them"
          )
        Set.empty[Long]
      }

    // rows written before the last_revid column whose revid the sweep can fill in
    def backfillable: Boolean = cachedRevs.exists { case (id, row) =>
      row.revId.isEmpty && liveById.get(id).exists(_.revId.isDefined)
    }

    if (slimRows.isDefined && refetch.isEmpty && goneIds.isEmpty && !backfillable) {
      logger.info(s"[csv-cache] ${yearContest.year}: unchanged since cached, not rewriting it")
      Future.successful(new ImageDB(yearContest, cached, monumentDb, config.minMpx))
    } else {
      def backfill(i: Image): Image =
        i.pageId.flatMap(liveById.get) match {
          case Some(live) =>
            i.copy(revId = live.revId.orElse(i.revId), revTs = live.timestamp.orElse(i.revTs))
          case None => i
        }

      val fetchedFuture =
        if (refetch.isEmpty) Future.successful(Iterable.empty[Image]) else fetch(refetch)
      fetchedFuture.map { fetched =>
        // slim rows can't be written back: re-read them in full, only now that
        // the fetch is done, so they aren't held during it
        val rows = slimRows.fold(cached)(_.readFull())
        val kept = rows.collect {
          case i
              if i.pageId.exists(id => !refetch.contains(id) && !goneIds.contains(id)) ||
                i.pageId.isEmpty =>
            backfill(i)
        }

        val fetchedIds = fetched.flatMap(_.pageId).toSet
        // A partial refetch (transient error resolving some ids) must not silently
        // drop a row we still know about: fall back to the stale cached copy.
        val missedRefetch = refetch -- fetchedIds
        val staleKept = rows.filter(_.pageId.exists(id => missedRefetch.contains(id) && !newIds.contains(id)))
        if (missedRefetch.nonEmpty)
          logger.warn(
            s"[csv-cache] ${yearContest.year}: refetch returned ${fetchedIds.size}/${refetch.size} " +
              s"images; keeping ${staleKept.size} stale row(s), ${(missedRefetch -- staleKept.flatMap(_.pageId).toSet).size} new id(s) lost this run"
          )

        val db = new ImageDB(
          yearContest,
          dedupByPageId(kept ++ fetched ++ staleKept ++ extraImages),
          monumentDb,
          config.minMpx
        )
        writeCache(db)
        if (slimRows.isDefined) db.copy(images = db.images.map(ImageCsvImporter.slim(_, valuePool)).toVector)
        else db
      }
    }
  }

  /** Keep the first image seen for each page id (rows with no page id pass
    * through). Order of preference is the caller's list order. */
  private def dedupByPageId(images: Iterable[Image]): Seq[Image] = {
    val seen = scala.collection.mutable.Set.empty[Long]
    images.iterator.filter { i =>
      i.pageId match {
        case Some(id) => seen.add(id)
        case None     => true
      }
    }.toVector
  }

  /** Resync the all-images CSV's rows (the images in no per-year CSV): a live
    * revid sweep of the Commons contest template, minus the per-year page ids,
    * diffed against the Commons-hosted cached rows, plus a fresh fetch of the
    * uk.wikipedia-hosted images (small set, different page-id space, so always
    * refetched rather than diffed). Cached rows are Commons-hosted unless their
    * `page_url` says otherwise; uk.wiki rows are never treated as deleted by the
    * Commons sweep. The per-year images, synced through their own CSVs, are
    * joined in front, as on the full-rebuild path. */
  private def resyncTotalCsv(
      monumentDb: Option[MonumentDB],
      dbsByYear: Seq[ImageDB],
      path: String
  ): Future[ImageDB] = {
    val perYearIds = ImageCsvExporter.perYearPageIds(dbsByYear)
    // skips the per-year copies an older, full all-images CSV still repeats
    val cached =
      ImageCsvImporter.imagesFromCsv(path, image => !ImageCsvExporter.inPerYear(perYearIds)(image), pool = valuePool)
    val (cachedWiki, cachedCommons) = cached.partition(ImageCsvExporter.isProjectWikiHosted)
    val writtenAt = cacheWrittenAt(path)
    val query = imageQuery.getOrElse(liveImageQuery)
    for {
      templateRevs <- query.imageIdsWithTemplate(contest)
      // per-year files are synced via their own CSVs; diffing them here would refetch them all
      commonsRevs = templateRevs.filterNot(r => perYearIds.contains(r.pageId))
      freshWiki <- imageQueryWiki.map(_.imagesWithTemplate(contest)).getOrElse(Future.successful(Nil))
      // an empty uk.wiki refetch when one was configured means the fetch failed;
      // fall back to the cached wiki rows and, since some of those may sit in
      // cachedCommons (rows cached without a page_url can't be told apart), also
      // stop trusting the sweep for deletions this run
      wikiFetchTrustworthy = freshWiki.nonEmpty || imageQueryWiki.isEmpty
      wiki =
        if (wikiFetchTrustworthy) freshWiki
        else {
          logger.warn(
            "[csv-cache] all-images: uk.wiki refetch returned nothing — keeping all cached " +
              "rows and skipping Commons deletion detection this run"
          )
          cachedWiki
        }
      extras <- syncImageDb(
        contest,
        monumentDb,
        cachedCommons,
        writeTotalCsvCache,
        commonsRevs,
        writtenAt,
        ids => query.imagesWithTemplateByIds(contest, ids),
        extraImages = wiki,
        sweepComplete = wikiFetchTrustworthy &&
          sweepLooksComplete(commonsRevs.size, cachedCommons.count(_.pageId.isDefined), None)
      )
    } yield new ImageDB(contest, dbsByYear.flatMap(_.images) ++ extras.images, monumentDb, config.minMpx)
  }

  private def fetchImageDb(
      yearContest: Contest,
      monumentDb: Some[MonumentDB],
      query: Option[ImageQuery] = None
  ): Future[ImageDB] =
    ImageDB.create(
      yearContest,
      query.orElse(imageQuery).getOrElse(getImageQuery(Some(yearContest.year))),
      monumentDb,
      config.minMpx
    )

  private def imagesFromCsvOpt(year: Int): Option[Seq[Image]] =
    config.imagesFromCsv.map { dir =>
      val path = ImageCsvExporter.filename(contest.campaign, year, isCurrent = false, dir)
      if (!new File(path).exists()) {
        throw new FileNotFoundException(
          s"--images-from-csv was set but $path is missing. " +
            s"Run --export-images-csv for campaign=${contest.campaign} year=$year first."
        )
      }
      // only past years come from here; they're never written back
      ImageCsvImporter.imagesFromCsv(path, pool = valuePool, slim = slimPastYears)
    }

  private def imagesByTemplate(
      monumentDb: Some[MonumentDB],
      dbsByYear: Seq[ImageDB],
      totalPageRevs: Seq[PageRevInfo]
  ): Future[ImageDB] = {
    val idsByYear = dbsByYear.flatMap(_.images.flatMap(_.pageId)).toSet
    val missingPageIds = totalPageRevs.map(_.pageId).toSet -- idsByYear
    Progress.phaseF(s"Fetching all-time images (${missingPageIds.size} files)") {
      for {
        commons <- totalImageQuery.imagesWithTemplateByIds(contest, missingPageIds)
        wiki <- imageQueryWiki.map(_.imagesWithTemplate(contest)).getOrElse(Future.successful(Nil))
      } yield {
        // the per-year images have their own CSVs; cache only the rest
        writeTotalCsvCache(new ImageDB(contest, (commons ++ wiki).toSeq, monumentDb))
        new ImageDB(contest, dbsByYear.flatMap(_.images) ++ commons ++ wiki, monumentDb)
      }
    }
  }

  private def imageRevsByTemplate(): Future[Seq[PageRevInfo]] =
    totalImageQuery.imageIdsWithTemplate(contest)
}
