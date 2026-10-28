package org.scalawiki.wlx.stat.cache

import org.scalawiki.wlx.dto.Contest
import org.scalawiki.wlx.query.MonumentQuery
import org.scalawiki.wlx.query.MonumentQuery.MonumentListPage
import org.scalawiki.wlx.stat.StatConfig
import org.scalawiki.wlx.stat.progress.Progress
import org.scalawiki.wlx.{MonumentDB, MonumentDbCache}
import org.slf4j.LoggerFactory

import java.io.File

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

/** Builds the [[MonumentDB]] for a contest, going through the on-disk monument list cache
  * ([[MonumentDbCache]], `csv-cache/<campaign>-monuments.csv`) when it is enabled.
  *
  * Fetching and parsing every monument list page from the wiki is the long pole of a stats run, so
  * with a cache present this runs a cheap `embeddedin` id+timestamp sweep, refetches only the pages
  * whose revision changed, drops pages a complete sweep confirms are gone, reuses the rest, and
  * rewrites the cache. `--monument-cache-refresh` (or the shared `--csv-cache-refresh`) forces a
  * full refetch; any sync error also falls back to a full fetch.
  */
class MonumentDbProvider(
    contest: Contest,
    monumentQuery: MonumentQuery,
    config: StatConfig
) {

  private val logger = LoggerFactory.getLogger(classOf[MonumentDbProvider])

  private val csvDir: String = config.effectiveCsvCacheDir

  // Monument lists share the CSV cache toggle; `--monument-cache-refresh` (or the
  // shared `--csv-cache-refresh`) forces a full refetch instead of a revision diff.
  private val cacheActive: Boolean = config.csvCache && config.imagesFromCsv.isEmpty
  private val refresh: Boolean =
    cacheActive && (config.monumentCacheRefresh || config.csvCacheRefresh)

  private def cachePath: String =
    MonumentDbCache.filename(contest.campaign, csvDir)

  private def listConfig = contest.uploadConfigs.head.listConfig

  /** Best-effort: a cache-write failure (read-only dir, disk full) must not fail the run or trigger
    * a full refetch — the wiki data we just built is fine.
    */
  private def persist(pages: Iterable[MonumentListPage]): Unit =
    if (cacheActive)
      try {
        MonumentDbCache.write(cachePath, pages)
        val (np, nm) = (pages.size, pages.iterator.map(_.monuments.size).sum)
        logger.info(s"[monument-cache] wrote $nm monuments from $np pages to $cachePath")
      } catch {
        case NonFatal(e) =>
          logger.warn(s"[monument-cache] could not write $cachePath: $e")
      }

  /** The monument DB for `contest`, via the CSV cache when it is enabled:
    *   - no cache file / `--monument-cache-refresh` / cache off -> full fetch, then (best effort)
    *     write the cache;
    *   - otherwise -> a cheap embeddedin id+revision sweep, refetch only the list pages whose
    *     revision changed, drop the ones the sweep confirms are gone, reuse the rest; rewrite the
    *     cache.
    */
  def gather(): Future[MonumentDB] = {
    val path = cachePath
    val template = monumentQuery.defaultListTemplate

    def fullFetch(): Future[MonumentDB] =
      Progress.phaseF("Fetching monument lists")(
        MonumentDB.getMonumentDb(contest, monumentQuery)
      )

    // After a full fetch the parsed monuments carry only their source page
    // title; pair each page with its current revision via one cheap sweep so the
    // next run can diff against it.
    def writeFullCache(db: MonumentDB): Future[Unit] =
      if (!cacheActive) Future.unit
      else
        Future.unit
          .flatMap(_ => monumentQuery.listPageRevs(template))
          .map { revList =>
            val revs = revList.map(r => r.title -> r).toMap
            persist(db.monuments.groupBy(_.page).toSeq.map { case (title, ms) =>
              val r = revs.get(title)
              MonumentListPage(title, r.flatMap(_.revId), r.flatMap(_.timestamp), ms.toSeq)
            })
          }
          .recover { case NonFatal(e) =>
            logger.warn(s"[monument-cache] revision sweep for cache write failed: $e")
          }

    def fetchAndCache(): Future[MonumentDB] =
      fullFetch().flatMap(db => writeFullCache(db).map(_ => db))

    if (!cacheActive || refresh || !new File(path).exists())
      fetchAndCache()
    else
      // syncMonumentDb starts with a synchronous prefix (reading + parsing the
      // CSV); run it inside the Future so a corrupt/format-changed cache file
      // falls back to a full fetch instead of throwing out of gather().
      Future.unit
        .flatMap(_ => syncMonumentDb(path, template))
        .recoverWith { case NonFatal(e) =>
          logger.warn(s"[monument-cache] sync failed ($e); refetching in full")
          fetchAndCache()
        }
  }

  private def syncMonumentDb(path: String, template: String): Future[MonumentDB] = {
    val cached = MonumentDbCache.read(path, listConfig)
    val cachedByTitle = cached.map(p => p.title -> p).toMap

    Progress
      .phaseF("Checking monument lists")(
        Future.unit.flatMap(_ => monumentQuery.listPageRevs(template))
      )
      .flatMap { live =>
        val liveTitles = live.map(_.title).toSet

        val changedTitles = live.iterator.collect {
          case r
              if cachedByTitle
                .get(r.title)
                .forall(c => r.revId.isEmpty || c.revId != r.revId) =>
            r.title
        }.toSet

        // A truncated sweep must not read as a mass deletion.
        val sweepComplete =
          !(live.isEmpty && cached.nonEmpty) && live.size >= cached.size * 0.5
        val dropTitles: Set[String] =
          if (sweepComplete) cachedByTitle.keySet -- liveTitles else Set.empty

        val refreshedF: Future[Seq[MonumentListPage]] =
          if (changedTitles.isEmpty) Future.successful(Nil)
          else
            Progress.barF(
              "Fetching changed monument lists",
              changedTitles.size.toLong
            ) { task =>
              monumentQuery.monumentsByPages(changedTitles, None).map { pages =>
                task.stepTo(pages.size.toLong)
                pages
              }
            }

        refreshedF.map { refreshed =>
          val reused =
            (cachedByTitle -- dropTitles -- changedTitles).values.toVector
          val merged: Seq[MonumentListPage] = reused ++ refreshed

          logger.info(
            s"[monument-cache] ${cached.size} cached pages: ${changedTitles.size} changed/new, " +
              s"${dropTitles.size} removed, ${reused.size} reused" +
              (if (sweepComplete) "" else " (sweep looked short - deletions skipped)")
          )
          persist(merged)

          val monuments = merged.flatMap(_.monuments)
          val filtered =
            if (contest.country.code == "ru")
              monuments.filter(_.page.contains("Природные памятники России"))
            else monuments
          new MonumentDB(contest, filtered)
        }
      }
  }
}
