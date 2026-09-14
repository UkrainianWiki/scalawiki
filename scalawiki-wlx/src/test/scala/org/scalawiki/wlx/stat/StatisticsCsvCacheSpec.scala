package org.scalawiki.wlx.stat

import org.scalawiki.MwBot
import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.query.ImageQuery.PageRevInfo
import org.scalawiki.wlx.query.{ImageQuery, MonumentQuery}
import org.scalawiki.wlx.{ImageCsvExporter, ImageCsvImporter, ImageDB}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.FutureMatchers
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import spray.util.pimpFuture

import java.nio.file.{Files, Path, Paths}
import java.time.ZonedDateTime
import scala.concurrent.Future

class StatisticsCsvCacheSpec(implicit ee: ExecutionEnv)
    extends Specification
    with Mockito
    with FutureMatchers {

  private val contest = Contest.WLEUkraine(2016)
  private val prevContest = contest.copy(year = 2015)
  private val campaign = contest.campaign

  private val monuments = Seq(new Monument(id = "123", name = "m"))

  private val duringContest = ZonedDateTime.parse("2016-05-01T00:00:00Z")
  private val afterContest = ZonedDateTime.parse("2017-03-01T00:00:00Z")

  private def img(title: String, id: Long, revId: Option[Long] = None): Image =
    Image(title, pageId = Some(id), monumentIds = Seq("123"), revId = revId,
      revTs = revId.map(_ => duringContest))

  /** A live-sweep entry; timestamp defaults to inside the contest window. */
  private def rev(id: Long, revId: Long, ts: ZonedDateTime = duringContest): PageRevInfo =
    PageRevInfo(id, Some(revId), Some(ts))

  private def cacheDir(): Path = Files.createTempDirectory("stats-csv-cache")

  private def yearCsv(dir: Path, year: Int) =
    Paths.get(ImageCsvExporter.filename(campaign, year, isCurrent = false, dir.toString))

  private def rowCount(p: Path): Int =
    ImageCsvImporter.imagesFromCsv(p.toString).size

  private def newImageQuery(): ImageQuery = {
    val q = mock[ImageQuery]
    // safe defaults so an unexpected sync path doesn't NPE
    q.imageIdsFromCategory(any[Contest]) returns Future.successful(Seq.empty[PageRevInfo])
    q.imageIdsWithTemplate(any[Contest]) returns Future.successful(Seq.empty[PageRevInfo])
    q.categoryFileCount(any[Contest]) returns Future.successful(None)
    q.imagesFromCategory(any[Contest]) returns Future.successful(Nil)
    q.imagesWithTemplate(any[Contest]) returns Future.successful(Nil)
    q.imagesWithTemplateByIds(any[Contest], any[Set[Long]]) returns Future.successful(Nil)
    q
  }

  private def stats(
      dir: Path,
      imageQuery: ImageQuery,
      startYear: Option[Int] = None,
      csvCache: Boolean = true,
      csvCacheRefresh: Boolean = false,
      csvCacheResync: Boolean = false,
      imagesFromCsv: Option[String] = None,
      exportImagesCsv: Option[String] = None
  ): Statistics = {
    val monumentQuery = mock[MonumentQuery]
    monumentQuery.byMonumentTemplate(date = None) returns Future.successful(monuments)
    monumentQuery.listPageRevs(any[String]) returns Future.successful(Nil)

    val cfg = StatConfig(
      campaign = campaign,
      csvCacheDir = dir.toString,
      csvCache = csvCache,
      csvCacheRefresh = csvCacheRefresh,
      csvCacheResync = csvCacheResync,
      imagesFromCsv = imagesFromCsv,
      exportImagesCsv = exportImagesCsv
    )
    new Statistics(contest, startYear, monumentQuery, Some(imageQuery), None, mock[MwBot], cfg)
  }

  "past contest year" should {

    "be fetched then written to the CSV cache on the first run, read on the next" in {
      val dir = cacheDir()
      val pastImages = Seq(img("File:A.jpg", 1L))

      val q1 = newImageQuery()
      q1.imagesFromCategory(prevContest) returns Future.successful(pastImages)
      q1.imagesFromCategory(contest) returns Future.successful(Nil)

      stats(dir, q1, startYear = Some(2015)).gatherData(total = false).await

      Files.exists(yearCsv(dir, 2015)) must beTrue

      // second run: a fresh query mock that fails if asked for 2015
      val q2 = newImageQuery()
      q2.imagesFromCategory(prevContest) returns Future.failed(new RuntimeException("should not fetch"))
      q2.imagesFromCategory(contest) returns Future.successful(Nil)

      val data = stats(dir, q2, startYear = Some(2015)).gatherData(total = false).await

      data.imageDbByYear(2015).map(_.images.map(_.title).toSeq) must beSome(Seq("File:A.jpg"))
      there was no(q2).imagesFromCategory(prevContest)
    }

    "be refetched and overwritten when csvCacheRefresh is set" in {
      val dir = cacheDir()
      ImageCsvExporter.export(new ImageDB(prevContest, Seq(img("File:Stale.jpg", 9L)), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imagesFromCategory(prevContest) returns Future.successful(Seq(img("File:Fresh.jpg", 1L)))
      q.imagesFromCategory(contest) returns Future.successful(Nil)

      val data = stats(dir, q, startYear = Some(2015), csvCacheRefresh = true)
        .gatherData(total = false).await

      data.imageDbByYear(2015).map(_.images.map(_.title).toSeq) must beSome(Seq("File:Fresh.jpg"))
      ImageCsvImporter.imagesFromCsv(yearCsv(dir, 2015).toString).map(_.title) must_== Seq("File:Fresh.jpg")
    }

    "be read verbatim (no wiki sweep) without --csv-cache-resync" in {
      val dir = cacheDir()
      ImageCsvExporter.export(new ImageDB(prevContest, Seq(img("File:P.jpg", 1L)), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imagesFromCategory(contest) returns Future.successful(Nil)

      val data = stats(dir, q, startYear = Some(2015)).gatherData(total = false).await

      data.imageDbByYear(2015).map(_.images.map(_.title).toSeq) must beSome(Seq("File:P.jpg"))
      there was no(q).imageIdsFromCategory(prevContest)
    }

    "be loaded slim when read verbatim, but full with --csv-cache-resync or --export-images-csv" in {
      val dir = cacheDir()
      val full = img("File:P.jpg", 1L, revId = Some(100L)).copy(
        url = Some("https://upload.wikimedia.org/p.jpg"),
        categories = Set("Some category", "Ineligible submissions for WLM 2015 in Ukraine")
      )
      ImageCsvExporter.export(new ImageDB(prevContest, Seq(full), None), campaign, isCurrent = false, dir.toString)

      def pastImage(resync: Boolean = false, export: Option[String] = None): Image = {
        val q = newImageQuery()
        q.imagesFromCategory(contest) returns Future.successful(Nil)
        q.imageIdsFromCategory(prevContest) returns Future.successful(Seq(rev(1L, 100L)))
        stats(dir, q, startYear = Some(2015), csvCacheResync = resync, exportImagesCsv = export)
          .gatherData(total = false).await
          .imageDbByYear(2015).get.images.head
      }

      val slim = pastImage()
      slim.title must_== "File:P.jpg"
      slim.monumentIds must_== Seq("123")
      slim.url must beNone
      slim.revId must beNone
      slim.categories must_== Set("Ineligible submissions for WLM 2015 in Ukraine")

      pastImage(resync = true) must_== full
      pastImage(export = Some(cacheDir().toString)) must_== full
      // the cached CSV still has everything
      ImageCsvImporter.imagesFromCsv(yearCsv(dir, 2015).toString) must_== Seq(full)
    }

    "with --csv-cache-resync: keep unchanged, refetch changed, drop deleted" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(prevContest, Seq(
          img("File:Keep.jpg", 1L, revId = Some(100L)),
          img("File:Edited.jpg", 2L, revId = Some(200L)),
          img("File:Deleted.jpg", 3L, revId = Some(300L))
        ), None),
        campaign, isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(prevContest) returns Future.successful(Seq(
        rev(1L, 100L),          // unchanged
        rev(2L, 222L)           // revid bumped -> changed
        // id 3 absent -> deleted
      ))
      q.imagesWithTemplateByIds(prevContest, Set(2L)) returns
        Future.successful(Seq(img("File:Edited-v2.jpg", 2L, revId = Some(222L))))

      val data = stats(dir, q, startYear = Some(2015), csvCacheResync = true)
        .gatherData(total = false).await

      data.imageDbByYear(2015).get.images.map(_.title).toSet must_==
        Set("File:Keep.jpg", "File:Edited-v2.jpg")
      there was one(q).imagesWithTemplateByIds(prevContest, Set(2L))
    }

    "with --csv-cache-resync: migrate a row with no revid using the upload-window end" in {
      // WLE UA 2015 upload window closed 2015-05-31 (dates.2015 in wle_ua.conf),
      // so that instant is the "changed since" cut-off for the no-revid rows.
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(prevContest, Seq(
          img("File:Untouched.jpg", 1L),   // no revid, no revTs
          img("File:LaterEdit.jpg", 2L)
        ), None),
        campaign, isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(prevContest) returns Future.successful(Seq(
        rev(1L, 10L, ts = ZonedDateTime.parse("2015-05-15T00:00:00Z")), // during contest -> keep
        rev(2L, 20L, ts = ZonedDateTime.parse("2015-07-01T00:00:00Z"))  // after window closed -> refetch
      ))
      q.imagesWithTemplateByIds(prevContest, Set(2L)) returns
        Future.successful(Seq(img("File:LaterEdit-v2.jpg", 2L, revId = Some(20L))))

      val data = stats(dir, q, startYear = Some(2015), csvCacheResync = true)
        .gatherData(total = false).await

      data.imageDbByYear(2015).get.images.map(_.title).toSet must_==
        Set("File:Untouched.jpg", "File:LaterEdit-v2.jpg")
      there was one(q).imagesWithTemplateByIds(prevContest, Set(2L))
      // kept row got its revid backfilled from the sweep
      ImageCsvImporter.imagesFromCsv(yearCsv(dir, 2015).toString)
        .find(_.title == "File:Untouched.jpg").flatMap(_.revId) must beSome(10L)
    }

    "with --csv-cache-resync: keep cached rows when the category sweep looks truncated" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(prevContest, (1 to 10).map(i => img(s"File:P$i.jpg", i.toLong, revId = Some(i.toLong))), None),
        campaign, isCurrent = false, dir.toString)

      val q = newImageQuery()
      // only 2 of 10 ids came back, and categoryinfo says there should be 10
      q.imageIdsFromCategory(prevContest) returns Future.successful(Seq(rev(1L, 1L), rev(2L, 2L)))
      q.categoryFileCount(prevContest) returns Future.successful(Some(10L))

      val data = stats(dir, q, startYear = Some(2015), csvCacheResync = true)
        .gatherData(total = false).await

      // nothing dropped, nothing refetched
      data.imageDbByYear(2015).get.images.map(_.title).toSet must_== (1 to 10).map(i => s"File:P$i.jpg").toSet
      there was no(q).imagesWithTemplateByIds(any[Contest], any[Set[Long]])
    }
  }

  "current contest year" should {

    "incrementally sync: keep cached images, pull only newly added ids" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(contest, Seq(img("File:Old.jpg", 1L, revId = Some(1L))), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(rev(1L, 1L), rev(2L, 2L)))
      q.imagesWithTemplateByIds(contest, Set(2L)) returns Future.successful(Seq(img("File:New.jpg", 2L)))

      val data = stats(dir, q).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSet must_== Set("File:Old.jpg", "File:New.jpg")
      rowCount(yearCsv(dir, 2016)) must_== 2
      there was no(q).imagesFromCategory(contest)
    }

    "refetch a cached image whose revision changed" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(contest, Seq(img("File:Stale.jpg", 1L, revId = Some(1L))), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(rev(1L, 99L)))
      q.imagesWithTemplateByIds(contest, Set(1L)) returns
        Future.successful(Seq(img("File:Fixed.jpg", 1L, revId = Some(99L))))

      val data = stats(dir, q).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSeq must_== Seq("File:Fixed.jpg")
      there was one(q).imagesWithTemplateByIds(contest, Set(1L))
    }

    "keep a cached image whose current revision is revision-deleted (no revid in the sweep)" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(contest, Seq(img("File:RevDel.jpg", 1L, revId = Some(5L))), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(PageRevInfo(1L, None, None)))

      val data = stats(dir, q).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSeq must_== Seq("File:RevDel.jpg")
      there was no(q).imagesWithTemplateByIds(any[Contest], any[Set[Long]])
    }

    "keep cached rows when the sweep is empty but the category is not" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(contest, Seq(
          img("File:A.jpg", 1L, revId = Some(1L)),
          img("File:B.jpg", 2L, revId = Some(2L))
        ), None),
        campaign, isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq.empty[PageRevInfo])

      val data = stats(dir, q).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSet must_== Set("File:A.jpg", "File:B.jpg")
    }

    "drop cached images no longer in the category" in {
      val dir = cacheDir()
      ImageCsvExporter.export(
        new ImageDB(contest, Seq(
          img("File:Keep.jpg", 1L, revId = Some(1L)),
          img("File:Gone.jpg", 2L, revId = Some(2L))
        ), None),
        campaign, isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(rev(1L, 1L)))

      val data = stats(dir, q).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSeq must_== Seq("File:Keep.jpg")
      there was no(q).imagesWithTemplateByIds(any[Contest], any[Set[Long]])
    }

    "be fully fetched and cached when no CSV exists yet" in {
      val dir = cacheDir()
      val q = newImageQuery()
      q.imagesFromCategory(contest) returns Future.successful(Seq(img("File:A.jpg", 1L)))

      val data = stats(dir, q).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSeq must_== Seq("File:A.jpg")
      rowCount(yearCsv(dir, 2016)) must_== 1
    }
  }

  "all-images CSV" should {

    def totalCsv(dir: Path): String = ImageCsvExporter.totalFilename(campaign, dir.toString)

    "be read as the per-year images plus its other rows; an old CSV repeating per-year rows is rewritten without them" in {
      val dir = cacheDir()
      val perYear = img("File:A.jpg", 1L, revId = Some(10L))
      ImageCsvExporter.export(new ImageDB(contest, Seq(perYear), None), campaign, isCurrent = false, dir.toString)
      // old format: the all-images CSV also holds a (staler) copy of the per-year image
      val staleCopy = perYear.copy(monumentIds = Seq("999"), revId = Some(9L))
      ImageCsvExporter.exportTotal(new ImageDB(contest, Seq(staleCopy, img("File:X.jpg", 5L, revId = Some(50L))), None),
        campaign, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(rev(1L, 10L)))

      val data = stats(dir, q).gatherData(total = true).await

      data.totalImageDb.images.map(_.title).toSeq must_== Seq("File:A.jpg", "File:X.jpg")
      // the per-year copy wins, and it is the same object, not a second copy
      data.totalImageDb.images.head.monumentIds must_== Seq("123")
      data.totalImageDb.images.head must beTheSameAs(data.currentYearImageDb.images.head)
      ImageCsvImporter.imagesFromCsv(totalCsv(dir)).map(_.title) must_== Seq("File:X.jpg")
      there was no(q).imagesWithTemplateByIds(any[Contest], any[Set[Long]])
    }

    "on a full fetch, cache only the images not in the per-year CSVs" in {
      val dir = cacheDir()
      ImageCsvExporter.export(new ImageDB(contest, Seq(img("File:A.jpg", 1L, revId = Some(10L))), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(rev(1L, 10L)))
      q.imageIdsWithTemplate(contest) returns Future.successful(Seq(rev(1L, 10L), rev(5L, 50L)))
      q.imagesWithTemplateByIds(contest, Set(5L)) returns
        Future.successful(Seq(img("File:X.jpg", 5L, revId = Some(50L))))

      val data = stats(dir, q).gatherData(total = true).await

      data.totalImageDb.images.map(_.title).toSeq must_== Seq("File:A.jpg", "File:X.jpg")
      ImageCsvImporter.imagesFromCsv(totalCsv(dir)).map(_.title) must_== Seq("File:X.jpg")
    }

    "with --csv-cache-resync: diff only the rows not in the per-year CSVs" in {
      val dir = cacheDir()
      val perYear = img("File:A.jpg", 1L, revId = Some(10L))
      ImageCsvExporter.export(new ImageDB(contest, Seq(perYear), None), campaign, isCurrent = false, dir.toString)
      // old format, with a copy of file 1 at an older revision than the live one
      ImageCsvExporter.exportTotal(
        new ImageDB(contest, Seq(perYear.copy(revId = Some(9L)), img("File:X.jpg", 5L, revId = Some(50L))), None),
        campaign, dir.toString)

      val q = newImageQuery()
      q.imageIdsFromCategory(contest) returns Future.successful(Seq(rev(1L, 10L)))
      q.imageIdsWithTemplate(contest) returns Future.successful(Seq(rev(1L, 10L), rev(5L, 50L)))

      val data = stats(dir, q, csvCacheResync = true).gatherData(total = true).await

      data.totalImageDb.images.map(_.title).toSeq must_== Seq("File:A.jpg", "File:X.jpg")
      // file 1 is synced by the per-year CSV, not refetched for the all-images one
      there was no(q).imagesWithTemplateByIds(any[Contest], any[Set[Long]])
      ImageCsvImporter.imagesFromCsv(totalCsv(dir)).map(_.title) must_== Seq("File:X.jpg")
    }

    "with --csv-cache-resync: refetch changed rows and keep uk.wiki images" in {
      val dir = cacheDir()
      val total = Seq(
        img("File:C-keep.jpg", 1L, revId = Some(11L)),
        img("File:C-edit.jpg", 2L, revId = Some(22L)),
        img("File:Wiki.jpg", 900L, revId = Some(90L))
      )
      ImageCsvExporter.exportTotal(new ImageDB(contest, total, None), campaign, dir.toString)
      // also a current-year CSV so the per-year sync doesn't fetch
      ImageCsvExporter.export(new ImageDB(contest, Nil, None), campaign, isCurrent = false, dir.toString)

      val commons = newImageQuery()
      commons.imageIdsFromCategory(contest) returns Future.successful(Seq.empty[PageRevInfo])
      commons.imageIdsWithTemplate(contest) returns Future.successful(Seq(rev(1L, 11L), rev(2L, 999L)))
      commons.imagesWithTemplateByIds(contest, Set(2L)) returns
        Future.successful(Seq(img("File:C-edit-v2.jpg", 2L, revId = Some(999L))))

      val wiki = mock[ImageQuery]
      wiki.imagesWithTemplate(contest) returns Future.successful(Seq(img("File:Wiki.jpg", 900L)))

      val monumentQuery = mock[MonumentQuery]
      monumentQuery.byMonumentTemplate(date = None) returns Future.successful(monuments)
      val cfg = StatConfig(campaign = campaign, csvCacheDir = dir.toString, csvCacheResync = true)
      val st = new Statistics(contest, None, monumentQuery, Some(commons), Some(wiki), mock[MwBot], cfg)

      val data = st.gatherData(total = true).await

      data.totalImageDb.images.map(_.title).toSet must_==
        Set("File:C-keep.jpg", "File:C-edit-v2.jpg", "File:Wiki.jpg")
    }

    "with --csv-cache-resync: keep cached uk.wiki rows when the uk.wiki refetch comes back empty" in {
      val dir = cacheDir()
      val total = Seq(
        img("File:C-keep.jpg", 1L, revId = Some(11L)),
        img("File:Wiki1.jpg", 900L, revId = Some(90L)),
        img("File:Wiki2.jpg", 901L, revId = Some(91L))
      )
      ImageCsvExporter.exportTotal(new ImageDB(contest, total, None), campaign, dir.toString)

      val commons = newImageQuery()
      commons.imageIdsFromCategory(contest) returns Future.successful(Seq.empty[PageRevInfo])
      commons.imageIdsWithTemplate(contest) returns Future.successful(Seq(rev(1L, 11L)))

      val wiki = mock[ImageQuery]
      // transient failure: the fetch resolves but returns nothing
      wiki.imagesWithTemplate(contest) returns Future.successful(Nil)

      val monumentQuery = mock[MonumentQuery]
      monumentQuery.byMonumentTemplate(date = None) returns Future.successful(monuments)
      val cfg = StatConfig(campaign = campaign, csvCacheDir = dir.toString, csvCacheResync = true)
      val st = new Statistics(contest, None, monumentQuery, Some(commons), Some(wiki), mock[MwBot], cfg)

      val data = st.gatherData(total = true).await

      data.totalImageDb.images.map(_.title).toSet must_==
        Set("File:C-keep.jpg", "File:Wiki1.jpg", "File:Wiki2.jpg")
    }
  }

  "csvCache = false" should {
    "always fetch and never touch the CSV cache" in {
      val dir = cacheDir()
      ImageCsvExporter.export(new ImageDB(contest, Seq(img("File:Cached.jpg", 1L)), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imagesFromCategory(contest) returns Future.successful(Seq(img("File:Fetched.jpg", 7L)))

      val data = stats(dir, q, csvCache = false).gatherData(total = false).await

      data.currentYearImageDb.images.map(_.title).toSeq must_== Seq("File:Fetched.jpg")
      there was no(q).imageIdsFromCategory(contest)
      // cache file left untouched
      ImageCsvImporter.imagesFromCsv(yearCsv(dir, 2016).toString).map(_.title) must_== Seq("File:Cached.jpg")
    }
  }

  "--images-from-csv with --csv-cache-refresh" should {
    "still read the strict CSVs (refresh applies only to the automatic cache)" in {
      val dir = cacheDir()
      ImageCsvExporter.export(new ImageDB(prevContest, Seq(img("File:Strict.jpg", 1L)), None), campaign,
        isCurrent = false, dir.toString)

      val q = newImageQuery()
      q.imagesFromCategory(prevContest) returns Future.failed(new RuntimeException("should not fetch"))
      q.imagesFromCategory(contest) returns Future.successful(Nil)

      val data = stats(dir, q, startYear = Some(2015), csvCacheRefresh = true,
        imagesFromCsv = Some(dir.toString)).gatherData(total = false).await

      data.imageDbByYear(2015).map(_.images.map(_.title).toSeq) must beSome(Seq("File:Strict.jpg"))
      there was no(q).imagesFromCategory(prevContest)
    }
  }
}
