package org.scalawiki.wlx

import com.github.tototoshi.csv.CSVReader
import org.scalawiki.dto.{Image, ImageMetadata}
import org.scalawiki.wlx.stat.rating.NumberOfInteriorImagesBonus

import java.io.File
import java.time.{ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.concurrent.ConcurrentHashMap

object ImageCsvImporter {

  private val exifPattern = DateTimeFormatter.ofPattern("yyyy:MM:dd HH:mm:ss")

  private val exifDateKey = "DateTimeOriginal"

  /** Hands out one shared instance per distinct value, for the image fields that
    * repeat across many images: authors, cameras, MIME types, category and
    * special nomination names and sets, monument id lists, widths and heights.
    * One pool can serve several CSV files, so e.g. an author seen in several
    * contest years is held once. Unique fields (titles, dates, page ids) aren't
    * pooled - the pool would only add an entry per image.
    *
    * Thread-safe. Each kind of value has its own pool: Scala equality treats
    * `Some(4000)` and `Some(4000L)` as equal, so a single pool could hand a
    * field an instance of the wrong type.
    */
  final class ValuePool {

    private final class Interner[T <: AnyRef] {
      private val values = new ConcurrentHashMap[T, T]()

      def apply(value: T): T = {
        val existing = values.putIfAbsent(value, value)
        if (existing == null) value else existing
      }
    }

    private val strings = new Interner[String]
    private val stringOpts = new Interner[Option[String]]
    private val intOpts = new Interner[Option[Int]]
    private val stringSeqs = new Interner[Seq[String]]
    private val stringSets = new Interner[Set[String]]

    def string(s: String): String = strings(s)

    def stringOpt(o: Option[String]): Option[String] =
      if (o.isEmpty) None else stringOpts(o.map(string))

    def intOpt(o: Option[Int]): Option[Int] =
      if (o.isEmpty) None else intOpts(o)

    def stringSeq(s: Seq[String]): Seq[String] =
      if (s.isEmpty) Seq.empty else stringSeqs(s.map(string))

    def stringSet(s: Set[String]): Set[String] =
      if (s.isEmpty) Set.empty else stringSets(s.map(string))
  }

  private def optStr(s: String): Option[String] = if (s.isEmpty) None else Some(s)

  private def optLong(s: String): Option[Long] = if (s.isEmpty) None else Some(s.toLong)

  private def optInt(s: String): Option[Int] = if (s.isEmpty) None else Some(s.toInt)

  private def splitSeq(s: String): Seq[String] = if (s.isEmpty) Seq.empty else s.split(";").toSeq

  private def splitSet(s: String): Set[String] = splitSeq(s).toSet

  private def toExifRaw(exifDate: String): String =
    ZonedDateTime
      .parse(exifDate)
      .withZoneSameInstant(ZoneOffset.UTC)
      .format(exifPattern)

  /** Whether a category can matter for a slim (past contest year) image: an
    * ineligible-submission category (`ImageDB.ineligible`, `Output`) or an
    * interior one (`NumberOfInteriorImagesBonus`). */
  private def keptInSlim(category: String): Boolean =
    category.toLowerCase.contains("ineligible") || NumberOfInteriorImagesBonus.isInteriorCategory(category)

  /** An image cut down to what a past contest year's image is read for.
    *
    * Past years' images are frozen (their CSVs aren't rewritten without
    * `--csv-cache-resync`), and what reads them - eligibility (`ImageDB`), list
    * filling (`ImageFiller.bestImage`), ratings and the all-years reports -
    * needs only the title, author, monument ids, page id, width, height, size,
    * special nominations, the EXIF date (only `DateTimeOriginal` is kept), the
    * upload date when there is no EXIF date (`OldPhotosBonus` falls back to it),
    * and the ineligible-submission and interior categories. URLs, MIME type,
    * uploader, camera, other categories and revision id/timestamp are dropped.
    *
    * A slim image must never be written back to a CSV: the dropped fields would
    * be lost from the cache.
    */
  def slim(image: Image, pool: ValuePool): Image = {
    val exifOnly = image.metadata.flatMap { m =>
      m.data.get(exifDateKey).map(d => ImageMetadata(Map(exifDateKey -> d)))
    }
    image.copy(
      url = None,
      pageUrl = None,
      mime = None,
      uploader = None,
      metadata = exifOnly,
      date = if (exifOnly.flatMap(_.date).isDefined) None else image.date,
      categories = pool.stringSet(image.categories.filter(keptInSlim)),
      revId = None,
      revTs = None
    )
  }

  def rowToImage(row: Map[String, String]): Image = rowToImage(row, new ValuePool, slim = false)

  /** `slim` gives [[slim]]'s image; the columns it drops aren't parsed. */
  def rowToImage(row: Map[String, String], pool: ValuePool, slim: Boolean): Image = {
    def col(name: String): String = row.getOrElse(name, "")

    // Slim images drop the camera and most categories: don't parse or pool them,
    // or the pool would keep every image's full category set alive.
    val camera = if (slim) None else pool.stringOpt(optStr(col("camera")))
    val exifDate = optStr(col("exif_date"))
    val metadata = (camera, exifDate) match {
      case (None, None) => None
      case _ =>
        val data = camera.map("Model" -> _).toMap ++
          exifDate.map(d => exifDateKey -> toExifRaw(d)).toMap
        Some(ImageMetadata(data))
    }
    val needsUploadDate = !slim || metadata.flatMap(_.date).isEmpty

    val image = Image(
      title = col("title"),
      url = optStr(col("url")),
      pageUrl = optStr(col("page_url")),
      size = optLong(col("size_bytes")),
      width = pool.intOpt(optInt(col("width"))),
      height = pool.intOpt(optInt(col("height"))),
      author = pool.stringOpt(optStr(col("author"))),
      date = if (needsUploadDate) optStr(col("upload_date")).map(ZonedDateTime.parse) else None,
      monumentIds = pool.stringSeq(splitSeq(col("monument_id"))),
      pageId = optLong(col("page_id")),
      metadata = metadata,
      categories =
        if (slim) splitSet(col("categories")).filter(keptInSlim) // pooled by slim() below
        else pool.stringSet(splitSet(col("categories"))),
      specialNominations = pool.stringSet(splitSet(col("special_nominations"))),
      mime = pool.stringOpt(optStr(col("mime"))),
      revId = if (slim) None else optLong(col("last_revid")),
      revTs = if (slim) None else optStr(col("last_revision_ts")).map(ZonedDateTime.parse)
    )
    if (slim) this.slim(image, pool) else image
  }

  /** Images from an image CSV, or empty if the file doesn't exist. Rows whose
    * image fails `keep` are dropped as they are read, never held. `pool` shares
    * repeated values (pass one pool to several reads to share across files);
    * `slim` loads [[slim]] images.
    */
  def imagesFromCsv(
      path: String,
      keep: Image => Boolean = _ => true,
      pool: ValuePool = new ValuePool,
      slim: Boolean = false
  ): Seq[Image] = {
    val file = new File(path)
    if (!file.exists()) return Seq.empty

    val reader = CSVReader.open(file, "UTF-8")
    try {
      // Row by row: `allWithHeaders()` would hold every row's string map in
      // memory at once, on top of the images built from them.
      reader.iteratorWithHeaders.map(row => rowToImage(row, pool, slim)).filter(keep).toVector
    } finally {
      reader.close()
    }
  }
}
