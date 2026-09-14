package org.scalawiki.wlx

import com.github.tototoshi.csv.CSVWriter
import org.scalawiki.dto.Image

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object ImageCsvExporter {

  val columns: Seq[String] = Seq(
    "title", "author", "upload_date", "monument_id", "page_id",
    "width", "height", "size_bytes", "mime", "camera", "exif_date",
    "categories", "special_nominations", "url", "page_url",
    "last_revid", "last_revision_ts"
  )

  def imageToRow(image: Image): Map[String, String] = Map(
    "title"               -> image.title,
    "author"              -> image.author.getOrElse(""),
    "upload_date"         -> image.date.map(_.toString).getOrElse(""),
    "monument_id"         -> image.monumentIds.mkString(";"),
    "page_id"             -> image.pageId.map(_.toString).getOrElse(""),
    "width"               -> image.width.map(_.toString).getOrElse(""),
    "height"              -> image.height.map(_.toString).getOrElse(""),
    "size_bytes"          -> image.size.map(_.toString).getOrElse(""),
    "mime"                -> image.mime.getOrElse(""),
    "camera"              -> image.metadata.flatMap(_.camera).getOrElse(""),
    "exif_date"           -> image.metadata.flatMap(_.date).map(_.toString).getOrElse(""),
    "categories"          -> image.categories.mkString(";"),
    "special_nominations" -> image.specialNominations.mkString(";"),
    "url"                 -> image.url.getOrElse(""),
    "page_url"            -> image.pageUrl.getOrElse(""),
    "last_revid"          -> image.revId.map(_.toString).getOrElse(""),
    "last_revision_ts"    -> image.revTs.map(_.toString).getOrElse("")
  )

  def filename(
      campaign: String,
      contestYear: Int,
      isCurrent: Boolean,
      outputDir: String
  ): String = {
    val name = if (isCurrent) {
      val fmt = DateTimeFormatter.ofPattern("MM-dd-HHmm")
      s"$campaign-$contestYear-${LocalDateTime.now().format(fmt)}.csv"
    } else {
      s"$campaign-$contestYear-images.csv"
    }
    if (outputDir.nonEmpty) s"$outputDir${java.io.File.separator}$name" else name
  }

  def totalFilename(campaign: String, outputDir: String): String = {
    val name = s"$campaign-all-images.csv"
    if (outputDir.nonEmpty) s"$outputDir${java.io.File.separator}$name" else name
  }

  def export(
      imageDb: ImageDB,
      campaign: String,
      isCurrent: Boolean,
      outputDir: String
  ): Unit =
    exportTo(imageDb, filename(campaign, imageDb.contest.year, isCurrent, outputDir))

  /** A row hosted on a project wiki (uk.wikipedia) rather than Commons: its
    * stored page URL points elsewhere. Such rows live in a different page-id
    * space, so a Commons page id matching theirs says nothing about them. */
  def isProjectWikiHosted(image: Image): Boolean =
    image.pageUrl.exists(url => !url.contains("commons.wikimedia.org"))

  def perYearPageIds(dbsByYear: Seq[ImageDB]): Set[Long] =
    dbsByYear.iterator.flatMap(_.images).flatMap(_.pageId).toSet

  /** Whether `image` is the Commons copy of an image with one of `perYearIds`. */
  def inPerYear(perYearIds: Set[Long])(image: Image): Boolean =
    !isProjectWikiHosted(image) && image.pageId.exists(perYearIds.contains)

  /** Write the all-images CSV. Images in `perYear` are left out: they have
    * their own per-year CSVs, and the all-time DB is read back as those plus
    * this file's rows, so repeating them would store nearly every image twice.
    * Written even when that leaves no rows, so the file still counts as cached.
    */
  def exportTotal(
      imageDb: ImageDB,
      campaign: String,
      outputDir: String,
      perYear: Seq[ImageDB] = Nil
  ): Unit = {
    val perYearIds = perYearPageIds(perYear)
    exportTo(imageDb.images.filterNot(inPerYear(perYearIds)).toSeq, totalFilename(campaign, outputDir), writeEmpty = true)
  }

  private def exportTo(imageDb: ImageDB, path: String): Unit =
    exportTo(imageDb.images.toSeq, path, writeEmpty = false)

  private def exportTo(images: Seq[Image], path: String, writeEmpty: Boolean): Unit = {
    if (images.isEmpty && !writeEmpty) return

    val file = new File(path)
    Option(file.getParentFile).foreach { dir =>
      // idempotent: no exception when the directory already exists, which lets
      // several per-year exports create the cache dir concurrently without racing
      java.nio.file.Files.createDirectories(dir.toPath)
    }
    val writer = CSVWriter.open(file, "UTF-8")
    try {
      writer.writeRow(columns)
      images.foreach { image =>
        val row = imageToRow(image)
        writer.writeRow(columns.map(c => row.getOrElse(c, "")))
      }
    } finally {
      writer.close()
    }
  }
}
