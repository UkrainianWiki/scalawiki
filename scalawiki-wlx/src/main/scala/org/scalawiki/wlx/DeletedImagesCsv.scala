package org.scalawiki.wlx

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import org.scalawiki.dto.Image
import org.scalawiki.util.AtomicFile

import java.io.File
import java.nio.file.Files
import java.time.LocalDate
import scala.util.Try

/** The cached files that were deleted on Commons, kept apart from the image CSV cache (which drops
  * them): `<campaign>-<year>-deleted-images.csv` for a contest year's category,
  * `<campaign>-all-deleted-images.csv` for the other files of the contest template. The image CSV's
  * columns ([[ImageCsvExporter.columns]]), as the file was last cached, and `deleted_detected`: the
  * date a sync found it deleted.
  */
object DeletedImagesCsv {

  final case class DeletedImage(image: Image, detected: LocalDate)

  val DetectedColumn = "deleted_detected"

  val columns: Seq[String] = ImageCsvExporter.columns :+ DetectedColumn

  def filename(campaign: String, year: Int, dir: String): String =
    path(dir, s"$campaign-$year-deleted-images.csv")

  def totalFilename(campaign: String, dir: String): String =
    path(dir, s"$campaign-all-deleted-images.csv")

  private def path(dir: String, name: String): String =
    if (dir.nonEmpty) s"$dir${File.separator}$name" else name

  /** The file's rows; none when it doesn't exist. */
  def read(path: String): Seq[DeletedImage] =
    if (!new File(path).exists()) Nil
    else {
      val reader = CSVReader.open(new File(path), "UTF-8")
      try
        reader.allWithHeaders().map { row =>
          DeletedImage(
            ImageCsvImporter.rowToImage(row),
            Try(LocalDate.parse(row.getOrElse(DetectedColumn, ""))).getOrElse(LocalDate.MIN)
          )
        }
      finally reader.close()
    }

  def write(path: String, rows: Seq[DeletedImage]): Unit = {
    val file = new File(path)
    Option(file.getParentFile).foreach(dir => Files.createDirectories(dir.toPath))
    AtomicFile.write(file.toPath) { temporary =>
      val writer = CSVWriter.open(temporary.toFile, "UTF-8")
      try {
        writer.writeRow(columns)
        rows.foreach { row =>
          val values =
            ImageCsvExporter.imageToRow(row.image) + (DetectedColumn -> row.detected.toString)
          writer.writeRow(columns.map(c => values.getOrElse(c, "")))
        }
      } finally writer.close()
    }
  }

  /** The rows of `path` after a sync: `found` added (detected `today`), and the files the sweep
    * listed again (`listed`: restored, or back in the category) removed. A file already there keeps
    * its first detection date. Rewritten only when that changes it.
    */
  def update(path: String, found: Seq[Image], listed: Long => Boolean, today: LocalDate): Unit = {
    val existing = read(path)
    val kept = existing.filterNot(_.image.pageId.exists(listed))
    val known = kept.flatMap(_.image.pageId).toSet
    val added = found.filterNot(_.pageId.exists(known.contains)).map(DeletedImage(_, today))
    if (added.nonEmpty || kept.size != existing.size) write(path, kept ++ added)
  }
}
