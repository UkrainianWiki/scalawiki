package org.scalawiki.wlx

import com.github.tototoshi.csv.CSVReader
import org.scalawiki.dto.{Image, ImageMetadata}
import org.scalawiki.wlx.dto.Contest
import org.specs2.mutable.Specification

import java.io.StringReader
import java.nio.file.{Files, Path}
import java.time.ZonedDateTime

class ImageCsvExporterSpec extends Specification {

  // A contest year guaranteed to be in the past so isCurrent = false
  val prevContest: Contest = Contest.WLMUkraine(2022)

  val sampleDate: ZonedDateTime = ZonedDateTime.parse("2022-09-15T10:30:00Z")

  val fullImage: Image = Image(
    title            = "File:Test.jpg",
    author           = Some("AuthorName"),
    date             = Some(sampleDate),
    monumentIds      = Seq("14-101-0001", "14-101-0002"),
    pageId           = Some(123456L),
    url              = Some("https://upload.wikimedia.org/test.jpg"),
    pageUrl          = Some("https://commons.wikimedia.org/wiki/File:Test.jpg"),
    width            = Some(3000),
    height           = Some(2000),
    size             = Some(1048576L),
    mime             = Some("image/jpeg"),
    mediaType        = Some("BITMAP"),
    metadata         = Some(ImageMetadata(Map(
      "Model"           -> "Canon EOS 5D",
      "DateTimeOriginal"-> "2022:09:15 10:30:00"
    ))),
    categories       = Set("WLM 2022", "Kyiv"),
    specialNominations = Set("WLM2022-UA-interior")
  )

  /** Write imageDb to a temp dir and return CSV content. */
  def exportAndRead(
      images: Seq[Image],
      contest: Contest,
      isCurrent: Boolean
  ): String = {
    val dir = Files.createTempDirectory("image-csv-spec")
    val imageDb = new ImageDB(contest, images, None)
    ImageCsvExporter.export(imageDb, contest.campaign, isCurrent, dir.toString)
    val expectedName = ImageCsvExporter.filename(contest.campaign, contest.year, isCurrent, dir.toString)
    new String(Files.readAllBytes(java.nio.file.Paths.get(expectedName)), "UTF-8")
  }

  def parseCsv(content: String): List[List[String]] = {
    val reader = CSVReader.open(new StringReader(content))
    try reader.all() finally reader.close()
  }

  "ImageCsvExporter.filename" should {

    "produce <campaign>-<year>-images.csv for a previous year (outputDir non-empty)" in {
      val name = ImageCsvExporter.filename("WLM-UA", 2022, isCurrent = false, outputDir = "output")
      name must_== s"output${java.io.File.separator}WLM-UA-2022-images.csv"
    }

    "produce bare <campaign>-<year>-images.csv when outputDir is empty" in {
      val name = ImageCsvExporter.filename("WLM-UA", 2022, isCurrent = false, outputDir = "")
      name must_== "WLM-UA-2022-images.csv"
    }

    "produce <campaign>-<year>-MM-dd-HHmm.csv for the current year" in {
      val name = ImageCsvExporter.filename("WLM-UA", 2025, isCurrent = true, outputDir = "")
      name must beMatching("WLM-UA-2025-\\d{2}-\\d{2}-\\d{4}\\.csv")
    }

    "include outputDir prefix for current year when outputDir is non-empty" in {
      val name = ImageCsvExporter.filename("WLM-UA", 2025, isCurrent = true, outputDir = "out")
      name must startWith(s"out${java.io.File.separator}WLM-UA-2025-")
    }
  }

  "ImageCsvExporter.export" should {

    "write all 18 expected header columns in the correct order" in {
      val content = exportAndRead(Seq(fullImage), prevContest, isCurrent = false)
      val header = parseCsv(content).head
      header must_== List(
        "title", "author", "upload_date", "monument_id", "page_id",
        "width", "height", "size_bytes", "mime", "camera", "exif_date",
        "categories", "special_nominations", "url", "page_url",
        "last_revid", "last_revision_ts", "media_type"
      )
    }

    "serialize a full image row correctly" in {
      val content = exportAndRead(Seq(fullImage), prevContest, isCurrent = false)
      val rows = parseCsv(content)
      val header = rows.head
      val row    = rows(1)
      def col(name: String) = row(header.indexOf(name))

      col("title")         must_== "File:Test.jpg"
      col("author")        must_== "AuthorName"
      col("upload_date")   must_== sampleDate.toString
      col("monument_id")   must_== "14-101-0001;14-101-0002"
      col("page_id")       must_== "123456"
      col("width")         must_== "3000"
      col("height")        must_== "2000"
      col("size_bytes")    must_== "1048576"
      col("mime")          must_== "image/jpeg"
      col("media_type")    must_== "BITMAP"
      col("camera")        must_== "Canon EOS 5D"
      col("url")           must_== "https://upload.wikimedia.org/test.jpg"
      col("page_url")      must_== "https://commons.wikimedia.org/wiki/File:Test.jpg"
    }

    "serialize exif_date as ISO-8601" in {
      val content = exportAndRead(Seq(fullImage), prevContest, isCurrent = false)
      val rows = parseCsv(content)
      val header = rows.head
      val row    = rows(1)
      row(header.indexOf("exif_date")) must_== "2022-09-15T10:30Z"
    }

    "serialize multi-value fields with semicolon separator" in {
      val content = exportAndRead(Seq(fullImage), prevContest, isCurrent = false)
      val rows   = parseCsv(content)
      val header = rows.head
      val row    = rows(1)
      def col(name: String) = row(header.indexOf(name))

      col("monument_id").split(";").toSet must_== Set("14-101-0001", "14-101-0002")
      col("categories").split(";").toSet  must_== Set("WLM 2022", "Kyiv")
      col("special_nominations")          must_== "WLM2022-UA-interior"
    }

    "serialize missing Optional fields as empty strings" in {
      val minimal = Image("File:Minimal.jpg")
      val content = exportAndRead(Seq(minimal), prevContest, isCurrent = false)
      val rows   = parseCsv(content)
      val header = rows.head
      val row    = rows(1)
      def col(name: String) = row(header.indexOf(name))

      col("author")      must_== ""
      col("upload_date") must_== ""
      col("monument_id") must_== ""
      col("page_id")     must_== ""
      col("camera")      must_== ""
      col("exif_date")   must_== ""
      col("url")         must_== ""
      col("page_url")    must_== ""
    }

    "create the output directory when it does not exist yet" in {
      val base = Files.createTempDirectory("image-csv-mkdir")
      val dir  = base.resolve("nested").resolve("csv-cache")
      val imageDb = new ImageDB(prevContest, Seq(fullImage), None)
      ImageCsvExporter.export(imageDb, prevContest.campaign, isCurrent = false, dir.toString)
      Files.exists(java.nio.file.Paths.get(
        ImageCsvExporter.filename(prevContest.campaign, prevContest.year, isCurrent = false, dir.toString)
      )) must beTrue
    }

    "not race when several years export into the same fresh directory concurrently" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      val dir = Files.createTempDirectory("image-csv-race").resolve("csv-cache")
      val years = 2012 to 2024
      val futures = years.map { y =>
        scala.concurrent.Future {
          val c = Contest.WLMUkraine(y)
          ImageCsvExporter.export(
            new ImageDB(c, Seq(fullImage.copy(pageId = Some(y.toLong))), None),
            c.campaign, isCurrent = false, dir.toString
          )
        }
      }
      scala.concurrent.Await.result(
        scala.concurrent.Future.sequence(futures), scala.concurrent.duration.Duration("10s")
      )
      years.foreach { y =>
        Files.exists(java.nio.file.Paths.get(
          ImageCsvExporter.filename(Contest.WLMUkraine(y).campaign, y, isCurrent = false, dir.toString)
        )) must beTrue
      }
      ok
    }

    "write no file when images is empty" in {
      val dir = Files.createTempDirectory("image-csv-empty")
      val imageDb = new ImageDB(prevContest, Seq.empty, None)
      ImageCsvExporter.export(imageDb, "WLM-UA", isCurrent = false, outputDir = dir.toString)
      val files = Option(dir.toFile.listFiles()).getOrElse(Array.empty)
      files must beEmpty
    }
  }
}
