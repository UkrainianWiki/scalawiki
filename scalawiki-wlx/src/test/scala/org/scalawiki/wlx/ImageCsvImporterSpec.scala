package org.scalawiki.wlx

import org.scalawiki.dto.{Image, ImageMetadata}
import org.scalawiki.wlx.dto.Contest
import org.specs2.mutable.Specification

import java.nio.file.Files
import java.time.ZonedDateTime

class ImageCsvImporterSpec extends Specification {

  val prevContest: Contest = Contest.WLMUkraine(2022)

  val sampleDate: ZonedDateTime = ZonedDateTime.parse("2022-09-15T10:30:00Z")

  val fullImage: Image = Image(
    title = "File:Test.jpg",
    author = Some("AuthorName"),
    date = Some(sampleDate),
    monumentIds = Seq("14-101-0001", "14-101-0002"),
    pageId = Some(123456L),
    url = Some("https://upload.wikimedia.org/test.jpg"),
    pageUrl = Some("https://commons.wikimedia.org/wiki/File:Test.jpg"),
    width = Some(3000),
    height = Some(2000),
    size = Some(1048576L),
    mime = Some("image/jpeg"),
    metadata = Some(
      ImageMetadata(
        Map("Model" -> "Canon EOS 5D", "DateTimeOriginal" -> "2022:09:15 10:30:00")
      )
    ),
    categories = Set("WLM 2022", "Kyiv"),
    specialNominations = Set("WLM2022-UA-interior")
  )

  val minimalImage: Image = Image("File:Minimal.jpg")

  def roundTrip(images: Seq[Image]): Seq[Image] = {
    val dir = Files.createTempDirectory("image-csv-import-spec")
    val imageDb = new ImageDB(prevContest, images, None)
    ImageCsvExporter.export(imageDb, prevContest.campaign, isCurrent = false, dir.toString)
    val path = ImageCsvExporter.filename(prevContest.campaign, prevContest.year, isCurrent = false, dir.toString)
    ImageCsvImporter.imagesFromCsv(path)
  }

  "ImageCsvImporter.imagesFromCsv" should {

    "round-trip a fully populated image" in {
      val imported = roundTrip(Seq(fullImage))
      imported must_== Seq(fullImage)
    }

    "round-trip an image with only optional fields absent" in {
      val imported = roundTrip(Seq(minimalImage))
      imported must_== Seq(minimalImage)
    }

    "round-trip the last revision id and timestamp" in {
      val withRev = fullImage.copy(
        revId = Some(987654321L),
        revTs = Some(ZonedDateTime.parse("2024-01-02T03:04:05Z"))
      )
      val imported = roundTrip(Seq(withRev))
      imported must_== Seq(withRev)
      imported.head.revId must beSome(987654321L)
      imported.head.revTs must beSome(ZonedDateTime.parse("2024-01-02T03:04:05Z"))
    }

    "leave revId / revTs empty for a CSV without those columns" in {
      // a header row from before the columns existed
      val dir = Files.createTempDirectory("image-csv-legacy")
      val path = dir.resolve("legacy.csv").toString
      val w = new java.io.PrintWriter(path)
      try {
        w.println("title,page_id,monument_id")
        w.println("File:Legacy.jpg,42,14-101-0001")
      } finally w.close()
      val imported = ImageCsvImporter.imagesFromCsv(path)
      imported.map(_.title) must_== Seq("File:Legacy.jpg")
      imported.head.revId must beNone
      imported.head.revTs must beNone
    }

    "round-trip metadata with only camera present" in {
      val cameraOnly = fullImage.copy(metadata = Some(ImageMetadata(Map("Model" -> "Nikon D850"))))
      val imported = roundTrip(Seq(cameraOnly))
      imported must_== Seq(cameraOnly)
    }

    "preserve monument id and category order/membership" in {
      val imported = roundTrip(Seq(fullImage))
      imported.head.monumentIds must_== Seq("14-101-0001", "14-101-0002")
      imported.head.categories must_== Set("WLM 2022", "Kyiv")
      imported.head.specialNominations must_== Set("WLM2022-UA-interior")
    }

    "drop rows failing the keep filter" in {
      val dir = Files.createTempDirectory("image-csv-keep-spec")
      ImageCsvExporter.export(new ImageDB(prevContest, Seq(fullImage, minimalImage), None), prevContest.campaign, isCurrent = false, dir.toString)
      val path = ImageCsvExporter.filename(prevContest.campaign, prevContest.year, isCurrent = false, dir.toString)
      ImageCsvImporter.imagesFromCsv(path, _.pageId.isDefined) must_== Seq(fullImage)
    }

    "share equal values between images, also across files read with the same pool" in {
      val dir = Files.createTempDirectory("image-csv-pool-spec")
      val nextYear = prevContest.copy(year = prevContest.year + 1)
      val other = fullImage.copy(title = "File:Other.jpg", pageId = Some(7L))
      val nextYearImage = fullImage.copy(title = "File:Next year.jpg", pageId = Some(8L))
      ImageCsvExporter.export(new ImageDB(prevContest, Seq(fullImage, other), None), prevContest.campaign, isCurrent = false, dir.toString)
      ImageCsvExporter.export(new ImageDB(nextYear, Seq(nextYearImage), None), prevContest.campaign, isCurrent = false, dir.toString)

      val pool = new ImageCsvImporter.ValuePool
      val first = ImageCsvImporter.imagesFromCsv(
        ImageCsvExporter.filename(prevContest.campaign, prevContest.year, isCurrent = false, dir.toString), pool = pool)
      val second = ImageCsvImporter.imagesFromCsv(
        ImageCsvExporter.filename(prevContest.campaign, nextYear.year, isCurrent = false, dir.toString), pool = pool)

      first ++ second must_== Seq(fullImage, other, nextYearImage)
      val (a, b, c) = (first(0), first(1), second(0))
      (b.author eq a.author) must beTrue
      (c.author eq a.author) must beTrue
      (b.categories eq a.categories) must beTrue
      (b.specialNominations eq a.specialNominations) must beTrue
      (b.monumentIds eq a.monumentIds) must beTrue
      (b.width eq a.width) must beTrue
      (b.metadata.get.data("Model") eq a.metadata.get.data("Model")) must beTrue
    }

    "return an empty sequence when the file does not exist" in {
      ImageCsvImporter.imagesFromCsv("does-not-exist-anywhere.csv") must beEmpty
    }

    "round-trip non-ASCII (Cyrillic) title, author, categories and monument ids" in {
      val cyrillic = fullImage.copy(
        title = "File:Пам'ятний знак.jpg",
        author = Some("Користувач:Іван Франко"),
        categories = Set("Вікі любить пам'ятки 2022", "Київ"),
        specialNominations = Set("WLM2022-UA-інтер'єр"),
        monumentIds = Seq("14-101-0001")
      )
      val imported = roundTrip(Seq(cyrillic))
      imported must_== Seq(cyrillic)
    }
  }

  "slim images" should {

    val withEverything = fullImage.copy(
      categories = Set("WLM 2022", "Kyiv", "Ineligible submissions for WLM 2022 in Ukraine", "Church interiors in Kyiv"),
      revId = Some(987654321L),
      revTs = Some(ZonedDateTime.parse("2024-01-02T03:04:05Z"))
    )

    // what past-year reports, ratings and eligibility read
    val expectedSlim = Image(
      title = withEverything.title,
      author = withEverything.author,
      monumentIds = withEverything.monumentIds,
      pageId = withEverything.pageId,
      width = withEverything.width,
      height = withEverything.height,
      size = withEverything.size,
      metadata = Some(ImageMetadata(Map("DateTimeOriginal" -> "2022:09:15 10:30:00"))),
      categories = Set("Ineligible submissions for WLM 2022 in Ukraine", "Church interiors in Kyiv"),
      specialNominations = withEverything.specialNominations
    )

    def csvOf(images: Seq[Image]): String = {
      val dir = Files.createTempDirectory("image-csv-slim-spec")
      ImageCsvExporter.export(new ImageDB(prevContest, images, None), prevContest.campaign, isCurrent = false, dir.toString)
      ImageCsvExporter.filename(prevContest.campaign, prevContest.year, isCurrent = false, dir.toString)
    }

    "keep only the fields past years are read for" in {
      ImageCsvImporter.slim(withEverything, new ImageCsvImporter.ValuePool) must_== expectedSlim
      ImageCsvImporter.imagesFromCsv(csvOf(Seq(withEverything)), slim = true) must_== Seq(expectedSlim)
    }

    "keep the upload date when there is no EXIF date" in {
      val noExif = withEverything.copy(metadata = Some(ImageMetadata(Map("Model" -> "Canon EOS 5D"))))
      val expected = expectedSlim.copy(metadata = None, date = Some(sampleDate))
      ImageCsvImporter.slim(noExif, new ImageCsvImporter.ValuePool) must_== expected
      ImageCsvImporter.imagesFromCsv(csvOf(Seq(noExif)), slim = true) must_== Seq(expected)
    }

    "read a CSV slim exactly as slimming its full images" in {
      val images = Seq(withEverything, minimalImage, fullImage.copy(title = "File:No metadata.jpg", metadata = None))
      val path = csvOf(images)
      val pool = new ImageCsvImporter.ValuePool
      ImageCsvImporter.imagesFromCsv(path, slim = true) must_==
        ImageCsvImporter.imagesFromCsv(path).map(ImageCsvImporter.slim(_, pool))
    }
  }

  "ImageCsvExporter.exportTotal / ImageCsvImporter" should {

    "round-trip images written via the campaign-scoped total filename" in {
      val dir = Files.createTempDirectory("image-csv-total-spec")
      val imageDb = new ImageDB(prevContest, Seq(fullImage), None)
      ImageCsvExporter.exportTotal(imageDb, prevContest.campaign, dir.toString)
      val path = ImageCsvExporter.totalFilename(prevContest.campaign, dir.toString)
      ImageCsvImporter.imagesFromCsv(path) must_== Seq(fullImage)
    }

    "leave out Commons copies of per-year images, keeping other rows and uk.wiki rows with the same page id" in {
      val dir = Files.createTempDirectory("image-csv-total-extras-spec")
      val extra = minimalImage.copy(pageId = Some(7L))
      val wikiSameId = minimalImage.copy(
        title = "File:Wiki.jpg",
        pageId = fullImage.pageId,
        pageUrl = Some("https://uk.wikipedia.org/wiki/File:Wiki.jpg")
      )
      val total = new ImageDB(prevContest, Seq(fullImage, extra, wikiSameId), None)
      val perYear = Seq(new ImageDB(prevContest, Seq(fullImage), None))
      ImageCsvExporter.exportTotal(total, prevContest.campaign, dir.toString, perYear)
      val path = ImageCsvExporter.totalFilename(prevContest.campaign, dir.toString)
      ImageCsvImporter.imagesFromCsv(path) must_== Seq(extra, wikiSameId)
    }

    "write a header-only file when every image is in a per-year CSV" in {
      val dir = Files.createTempDirectory("image-csv-total-empty-spec")
      val db = new ImageDB(prevContest, Seq(fullImage), None)
      ImageCsvExporter.exportTotal(db, prevContest.campaign, dir.toString, perYear = Seq(db))
      val path = ImageCsvExporter.totalFilename(prevContest.campaign, dir.toString)
      new java.io.File(path).exists must beTrue
      ImageCsvImporter.imagesFromCsv(path) must beEmpty
    }
  }
}
