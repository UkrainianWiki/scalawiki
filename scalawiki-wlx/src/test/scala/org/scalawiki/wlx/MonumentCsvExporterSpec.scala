package org.scalawiki.wlx

import com.github.tototoshi.csv.CSVReader
import org.scalawiki.wlx.query.MonumentQuery
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

import java.io.StringReader
import java.nio.file.Files
import scala.concurrent.Future

class MonumentCsvExporterSpec extends Specification with Mockito {

  // Real mapping from ua_uk.json
  lazy val realMapping: UaUkMapping =
    UaUkJsonMapping.load("monuments_config/ua_uk.json")

  def exportToString(
      rows: Iterable[Map[String, String]],
      mapping: UaUkMapping
  ): String = {
    val path = Files.createTempFile("monument-csv-test", ".csv")
    try {
      MonumentCsvExporter.export(rows, mapping, path.toString)
      new String(Files.readAllBytes(path))
    } finally {
      Files.deleteIfExists(path)
    }
  }

  def parseCsv(content: String): List[List[String]] = {
    val reader = CSVReader.open(new StringReader(content))
    try reader.all()
    finally reader.close()
  }

  "MonumentCsvExporter" should {

    "write header and one data row for a single input row" in {
      val rows = Seq(Map("назва" -> "Церква", "ID" -> "14-101-0001"))
      val content = exportToString(rows, realMapping)
      val parsed = parseCsv(content)
      parsed.size must_== 2 // header + 1 data row
      parsed.head must contain("name")
      parsed.head must contain("id")
      val idIdx = parsed.head.indexOf("id")
      parsed(1)(idIdx) must_== "14-101-0001"
    }

    "write union of keys from multiple rows with different fields" in {
      val rows = Seq(
        Map("ID" -> "01-001-0001", "назва" -> "A"),
        Map("ID" -> "02-001-0001", "район" -> "Центральний")
      )
      val content = exportToString(rows, realMapping)
      val parsed = parseCsv(content)
      val header = parsed.head
      header must contain("id")
      header must contain("name")
      header must contain("adm2")
      // rows should have blanks for missing fields
      parsed.size must_== 3
    }

    "escape commas, quotes, and newlines in values" in {
      val rows = Seq(Map("назва" -> "A, B \"C\"\nD"))
      val content = exportToString(rows, realMapping)
      val parsed = parseCsv(content)
      parsed.size must_== 2
      val nameIdx = parsed.head.indexOf("name")
      parsed(1)(nameIdx) must_== "A, B \"C\"\nD"
    }

    "produce no output file for empty input" in {
      val path = Files.createTempFile("monument-csv-empty", ".csv")
      Files.deleteIfExists(path) // ensure it doesn't exist
      MonumentCsvExporter.export(Seq.empty, realMapping, path.toString)
      Files.exists(path) must beFalse
    }

    "inject Text literals from sql_data into every row" in {
      val rows = Seq(Map("ID" -> "14-101-0001"))
      val content = exportToString(rows, realMapping)
      val parsed = parseCsv(content)
      val header = parsed.head
      header must contain("adm0")
      val adm0Idx = header.indexOf("adm0")
      parsed(1)(adm0Idx) must_== "ua"
    }

    "produce deterministic column order across multiple runs" in {
      val rows = Seq(Map("ID" -> "1", "назва" -> "A", "район" -> "R"))
      val content1 = exportToString(rows, realMapping)
      val content2 = exportToString(rows, realMapping)
      parseCsv(content1).head must_== parseCsv(content2).head
    }
  }

  // exportFromWiki blocks on byMonumentTemplateMaps via Await.result at its one
  // sync/async boundary, exercising the real path.
  private def buildMonumentQuery(rows: Seq[Map[String, String]]): MonumentQuery = {
    val q = mock[MonumentQuery]
    q.byMonumentTemplateMaps() returns Future.successful(rows)
    q
  }

  "MonumentCsvExporter.exportFromWiki" should {

    "write CSV to the specified filename" in {
      val path = Files.createTempFile("monument-export-test", ".csv")
      Files.deleteIfExists(path)
      try {
        val q = buildMonumentQuery(Seq(Map("ID" -> "14-101-0001", "назва" -> "Test")))
        MonumentCsvExporter.exportFromWiki(q, "wlm-ua", Some(path.toString))
        Files.exists(path) must beTrue
        val content = new String(Files.readAllBytes(path), "UTF-8")
        content must contain("id")
        content must contain("14-101-0001")
      } finally Files.deleteIfExists(path)
    }

    "produce no output file for an empty monument list" in {
      val path = Files.createTempFile("monument-export-empty", ".csv")
      Files.deleteIfExists(path)
      try {
        MonumentCsvExporter.exportFromWiki(
          buildMonumentQuery(Seq.empty),
          "wlm-ua",
          Some(path.toString)
        )
        Files.exists(path) must beFalse
      } finally Files.deleteIfExists(path)
    }
  }

  "MonumentCsvExporter.defaultFilename" should {
    "match the <campaign>-yyyy-MM-dd-HHmm.csv pattern" in {
      MonumentCsvExporter.defaultFilename("WLM-UA") must beMatching(
        "WLM-UA-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.csv"
      )
    }
  }
}
