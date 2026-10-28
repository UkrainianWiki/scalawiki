package org.scalawiki.wlx

import com.github.tototoshi.csv.CSVWriter
import org.scalawiki.wlx.query.MonumentQuery

import java.io.File

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MonumentCsvExporter {

  /** Default output filename for a monument export: `<campaign>-yyyy-MM-dd-HHmm.csv`. */
  def defaultFilename(campaign: String): String = {
    val now = java.time.LocalDateTime.now()
    val fmt = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd-HHmm")
    s"$campaign-${now.format(fmt)}.csv"
  }

  /** Fetch every monument list for `campaign` from the wiki and write the rows to `outFile` (or
    * [[defaultFilename]] when it is empty / `None`), applying the `monuments_config/ua_uk.json`
    * mapping. Writes nothing for an empty list.
    */
  def exportFromWiki(
      monumentQuery: MonumentQuery,
      campaign: String,
      outFile: Option[String] = None
  ): Unit = {
    val path = outFile.filter(_.nonEmpty).getOrElse(defaultFilename(campaign))
    // CLI-only entry point: this is the one sync/async boundary, so block here
    // (no arbitrary timeout) rather than propagate a Future through `main`.
    val maps = Await.result(monumentQuery.byMonumentTemplateMaps(), Duration.Inf)
    val mapping = UaUkJsonMapping.load("monuments_config/ua_uk.json")
    export(maps, mapping, path)
  }

  /** Export monument rows to a CSV file.
    *
    *   - Applies UaUkJsonMapping.applyMapping to each row.
    *   - Computes header via UaUkJsonMapping.headerColumns (sql_data keys first, then
    *     alphabetical).
    *   - Writes nothing if rows is empty.
    *
    * @param rows
    *   raw monument parameter maps (as returned by parseToMaps)
    * @param mapping
    *   loaded UaUkMapping (from UaUkJsonMapping.load)
    * @param outputPath
    *   path to the output CSV file
    */
  def export(
      rows: Iterable[Map[String, String]],
      mapping: UaUkMapping,
      outputPath: String
  ): Unit = {
    // Materialize once so we can iterate twice (header + rows)
    val mappedRows: Seq[Map[String, String]] =
      rows.map(UaUkJsonMapping.applyMapping(_, mapping)).toSeq

    if (mappedRows.isEmpty) return

    val headers = UaUkJsonMapping.headerColumns(mappedRows, mapping)

    val writer = CSVWriter.open(new File(outputPath), "UTF-8")
    try {
      writer.writeRow(headers)
      mappedRows.foreach { row =>
        writer.writeRow(headers.map(h => row.getOrElse(h, "")))
      }
    } finally {
      writer.close()
    }
  }
}
