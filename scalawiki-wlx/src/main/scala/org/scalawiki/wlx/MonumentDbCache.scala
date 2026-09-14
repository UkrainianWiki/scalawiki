package org.scalawiki.wlx

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import org.scalawiki.wlx.dto.Monument
import org.scalawiki.wlx.dto.lists.ListConfig
import org.scalawiki.wlx.query.MonumentQuery.MonumentListPage
import play.api.libs.json.{JsObject, JsString, Json}

import java.io.File
import java.time.ZonedDateTime

/** On-disk cache of parsed monument lists, one CSV per campaign
  * (`csv-cache/<campaign>-monuments.csv`).
  *
  * Fetching and parsing every monument list page from the wiki is the long pole
  * of a stats run. Each row here is one [[Monument]] plus the source page it came
  * from and that page's revision id / timestamp when it was cached, so a later
  * run can diff a cheap id+revision sweep against the file and re-fetch only the
  * pages that changed. Mirrors [[ImageCsvExporter]] / [[ImageCsvImporter]] for
  * images.
  */
object MonumentDbCache {

  /** Provenance first, then the [[Monument]] fields. `other_params` holds the
    * open-ended `otherParams` map as a JSON object; `list_config` is not stored
    * (re-attached from the contest config on read). */
  val columns: Seq[String] = Seq(
    "source_page", "page_revid", "page_ts",
    "id", "name", "name_detail", "year", "description", "article",
    "city", "city_type", "place", "user", "area", "lat", "lon",
    "typ", "sub_type", "photo", "gallery", "resolution", "state_id",
    "contest", "source", "other_params"
  )

  def filename(campaign: String, outputDir: String): String = {
    val name = s"$campaign-monuments.csv"
    if (outputDir.nonEmpty) s"$outputDir${File.separator}$name" else name
  }

  private def opt(s: String): Option[String] = if (s.isEmpty) None else Some(s)

  private def monumentToRow(page: MonumentListPage, m: Monument): Seq[String] = {
    val otherParams =
      if (m.otherParams.isEmpty) ""
      else Json.stringify(JsObject(m.otherParams.view.mapValues(JsString).toSeq))
    val byName: Map[String, String] = Map(
      "source_page"  -> page.title,
      "page_revid"   -> page.revId.map(_.toString).getOrElse(""),
      "page_ts"      -> page.timestamp.map(_.toString).getOrElse(""),
      "id"           -> m.id,
      "name"         -> m.name,
      "name_detail"  -> m.nameDetail.getOrElse(""),
      "year"         -> m.year.getOrElse(""),
      "description"  -> m.description.getOrElse(""),
      "article"      -> m.article.getOrElse(""),
      "city"         -> m.city.getOrElse(""),
      "city_type"    -> m.cityType.getOrElse(""),
      "place"        -> m.place.getOrElse(""),
      "user"         -> m.user.getOrElse(""),
      "area"         -> m.area.getOrElse(""),
      "lat"          -> m.lat.getOrElse(""),
      "lon"          -> m.lon.getOrElse(""),
      "typ"          -> m.typ.getOrElse(""),
      "sub_type"     -> m.subType.getOrElse(""),
      "photo"        -> m.photo.getOrElse(""),
      "gallery"      -> m.gallery.getOrElse(""),
      "resolution"   -> m.resolution.getOrElse(""),
      "state_id"     -> m.stateId.getOrElse(""),
      "contest"      -> m.contest.map(_.toString).getOrElse(""),
      "source"       -> m.source.getOrElse(""),
      "other_params" -> otherParams
    )
    columns.map(byName)
  }

  private def rowToMonument(row: Map[String, String], listConfig: ListConfig): Monument = {
    val otherParams = opt(row.getOrElse("other_params", "")) match {
      case None => Map.empty[String, String]
      case Some(json) =>
        Json.parse(json).as[JsObject].value.view.mapValues(_.as[String]).toMap
    }
    Monument(
      page = row.getOrElse("source_page", ""),
      id = row.getOrElse("id", ""),
      name = row.getOrElse("name", ""),
      nameDetail = opt(row.getOrElse("name_detail", "")),
      year = opt(row.getOrElse("year", "")),
      description = opt(row.getOrElse("description", "")),
      article = opt(row.getOrElse("article", "")),
      city = opt(row.getOrElse("city", "")),
      cityType = opt(row.getOrElse("city_type", "")),
      place = opt(row.getOrElse("place", "")),
      user = opt(row.getOrElse("user", "")),
      area = opt(row.getOrElse("area", "")),
      lat = opt(row.getOrElse("lat", "")),
      lon = opt(row.getOrElse("lon", "")),
      typ = opt(row.getOrElse("typ", "")),
      subType = opt(row.getOrElse("sub_type", "")),
      photo = opt(row.getOrElse("photo", "")),
      gallery = opt(row.getOrElse("gallery", "")),
      resolution = opt(row.getOrElse("resolution", "")),
      stateId = opt(row.getOrElse("state_id", "")),
      contest = opt(row.getOrElse("contest", "")).map(_.toLong),
      source = opt(row.getOrElse("source", "")),
      otherParams = otherParams,
      listConfig = Some(listConfig)
    )
  }

  def write(path: String, pages: Iterable[MonumentListPage]): Unit = {
    val file = new File(path)
    Option(file.getParentFile).foreach(dir =>
      java.nio.file.Files.createDirectories(dir.toPath)
    )
    val writer = CSVWriter.open(file, "UTF-8")
    try {
      writer.writeRow(columns)
      pages.foreach { page =>
        page.monuments.foreach(m => writer.writeRow(monumentToRow(page, m)))
      }
    } finally writer.close()
  }

  /** Read the cache back as one [[MonumentListPage]] per source page (order of
    * first appearance preserved). Returns `Nil` if the file is absent. */
  def read(path: String, listConfig: ListConfig): Seq[MonumentListPage] = {
    val file = new File(path)
    if (!file.exists()) return Nil

    val byPage = scala.collection.mutable.LinkedHashMap.empty[String, MonumentListPage]
    val reader = CSVReader.open(file, "UTF-8")
    // Row by row: `allWithHeaders()` would hold every row's string map in
    // memory at once, on top of the monuments built from them.
    try reader.iteratorWithHeaders.foreach { row =>
      val title = row.getOrElse("source_page", "")
      val existing = byPage.get(title)
      val monument = rowToMonument(row, listConfig)
      val page = existing match {
        case Some(p) => p.copy(monuments = p.monuments :+ monument)
        case None =>
          MonumentListPage(
            title,
            opt(row.getOrElse("page_revid", "")).map(_.toLong),
            opt(row.getOrElse("page_ts", "")).map(ZonedDateTime.parse),
            Vector(monument)
          )
      }
      byPage.update(title, page)
    }
    finally reader.close()
    byPage.values.toVector
  }
}
