package org.scalawiki.wlx.stat.reports

import org.scalawiki.dto.markup.Table
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}

class MostPopularMonuments(val stat: ContestStat) extends Reporter {

  def this(
      imageDbs: Seq[ImageDB],
      totalImageDb: ImageDB,
      monumentDb: MonumentDB
  ) = this(
    ContestStat(
      monumentDb.contest,
      imageDbs.headOption
        .map(_.contest.year)
        .getOrElse(monumentDb.contest.year),
      Some(monumentDb),
      imageDbs.lastOption.getOrElse(totalImageDb),
      totalImageDb,
      imageDbs // .headOption.map(_ => imageDbs.init).getOrElse(Seq.empty)
    )
  )

  val name = "Most photographed objects"

  override def category: String =
    contest.contestType.name + " in " + contest.country.name

  def table: Table =
    mostPopularMonumentsTable(
      stat.dbsByYear,
      stat.totalImageDb,
      stat.monumentDb.get
    )

  def mostPopularMonumentsTable(
      imageDbs: Seq[ImageDB],
      totalImageDb: ImageDB,
      monumentDb: MonumentDB
  ): Table = {
    val imageDbsByYear = imageDbs.groupBy(_.contest.year)
    val yearSeq = imageDbsByYear.keys.toSeq.sorted

    val authorsColumns = Seq("N", "Id", "Name", "Category") ++ Seq("authors", "photos")

    val photosCountTotal = totalImageDb.imageCountById
    val authorsCountTotal = totalImageDb.authorsCountById

    val topAuthors = topN(100, authorsCountTotal)

    val rows = topAuthors.zipWithIndex.map { case (id, index) =>
      val monument = monumentDb.byId(id).get
      Seq(
        (index + 1).toString,
        s"{{nobr|$id}}",
        monument.name.replaceAll("\\[\\[", "[[:uk:"),
        monument.galleryLink
      ) ++ Seq(
        authorsCountTotal.getOrElse(id, 0).toString,
        photosCountTotal.getOrElse(id, 0).toString
      )
    }

    val yearsStr = Seq(yearSeq.headOption, yearSeq.lastOption).flatten
      .mkString("(", " - ", ")")
    Table(
      authorsColumns,
      rows,
      name + s" in ${stat.imageDbsByYear.size} year(s) $yearsStr"
    )
  }

  def mostPopularMonumentsInRegions(
      totalImageDb: ImageDB,
      monumentDb: MonumentDB
  ): Map[String, Table] = {
    val authorsColumns = Seq("N", "Id", "Name", "Category") ++
      Seq("authors", "photos")

    val country = monumentDb.contest.country
    monumentDb.regionIds.map { regionId =>
      val regionName = country.regionName(regionId)

      val shortRegionName = regionName
        .replaceAll("область", "")
        .replaceAll("Автономна Республіка", "АР")

      val imageDb = ImageDB(
        totalImageDb.contest,
        totalImageDb.imagesByRegion(regionId),
        Some(monumentDb)
      )

      val photosCountTotal = imageDb.imageCountById
      val authorsCountTotal = imageDb.authorsCountById

      val topAuthors = topN(5, authorsCountTotal)

      val rows = topAuthors.zipWithIndex.map { case (id, index) =>
        val monument = monumentDb.byId(id).get
        Seq(
          (index + 1).toString,
          s"{{nobr|$id}}",
          monument.name.replaceAll("\\[\\[", "[[:uk:"),
          monument.galleryLink
        ) ++ Seq(
          authorsCountTotal.getOrElse(id, 0).toString,
          photosCountTotal.getOrElse(id, 0).toString
        )
      }

      shortRegionName -> Table(authorsColumns, rows)
    }.toMap
  }

  override def asText: String = {
    val header = s"\n==$name==\n"

    val categoryText = s"\n[[Category:$category]]"

    val byRegions = mostPopularMonumentsInRegions(
      stat.totalImageDb,
      stat.monumentDb.get
    ).toSeq
      .sortBy(_._1)
      .map { case (name, table) =>
        s"\n==$name==\n" + table.asWiki
      }
      .mkString

    header + table.asWiki + byRegions + categoryText
  }

  def topN(n: Int, stat: Map[String, Int]): Seq[String] =
    stat.toSeq.sortBy(-_._2).take(n).map(_._1)

}
