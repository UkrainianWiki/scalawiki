package org.scalawiki.wlx

import org.scalawiki.dto.Image
import org.scalawiki.wlx.ImageDB.allowList
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.query.ImageQuery

import java.time.ZonedDateTime
import scala.concurrent.Future

case class ImageDB(
    contest: Contest,
    images: Iterable[Image],
    monumentDb: Option[MonumentDB],
    minMpx: Option[Float] = None,
    recentlyTakenFiles: Option[String] = None,
    ignoreRecentlyTaken: Boolean = false
) {

  def this(contest: Contest, images: Seq[Image]) = this(contest, images, None)

  def this(contest: Contest, images: Seq[Image], monumentDb: MonumentDB) =
    this(contest, images, Some(monumentDb))

  lazy val _byMegaPixels: Grouping[Int, Image] =
    new Grouping("mpx", ImageGrouping.byMpx, images)

  private lazy val withCorrectIds: Seq[Image] = monumentDb
    .fold(images) { db =>
      images.filter(_.monumentId.exists(db.ids.contains))
    }
    .toSeq

  lazy val sansIneligible: Seq[Image] =
    withCorrectIds.filterNot(_.pageId.exists(ineligibleIds.contains))

  // Photos created after this instant count as "recently taken" and are ineligible
  // for the main nominations (a wartime security rule; see
  // `dates.<year>.latest-allowed-pictured-date` in the campaign .conf). Years with
  // no configured date keep the historical default of 30 June of the contest year.
  private val latestAllowedPictured: ZonedDateTime =
    contest
      .dates()
      .flatMap(_.latestAllowedPicturedInstant)
      .getOrElse(ZonedDateTime.parse(s"${contest.year}-06-30T23:59:59Z"))

  val filesList = recentlyTakenFiles.toList
    .flatMap(file => scala.io.Source.fromFile(file).getLines.toList)

  lazy val ineligible: Seq[Image] = withCorrectIds.filter { i =>
    val after30 = !ignoreRecentlyTaken &&
      i.metadata.exists(_.date.exists(_.isAfter(latestAllowedPictured))) &&
      !i.specialNominations.contains(s"WLM${contest.year}-UA-interior") &&
      (filesList.isEmpty || filesList.contains(i.title))

    //        !i.pageId.exists(allowList.contains)

    val category = i.categories
      .contains(s"Ineligible submissions for WLM ${contest.year} in Ukraine")

    val mpx = !i.atLeastMpx(minMpx)

    after30 || category || mpx
  }

  lazy val ineligibleIds: Set[Long] = ineligible.flatMap(_.pageId).toSet

  lazy val _byId: Grouping[String, Image] =
    new Grouping("monument", ImageGrouping.byMonument, sansIneligible)

  lazy val _byRegion: Grouping[String, Image] =
    new Grouping("monument", ImageGrouping.byRegion, sansIneligible)

  lazy val _byAuthor: Grouping[String, Image] =
    new Grouping("author", ImageGrouping.byAuthor, sansIneligible)

  lazy val _byRegionAndId: NestedGrouping[String, Image] =
    _byRegion.compose(ImageGrouping.byMonument)

  lazy val _byRegionAndAuthor: NestedGrouping[String, Image] =
    _byRegion.compose(ImageGrouping.byAuthor)

  lazy val _byAuthorAndId: NestedGrouping[String, Image] =
    _byAuthor.compose(ImageGrouping.byMonument)

  lazy val _byAuthorAndRegion: NestedGrouping[String, Image] =
    _byAuthor.compose(ImageGrouping.byRegion)

  lazy val _byMegaPixelsAndAuthor =
    _byMegaPixels.grouped.mapValues { images =>
      images.groupBy(_.author.getOrElse(""))
    }

  def byMegaPixels(mp: Int): Iterable[Image] = _byMegaPixels.by(mp)

  def ids: Set[String] = _byId.keys

  def authors: Set[String] = _byAuthor.keys

  def byId(id: String): Seq[Image] = _byId.by(id)

  def containsId(id: String): Boolean = _byId.contains(id)

  def imagesByRegion(regId: String): Iterable[Image] =
    if (regId.length == 2) {
      _byRegion.by(regId)
    } else {
      val parentId = regId.substring(0, 2)
      val topImages = _byRegion.by(parentId)
      topImages.filter(
        _.monumentId.exists(_.replace("-", "").startsWith(regId))
      )
    }

  def idsByRegion(regId: String): Set[String] = {
    if (regId.length == 2) {
      _byRegionAndId.by(regId).keys
    } else {
      val parentId = regId.substring(0, 2)
      val topKeys = _byRegionAndId.by(parentId).keys
      topKeys.filter(_.replace("-", "").startsWith(regId))
    }
  }

  def idByAuthor(author: String): Set[String] = _byAuthorAndId.by(author).keys

  def authorsByRegion(regId: String): Set[String] =
    _byRegionAndAuthor.by(regId).keys

  def byMegaPixelFilterAuthorMap(
      predicate: Int => Boolean
  ): Map[String, Seq[Image]] = {
    _byMegaPixels.grouped
      .filterKeys(mpx => mpx >= 0 && predicate(mpx))
      .values
      .flatten
      .toSeq
      .groupBy(_.author.getOrElse(""))
  }

  def authorsCountById: Map[String, Int] =
    _byId.grouped.mapValues(_.flatMap(_.author).toSet.size).toMap

  def imageCountById: Map[String, Int] = _byId.grouped.mapValues(_.size).toMap

  def byNumberOfAuthors: Map[Int, Map[String, Iterable[Image]]] = {
    _byId.grouped
      .groupBy { case (id, photos) =>
        photos.flatMap(_.author).toSet.size
      }
      .mapValues(_.toMap)
      .toMap
  }

  def byNumberOfPhotos: Map[Int, Map[String, Iterable[Image]]] = {
    _byId.grouped
      .groupBy { case (id, photos) =>
        photos.size
      }
      .mapValues(_.toMap)
      .toMap
  }

  def subSet(
      monuments: Seq[Monument],
      withFalseIds: Boolean = false
  ): ImageDB = {
    val subSetMonumentDb =
      new MonumentDB(monumentDb.get.contest, monuments, withFalseIds)
    val subSetImages =
      images.filter(_.monumentId.fold(false)(subSetMonumentDb.ids.contains))
    new ImageDB(contest, subSetImages, Some(subSetMonumentDb))
  }

  def subSet(f: Image => Boolean): ImageDB = {
    new ImageDB(contest, images.filter(f), monumentDb)
  }

}

class Grouping[T, F](name: String, val f: F => T, data: Iterable[F]) {

  val grouped: Map[T, Seq[F]] = data.groupBy(f).mapValues(_.toSeq).toMap

  def by(key: T): Seq[F] = grouped.getOrElse(key, Nil)

  def contains(id: T): Boolean = grouped.contains(id)

  def headBy(key: T): F = by(key).head

  def headOptionBy(key: T): Option[F] = by(key).headOption

  def keys: Set[T] = grouped.keySet

  def size: Int = grouped.size

  def compose(g: F => T): NestedGrouping[T, F] =
    new NestedGrouping(
      grouped.mapValues(v => new Grouping("", g, v)).toMap
    )

}

class NestedGrouping[T, F](val grouped: Map[T, Grouping[T, F]]) {

  val keys: Set[T] = grouped.keySet

  def by(key: T): Grouping[T, F] =
    grouped.getOrElse(key, new Grouping[T, F]("", null, Seq.empty))

  def by(key1: T, key2: T): Iterable[F] = by(key1).by(key2)

  def headBy(key1: T, key2: T): F = by(key1).headBy(key2)

  def headOptionBy(key1: T, key2: T): Option[F] = by(key1).headOptionBy(key2)

}

object ImageGrouping {

  def byMpx: Image => Int = (i: Image) => i.mpx.map(_.toInt).getOrElse(-1)

  def byMonument: Image => String = (i: Image) => i.monumentId.getOrElse("")

  def byRegion: Image => String = (i: Image) => Monument.getRegionId(i.monumentId)

  def byAuthor: Image => String = (i: Image) => i.author.getOrElse("")

}

object ImageDB {

  import scala.concurrent.ExecutionContext.Implicits.global

  def create(
      contest: Contest,
      imageQuery: ImageQuery,
      monumentDb: Option[MonumentDB],
      minMpx: Option[Float] = None
  ): Future[ImageDB] = {
    imageQuery.imagesFromCategory(contest).map { images =>
      new ImageDB(contest, images, monumentDb, minMpx)
    }
  }

  private val allowList = Set[Long](139033190, 139033189, 138896313, 138896491, 138679587,
    139500804, 139899874, 139899873, 139643873, 139473382, 139843450, 138561292, 138543453,
    139756664, 138561778, 138561777, 138543414, 138543415, 139842986, 139431707, 138397222,
    139431706, 139431716, 139431703, 138393699, 139676985, 139676986, 139641232, 139643516,
    139685413, 139590379, 138393495, 139590381, 139590378, 138909394, 138478400, 139033188,
    138397226, 138943312, 139913130, 139640691, 138942396, 138896118, 139783542, 139783543,
    138896202, 138944112, 138896244, 139783534, 139430660, 138376808, 138942397, 139363471,
    139363470, 139363466, 139237579, 139313124, 138393537, 139684886, 138394378, 138397346,
    139723578, 139723568, 139723569, 138394430, 138393538, 139721126, 138397221, 139295608,
    138394203, 139363709, 138397220, 138397227, 139363710, 139363713, 139363711, 138376523,
    138397390, 138942398, 138950033, 138376807, 138950032, 138397349, 139276786, 139684887,
    138942408, 139431710, 139363714, 139431708, 138943859, 138394379, 138943647, 138909161,
    139646725, 138393972, 138909396, 139723573, 139431705, 139313128, 139723576, 139783536,
    139723572, 139723570, 139313132, 139173215, 139723577, 139723571, 139660924, 139660925,
    139913381, 138942403, 138943310, 139778492, 138376590, 139751221, 139721718, 139721714,
    139783537, 139751228, 138942404, 138393704, 138393697, 138393702, 138888154, 139025954,
    139590377, 139913380, 139643684, 139643683, 139643872, 139751220, 139751222, 139363468,
    139363467, 139643871, 139237811, 139785053, 139091352, 138943860, 138397224, 139401104,
    139400816, 139313126, 138909243, 138376524, 139313129, 139641093, 139640690, 139313125,
    139721715, 139721713, 138397215, 138393861, 138393700, 139313130, 138943316, 138397350,
    138949957, 139723584, 139751226, 138478405, 138943315, 138943311, 138943648, 138376522,
    138376615, 138393776, 138397345, 139237058, 138943314, 138943309, 138943308)
}
