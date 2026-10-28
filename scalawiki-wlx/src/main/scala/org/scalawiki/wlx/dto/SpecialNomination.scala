package org.scalawiki.wlx.dto

import com.typesafe.config.{Config, ConfigFactory}
import org.scalawiki.dto.Image
import org.scalawiki.wlx.query.MonumentQuery
import net.ceedubs.ficus.Ficus._
import org.scalawiki.wlx.MonumentDB
import org.scalawiki.wlx.stat.ContestStat
import org.scalawiki.wlx.stat.reports.DesnaRegionSpecialNomination

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/** Describes monument lists for contest special nominations
  *
  * @param name
  *   Name of the special nomination
  * @param listTemplate
  *   name of template that monument lists consist of
  * @param pages
  *   pages that contain lists of monuments, ot templates that contains links to these pages
  * @param fileTemplate
  *   template on the file page that enters a file into the nomination
  * @param mediaType
  *   kind of file the nomination takes, whatever its monument: only [[SpecialNomination.Video]] is
  *   supported
  * @param idPrefix
  *   the first segment of the ids its lists give monuments of their own, in place of the region's
  *   KOATUU code (e.g. "99" in `99-141-2306`, whose second segment still carries the region); those
  *   lists also hold monuments with their regions' ids
  */
case class SpecialNomination(
    name: String,
    listTemplate: Option[String],
    pages: Seq[String],
    years: Seq[Int] = Nil,
    cities: Seq[AdmDivision] = Nil,
    fileTemplate: Option[String] = None,
    mediaType: Option[String] = None,
    idPrefix: Option[String] = None
) {

  /** Whether the nomination runs in `year`: in its [[years]], or in every year if it lists none.
    */
  def inYear(year: Int): Boolean = years.isEmpty || years.contains(year)

  /** Whether the file itself places `image` in this nomination - by its [[fileTemplate]] or its
    * [[mediaType]] - rather than its monument.
    */
  def matchesFile(image: Image): Boolean =
    fileTemplate.exists(image.specialNominations.contains) ||
      (mediaType.contains(SpecialNomination.Video) && image.isVideo)
}

object SpecialNomination {

  import scala.collection.JavaConverters._

  /** [[SpecialNomination.mediaType]] of a nomination for videos. */
  val Video = "video"

  private val mediaTypes = Set(Video)

  def load(name: String): Seq[SpecialNomination] = {
    fromConfig(ConfigFactory.load(name))
  }

  def fromConfig(config: Config): Seq[SpecialNomination] = {

    Try {
      config.getConfigList("nominations")
    }.recover { case x: Throwable =>
      println(x)
      new java.util.ArrayList()
    }.toOption
      .map(_.asScala.toSeq.map { c =>
        new SpecialNomination(
          c.getString("name"),
          c.as[Option[String]]("listTemplate"),
          c.as[Option[Seq[String]]]("pages").getOrElse(Nil),
          c.as[Option[Seq[Int]]]("years").getOrElse(Nil),
          if (c.hasPath("cities")) {
            c.getConfigList("cities").asScala.toSeq.map { citiConf =>
              val name = citiConf.getString("name")
              val code = citiConf.getString("code")
              lookupCity(name, code).head
            }
          } else Nil,
          c.as[Option[String]]("fileTemplate"),
          c.as[Option[String]]("mediaType").map { mediaType =>
            require(
              mediaTypes.contains(mediaType),
              s"Unknown mediaType '$mediaType' of special nomination ${c.getString("name")}, " +
                s"expected one of ${mediaTypes.mkString(", ")}"
            )
            mediaType
          },
          c.as[Option[String]]("idPrefix")
        )
      })
      .getOrElse(Seq.empty)
  }

  lazy val nominations = load("wlm_ua.conf")

  /** A nomination's monuments: the rows of its own lists, and those it takes from the main monument
    * lists (by city, or by place for "Пам'ятки Подесення").
    */
  case class NominationMonuments(fromLists: Seq[Monument], fromMonumentDb: Seq[Monument] = Nil) {
    def all: Seq[Monument] = fromMonumentDb ++ fromLists
  }

  /** The nominations running in `year` that have an [[idPrefix]], by it. */
  def byIdPrefix(nominations: Seq[SpecialNomination], year: Int): Map[String, SpecialNomination] =
    byIdPrefix(nominations.filter(_.inYear(year)))

  /** The nominations that have an [[idPrefix]], by it, whatever their years: a prefix belongs to
    * its lists, which files of other years may still cite. Of nominations sharing a prefix, the one
    * of the latest year.
    */
  def byIdPrefix(nominations: Seq[SpecialNomination]): Map[String, SpecialNomination] =
    nominations
      .filter(_.idPrefix.nonEmpty)
      .sortBy(n => if (n.years.isEmpty) Int.MaxValue else n.years.max)
      .map(n => n.idPrefix.get -> n)
      .toMap

  def getMonumentsMap(
      nominations: Seq[SpecialNomination],
      stat: ContestStat
  ): Future[Map[SpecialNomination, Seq[Monument]]] =
    getNominationMonuments(nominations, stat).map(_.map { case (nomination, m) =>
      nomination -> m.all
    })

  def getNominationMonuments(
      nominations: Seq[SpecialNomination],
      stat: ContestStat
  ): Future[Map[SpecialNomination, NominationMonuments]] = {
    val contest = stat.contest
    val monumentQuery =
      MonumentQuery.create(contest, reportDifferentRegionIds = false)

    def byPages(pages: Seq[String], listTemplate: String): Future[Seq[Monument]] =
      Future
        .traverse(pages)(page => monumentQuery.byPage(page, listTemplate).map(_.toSeq))
        .map(_.flatten)

    Future
      .traverse(nominations.filter(_.listTemplate.nonEmpty)) { nomination =>
        val listTemplate = nomination.listTemplate.get
        val monumentsF: Future[NominationMonuments] =
          if (nomination.pages.nonEmpty && nomination.name != "Пам'ятки Подесення") {
            byPages(nomination.pages, listTemplate).map(NominationMonuments(_))
          } else if (nomination.cities.nonEmpty) {
            Future.successful(
              NominationMonuments(Nil, monumentsInCities(nomination.cities, stat.monumentDb.get))
            )
          } else if (nomination.name == "Пам'ятки Подесення") {
            val desna = DesnaRegionSpecialNomination()
            val placeIds = desna.places.flatMap(desna.getPlace).map(_.code)
            val k2k = placeIds.flatMap(Katotth.toKoatuu.get)
            val allPlaceIds = (placeIds ++ k2k).toSet

            val monumentDb = stat.monumentDb.get
            val fromDb = monumentDb.allMonuments.filter { monument =>
              monumentDb.placeByMonumentId
                .get(monument.id)
                .exists(allPlaceIds.contains)
            }
            byPages(nomination.pages, listTemplate).map(NominationMonuments(_, fromDb))
          } else {
            Future.successful(NominationMonuments(Nil))
          }
        monumentsF.map(monuments => nomination -> monuments)
      }
      .map(_.toMap)
  }

  def monumentsInCities(
      cities: Seq[AdmDivision],
      monumentDb: MonumentDB
  ): Seq[Monument] = {
    val placeIds = cities.map(_.code).toSet
    monumentDb.allMonuments.filter { monument =>
      monumentDb.placeByMonumentId.get(monument.id).exists(placeIds.contains)
    }
  }

  def lookupCity(name: String, code: String): Seq[AdmDivision] = {
    Country.Ukraine.byIdAndName(code.take(2), name).filter(_.code == code)
  }

}
