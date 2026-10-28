package org.scalawiki.wlx.stat.reports

import org.scalawiki.MwBot
import org.scalawiki.dto.markup.Table
import org.scalawiki.wlx.ImageDB
import org.scalawiki.wlx.stat._
import org.scalawiki.wlx.stat.rating.{NumberOfImagesInPlaceBonus, RateSum, Rater}

class AuthorMonuments(
    val stat: ContestStat,
    gallery: Boolean = false,
    commons: Option[MwBot] = None
) extends Reporter {

  override def contest = stat.contest

  override def name: String = "Number of objects pictured by uploader"

  val country = contest.country

  val imageDb = stat.currentYearImageDb
  val currentImageIds = imageDb.images.flatMap(_.pageId).toSet

  val totalImageDb = stat.totalImageDb

  val oldImages = totalImageDb.images.filter(image => !currentImageIds.contains(image.pageId.get))

  val oldImageDb = new ImageDB(contest, oldImages, stat.monumentDb)

  val oldIds = oldImageDb.ids

  val rater = Rater.create(stat)

  val monumentDb = stat.monumentDb.get

  def withRating: Boolean = rater.withRating

  def rowData(
      ids: Set[String],
      images: Int,
      userOpt: Option[String] = None
  ): Seq[String] = {

    val objects = optionalUserGalleryLink(ids.size, userOpt)

    val ratingColumns = if (withRating) {
      val oldAuthorIds = userOpt.map(oldImageDb.idByAuthor).getOrElse(Set.empty)
      Seq(
        (ids intersect oldIds intersect oldAuthorIds).size, // existing
        (ids intersect oldIds -- oldAuthorIds).size, // new for author
        (ids -- oldIds).size, // new
        userOpt
          .map { user =>
            rater.rateMonumentIds(ids, user).toString
          }
          .getOrElse(ids.size)
      )
    } else Nil

    val byRegion = country.regionIds.toSeq.map { regionId =>
      val regionIds = monumentDb.byRegion(regionId).map(_.id).toSet
      val currentIds = regionIds intersect ids
      val rating: Double = userOpt
        .map(user => rater.rateMonumentIds(currentIds, user))
        .getOrElse(currentIds.size)
      optionalUserGalleryLink(
        rating,
        userOpt,
        country.regionById.get(regionId).map(_.name)
      )
    }

    ((objects +: ratingColumns :+ images) ++ byRegion).map(_.toString)
  }

  override def table: Table = {

    val columns = Seq("User", "Objects pictured") ++
      (if (withRating) Seq("Existing", "New for author", "New", "Rating")
       else Seq.empty) ++
      Seq("Photos uploaded") ++
      country.regionNames

    val totalData = "Total" +: rowData(imageDb.ids, imageDb.images.size)

    val authorsRating = imageDb.authors.map { user =>
      user -> rater.rateMonumentIds(imageDb.idByAuthor(user), user)
    }.toMap

    val authors = imageDb.authors.toSeq.sortBy { user =>
      (-authorsRating(user), user)
    }

    val authorsData = authors.map { user =>
      val noTemplateUser =
        user.replaceAll("\\{\\{", "").replaceAll("\\}\\}", "")
      val userLink = s"[[User:$noTemplateUser|$noTemplateUser]]"

      userLink +:
        rowData(
          imageDb._byAuthorAndId.by(user).keys,
          imageDb._byAuthor.by(user).size,
          Some(user)
        )
    }

    reportUnknownPlaces()

    Table(columns, totalData +: authorsData, name)
  }

  def reportUnknownPlaces() = {
    (rater match {
      case sum: RateSum =>
        sum.raters.collectFirst { case r: NumberOfImagesInPlaceBonus => r }
      case r: NumberOfImagesInPlaceBonus => Some(r)
      case _                             => None
    }).map { reportRater =>
      reportRater.unknownPlaceMonumentsByAuthor.toSeq
        .sortBy(-_._2.size)
        .map { case (author, monuments) =>
          s"# $author, ${monuments.size} unknown place ids: ${monuments.toSeq.sorted.mkString(", ")}"
        }
        .mkString("\n")
    }.map { text =>
      for (bot <- commons) {
        bot.page(page + " unknown places").edit(text)
      }
    }
  }

  private def optionalUserGalleryLink(
      number: Double,
      userOpt: Option[String],
      regionOpt: Option[String] = None
  ) = {
    if (gallery && userOpt.isDefined && number > 0) {
      userGalleryLink(number, userOpt, regionOpt)
    } else {
      number
    }
  }

  private def userGalleryLink(
      number: Double,
      userOpt: Option[String],
      regionOpt: Option[String] = None
  ) = {
    val noTemplateUser = userOpt.get
      .split("\\|")
      .last
      .replaceAll("\\{\\{", "")
      .replaceAll("\\}\\}", "")

    val galleryPage =
      "Commons:" + contest.name + "/" + noTemplateUser + regionOpt.fold("") { region =>
        "#" + region.replaceAll(" ", "_")
      }
    val galleryText = Output.galleryByRegionAndId(
      imageDb.monumentDb.get,
      imageDb.subSet(_.author == userOpt),
      oldImageDb,
      rater,
      stat.config.exists(_.previousYearsGallery)
    )

    for (bot <- commons if regionOpt.isEmpty) {
      bot.page(galleryPage).edit(galleryText)
    }

    "[[" + galleryPage + "|" + number + "]]"
  }
}
