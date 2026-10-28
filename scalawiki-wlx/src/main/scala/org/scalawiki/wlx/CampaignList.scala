package org.scalawiki.wlx

import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.Query
import org.scalawiki.dto.cmd.query.list._
import org.scalawiki.dto.{Namespace, Page}
import org.scalawiki.wlx.CampaignList.categoriesMembers
import org.scalawiki.wlx.dto.{Contest, ContestType, HasImagesCategory}
import org.scalawiki.{HasBot, MwBot, WithBot}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CampaignList {
  self: HasBot =>

  def categoryMembers(parent: String): Future[Iterable[Page]] = {
    bot.run(
      Action(
        Query(
          ListParam(
            CategoryMembers(
              CmTitle(parent),
              CmNamespace(Seq(Namespace.CATEGORY)),
              CmLimit("max")
            )
          )
        )
      )
    )
  }

  def titles(pages: Seq[Iterable[Page]]): Seq[String] =
    pages.flatten.map(_.title)

  def getContests(hasImages: HasImagesCategory): Future[Iterable[Contest]] =
    contestsFromCategory(hasImages.imagesCategory)

  def contestsFromCategory(parent: String): Future[Iterable[Contest]] = {
    for (cats <- categoryMembers(parent))
      yield for (
        cat <- cats;
        contest <- CountryParser.fromCategoryName(cat.title)
      )
        yield contest
  }

  def categoriesMembers(categories: Seq[String]): Future[Seq[Iterable[Page]]] =
    Future.sequence(categories.map(categoryMembers))

}

object CampaignList extends CampaignList with WithBot {

  override def host = MwBot.commons

  def yearsContests(
      types: Seq[ContestType] = ContestType.all
  ): Future[Seq[Contest]] = {
    for (yearsCats <- categoriesMembers(types.map(_.imagesCategory)))
      yield for (contest <- titles(yearsCats).flatMap(CountryParser.fromCategoryName))
        yield contest
  }

  def main(args: Array[String]): Unit = {
    for (yearContests <- yearsContests()) {
      for (campaignCats <- categoriesMembers(yearContests.map(_.imagesCategory))) {
        titles(campaignCats).foreach(println)
      }
    }
  }
}
