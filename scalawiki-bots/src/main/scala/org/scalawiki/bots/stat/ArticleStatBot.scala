package org.scalawiki.bots.stat

import org.scalawiki.MwBot
import org.scalawiki.bots.FileUtils
import org.scalawiki.cache.CachedBot
import org.scalawiki.dto.filter.RevisionFilterDateAndUser
import org.scalawiki.dto.{Page, Site}
import org.scalawiki.query.QueryLibrary

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ArticleStatBot(implicit val bot: MwBot = MwBot.fromHost(MwBot.ukWiki)) extends QueryLibrary {

  def pagesRevisions(ids: Seq[Long]): Future[Iterable[Option[Page]]] = {
    Future.traverse(ids)(pageRevisions)
  }

  def pageRevisions(id: Long): Future[Option[Page]] = {
    bot.run(pageRevisionsQuery(id)).map(_.headOption)
  }

  def stat(event: ArticlesEvent): Future[EventStat] = {

    val from = Some(event.start)
    val to = Some(event.end)

    val revisionFilter = new RevisionFilterDateAndUser(from, to)

    val newPagesIdsF = articlesWithTemplate(event.newTemplate)
    val improvedPagesIdsF = articlesWithTemplate(event.improvedTemplate)

    Future.sequence(Seq(newPagesIdsF, improvedPagesIdsF)).flatMap { ids =>
      val newPagesIds = ids.head
      val improvedPagesIds = ids.last
      println(s"New ${newPagesIds.size} $newPagesIds")
      println(s"Improved ${improvedPagesIds.size} $improvedPagesIds")

      val allIds = newPagesIds.toSet ++ improvedPagesIds.toSet

      pagesRevisions(allIds.toSeq).map { allPages =>
        val revStats = allPages.iterator.flatten.toIndexedSeq.zipWithIndex
          .flatMap {
            case (page, index) if page.history.editedIn(revisionFilter) =>
              bot.log.info(
                s"$index/${allIds.size} making revision stat for ${page.title}"
              )
              Some(RevisionStat.fromPage(page, revisionFilter))
            case (page, index) =>
              bot.log.info(
                s"$index/${allIds.size} skipping ${page.title} because it's rejected by revision filter"
              )
              None
          }
          .sortBy(-_.addedOrRewritten)

        EventStat(event, revStats)
      }
    }
  }
}

case class EventStat(event: ArticlesEvent, revStats: Seq[RevisionStat]) {

  println(s"making event stat for ${event.name}")

  val from = Some(event.start)
  val to = Some(event.end)

  val revisionFilter = new RevisionFilterDateAndUser(from, to)

  val (created, improved) = revStats.partition(_.history.createdAfter(from))

  val allStat = new ArticleStat(revisionFilter, revStats, "All")
  val createdStat = new ArticleStat(revisionFilter, created, "created")
  val improvedStat = new ArticleStat(revisionFilter, improved, "improved")

  def asWiki = Seq(allStat, createdStat, improvedStat).mkString("\n")
}

object ArticleStatBot {

  def contestStat(event: ArticlesEvent) = {
    val cacheName = event.id.getOrElse(event.name)
    val mwBot = new CachedBot(Site.ukWiki, cacheName, true)
    val bot = new ArticleStatBot()(mwBot)

    bot.stat(event)
  }

  def main(args: Array[String]) {

    val (contests, weeks) = Events.events()

    val sweden = weeks.find(_.newTemplate.startsWith("2019Sweden-week")).toSeq
    Future
      .sequence(sweden.map(contestStat))
      .map(_.map(_.asWiki).mkString("\n"))
      .map { wikitext =>
        FileUtils.write("articles.wiki", wikitext)
      }
  }

  def eventSummary(stats: Seq[Long]): Unit = {
    val events = stats.size
    val added = stats.sum
    println(s"!!!!!!!!! weeks: $events, bytes: $added")
    println(s"!!!!!!!!! weeks: $stats")
  }
}
