package org.scalawiki.wlx.query

import org.scalawiki.MwBot
import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.prop._
import org.scalawiki.dto.cmd.query.{PageIdsParam, Query}
import org.scalawiki.dto.{Namespace, Page}
import org.scalawiki.query.{PageQuery, QueryLibrary}
import org.scalawiki.wlx.WlxTemplateParser
import org.scalawiki.wlx.dto.lists.OtherTemplateListConfig
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.stat.progress.Progress

import java.time.ZonedDateTime
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MonumentQuery {

  def contest: Contest

  def defaultListTemplate: String = contest.uploadConfigs.head.listTemplate

  def byMonumentTemplate(
      generatorTemplate: String = defaultListTemplate,
      date: Option[ZonedDateTime] = None,
      listTemplate: Option[String] = None
  ): Future[Iterable[Monument]]

  def byMonumentTemplateMaps(
      generatorTemplate: String = defaultListTemplate,
      date: Option[ZonedDateTime] = None,
      listTemplate: Option[String] = None
  ): Future[Iterable[Map[String, String]]]

  def byPage(
      page: String,
      template: String,
      date: Option[ZonedDateTime] = None
  ): Future[Iterable[Monument]]

  /** Cheap sweep of the pages that currently embed the list template, with each one's latest
    * revision id + timestamp and no page content. The change token for the monument CSV cache
    * (mirrors [[org.scalawiki.wlx.query.ImageQuery.imageIdsFromCategory]]).
    */
  def listPageRevs(
      generatorTemplate: String = defaultListTemplate
  ): Future[Seq[MonumentQuery.MonumentListRev]]

  /** Fetch the current content of specific list pages and parse them, one entry per page (title +
    * latest revision + its monuments). Used by the monument CSV cache to refresh only the pages
    * whose revision changed.
    */
  def monumentsByPages(
      titles: Set[String],
      listTemplate: Option[String] = None
  ): Future[Seq[MonumentQuery.MonumentListPage]]
}

class MonumentQueryApi(
    val contest: Contest,
    reportDifferentRegionIds: Boolean = false
)(implicit val bot: MwBot)
    extends MonumentQuery
    with QueryLibrary {

  val host = getHost.get

  val defaultListConfig = contest.uploadConfigs.head.listConfig

  def getHost: Option[String] = contest.listsHost

  private def templateTitle(generatorTemplate: String): String =
    if (generatorTemplate.startsWith("Template")) generatorTemplate
    else "Template:" + generatorTemplate

  private val listNamespaces = Set(Namespace.PROJECT, Namespace.MAIN)

  /** Parse one list page's wikitext into Monuments, applying the same "новий АТУ" skip and
    * list-config resolution as [[byMonumentTemplate]].
    */
  private def parseListPage(
      pageTitle: String,
      text: String,
      listTemplate: Option[String]
  ): Seq[Monument] =
    if (pageTitle.contains("новий АТУ")) Nil
    else {
      val listConfig = listTemplate.fold(defaultListConfig)(
        new OtherTemplateListConfig(_, defaultListConfig)
      )
      val template = listTemplate.getOrElse(defaultListTemplate)
      Monument.monumentsFromText(text, pageTitle, template, listConfig).toSeq
    }

  override def listPageRevs(
      generatorTemplate: String
  ): Future[Seq[MonumentQuery.MonumentListRev]] =
    bot
      .page(templateTitle(generatorTemplate))
      .revisionsByGenerator(
        "embeddedin",
        "ei",
        listNamespaces,
        Set("ids", "timestamp"),
        None,
        "500"
      )
      .map { pages =>
        pages.iterator.map { page =>
          val rev = page.revisions.headOption
          MonumentQuery.MonumentListRev(
            page.title,
            rev.flatMap(_.revId),
            rev.flatMap(_.timestamp)
          )
        }.toIndexedSeq
      }

  override def monumentsByPages(
      titles: Set[String],
      listTemplate: Option[String]
  ): Future[Seq[MonumentQuery.MonumentListPage]] =
    if (titles.isEmpty) Future.successful(Nil)
    else
      Future
        .traverse(titles.grouped(50).toSeq) { chunk =>
          PageQuery
            .byTitles(chunk, bot)
            .revisions(
              props = Set("ids", "content", "timestamp", "user", "userid", "comment"),
              limit = None
            )
        }
        .map { batches =>
          batches.flatten.iterator.map { page =>
            val rev = page.revisions.headOption
            MonumentQuery.MonumentListPage(
              page.title,
              rev.flatMap(_.revId),
              rev.flatMap(_.timestamp),
              parseListPage(page.title, page.text.getOrElse(""), listTemplate)
            )
          }.toIndexedSeq
        }

  /** Shared page-fetching logic for both Monument parsing and raw-map extraction. Does NOT include
    * reportDifferentRegionIds side-effects — those stay in byMonumentTemplate.
    *
    * @param parser
    *   (pageName, wikiText) => Iterable[T] — applied per page
    */
  private def byMonumentTemplateGeneric[T](
      generatorTemplate: String,
      date: Option[ZonedDateTime],
      listTemplate: Option[String],
      parser: (String, String) => Iterable[T]
  ): Future[Iterable[T]] = {
    val title =
      if (generatorTemplate.startsWith("Template")) generatorTemplate
      else "Template:" + generatorTemplate

    // Parsing the wikitext of every list page into Monuments is CPU-bound and
    // the long pole once the API responses are cached, so show a bar over it.
    def parseWithProgress(pages: Iterable[Page])(f: Page => Iterable[T]): Iterable[T] = {
      val seq = pages.toSeq
      Progress.bar("Parsing monument lists", seq.size.toLong) { task =>
        seq.flatMap { page =>
          task.msg(page.title)
          val parsed = f(page)
          task.step()
          parsed
        }
      }
    }

    if (date.isEmpty) {
      bot
        .page(title)
        .revisionsByGenerator(
          "embeddedin",
          "ei",
          Set(Namespace.PROJECT, Namespace.MAIN),
          Set("ids", "content", "timestamp", "user", "userid", "comment"),
          None,
          "100"
        ) map { pages =>
        parseWithProgress(pages) { page =>
          if (!page.title.contains("новий АТУ"))
            parser(page.title, page.text.getOrElse(""))
          else Nil
        }
      }
    } else {
      articlesWithTemplate(title).flatMap { ids =>
        Future.traverse(ids)(id => pageRevisions(id, date.get)).map { pages =>
          parseWithProgress(pages.flatten) { page =>
            parser(page.title, page.text.getOrElse(""))
          }
        }
      }
    }
  }

  override def byMonumentTemplate(
      generatorTemplate: String,
      date: Option[ZonedDateTime] = None,
      listTemplate: Option[String] = None
  ): Future[Iterable[Monument]] = {
    val differentRegionIds = new ArrayBuffer[String]()
    val listConfig = listTemplate.fold(defaultListConfig)(
      new OtherTemplateListConfig(_, defaultListConfig)
    )
    val template = listTemplate.getOrElse(generatorTemplate)

    byMonumentTemplateGeneric(
      generatorTemplate,
      date,
      listTemplate,
      (page, text) => {
        val monuments = Monument.monumentsFromText(text, page, template, listConfig)
        if (date.isEmpty) {
          val regionIds = monuments.map(_.id.split("-").init.mkString("-")).toSet
          if (regionIds.size > 1 && reportDifferentRegionIds) {
            differentRegionIds.append(
              s"* [[$page]]: ${regionIds.toSeq.sorted.mkString(", ")}"
            )
          }
        }
        monuments
      }
    ).flatMap { monuments =>
      if (date.isEmpty && reportDifferentRegionIds) {
        bot
          .page(s"Вікіпедія:${contest.name}/differentRegionIds")
          .edit(differentRegionIds.sorted.mkString("\n"))
          .map(_ => monuments)
      } else {
        Future.successful(monuments)
      }
    }
  }

  override def byMonumentTemplateMaps(
      generatorTemplate: String,
      date: Option[ZonedDateTime] = None,
      listTemplate: Option[String] = None
  ): Future[Iterable[Map[String, String]]] = {
    val listConfig = listTemplate.fold(defaultListConfig)(
      new OtherTemplateListConfig(_, defaultListConfig)
    )
    byMonumentTemplateGeneric(
      generatorTemplate,
      date,
      listTemplate,
      (page, text) => new WlxTemplateParser(listConfig, page).parseToMaps(text)
    )
  }

  override def byPage(
      page: String,
      template: String,
      date: Option[ZonedDateTime] = None
  ): Future[Iterable[Monument]] = {
    val config = new OtherTemplateListConfig(template, defaultListConfig)
    if (!page.startsWith("Template")) {
      bot
        .page(page)
        .revisions(
          Set.empty,
          Set("content", "timestamp", "user", "userid", "comment"),
          limit = None // only the current revision is used below
        )
        .map { revs =>
          revs.headOption
            .map(page =>
              Monument
                .monumentsFromText(
                  page.text.getOrElse(""),
                  page.title,
                  template,
                  config
                )
                .toSeq
            )
            .getOrElse(Seq.empty)
        }
    } else {
      byMonumentTemplate(page, date, Some(template))
    }
  }

  def monumentsByDate(
      page: String,
      template: String,
      date: ZonedDateTime
  ): Future[Iterable[Monument]] = {
    articlesWithTemplate(page).flatMap { ids =>
      Future.traverse(ids)(id => pageRevisions(id, date)).map { pages =>
        pages.flatten.flatMap(page =>
          Monument.monumentsFromText(
            page.text.getOrElse(""),
            page.title,
            template,
            defaultListConfig
          )
        )
      }
    }
  }

  def pageRevisions(id: Long, date: ZonedDateTime): Future[Option[Page]] = {
    import org.scalawiki.dto.cmd.query.prop.rvprop._

    // rvstart + default rvdir=older enumerates backwards from `date`; only the
    // first hit (the revision current as of `date`) is used, so ask for exactly
    // one and cap DslQuery so it can't page through the rest of the history.
    val action = Action(
      Query(
        PageIdsParam(Seq(id)),
        Prop(
          Info(),
          Revisions(
            RvProp(Content, Ids, Size, User, UserId, Timestamp),
            RvLimit("1"),
            RvStart(date)
          )
        )
      )
    )

    bot.run(action, limit = Some(1L)).map { pages =>
      pages.headOption
    }
  }

}

object MonumentQuery {

  /** A list page's title and its latest revision (id + timestamp). The cheap change token for the
    * monument CSV cache; `revId` / `timestamp` are empty when the current revision is
    * revision-deleted.
    */
  case class MonumentListRev(
      title: String,
      revId: Option[Long] = None,
      timestamp: Option[ZonedDateTime] = None
  )

  /** One list page with the monuments parsed from its current revision. */
  case class MonumentListPage(
      title: String,
      revId: Option[Long],
      timestamp: Option[ZonedDateTime],
      monuments: Seq[Monument]
  )

  def create(contest: Contest, reportDifferentRegionIds: Boolean = false)(implicit
      bot: MwBot = MwBot.fromHost(MwBot.ukWiki)
  ): MonumentQuery =
    new MonumentQueryApi(contest, reportDifferentRegionIds)(bot)

}
