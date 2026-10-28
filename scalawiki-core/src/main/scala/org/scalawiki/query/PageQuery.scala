package org.scalawiki.query

import org.scalawiki.MwBot
import org.scalawiki.dto.Page

import scala.concurrent.Future

trait PageQuery {

  /** @param limit
    *   value for the `rvlimit` API parameter, or `None` to omit it entirely. `Some("max")` (the
    *   default) walks the whole revision history, paging through `rvcontinue`. `None` asks
    *   MediaWiki for the current revision only — use it when you just need the live page text/revid
    *   and don't want to download (and retain) the full history with content.
    */
  def revisions(
      namespaces: Set[Int] = Set.empty,
      props: Set[String] = Set.empty,
      continueParam: Option[(String, String)] = None,
      limit: Option[String] = Some("max")
  ): Future[Iterable[Page]]

}

object PageQuery {

  def byTitles(titles: Set[String], bot: MwBot): PageQuery =
    new PageQueryImplDsl(Right(titles), bot)

  def byTitle(title: String, bot: MwBot): SinglePageQuery =
    new PageQueryImplDsl(Right(Set(title)), bot)

  def byIds(ids: Set[Long], bot: MwBot): PageQuery =
    new PageQueryImplDsl(Left(ids), bot)

  def byId(id: Long, bot: MwBot): SinglePageQuery =
    new PageQueryImplDsl(Left(Set(id)), bot)
}
