package org.scalawiki.dto

import org.scalawiki.dto.history.History

case class Page(
    id: Option[Long],
    ns: Option[Int],
    title: String,
    revisions: Seq[Revision] = Seq.empty,
    images: Seq[Image] = Seq.empty,
    editToken: Option[String] = None,
    missing: Boolean = false,
    length: Option[Int] = None,
    subjectId: Option[Long] = None,
    talkId: Option[Long] = None,
    langLinks: Map[String, String] = Map.empty,
    links: Seq[Page] = Seq.empty,
    categoryInfo: Option[CategoryInfo] = None,
    invalidReason: Option[String] = None,
    categories: Seq[String] = Seq.empty
) /*extends HasId[Page]*/ {
  val history = new History(revisions)

  def titleWithoutNs: String = title.split("\\:").last

  def withText(text: String): Page =
    copy(revisions = Page.revisionsFromText(Some(text)))

  def text: Option[String] = revisions.headOption.flatMap(_.content)

  def isTalkPage: Boolean = ns.exists(_ % 2 == 1)

  def isArticle: Boolean = ns.contains(Namespace.MAIN)

  def withId(id: Long): Page = copy(id = Some(id))

  def lastRevisionUser: Option[Contributor] =
    revisions.headOption.flatMap(_.user)

  def appendLists(other: Page): Page = copy(
    revisions = this.revisions ++ other.revisions,
    images = this.images ++ other.images,
    langLinks = this.langLinks ++ other.langLinks,
    links = this.links ++ other.links,
    categories = this.categories ++ other.categories
  )

  def withoutContent: Page = copy(revisions = revisions.map(_.withoutContent))

}

object Page {

  def full(
      id: Option[Long],
      ns: Option[Int],
      title: String,
      missing: Boolean,
      subjectId: Option[Long],
      talkId: Option[Long],
      invalidReason: Option[String]
  ): Page = {
    new Page(
      id,
      ns,
      title,
      missing = missing,
      subjectId = subjectId,
      talkId = talkId,
      invalidReason = invalidReason
    )
  }

  def noText(
      id: Long,
      ns: Option[Int],
      title: String,
      missing: Option[String] = None
  ) =
    new Page(Some(id), ns, title, missing = missing.fold(false)(_ => true))

  def withText(id: Long, ns: Option[Int], title: String, text: Option[String]) =
    new Page(Some(id), ns, title, revisionsFromText(text))

  def withRevisionsText(
      id: Long,
      ns: Option[Int],
      title: String,
      texts: Seq[String]
  ) =
    new Page(Some(id), ns, title, Revision.many(texts: _*))

  def withRevisions(
      id: Long,
      ns: Option[Int],
      title: String,
      editToken: Option[String],
      revisions: Seq[Revision],
      missing: Option[String]
  ) =
    new Page(
      Some(id),
      ns,
      title,
      revisions,
      Seq.empty,
      editToken,
      missing.fold(false)(_ => true)
    )

  def withImages(id: Long, ns: Option[Int], title: String, images: Seq[Image]) =
    new Page(Some(id), ns, title, Seq.empty, images)

  def apply(title: String) = new Page(Some(0L), None, title)

  def apply(id: Long) = new Page(Some(0L), None, null)

  def apply(id: Long, ns: Option[Int], title: String) =
    new Page(Some(id), ns, title)

  def withEditToken(
      id: Option[Long],
      ns: Option[Int],
      title: String,
      editToken: Option[String]
  ): Page = new Page(id, ns, title, Nil, Nil, editToken)

  def revisionsFromText(text: Option[String]): Seq[Revision] =
    text.fold(Seq.empty[Revision])(content => Revision.many(content))

  def groupById(pages: Seq[Page]): Map[Long, Seq[Page]] =
    pages.filter(_.id.isDefined).groupBy(_.id.get)
}
