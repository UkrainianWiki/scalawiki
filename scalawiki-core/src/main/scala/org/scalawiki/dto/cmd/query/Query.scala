package org.scalawiki.dto.cmd.query

import org.scalawiki.dto.cmd._
import org.scalawiki.dto.cmd.query.list.{ListArg, ListParam}
import org.scalawiki.dto.cmd.query.meta.{MetaArg, MetaParam}
import org.scalawiki.dto.cmd.query.prop.{Revisions, Prop, PropArg}

/** ?action=query
  */

case class Query(override val params: QueryParam[Any]*)
    extends EnumArgument[ActionArg]("query", "Various queries.")
    with ActionArg
    with ArgWithParams[QueryParam[Any], ActionArg] {

  val lists: Seq[ListArg] = byPF({ case p: ListParam => p }).flatMap(_.args)
  val props: Seq[PropArg] = byPF({ case p: Prop => p }).flatMap(_.args)

  def revisions = props.seq.collectFirst { case r: Revisions => r }

  def revisionsWithoutContent: Query =
    Query(
      params.filterNot(_.isInstanceOf[Prop]) :+
        Prop(
          props.filterNot(_.isInstanceOf[Revisions]) ++
            revisions.map(_.withoutContent).toSeq: _*
        ): _*
    )

  val metas: Seq[MetaArg] = byPF({ case p: MetaParam => p }).flatMap(_.args)
}

/** Marker trait for parameters available together with ?action=query
  */
trait QueryParam[+T] extends Parameter[T]

case class TitlesParam(override val args: Seq[String])
    extends StringListParameter("titles", "A list of titles to work on")
    with QueryParam[String]

case class PageIdsParam(override val args: Iterable[Long])
    extends IdListParameter("pageids", "A list of page IDs to work on")
    with QueryParam[Long]

case class RevIdsParam(override val args: Seq[Long])
    extends IdListParameter("revids", "A list of revision IDs to work on")
    with QueryParam[Long]

/** Resolves redirects in titles, pageids and revids, and in the pages a generator returns:
  * the query answers the pages they point to. The API takes the parameter's presence as
  * true, so it is left out when false.
  */
case class RedirectsParam(override val arg: Boolean = true)
    extends BooleanParameter("redirects", "Automatically resolve redirects")
    with QueryParam[Boolean] {

  override def pairs: Seq[(String, String)] = if (arg) Seq(name -> "") else Nil
}

//indexpageids Include an additional pageids section listing all returned page IDs.
//export Export the current revisions of all given or generated pages.
//exportnowrap Return the export XML without wrapping it in an XML result (same format as Special:Export). Can only be used with export.
//iwurl Whether to get the full URL if the title is an interwiki link.
//continue When present, formats query-continue as key-value pairs that should simply be merged into the original request. This parameter must be set to an empty string in the initial query. This parameter is recommended for all new development, and will be made default in the next API version.
//rawcontinue Currently ignored. In the future, continue will become the default and this will be needed to receive the raw query-continue data.
//generator Get the list of pages to work on by executing the specified query module. Note: Generator parameter names must be prefixed with a "g", see examples.
//redirects Automatically resolve redirects in titles, pageids, and revids, and in pages returned by generator.
//converttitles Convert titles to other variants if necessary. Only works if the wiki's content language supports variant conversion. Languages that support variant conversion include gan, iu, kk, ku, shi, sr, tg, uz, zh.
