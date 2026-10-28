package org.scalawiki.dto.cmd.query.prop

import java.time.ZonedDateTime

import org.scalawiki.dto.cmd._
import org.scalawiki.dto.cmd.query.Module
import org.scalawiki.dto.cmd.query.prop.rvprop.RvProp

/** ?action=query&amp;prop=revisions See more at https://www.mediawiki.org/wiki/API:Revisions
  */
case class Revisions(override val params: RvParam*)
    extends Module[PropArg]("rv", "revisions", "Get revision information.")
    with PropArg
    with ArgWithParams[RvParam, PropArg] {

  def withoutContent = {
    val filtered: Seq[RvParam] = params.map {
      case p: RvProp => p.withoutContent
      case other     => other
    }
    Revisions(filtered: _*)
  }

  def hasContent: Boolean = prop.exists(_.hasContent)

  def prop = byPF({ case p: RvProp => p }).headOption

}

/** Marker trait for parameters used with prop=revisions
  */
trait RvParam extends Parameter[Any]

package rvprop {

  /** ?action=query&amp;prop=revisions&amp;rvprop=
    */
  case class RvProp(override val args: RvPropArg*)
      extends EnumParameter[RvPropArg](
        "rvprop",
        "Which properties to get for each revision:"
      )
      with RvParam {

    def hasContent: Boolean = args.contains(Content)

    def withoutContent: RvProp = RvProp(args.filter(_ != Content): _*)
  }

  /** Trait for rvprop= arguments
    */

  trait RvPropArg extends EnumArg[RvPropArg] {
    val param = RvProp
  }

  /** rvprop= arguments
    */
  object Ids extends EnumArgument[RvPropArg]("ids", "The ID of the revision.") with RvPropArg

  object Flags extends EnumArgument[RvPropArg]("flags", "Revision flags (minor).") with RvPropArg

  object Timestamp
      extends EnumArgument[RvPropArg](
        "timestamp",
        "The timestamp of the revision."
      )
      with RvPropArg

  object User extends EnumArgument[RvPropArg]("user", "User that made the revision.") with RvPropArg

  object UserId
      extends EnumArgument[RvPropArg](
        "userid",
        "User ID of the revision creator."
      )
      with RvPropArg

  object Size
      extends EnumArgument[RvPropArg]("size", "Length (bytes) of the revision.")
      with RvPropArg

  object Sha1
      extends EnumArgument[RvPropArg](
        "sha1",
        "SHA-1 (base 16) of the revision."
      )
      with RvPropArg

  object ContentModel
      extends EnumArgument[RvPropArg](
        "contentmodel",
        "Content model ID of the revision."
      )
      with RvPropArg

  object Comment
      extends EnumArgument[RvPropArg](
        "comment",
        "Comment by the user for the revision."
      )
      with RvPropArg

  object ParsedComment
      extends EnumArgument[RvPropArg](
        "parsedcomment",
        "Parsed comment by the user for the revision."
      )
      with RvPropArg

  object Content extends EnumArgument[RvPropArg]("content", "Text of the revision.") with RvPropArg

  object Tags extends EnumArgument[RvPropArg]("tags", "Tags for the revision.") with RvPropArg

}

case class RvLimit(override val arg: String)
    extends StringParameter(
      "rvlimit",
      "The maximum number of revisions to return."
    )
    with RvParam

case class RvStartId(override val arg: Long)
    extends LongParameter("rvstartid", "Revision ID to start listing from.")
    with RvParam
case class RvEndId(override val arg: Long)
    extends LongParameter("rvendid", "Revision ID to stop listing at.")
    with RvParam

case class RvStart(override val arg: ZonedDateTime)
    extends DateTimeParameter("rvstart", "Timestamp to start listing from.")
    with RvParam
case class RvEnd(override val arg: ZonedDateTime)
    extends DateTimeParameter("rvend", "Timestamp to end listing at.")
    with RvParam

case class RvDir(override val args: RvDirArg*)
    extends EnumParameter[RvDirArg](
      "rvdir",
      "Which properties to get for each revision:"
    )
    with RvParam

trait RvDirArg extends EnumArg[RvDirArg] { val param = RvDir }

object Older extends EnumArgument[RvDirArg]("older", "List newest revisions first.") with RvDirArg
object Newer extends EnumArgument[RvDirArg]("newer", "List oldest revisions first.") with RvDirArg

case class RvUser(override val arg: String)
    extends StringParameter("rvuser", "Only list revisions made by this user.")
    with RvParam
case class RvExcludeUser(override val arg: String)
    extends StringParameter(
      "rvexcludeuser",
      "Do not list revisions made by this user."
    )
    with RvParam

case class RvExpandTemplates(override val arg: Boolean = true)
    extends BooleanParameter(
      "rvexpandtemplates",
      "Expand templates in rvprop=content output."
    )
    with RvParam
case class RvGenerateXml(override val arg: Boolean = true)
    extends BooleanParameter(
      "rvgeneratexml",
      "Generate XML parse tree for revision content."
    )
    with RvParam
case class RvParse(override val arg: Boolean = true)
    extends BooleanParameter("rvparse", "Parse revision content.")
    with RvParam
case class RvSection(override val arg: Int)
    extends IntParameter(
      "rvsection",
      "If rvprop=content is set, only retrieve the contents of this section."
    )
    with RvParam
case class RvSlots(override val arg: String)
    extends StringParameter("rvslots", "rvslots")
    with RvParam

// TODO single Enum value arg
case class RvDiffTo(override val args: RvDiffToArg*)
    extends EnumParameter[RvDiffToArg](
      "rvdiffto",
      "Revision ID to diff each revision to."
    )
    with RvParam

trait RvDiffToArg extends EnumArg[RvDiffToArg] { val param = RvDiffTo }

object Prev extends EnumArgument[RvDiffToArg]("prev", "previous revision.") with RvDiffToArg
object Next extends EnumArgument[RvDiffToArg]("next", "next revision.") with RvDiffToArg
object Cur extends EnumArgument[RvDiffToArg]("cur", "current revision.") with RvDiffToArg

case class RvDiffToText(override val arg: String)
    extends StringParameter("rvdifftotext", "Text to diff each revision to.")
    with RvParam
case class RvTag(override val arg: String)
    extends StringParameter(
      "rvtag",
      "Only list revisions tagged with this tag."
    )
    with RvParam

// TODO single Enum value arg
case class RvContentFormat(override val args: RvContentFormatArg*)
    extends EnumParameter[RvContentFormatArg](
      "rvcontentformat",
      "Serialization format used for difftotext and expected for output of content."
    )
    with RvParam

trait RvContentFormatArg extends EnumArg[RvContentFormatArg] {
  val param = RvContentFormat
}

object TextXWiki
    extends EnumArgument[RvContentFormatArg]("text/x-wiki", "text/x-wiki")
    with RvContentFormatArg
object TextJavaScript
    extends EnumArgument[RvContentFormatArg](
      "text/javascript",
      "text/javascript"
    )
    with RvContentFormatArg
object ApplicationJson
    extends EnumArgument[RvContentFormatArg](
      "application/json",
      "application/json"
    )
    with RvContentFormatArg
object TextCss
    extends EnumArgument[RvContentFormatArg]("text/css", "text/css")
    with RvContentFormatArg
object TextPlain
    extends EnumArgument[RvContentFormatArg]("text/plain", "text/plain")
    with RvContentFormatArg
