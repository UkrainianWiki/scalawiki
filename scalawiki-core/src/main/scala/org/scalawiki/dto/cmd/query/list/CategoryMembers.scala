package org.scalawiki.dto.cmd.query.list

import org.scalawiki.dto.cmd._
import org.scalawiki.dto.cmd.query.Module

case class CategoryMembers(override val params: CmParam[Any]*)
    extends Module[ListArg](
      "cm",
      "categorymembers",
      "List all pages in a given category."
    )
    with ListArg
    with ArgWithParams[CmParam[Any], ListArg] {

  def this(
      title: Option[String],
      pageId: Option[Long],
      namespaces: Set[Int],
      limit: Option[String],
      params: CmParam[Any]*
  ) = {
    this(
      Seq(
        title.map(CmTitle).toSeq,
        pageId.map(CmPageId).toSeq,
        limit.map(CmLimit).toSeq
      ).flatten ++
        (if (namespaces.nonEmpty)
           Seq(CmNamespace(namespaces.toSeq))
         else Seq.empty)
        ++ params: _*
    )
  }
}

trait CmParam[+T] extends Parameter[T]

trait CmTypeArg extends EnumArg[CmTypeArg] {
  val param = CmType
}

case class CmTitle(override val arg: String)
    extends StringParameter(
      "cmtitle",
      "Title to search. Cannot be used together with cmpageid."
    )
    with CmParam[String]

case class CmPageId(override val arg: Long)
    extends IdParameter(
      "cmpageid",
      "Page ID to search. Cannot be used together with cmtitle."
    )
    with CmParam[Long]

case class CmNamespace(override val args: Seq[Int])
    extends IntListParameter(
      "cmnamespace",
      "Only include pages in these namespaces."
    )
    with CmParam[Int]

case class CmLimit(override val arg: String)
    extends StringParameter("cmlimit", "How many total pages to return.")
    with CmParam[String]

case class CmType(override val args: CmTypeArg*)
    extends EnumParameter[CmTypeArg](
      "cmtype",
      "Which type of category members to include"
    )
    with CmParam[CmTypeArg]

object CmTypePage extends EnumArgument[CmTypeArg]("page", "pages") with CmTypeArg

object CmTypeSubCat extends EnumArgument[CmTypeArg]("subcat", "subcats") with CmTypeArg

object CmTypeFile extends EnumArgument[CmTypeArg]("file", "files") with CmTypeArg

trait CmPropArg extends EnumArg[CmPropArg] {
  val param = CmProp
}

case class CmProp(override val args: CmPropArg*)
    extends EnumParameter[CmPropArg](
      "cmprop",
      "Which properties to get for each revision:"
    )
    with CmParam[CmPropArg]

package cmprop {

  object Ids extends EnumArgument[CmPropArg]("ids", "Adds the page ID.") with CmPropArg

  object Title
      extends EnumArgument[CmPropArg](
        "title",
        "Adds the title and namespace ID of the page."
      )
      with CmPropArg

  object SortKey
      extends EnumArgument[CmPropArg](
        "sortkey",
        "Adds the sortkey used for sorting in the category (hexadecimal string)."
      )
      with CmPropArg

  object SortKeyPrefix
      extends EnumArgument[CmPropArg](
        "sortkeyprefix",
        "Adds the sortkey prefix used for sorting in the category (human-readable part of the sortkey)."
      )
      with CmPropArg

  object Timestamp
      extends EnumArgument[CmPropArg](
        "timestamp",
        "Adds the timestamp of when the page was included."
      )
      with CmPropArg

}

trait CmSortArg extends EnumArg[CmSortArg] {
  val param = CmSort
}

case class CmSort(override val args: CmSortArg*)
    extends EnumParameter[CmSortArg]("cmsort", "Property to sort by.")
    with CmParam[CmSortArg]

package cmsort {

  object SortKey extends EnumArgument[CmSortArg]("sortkey") with CmSortArg

  object Timestamp extends EnumArgument[CmSortArg]("timestamp") with CmSortArg

}

trait CmDirArg extends EnumArg[CmDirArg] {
  val param = CmDir
}

case class CmDir(override val args: CmDirArg*)
    extends EnumParameter[CmDirArg]("cmdir", "In which direction to sort.")
    with CmParam[CmDirArg]

object Asc extends EnumArgument[CmDirArg]("asc") with CmDirArg

object Desc extends EnumArgument[CmDirArg]("desc") with CmDirArg

//cmstart
//Timestamp to start listing from. Can only be used with cmsort=timestamp.
//
//Type: timestamp (allowed formats)
//cmend
//Timestamp to end listing at. Can only be used with cmsort=timestamp.
//
//Type: timestamp (allowed formats)
