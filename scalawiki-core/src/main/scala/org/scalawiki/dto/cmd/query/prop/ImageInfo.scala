package org.scalawiki.dto.cmd.query.prop

import java.time.ZonedDateTime

import org.scalawiki.dto.cmd._
import org.scalawiki.dto.cmd.query.Module

/** ?action=query&amp;prop=imageinfo See more at https://www.mediawiki.org/wiki/API:Imageinfo
  */
case class ImageInfo(override val params: IiParam*)
    extends Module[PropArg]("ii", "imageinfo", "Gets image information.")
    with PropArg
    with ArgWithParams[IiParam, PropArg]

/** Marker trait for parameters used with prop=imageinfo
  */
trait IiParam extends Parameter[Any]

package iiprop {

  /** ?action=query&amp;prop=imageinfo&amp;iiprop=
    */
  case class IiProp(override val args: IiPropArg*)
      extends EnumParameter[IiPropArg]("iiprop", "Which properties to get.")
      with IiParam

  /** Trait for iiprop= arguments
    */

  trait IiPropArg extends EnumArg[IiPropArg] {
    val param = IiProp
  }

  /** iiprop= arguments
    */

  object Timestamp
      extends EnumArgument[IiPropArg](
        "timestamp",
        "The time and date of the revision (default)"
      )
      with IiPropArg

  object User
      extends EnumArgument[IiPropArg](
        "user",
        "The user who made the revision (default)"
      )
      with IiPropArg

  object UserId
      extends EnumArgument[IiPropArg](
        "userid",
        "User ID of the revision creator."
      )
      with IiPropArg

  object Comment
      extends EnumArgument[IiPropArg](
        "comment",
        "Comment by the user for the revision."
      )
      with IiPropArg

  object ParsedComment
      extends EnumArgument[IiPropArg](
        "parsedcomment",
        "Parsed comment by the user for the revision."
      )
      with IiPropArg

  object CanonicalTitle
      extends EnumArgument[IiPropArg](
        "canonicaltitle",
        "The canonical title of the image file."
      )
      with IiPropArg

  object Url
      extends EnumArgument[IiPropArg](
        "url",
        "URLs of the image and its description page."
      )
      with IiPropArg

  object Size
      extends EnumArgument[IiPropArg](
        "size",
        "The image's size in bytes, plus width and height. " +
          "A page count is also returned if the image is in a format that supports multiple pages."
      )
      with IiPropArg

  object Dimensions
      extends EnumArgument[IiPropArg](
        "dimensions",
        "dimensions: (Alias for size)"
      )
      with IiPropArg

  object Sha1 extends EnumArgument[IiPropArg]("sha1", "The image's SHA-1 hash.") with IiPropArg

  object Mime extends EnumArgument[IiPropArg]("mime", "The image's MIME type.") with IiPropArg

  object ThumbMime
      extends EnumArgument[IiPropArg](
        "thumbmime",
        "The image thumbnail's MIME type."
      )
      with IiPropArg

  object MediaType
      extends EnumArgument[IiPropArg](
        "mediatype",
        "The media type of the image."
      )
      with IiPropArg

  object Metadata
      extends EnumArgument[IiPropArg](
        "metadata",
        "Exif metadata for the image, if available."
      )
      with IiPropArg

  object CommonMetadata
      extends EnumArgument[IiPropArg](
        "commonmetadata",
        "Generic metadata for the file format, if available."
      )
      with IiPropArg

  object ExtMetadata
      extends EnumArgument[IiPropArg](
        "extmetadata",
        "HTML metadata from extensions which implement the GetExtendedMetadata hook. " +
          "Also contains most of metadata, but in a somewhat standardized format."
      )
      with IiPropArg

  object ArchiveName
      extends EnumArgument[IiPropArg](
        "archivename",
        "Archive name (old images only)."
      )
      with IiPropArg

  object BitDepth
      extends EnumArgument[IiPropArg]("bitdepth", "The bit depth of the image.")
      with IiPropArg

  object IiPropArgs {
    val args = Seq(
      Timestamp,
      User,
      UserId,
      Comment,
      ParsedComment,
      CanonicalTitle,
      Url,
      Size,
      Dimensions,
      Sha1,
      Mime,
      ThumbMime,
      MediaType,
      Metadata,
      CommonMetadata,
      ExtMetadata,
      ArchiveName,
      BitDepth
    )
    val argsByName: Map[String, IiPropArg] =
      args.groupBy(_.name).mapValues(_.head).toMap

    def byNames(names: Seq[String]): Seq[IiPropArg] = {
      names.flatMap(argsByName.get)
    }
  }

}

case class IiLimit(override val arg: String)
    extends StringParameter(
      "iilimit",
      "How many image revisions to return (1 by default)"
    )
    with IiParam

case class IiStart(override val arg: ZonedDateTime)
    extends DateTimeParameter("iistart", "Timestamp to start listing from")
    with IiParam

case class IiEnd(override val arg: ZonedDateTime)
    extends DateTimeParameter("iistart", "Timestamp to stop listing at")
    with IiParam

case class IiUrlWidth(override val arg: Int)
    extends IntParameter(
      "iiurlwidth",
      "If iiprop=url is set, a URL to an image scaled to this width will be returned as well in thumburl along with thumbwidth and thumbheight. " +
        "Old versions of images can't be scaled"
    )
    with IiParam

case class IiUrlHeight(override val arg: Int)
    extends IntParameter("iiurlheight", "Similar to iiurlwidth")
    with IiParam

case class IiMetadataVersion(override val arg: String)
    extends StringParameter(
      "iimetadataversion",
      "What version of metadata to use. Only affects JPEGs (as of this writing). You usually want this set to latest."
    )
    with IiParam

case class IiExtMetadataLanguage(override val arg: String)
    extends StringParameter(
      "iiextmetadatalanguage",
      "What language to fetch extmetadata in. This affects both which translation to fetch, if multiple are available, " +
        "as well as how things like numbers and various values are formatted."
    )
    with IiParam

case class IiExtMetadataMultilang(override val arg: Boolean = true)
    extends BooleanParameter(
      "iiextmetadatamultilang",
      "If translations for extmetadata property are available, fetch all of them. " +
        "The values of the multi-languaged metadata items will be in the multi-language array format. " +
        "(Which items are multilanguaged might change from image to image.)"
    )
    with IiParam

case class IiExtMetadataFilter(override val arg: Boolean = true)
    extends BooleanParameter(
      "iiextmetadatafilter",
      "If specified and non-empty, only these keys will be returned for prop=extmetadata"
    )
    with IiParam

case class IiUrlParam(override val arg: String)
    extends StringParameter(
      "iiurlparam",
      "The thumb parameter string. Allows user to optionally specify other parameters than width and height (like page number for pdfs). " +
        "Format of the field varies with image format. PDF uses a format like page<number>-<width>px (e.g. page3-140px ). " +
        "Its generally the part before the filename in the url of a thumbnail."
    )
    with IiParam

case class IiLocalOnly(override val arg: Boolean = true)
    extends BooleanParameter("iilocalonly", "Only show local images.")
    with IiParam
