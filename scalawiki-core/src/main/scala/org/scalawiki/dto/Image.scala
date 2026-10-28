package org.scalawiki.dto

import java.nio.file.{Files, Paths}
import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import org.scalawiki.MwBot
import org.scalawiki.dto.markup.Gallery
import org.scalawiki.wikitext.TemplateParser
import org.sweble.wikitext.engine.nodes.EngPage

import scala.util.Try
import scala.util.matching.Regex

case class ImageMetadata(data: Map[String, String]) {

  def camera: Option[String] = data.get("Model")

  def date: Option[ZonedDateTime] =
    data
      .get("DateTimeOriginal")
      .flatMap { s =>
        val parsed = Try(LocalDateTime.parse(s, ImageMetadata.df).atZone(ZoneOffset.UTC))
//        parsed.failed.foreach(println)
        parsed.toOption
      }
}

object ImageMetadata {
  private val pattern = "yyyy:MM:dd HH:mm:ss"
  private val df = DateTimeFormatter.ofPattern(pattern)
}

case class Image(
    title: String,
    url: Option[String] = None,
    pageUrl: Option[String] = None,
    size: Option[Long] = None,
    width: Option[Int] = None,
    height: Option[Int] = None,
    author: Option[String] = None,
    uploader: Option[User] = None,
    year: Option[String] = None,
    date: Option[ZonedDateTime] = None,
    monumentIds: Seq[String] = Nil,
    pageId: Option[Long] = None,
    metadata: Option[ImageMetadata] = None,
    categories: Set[String] = Set.empty,
    specialNominations: Set[String] = Set.empty,
    mime: Option[String] = None,
    mediaType: Option[String] = None,
    revId: Option[Long] = None,
    revTs: Option[ZonedDateTime] = None
) extends Ordered[Image] {

  def compare(that: Image): Int = title.compareTo(that.title)

  def monumentId: Option[String] = monumentIds.headOption

  def download(filename: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    for (bytes <- MwBot.fromSite(Site.commons).getByteArray(url.get))
      Files.write(Paths.get(filename), bytes)
  }

  def pixels: Option[Long] = for (w <- width; h <- height) yield w * h

  def mpx: Option[Float] = pixels.map(_ / Math.pow(10, 6)).map(_.toFloat)

  def atLeastMpx(minMpxOpt: Option[Float]): Boolean =
    minMpxOpt.fold(true)(minMpx => mpx.exists(_ >= minMpx))

  def mpxStr: String = mpx.fold("")(v => f"$v%1.2f Mpx ")

  def resolution: Option[String] =
    for (w <- width; h <- height) yield w + " x " + h

  def resizeTo(resizeToX: Int, resizeToY: Int): Int =
    Image.resizedWidth(width.get, height.get, resizeToX, resizeToY)

  def withAuthor(newAuthor: String): Image = this.copy(author = Some(newAuthor))

  def withMonument(monumentId: String): Image =
    this.copy(monumentIds = Seq(monumentId))

  /** Whether the file is a video, see [[Image.isVideo(title* Image.isVideo]]. */
  def isVideo: Boolean = Image.isVideo(title, mediaType)
}

object Image {

  /** MediaWiki's `mediatype` for video files. */
  val VideoMediaType = "VIDEO"

  /** Video file extensions Commons accepts, for files with no media type. */
  val videoExtensions: Seq[String] = Seq(".webm", ".ogv", ".mpg", ".mpeg")

  /** Whether the file `title` is a video: by MediaWiki's `mediatype` when known (the MIME type
    * can't tell - Ogg video and audio are both `application/ogg`), else by the file extension.
    */
  def isVideo(title: String, mediaType: Option[String]): Boolean =
    mediaType
      .map(_ == VideoMediaType)
      .getOrElse(videoExtensions.exists(title.toLowerCase.endsWith))

  val categoryRegex = "\\[\\[Category:([^]]+)\\]\\]".r

  def fromPageImages(page: Page): Option[Image] =
    page.images.headOption

  def fromPageRevision(
      page: Page,
      monumentIdTemplate: Option[String],
      specialNominationTemplates: Set[String] = Set.empty
  ): Option[Image] = {
    page.revisions.headOption.map { revision =>
      val content = revision.content.getOrElse("")
      val parsedPage = TemplateParser.parsePage(content)
      val ids = monumentIdTemplate.toList.flatMap { template =>
        TemplateParser
          .collectTemplates(parsedPage, template)
          .flatMap(_.getParamOpt("1"))
      }
      val specialNominations =
        TemplateParser.collectTemplateNames(parsedPage, specialNominationTemplates)

      // none when the page names no author, or names them as `Я`: the caller looks up the uploader
      val author = authorValue(parsedPage).filterNot(selfAuthors.contains).map(parseUser)

      // TODO category maps
      val categories = categoryRegex
        .findAllIn(content)
        .matchData
        .map(_.group(1).intern())
        .toSet

      new Image(
        page.title,
        author = author,
        date = revision.timestamp,
        monumentIds = ids,
        pageId = page.id,
        categories = categories,
        specialNominations = specialNominations,
        revId = revision.revId,
        revTs = revision.timestamp
      )
    }
  }

  def fromPage(
      monumentIdTemplate: Option[String],
      specialNominationTemplates: Set[String] = Set.empty
  )(page: Page): Option[Image] =
    for {
      fromImage <- Image.fromPageImages(page)
      fromRev <- Image.fromPageRevision(
        page,
        monumentIdTemplate,
        specialNominationTemplates
      )
    } yield fromImage.copy(
      monumentIds = fromRev.monumentIds,
      // none: the caller looks up the original uploader, see authorFromUploader
      author = fromRev.author.map(renamed),
      categories = fromRev.categories,
      specialNominations = fromRev.specialNominations,
      revId = fromRev.revId,
      revTs = fromRev.revTs
    )

  private[scalawiki] def renamed(author: String): String =
    AuthorsMap.renames.getOrElse(author, author)

  /** The author of a file whose page names none, from its ORIGINAL uploader (the first version's):
    * a later version may be someone's retouch or a bot's resize. Not a bot: a bot uploads other
    * people's photos (e.g. a museum's), so it is never their author.
    */
  def authorFromUploader(originalUploader: String): Option[String] =
    Some(originalUploader)
      .filterNot(login => login.isBlank || login.toLowerCase.endsWith("bot"))
      .map(renamed)

  def getAuthorFromPage(content: String): String =
    getAuthorFromPage(TemplateParser.parsePage(content))

  /** File description templates and their author parameters, tried in order: Commons'
    * {{Information}}, then uk.wikipedia's {{Зображення}} (files copied there from Commons keep the
    * English parameter names).
    */
  val authorTemplates: Seq[(String, Seq[String])] = Seq(
    "Information" -> Seq("author", "Author"),
    "Зображення" -> Seq("Автор", "автор", "author", "Author")
  )

  def getAuthorFromPage(parsedPage: EngPage): String =
    authorValue(parsedPage).map(parseUser).getOrElse("")

  private def authorValue(parsedPage: EngPage): Option[String] =
    authorTemplates.iterator
      .flatMap { case (name, params) =>
        TemplateParser
          .getTemplate(parsedPage, Some(name))
          .flatMap(t => params.flatMap(t.getParamOpt).map(_.trim).find(_.nonEmpty))
      }
      .nextOption()

  /** An author that stands for the uploader: uk.wikipedia's `Автор = Я` ("me"). */
  private val selfAuthors = Set("Я", "я")

  /** A link to a user page in any user namespace and project (`[[:uk:Користувач:Name|…]]`,
    * `{{User:Name/author}}`), or a user template (`{{U|Name}}`,
    * `{{user at project|Name|wikipedia|uk}}`). The name ends where the link's target does, or at a
    * subpage.
    */
  private val userRef =
    ("(?iu)(?<!\\p{L})(?:user|користувач|користувачка|участник|участница|wikipedysta):([^|\\[\\]{}/#]+)" +
      "|\\{\\{\\s*(?:u|user|ut|user at project|original uploader)\\s*\\|\\s*([^|{}]+)").r

  /** `{{Unknown|author}}` and the like: nobody to credit, not even the uploader. */
  private val unknownAuthor =
    "(?iu)^\\{\\{\\s*(?:unknown|anonymous|author\\s*\\|\\s*unknown|creator:\\?)".r

  /** A photographer's credit template (`{{Yuriy Kvach}}`), the whole author value: its page links
    * to their user page.
    */
  private val creditTemplateRef = "^\\{\\{([^{}|]+)\\}\\}$".r

  private val langTemplate = "(?iu)\\{\\{\\s*lang\\s*\\|[^|{}]*\\|([^{}]*)\\}\\}".r
  private val pipedWikiLink = "\\[\\[[^\\[\\]|]*\\|([^\\[\\]]*)\\]\\]".r
  // the target, without the leading colon and interwiki prefixes of `[[:uk:Name]]`
  private val wikiLink = "\\[\\[:?(?:(?:w|[a-z]{2,3}):)*([^\\[\\]|]*)\\]\\]".r
  private val extLink = "\\[(?:https?:)?//\\S+\\s+([^\\]]*)\\]".r

  /** The first user named in `text`, see [[userRef]]. */
  def userIn(text: String): Option[String] =
    userRef
      .findFirstMatchIn(text)
      .map(m => Option(m.group(1)).getOrElse(m.group(2)).trim)
      .filter(_.nonEmpty)

  /** The template of [[creditTemplateRef]], if that is the whole `author`. */
  def creditTemplate(author: String): Option[String] =
    creditTemplateRef.findFirstMatchIn(author.trim).map(_.group(1).trim)

  /** The author named by an author parameter's wikitext: the user it links to, or else its text
    * without markup. Empty for an unknown author; a credit template is left as it is, see
    * [[creditTemplate]].
    */
  def parseUser(authorValue: String): String = {
    val value = authorValue.trim
    if (unknownAuthor.findFirstIn(value).isDefined) ""
    else
      userIn(value).getOrElse {
        Seq(langTemplate, pipedWikiLink, wikiLink, extLink)
          .foldLeft(value)((text, markup) =>
            markup.replaceAllIn(text, m => Regex.quoteReplacement(m.group(1)))
          )
          .trim
      }
  }

  def basic(
      title: String,
      timestamp: Option[ZonedDateTime],
      uploader: Option[String],
      size: Option[Long],
      width: Option[Int],
      height: Option[Int],
      url: Option[String],
      pageUrl: Option[String],
      pageId: Option[Long],
      metadata: Option[Map[String, String]] = None,
      mime: Option[String] = None,
      mediaType: Option[String] = None
  ) =
    new Image(
      title = title,
      date = timestamp,
      uploader = uploader.map(name => User(None, Some(name))),
      size = size,
      width = width,
      height = height,
      url = url,
      pageUrl = pageUrl,
      pageId = pageId,
      metadata = metadata.map(ImageMetadata.apply),
      mime = mime,
      mediaType = mediaType
    )

  def gallery(
      images: Iterable[String],
      descriptions: Iterable[String] = Seq.empty
  ): String =
    Gallery.asWiki(images, descriptions)

  def resizedWidth(w: Int, h: Int, resizeToX: Int, resizeToY: Int): Int = {
    val xRatio = w.toDouble / resizeToX
    val yRatio = h.toDouble / resizeToY

    val width = Math.min(resizeToX, w / yRatio)
    width.toInt
  }
}

object AuthorsMap {
  val renames = Map(
    "ЯдвигаВереск" -> "Wereskowa",
    "Михаил Титаренко Александрович" -> "Тітаренко Михайло"
  )
}
