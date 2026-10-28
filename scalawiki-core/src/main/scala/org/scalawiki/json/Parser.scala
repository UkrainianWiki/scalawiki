package org.scalawiki.json

import org.scalawiki.dto.cmd.query.Query
import org.scalawiki.dto.cmd.query.list.ListArg
import org.scalawiki.dto.cmd.query.meta.MetaArg
import org.scalawiki.dto.cmd.query.prop.PropArg
import org.scalawiki.dto.cmd.{Action, EnumArg}
import org.scalawiki.dto.{Image, _}
import play.api.libs.json._

import scala.util.Try

class Parser(val action: Action) {

  val params: Map[String, String] = action.pairs.toMap

  var continue = Map.empty[String, String]

  def parse(str: String): Try[Seq[Page]] = {
    Try {
      val json = Json.parse(str)
      val jsonObj = json.asInstanceOf[JsObject]

      if (jsonObj.value.contains("error")) {
        throw mwException(jsonObj)
      }

      val queryArg = lists.headOption.orElse[EnumArg[_]](meta.headOption)
      val queryChild = queryArg.fold("pages")(arg => arg.name)

      continue = getContinue(json)
      jsonObj.value match {
        case value if value.contains("query") =>
          parseQueryAction(json, queryChild)
        case _ => Seq.empty
      }
    }
  }

  private def parseQueryAction(json: JsValue, queryChild: String): Seq[Page] = {
    val pagesJson = (json \ "query" \ queryChild).get

    val jsons = (queryChild match {
      case "pages"                     => pagesJson.asInstanceOf[JsObject].values
      case "allusers" | "usercontribs" => pagesJson.asInstanceOf[JsArray].value
      case "globaluserinfo"            => Seq(pagesJson)
      case _                           => pagesJson.asInstanceOf[JsArray].value
    }).map(_.asInstanceOf[JsObject])

    jsons.flatMap { j =>
      queryChild match {
        case "pages"              => parsePage(j)
        case "allusers" | "users" => Some(parseUser(j, queryChild))
        case "usercontribs"       => Some(parseUserContrib(j))
        case "globaluserinfo"     => Some(parseGlobalUserInfo(j))
        case _                    => parsePage(j)
      }
    }.toSeq
  }

  private def mwException(jsonObj: JsObject): MwException = {
    jsonObj.validate(MwReads.errorReads).get
  }

  def parsePage(pageJson: JsObject): Option[Page] = {
    // TODO handle errors
    pageJson.validate(Parser.pageReads).fold(_ => Option.empty[Page], page => Some(page)).map {
      page =>
        val revisions = page.id.fold(Seq.empty[Revision]) { pageId =>
          pageJson.validate(Parser.revisionsReads(pageId)).getOrElse(Seq.empty)
        }

        val images = getImages(pageJson, page)
        val langLinks = getLangLinks(pageJson)
        val links = getLinks(pageJson)
        val categoryInfo = getCategoryInfo(pageJson)
        val categories = getCategories(pageJson)

        page.copy(
          revisions = revisions,
          images = images,
          langLinks = langLinks,
          links = links,
          categoryInfo = categoryInfo,
          categories = categories
        )
    }
  }

  private def getImages(pageJson: JsObject, page: Page): Seq[Image] = {
    pageJson
      .validate {
        if (pageJson.value.contains("imageinfo")) {
          Parser.imageInfoReads(page.id, Some(page.title))
        } else {
          //      if (pageJson.value.contains("images")) {
          Parser.imageReads()
        }
      }
      .getOrElse(Seq.empty)
  }

  // hacky wrapping into page // TODO refactor return types
  def parseUser(userJson: JsObject, queryChild: String): Page = {
    val hasEmptyRegistration = userJson.value
      .get("registration")
      .collect({ case jsStr: JsString => jsStr.value.isEmpty })
      .getOrElse(false)
    val mappedJson =
      if (hasEmptyRegistration) userJson - "registration" else userJson

    // TODO move out of loop or get from request?
    val prefix = queryChild match {
      case "allusers" => "au"
      case "users"    => "us"
    }
    val props = params
      .get(prefix + "prop")
      .map(_.split("\\|"))
      .getOrElse(Array.empty[String])
      .toSet

    val blocked =
      if (props.contains("blockinfo")) Some(userJson.keys.contains("blockid"))
      else None
    val emailable =
      if (props.contains("emailable")) Some(userJson.keys.contains("emailable"))
      else None
    val jsResult = mappedJson.validate(Parser.userReads)
    val user = jsResult.get.copy(blocked = blocked, emailable = emailable)
    new Page(
      id = None,
      title = user.name.get,
      ns = Some(Namespace.USER),
      revisions = Seq(Revision(user = Some(user)))
    )
  }

  private def parseUserContrib(userJson: JsObject): Page = {
    val userContribs = userJson.validate(Parser.userContribReads).get
    userContribs.toPage
  }

  private def getContinue(json: JsValue): Map[String, String] = {
    (json \ "continue")
      .asOpt[JsObject]
      .map(
        _.value
          .mapValues[String] {
            case JsNumber(n) => n.toString()
            case JsString(s) => s
          }
          .toMap
      )
      .getOrElse(Map.empty[String, String])
  }

  private def getLangLinks(pageJson: JsObject): Map[String, String] = {
    (pageJson \ "langlinks")
      .asOpt[Seq[Map[String, String]]]
      .map {
        _.map(l => l("lang") -> l("*")).toMap
      }
      .getOrElse(Map.empty[String, String])
  }

  private def getLinks(pageJson: JsObject): Seq[Page] = {
    (pageJson \ "links")
      .asOpt[JsArray]
      .map {
        _.value
          .map { l =>
            new Page(
              id = None,
              ns = (l \ "ns").asOpt[Int],
              title = (l \ "title").as[String]
            )
          }
          .toSeq
      }
      .getOrElse(Nil)
  }

  /** The titles of the page's categories (prop=categories). */
  private def getCategories(pageJson: JsObject): Seq[String] =
    (pageJson \ "categories")
      .asOpt[Seq[JsObject]]
      .map(_.flatMap(c => (c \ "title").asOpt[String]))
      .getOrElse(Nil)

  private def getCategoryInfo(pageJson: JsObject): Option[CategoryInfo] =
    pageJson.validate(Parser.categoryInfoReads()).getOrElse(None)

  private def parseGlobalUserInfo(json: JsObject) = {
    if (!json.value.contains("missing")) {

      val gui = json.validate(Parser.globalUserInfoReads).get

      val user = new User(
        id = Some(gui.id),
        login = Some(gui.name),
        editCount = Some(gui.editCount),
        registration = Some(gui.registration),
        sulAccounts = gui.merged
      )

      new Page(
        id = None,
        title = gui.name,
        ns = Some(Namespace.USER),
        revisions = Seq(Revision(user = Some(user)))
      )
    } else {
      new Page(
        id = None,
        title = "missing",
        ns = Some(Namespace.USER),
        revisions = Seq.empty
      )
    }
  }

  def query: Seq[Query] = action.query.toSeq

  def lists: Seq[ListArg] = query.flatMap(_.lists)

  def props: Seq[PropArg] = query.flatMap(_.props)

  def meta: Seq[MetaArg] = query.flatMap(_.metas)

  //  def generator: Option[Generator] = query.flatMap(_.byType(manifest[Generator])).headOption

}

object Parser {

  private val pageReads: Reads[Page] = PageReads()

  private val userReads: Reads[User] = UserReads()

  private def revisionsReads(pageId: Long): Reads[Seq[Revision]] = {
    implicit val revisionReads: Reads[Revision] = RevisionRead(Some(pageId))
    (__ \ "revisions").read[Seq[Revision]]
  }

  private def imageInfoReads(
      pageId: Option[Long],
      title: Option[String]
  ): Reads[Seq[Image]] = {
    implicit val imageReads: Reads[Image] =
      ImageReads(title = title, pageId = pageId)
    (__ \ "imageinfo").read[Seq[Image]]
  }

  private def imageReads(): Reads[Seq[Image]] = {
    implicit val imageReads: Reads[Image] = ImageReads(None, None)
    (__ \ "images").read[Seq[Image]]
  }

  private def categoryInfoReads(): Reads[Option[CategoryInfo]] = {
    implicit val categoryInfoReads: Reads[CategoryInfo] = CategoryInfoReads()
    (__ \ "categoryinfo").readNullable[CategoryInfo]
  }

  private def globalUserInfoReads: Reads[GlobalUserInfo] = GlobalUserInfoReads()

  private def userContribReads: Reads[UserContrib] = UserContributorReads()
}
