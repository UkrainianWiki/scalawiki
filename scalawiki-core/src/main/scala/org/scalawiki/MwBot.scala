package org.scalawiki

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.event.LoggingAdapter
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.model.Uri.Query
import org.apache.pekko.http.scaladsl.model._
import org.jsoup.Jsoup
import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.{LoginResponse, MwException, Page, Site}
import org.scalawiki.http.HttpClient
import org.scalawiki.json.MwReads._
import org.scalawiki.query.{DslQuery, PageQuery, SinglePageQuery}
import play.api.libs.json._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

trait ActionBot {

  def run(
      action: Action,
      context: Map[String, String] = Map.empty,
      limit: Option[Long] = None
  ): Future[Iterable[Page]]

  def log: LoggingAdapter

}

trait MwBot extends ActionBot {

  def host: String

  def login(user: String, password: String): Future[String]

  def run(
      action: Action,
      context: Map[String, String] = Map.empty,
      limit: Option[Long] = None
  ): Future[Iterable[Page]]

  def get(params: Map[String, String]): Future[String]

  def post(params: Map[String, String]): Future[String]

  def getByteArray(url: String): Future[Array[Byte]]

  def post[T](reads: Reads[T], params: (String, String)*): Future[T]

  def post[T](reads: Reads[T], params: Map[String, String]): Future[T]

  def postMultiPart[T](reads: Reads[T], params: Map[String, String]): Future[T]

  def postFile[T](
      reads: Reads[T],
      params: Map[String, String],
      fileParam: String,
      filename: String
  ): Future[T]

  def page(title: String): SinglePageQuery

  def page(id: Long): SinglePageQuery

  def pageText(title: String): Future[String]

  def token: String

  /** Drop any cached CSRF/edit token so the next [[token]] read fetches a fresh one. Call this when
    * the wiki rejects an edit with `badtoken` (the token expired mid-run). No-op for bots that do
    * not cache a token.
    */
  def invalidateToken(): Unit = ()

  def await[T](future: Future[T]): T

  def system: ActorSystem

  def mediaWikiVersion: MediaWikiVersion
}

case class MediaWikiVersion(version: String) extends Ordered[MediaWikiVersion] {
  override def compare(that: MediaWikiVersion): Int =
    version.compareTo(that.version)
}

object MediaWikiVersion {

  val UNKNOWN: MediaWikiVersion = MediaWikiVersion("(UNKNOWN)")

  val MW_1_24: MediaWikiVersion = MediaWikiVersion("1.24")

  def fromGenerator(generator: String): MediaWikiVersion = {
    "MediaWiki (\\d+\\.\\d+)".r
      .findFirstMatchIn(generator)
      .map(_.group(1))
      .fold(UNKNOWN)(MediaWikiVersion.apply)
  }
}

class MwBotImpl(
    val site: Site,
    val http: HttpClient = HttpClient.get(MwBot.system),
    val system: ActorSystem = MwBot.system
) extends MwBot {

  def this(host: String) = this(Site.host(host))

  def this(host: String, http: HttpClient) = this(Site.host(host), http)

  implicit val sys: ActorSystem = system

  import system.dispatcher

  override def host: String = site.domain

  val baseUrl: String =
    site.protocol + "://" + host + site.portStr + site.scriptPath

  val indexUrl: String = baseUrl + "/index.php"

  val apiUrl: String = baseUrl + "/api.php"

  def encodeTitle(title: String): String = MwUtils.normalize(title)

  override def log: LoggingAdapter = system.log

  override def login(user: String, password: String): Future[String] = {
    require(user != null, "User is null")
    require(password != null, "Password is null")

    log.info(s"$host login user: $user")

    tryLogin(user, password).flatMap { loginResponse =>
      tryLogin(user, password, loginResponse.token).map { response =>
        if (response.result != "Success")
          log.warning(
            s"$host login of $user failed: ${response.result}" +
              response.reason.fold("")(r => s" ($r)")
          )
        response.result
      }
    }
  }

  def tryLogin(
      user: String,
      password: String,
      token: Option[String] = None
  ): Future[LoginResponse] = {
    val loginParams = Map(
      "action" -> "login",
      "lgname" -> user,
      "lgpassword" -> password,
      "format" -> "json",
      "utf8" -> ""
    ) ++ token.map("lgtoken" -> _)

    for (
      response <- http.post(apiUrl, loginParams);
      body <- http.getBody(response)
    ) yield {

      val contentType = response.entity.contentType
      contentType match {
        case HttpClient.JSON_UTF8 =>
          val jsResult = Json.parse(body).validate(loginResponseReads)
          if (jsResult.isError) {
            throw new RuntimeException("Parsing failed: " + jsResult)
          } else {
            jsResult.get
          }
        case _ =>
          val html = Jsoup.parse(body)
          val elements = html.select("code")
          val details = if (!elements.isEmpty) elements.first().text() else body
          throw MwException(response.status.toString(), details)
      }
    }
  }

  @volatile private var tokenCache: Option[String] = None

  override def token: String = synchronized {
    tokenCache.getOrElse {
      val fetched = await(getToken)
      tokenCache = Some(fetched)
      fetched
    }
  }

  override def invalidateToken(): Unit = synchronized { tokenCache = None }

  override lazy val mediaWikiVersion: MediaWikiVersion = await(
    getMediaWikiVersion
  )

  def getMediaWikiVersion: Future[MediaWikiVersion] =
    get(
      siteInfoReads,
      "action" -> "query",
      "meta" -> "siteinfo"
    ) map MediaWikiVersion.fromGenerator

  def getToken: Future[String] = {
    val isMW_1_24 = mediaWikiVersion >= MediaWikiVersion.MW_1_24

    if (isMW_1_24) {
      get(tokenReads, "action" -> "query", "meta" -> "tokens")
    } else {
      get(
        editTokenReads,
        "action" -> "query",
        "prop" -> "info",
        "intoken" -> "edit",
        "titles" -> "foo"
      )
    }
  }

  def getTokens: Future[String] = get(tokensReads, "action" -> "tokens")

  override def run(
      action: Action,
      context: Map[String, String] = Map.empty,
      limit: Option[Long] = None
  ): Future[Iterable[Page]] = {
    new DslQuery(action, this, context)
      .run(limit = limit)
      .map(_.allPages)
      .recover { case t: Throwable =>
        log.error(t, s"Error ${t.getMessage} running action" + action)
        throw t
      }
  }

  def get[T](reads: Reads[T], params: (String, String)*): Future[T] =
    http.get(getUri(params: _*)) map { body =>
      parseJson(reads, body).get
    }

  def parseJson[T](reads: Reads[T], body: String): JsResult[T] =
    Json.parse(body).validate(reads)

  def parseResponse[T](
      reads: Reads[T],
      response: Future[HttpResponse]
  ): Future[T] =
    response flatMap http.getBody map { body =>
      parseJson(reads, body).getOrElse {
        val exception =
          parseJson(errorReads, body).getOrElse(MwException("", body))
        // Expected errors (edit conflicts, an expired CSRF token, replication
        // lag / rate limiting) are recovered by the retry / re-read machinery
        // above — log them without a stack trace so a normal, self-correcting
        // event on a long run doesn't read as a failure. Throttling stays at
        // WARN so the operator sees it; conflicts / token refresh are routine.
        if (exception.transient && exception.code != "badtoken")
          log.warning(s"mediawiki ${exception.code}: ${exception.info} — backing off")
        else if (exception.expected)
          log.info(s"mediawiki ${exception.code}: ${exception.info}")
        else
          log.error(exception, "mediawiki error")
        throw exception
      }
    }

  override def getByteArray(url: String): Future[Array[Byte]] =
    http.getResponse(url) flatMap { response =>
      response.entity.toStrict(5 minutes).map(_.data.toArray)
    }

  override def post[T](reads: Reads[T], params: (String, String)*): Future[T] =
    post(reads, params.toMap)

  override def post[T](
      reads: Reads[T],
      params: Map[String, String]
  ): Future[T] =
    parseResponse(reads, http.post(apiUrl, params))

  override def postMultiPart[T](
      reads: Reads[T],
      params: Map[String, String]
  ): Future[T] =
    parseResponse(reads, http.postMultiPart(apiUrl, params))

  override def postFile[T](
      reads: Reads[T],
      params: Map[String, String],
      fileParam: String,
      filename: String
  ): Future[T] =
    parseResponse(reads, http.postFile(apiUrl, params, fileParam, filename))

  def pagesByTitle(titles: Set[String]): PageQuery =
    PageQuery.byTitles(titles, this)

  def pagesById(ids: Set[Long]): PageQuery = PageQuery.byIds(ids, this)

  override def page(title: String): SinglePageQuery =
    PageQuery.byTitle(title, this)

  override def page(id: Long): SinglePageQuery = PageQuery.byId(id, this)

  override def pageText(title: String): Future[String] = {
    val url = getIndexUri("title" -> encodeTitle(title), "action" -> "raw")
    http.get(url)
  }

  def getIndexUri(params: (String, String)*): Uri =
    Uri(indexUrl) withQuery Query(
      params ++ Seq("format" -> "json", "utf8" -> ""): _*
    )

  def getUri(params: (String, String)*): Uri =
    Uri(apiUrl) withQuery Query(
      params ++ Seq("format" -> "json", "utf8" -> ""): _*
    )

  override def get(params: Map[String, String]): Future[String] = {
    val uri: Uri = getUri(params)

    log.info(s"$host GET url: $uri")
    http.get(uri)
  }

  override def post(params: Map[String, String]): Future[String] = {
    val uri: Uri = Uri(apiUrl)
    log.info(s"$host POST equivalent to: ${getUri(params)}")
    http.postUri(uri, params ++ Map("format" -> "json")) flatMap http.getBody
  }

  def getUri(params: Map[String, String]): Uri =
    Uri(apiUrl) withQuery Query(params ++ Map("format" -> "json"))

  override def await[T](future: Future[T]): T =
    Await.result(future, http.timeout)
}

object MwBot {

  import org.apache.pekko.http.caching.scaladsl.Cache

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._

  val system: ActorSystem = ActorSystem()

  def create(
      site: Site,
      loginInfo: Option[LoginInfo],
      http: HttpClient = HttpClient.get(MwBot.system)
  ): MwBot = {
    val bot = new MwBotImpl(site, http)

    // A failed login must stop the caller: carrying on anonymously only fails
    // every edit later (assertbotfailed), and each new bot retries the login.
    loginInfo.foreach { info =>
      val result = bot.await(bot.login(info.login, info.password))
      if (result != "Success")
        throw MwException(
          s"login-$result",
          s"${site.domain}: login of ${info.login} failed: $result"
        )
    }
    bot
  }

  val cache: Cache[String, MwBot] = LfuCache(system)

  def fromHost(
      host: String,
      protocol: String = "https",
      port: Option[Int] = None,
      loginInfo: Option[LoginInfo] = LoginInfo.fromEnv(),
      http: HttpClient = HttpClient.get(MwBot.system)
  ): MwBot = {
    fromSite(Site.host(host, protocol, port), loginInfo, http)
  }

  def fromSite(
      site: Site,
      loginInfo: Option[LoginInfo] = LoginInfo.fromEnv(),
      http: HttpClient = HttpClient.get(MwBot.system)
  ): MwBot = {
    Await.result(
      cache(
        site.domain,
        { () =>
          Future {
            create(site, loginInfo, http)
          }
        }
      ),
      1.minute
    )
  }

  val commons: String = Site.commons.domain

  val ukWiki: String = Site.ukWiki.domain

}
