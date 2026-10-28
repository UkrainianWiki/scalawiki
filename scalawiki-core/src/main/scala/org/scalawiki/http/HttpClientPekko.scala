package org.scalawiki.http

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.client.RequestBuilding._
import org.apache.pekko.http.scaladsl.coding.{Deflate, Gzip, NoCoding}
import org.apache.pekko.http.scaladsl.model.Multipart.FormData.BodyPart
import org.apache.pekko.http.scaladsl.model.headers.{HttpEncodings, `Accept-Encoding`, `User-Agent`}
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.util.Timeout
import net.spraycookies.tldlist.DefaultEffectiveTldList
import net.spraycookies.{CookieHandling, CookieJar}
import org.scalawiki.MwBot

import java.nio.file.Paths
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, _}
import scala.language.postfixOps

class HttpClientPekko(val system: ActorSystem = MwBot.system) extends HttpClient {

  implicit val sys: ActorSystem = system

  // https://www.mediawiki.org/wiki/API:Etiquette requires a descriptive User-Agent
  // with contact info; Wikimedia wikis enforce this and throttle/block requests
  // that look anonymous. Override with SCALAWIKI_USER_AGENT to identify a specific
  // bot/contact, e.g. "MyBot/1.0 (https://example.org/MyBot; bot@example.org)".
  val userAgent = scala.sys.env.getOrElse(
    "SCALAWIKI_USER_AGENT",
    "ScalaWiki/0.7 (https://github.com/UkrainianWiki/scalawiki; ukwikidev@wikimedia.org.ua)"
  )

  import system.dispatcher

  def decodeResponse(response: HttpResponse): HttpResponse = {
    val decoder = response.encoding match {
      case HttpEncodings.gzip ⇒
        Gzip
      case HttpEncodings.deflate ⇒
        Deflate
      case HttpEncodings.identity ⇒
        NoCoding
    }

    decoder.decodeMessage(response)
  }

  def sendReceive: HttpRequest => Future[HttpResponse] = req => Http().singleRequest(req)

  override implicit val timeout: Duration = 15.minutes

  val cookieJar = new CookieJar(DefaultEffectiveTldList)
  val cookied = CookieHandling.withCookies(Some(cookieJar), Some(cookieJar)) _

  def submit: HttpRequest => Future[HttpResponse] = {
    implicit val timeout: Timeout = 5.minutes
    addHeaders(
      `Accept-Encoding`(HttpEncodings.gzip),
      `User-Agent`(userAgent)
    ) ~>
      cookied(
        sendReceive
          ~> decodeResponse
      )
  }

  override def get(url: String): Future[String] =
    submit(Get(url)) flatMap getBody

  override def get(url: Uri): Future[String] = submit(Get(url)) flatMap getBody

  override def getResponse(url: Uri): Future[HttpResponse] = submit(Get(url))

  override def getResponse(url: String): Future[HttpResponse] = submit(Get(url))

  override def post(
      url: String,
      params: Map[String, String]
  ): Future[HttpResponse] = {
    submit(Post(url, FormData(params)))
  }

  override def postUri(
      url: Uri,
      params: Map[String, String]
  ): Future[HttpResponse] = {
    submit(Post(url, FormData(params)))
  }

  override def postMultiPart(
      url: String,
      params: Map[String, String]
  ): Future[HttpResponse] = {
    postMultiPart(Uri(url), params)
  }

  override def postMultiPart(
      url: Uri,
      params: Map[String, String]
  ): Future[HttpResponse] = {
    val bodyParts = params.map { case (key, value) =>
      BodyPart(key, HttpEntity(value))
    }.toList
    submit(Post(url, Multipart.FormData(bodyParts: _*)))
  }

  override def postFile(
      url: String,
      params: Map[String, String],
      fileParam: String,
      filename: String
  ): Future[HttpResponse] = {

    val bodyParts = params.map { case (key, value) =>
      BodyPart(key, HttpEntity(value))
    } ++ Seq(
      BodyPart.fromPath(fileParam, MediaTypes.`image/jpeg`, Paths.get(filename))
    )
    submit(Post(Uri(url), Multipart.FormData(bodyParts.toList: _*)))
  }

  def getBody(response: HttpResponse): Future[String] =
    response.entity.toStrict(5 minutes).map(_.data.utf8String)
}
