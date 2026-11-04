package org.scalawiki.mockserver

import org.apache.pekko.actor.ActorSystem
import org.mockserver.model.HttpRequest.request
import org.mockserver.model.HttpResponse.response
import org.mockserver.model.Parameter
import org.mockserver.model.ParameterBody.params
import org.mockserver.model.Header.header
import com.google.common.net.MediaType
import com.google.common.net.HttpHeaders.CONTENT_TYPE
import org.scalawiki.MwBot
import org.scalawiki.http.HttpClientPekko
import org.specs2.mutable.Specification

import scala.collection.JavaConverters._
import scala.concurrent.{Await, Future}

class BaseMockServerSpec extends Specification with StubServer {

  sequential

  val system: ActorSystem = ActorSystem()
  val http = new HttpClientPekko(system)
  val apiUrl = "/w/api.php"

  // never the environment's account: it would be sent to the mock server
  def getBot: MwBot = MwBot.fromHost(Host, Protocol, Some(Port), loginInfo = None)

  def login(wiki: MwBot, username: String, passwd: String): String =
    await(wiki.login(username, passwd))

  def await[T](future: Future[T]): T = Await.result(future, http.timeout)

  def stubResponse(path: String, code: Int, body: String) = {
    mockServer
      .when(
        request()
          .withMethod("POST")
          .withPath(path)
      )
      .respond(
        response()
          .withStatusCode(code)
          .withHeaders(
            header(CONTENT_TYPE, MediaType.JSON_UTF_8.toString)
          )
          .withBody(body)
      )
  }

  def stubOk(parameters: Map[String, String], body: String): Unit =
    stubResponse(parameters, 200, body)

  def stubResponse(
      parameters: Map[String, String],
      code: Int,
      body: String
  ): Unit = {
    mockServer
      .when(
        request()
          .withMethod("POST")
          .withPath(apiUrl)
          .withBody(
            params(
              parameters
                .map { case (name, value) => new Parameter(name, value) }
                .toBuffer
                .asJava
            )
          )
      )
      .respond(
        response()
          .withStatusCode(code)
          .withHeaders(
            header(CONTENT_TYPE, MediaType.JSON_UTF_8.toString)
          )
          .withBody(body)
      )
  }

}
