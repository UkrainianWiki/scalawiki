package net.spraycookies

import org.apache.pekko.http.scaladsl.model.headers.HttpCookie
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object CookieJarSpecification extends Properties("CookieHandling") {

  import org.apache.pekko.http.scaladsl.model.headers.{Cookie, `Set-Cookie`}
  import org.apache.pekko.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
  import net.spraycookies.tldlist.EffectiveTldList
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global

  def emptyRequest = HttpRequest()

  def emptyTldlist = new EffectiveTldList {
    def contains(domain: String) = false
  }

  val genToken = {
    val separators = List(
      '(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '{', '}', ' ',
      '\t'
    )
    val allowedChars = Range(32, 126).map(_.toChar).toSet -- separators
    val genTokenChar = Gen.oneOf(allowedChars.toSeq)
    Gen.containerOf[List, Char](genTokenChar).suchThat(_ != Nil).map(_.mkString)
  }

  val genCookieValue = {
    val genCookieValueChar = Gen
      .oneOf(
        Gen.choose(0x21, 0x21),
        Gen.choose(0x23, 0x2b),
        Gen.choose(0x2d, 0x3a),
        Gen.choose(0x3c, 0x5b),
        Gen.choose(0x5d, 0x7e)
      )
      .map(i ⇒ i.toChar)
    Gen
      .containerOf[List, Char](genCookieValueChar)
      .suchThat(_ != Nil)
      .map(_.mkString)
  }

  val genBareCookie = for {
    name ← genToken
    value ← genCookieValue
  } yield HttpCookie(name, value)

  val genBareCookieList = Gen.containerOf[List, HttpCookie](genBareCookie)

  property("withCookies") = forAll(genBareCookieList) { cookies ⇒
    {
      val jar = new CookieJar(emptyTldlist)

      val addingPipeline = (req: HttpRequest) ⇒ {
        val resp = HttpResponse()
        Future {
          val setCookieHeaders = cookies.map(`Set-Cookie`(_))
          resp.withHeaders(setCookieHeaders)
        }
      }

      val testingPipeline = (req: HttpRequest) ⇒ {
        Future {
          val httpCookies = req.headers
            .collect({ case Cookie(httpCookies) ⇒ httpCookies })
            .flatten
          if (httpCookies.length > cookies.length)
            throw new Exception("received more cookies than expected")
          else if (
            !cookies
              .forall(expected ⇒ httpCookies.exists(received ⇒ received.name == expected.name))
          )
            throw new Exception("reponse didn't contain cookies for all names")
          else if (
            !httpCookies
              .forall(received ⇒ cookies.exists(testcookie ⇒ testcookie.name == received.name))
          )
            throw new Exception(
              "reponse contained a cookie with a name that is not expected"
            )
          else HttpResponse()
        }
      }

      val cookiedPipeline =
        CookieHandling.withCookies(None, Some(jar))(addingPipeline)
      Await.result(cookiedPipeline(emptyRequest), 10.seconds)
      val cookiedTestPipeline =
        CookieHandling.withCookies(Some(jar), None)(testingPipeline)
      val fTestResult = cookiedTestPipeline(HttpRequest())
      val testResult =
        try {
          Await.result(fTestResult, 10.seconds)
          true
        } catch {
          case t: Exception ⇒
            println(s"test failed $t")
            false
        }

      testResult
    }
  }
}
