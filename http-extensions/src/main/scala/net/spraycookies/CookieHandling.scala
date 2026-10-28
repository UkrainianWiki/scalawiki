package net.spraycookies

import org.apache.pekko.http.scaladsl.client.RequestBuilding
import org.apache.pekko.http.scaladsl.model.headers.{Cookie, `Set-Cookie`}
import org.apache.pekko.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}

import scala.concurrent.{ExecutionContext, Future}

object CookieHandling {

  def withCookies(
      cookieSource: Option[CookieJar],
      cookieTarget: Option[CookieJar]
  )(
      innerPipeline: HttpRequest ⇒ Future[HttpResponse]
  )(implicit context: ExecutionContext) = { req: HttpRequest ⇒
    {
      val cookiedReq =
        cookieSource.foldLeft(req)((_, jar) ⇒ addCookies(jar)(req))
      val fResp = innerPipeline(cookiedReq)
      fResp.map(res ⇒ {
        cookieTarget.foldLeft(res)((_, jar) ⇒ storeCookies(jar, req.uri)(res))
      })
    }
  }

  def addCookies(cookieJar: CookieJar): HttpRequest ⇒ HttpRequest = { req: HttpRequest ⇒
    {
      val cookies = cookieJar.cookiesFor(req.uri)
      if (cookies.isEmpty) req
      else {
        val cookieHeader = Cookie(cookies.map(_.pair()).toList)
        RequestBuilding.addHeader(cookieHeader)(req)
      }
    }
  }

  def storeCookies(
      cookieJar: CookieJar,
      uri: ⇒ Uri
  ): HttpResponse ⇒ HttpResponse = { res: HttpResponse ⇒
    {
      val cookieHeaders = res.headers collect { case c: `Set-Cookie` ⇒ c }
      for (c ← cookieHeaders.map(ch ⇒ ch.cookie)) {
        cookieJar.setCookie(c, uri)
      }
      res
    }
  }
}
