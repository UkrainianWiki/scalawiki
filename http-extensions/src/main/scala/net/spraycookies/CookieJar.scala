package net.spraycookies

import org.apache.pekko.http.scaladsl.model.{DateTime, Uri}
import org.apache.pekko.http.scaladsl.model.headers.HttpCookie
import net.spraycookies.tldlist.EffectiveTldList

import scala.annotation.tailrec
import scala.language.implicitConversions

class CookieJar(blacklist: EffectiveTldList) {

  private case class StoredCookie(
      name: String,
      content: String,
      expires: Option[DateTime],
      domain: String,
      path: String,
      httpOnly: Boolean,
      secure: Boolean
  )

  private object StoredCookie {
    implicit def toHttpCookie(src: StoredCookie): HttpCookie = {
      HttpCookie(
        src.name,
        src.content,
        src.expires,
        None,
        Some(src.domain),
        Some(src.path),
        src.secure,
        src.httpOnly,
        None
      )
    }

    implicit def toStoredCookie(
        src: HttpCookie
    )(implicit uri: Uri): StoredCookie = {
      val domain = src.domain.getOrElse(uri.authority.host.address)
      val path = src.path.getOrElse(uri.path.toString)
      val expiration = src.expires match {
        case x: Some[DateTime] ⇒ x
        case None ⇒ src.maxAge.map(age ⇒ DateTime.now + age)
      }
      StoredCookie(
        src.name,
        src.value,
        expiration,
        domain,
        path,
        src.httpOnly,
        src.secure
      )
    }
  }

  private var jar: CookieJar_ = CookieJar_("", Map.empty, Map.empty)

  def cookiesFor(uri: Uri) =
    jar.cookiesFor(uri).map(c ⇒ StoredCookie.toHttpCookie(c))

  def setCookie(cookie: HttpCookie, source: Uri) = {
    val storedCookie = StoredCookie.toStoredCookie(cookie)(source)
    if (isAllowedFor(storedCookie, source)) {
      jar = jar.setCookie(storedCookie)
      true
    } else false
  }

  private def isAllowedFor(cookie: StoredCookie, source: Uri): Boolean = {
    !blacklist.contains(cookie.domain) &&
    isDomainPostfix(cookie.domain, source.authority.host.address)
    // status of cookie paths is not clear to me, not implemented
  }

  private def isDomainPostfix(needle: String, haystack: String) = {
    val needleElements = needle.split('.').toList.reverse
    val haystackElements = haystack.split('.').toList.reverse
    haystackElements.startsWith(needleElements)
  }

  private case class CookieJar_(
      domainElement: String,
      subdomains: Map[String, CookieJar_],
      cookies: Map[String, StoredCookie]
  ) {
    def cookiesFor(uri: Uri) = {
      val domain = uri.authority.host.address
      val domainElements = domain.split('.').toList.reverse
      _getCookies(domainElements, uri, Map.empty).values
    }

    @tailrec
    private def _getCookies(
        domain: List[String],
        uri: Uri,
        accum: Map[String, StoredCookie]
    ): Map[String, StoredCookie] = {
      val now = DateTime.now
      val newCookies = removeStale(cookies, now)
        .filter(c ⇒ uri.scheme == "https" || !c._2.secure)
        .filter(c ⇒ uri.path.startsWith(Uri.Path(c._2.path)))
      val totalCookies = accum ++ newCookies
      domain match {
        case Nil ⇒ totalCookies
        case head :: tail ⇒
          subdomains.get(head) match {
            case None ⇒ totalCookies
            case Some(jar) ⇒ jar._getCookies(tail, uri, totalCookies)
          }
      }
    }

    def setCookie(cookie: StoredCookie) = {
      val trimmed =
        if (cookie.domain.indexOf('.') == 0) cookie.domain.substring(1)
        else cookie.domain
      val domainElements = trimmed.split('.').toList.reverse
      _setCookie(domainElements, cookie)
    }

    private def _setCookie(
        domain: List[String],
        cookie: StoredCookie
    ): CookieJar_ = {
      val now = DateTime.now
      domain match {
        case Nil ⇒
          val newCookies = removeStale(cookies, now) + (cookie.name -> cookie)
          this.copy(cookies = newCookies)
        case head :: tail ⇒
          lazy val newSubJar = CookieJar_(head, Map.empty, Map.empty)
          val subJar = subdomains.getOrElse(head, newSubJar)
          this.copy(subdomains = subdomains + (head -> subJar._setCookie(tail, cookie)))
      }
    }

    def removeStale(cookies: Map[String, StoredCookie], cutoff: DateTime) =
      cookies.filter(c ⇒ c._2.expires.forall(_ > cutoff))
  }

}
