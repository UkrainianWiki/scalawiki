package org.scalawiki.cache

import org.scalawiki.dto.Site
import org.scalawiki.http.HttpClient
import org.scalawiki.{MwBot, MwBotImpl}
import org.specs2.mutable.Specification

class CachedBotSpec extends Specification {

  "CachedBot" should {
    "share the session (HTTP client, so login cookies) of the shared bot for its site" in {
      val site = Site.host("cached-bot-spec.example.org")
      // created first without credentials, so the spec never tries to log in
      val shared = MwBot.fromSite(site, loginInfo = None).asInstanceOf[MwBotImpl]

      new CachedBot(site, "cached-bot-spec", persistent = false).http must beTheSameAs(shared.http)
    }

    "use an explicitly given HTTP client" in {
      val http = HttpClient.get(MwBot.system)
      new CachedBot(Site.host("cached-bot-spec-2.example.org"), "cached-bot-spec-2", persistent = false, http).http must
        beTheSameAs(http)
    }
  }
}
