package org.scalawiki

import org.apache.pekko.actor.ActorSystem
import org.scalawiki.http.HttpClientPekko
import org.specs2.mutable.Specification

import scala.concurrent.{Await, Future}

class BaseIntegrationSpec extends Specification {

  sequential

  val ukWiki = "uk.wikipedia.org"
  val commons = "commons.wikimedia.org"

  val system: ActorSystem = ActorSystem()
  val http = new HttpClientPekko(system)

  def getUkWikiBot: MwBot = MwBot.fromHost(ukWiki)

  def getCommonsBot: MwBot = MwBot.fromHost(commons)

  def login(wiki: MwBot, username: String, passwd: String): String =
    await(wiki.login(username, passwd))

  def login(wiki: MwBot): String = {
    LoginInfo
      .fromEnv()
      .map { loginInfo =>
        await(wiki.login(loginInfo.login, loginInfo.password))
      }
      .getOrElse("No LoginInfo")
  }

  /** A subpage of the logged-in account's user page (the account part of a
    * `Account@botpassword` login), for test edits. */
  def userSubpage(subpage: String): String = {
    val account = LoginInfo.fromEnv().map(_.login.takeWhile(_ != '@')).getOrElse("Example")
    s"User:$account/$subpage"
  }

  def await[T](future: Future[T]): T = Await.result(future, http.timeout)

}
