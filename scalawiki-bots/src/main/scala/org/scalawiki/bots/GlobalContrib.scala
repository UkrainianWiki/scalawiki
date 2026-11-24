package org.scalawiki.bots

import java.time.{ZoneOffset, ZonedDateTime}

import org.scalawiki.MwBot
import org.scalawiki.dto.{SulAccount, User}
import org.scalawiki.query.QueryLibrary
import org.scalawiki.time.TimeRange

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GlobalContrib extends QueryLibrary {

  val bot = MwBot.fromHost(MwBot.commons)

  def editsBefore(
      user: User,
      acc: SulAccount,
      start: ZonedDateTime
  ): Future[Long] = {
    val host = acc.url.replace("https://", "")
    val siteBot = MwBot.fromHost(host)

    val action = userContribs(
      user.name.get,
      TimeRange(None, Some(start)),
      limit = "500",
      dir = "newer"
    )
    siteBot.run(action).map { edits =>
      println(s"$host - ${edits.size}")
      edits.size.toLong
    }
  }

  def checkContribs(username: String, start: ZonedDateTime): Future[Long] = {
    bot.run(globalUserInfo(username)).flatMap { pages =>
      pagesToUsers(pages)
        .collect { case u: User => u }
        .headOption
        .fold(Future(0L)) { user =>
          val activeAccounts = user.sulAccounts.filter(_.editCount > 0)

          println(s"${user.name.get} - accounts ${activeAccounts.size}: ${activeAccounts
              .map(_.wiki)}")

          Future
            .traverse(activeAccounts)(acc => editsBefore(user, acc, start))
            .map(_.sum)
        }
    }
  }

}

object GlobalContrib {
  /** usage: GlobalContrib <user name> */
  def main(args: Array[String]) {
    require(args.length == 1, "usage: GlobalContrib <user name>")

    new GlobalContrib()
      .checkContribs(
        args(0),
        ZonedDateTime.of(2016, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC)
      )
      .map { size =>
        println(size)
      }
  }

  def countNewComers(authors: Set[String], date: ZonedDateTime): Unit = {
    val gc = new GlobalContrib()

    Future
      .traverse(authors) { author =>
        gc.checkContribs(author, date).map(author -> _)
      }
      .map { set =>
        val seq = set.toSeq
        val sorted = seq.sortBy(-_._2)
        sorted.foreach(println)

        val before = seq.filter(_._2 > 0)

        println(s"Before ${before.size}")

        val newComers = seq.filter(_._2 == 0)

        println(s"newComers ${newComers.size}")
      }
  }
}
