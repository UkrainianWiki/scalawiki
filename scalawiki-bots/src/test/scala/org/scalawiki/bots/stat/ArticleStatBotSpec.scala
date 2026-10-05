package org.scalawiki.bots.stat

import java.time.ZonedDateTime

import org.scalawiki.util.{HttpStub, MockBotSpec}
import org.specs2.mutable.Specification
import spray.util.pimpFuture

class ArticleStatBotSpec extends Specification with MockBotSpec {

  "stat " should {
    "no stat" in {
      val event = new ArticlesEvent(
        "Martians",
        ZonedDateTime.now,
        ZonedDateTime.now,
        "Martians-week-new",
        "Martians-week-improved"
      )

      val commands = getCommands(emptyResponse, emptyResponse)

      val asb = new ArticleStatBot()(getBot(commands: _*))

      val stat = asb.stat(event).await()

      val all = stat.allStat.userStat
      checkStat(all, Set.empty, Map.empty, Seq.empty)

      val improved = stat.improvedStat.userStat
      checkStat(improved, Set.empty, Map.empty, Seq.empty)

      val created = stat.improvedStat.userStat
      checkStat(created, Set.empty, Map.empty, Seq.empty)

    }

    "created 1 article" in {
      val event = new ArticlesEvent(
        "Martians",
        ZonedDateTime.parse("2015-06-07T16:45:30Z"),
        ZonedDateTime.now,
        "Martians-week-new",
        "Martians-week-improved"
      )

      val createdResponse =
        """{ "query": { "pages": {
          "569559": { "pageid": 456, "subjectid": 123, "ns": 1, "title": "Talk:Welfare reform"}
          }
          }}"""

      val listCommands = getCommands(createdResponse, emptyResponse)

      val revsRequest = Map(
        "format" -> "json",
        "pageids" -> "123",
        "rvprop" -> "content|ids|size|user|userid|timestamp",
        "prop" -> "info|revisions",
        "rvlimit" -> "max",
        "action" -> "query",
        "continue" -> ""
      )

      val pageText1 = "some text"

      val revResponse =
        s"""{"query": {"pages": {
            |"569559": {"pageid": 123, "ns": 1, "title": "Welfare reform",
            |"revisions": [
            |{"revid": 1, "user": "u1", "comment": "c1", "*": "$pageText1", "parentid": 0, "timestamp": "2016-06-07T16:45:30Z"}] }
            |}}}""".stripMargin

      val commands = listCommands :+ HttpStub(revsRequest, revResponse)

      val asb = new ArticleStatBot()(getBot(commands: _*))

      val stat = asb.stat(event).await()

      val added = pageText1.split("\\s").mkString.length

      val all = stat.allStat.userStat
      all.users === Set("u1")
      all.byUserAddedOrRemoved === Seq(("u1", added))

      val improved = stat.improvedStat.userStat
      checkStat(improved, Set.empty, Map.empty, Seq.empty)

      val created = stat.createdStat.userStat
      created.users === Set("u1")
      created.byUserAddedOrRemoved === Seq(("u1", added))
    }
  }

  def checkStat(
      stat: UserStat,
      users: Set[String],
      byUser: Map[String, Seq[RevisionStat]],
      byUserAddedOrRemoved: Seq[(String, Long)]
  ) = {
    stat.users === users
    stat.byUser === byUser
    stat.byUserAddedOrRemoved === byUserAddedOrRemoved
  }

  val emptyResponse =
    """{ "query": {
      |        "pages": {
      |         }
      |    }
      |}""".stripMargin

  def getCommands(
      newResponse: String,
      improvedResponse: String
  ): Seq[HttpStub] = {
    val newParams = Map(
      "format" -> "json",
      "generator" -> "embeddedin",
      "inprop" -> "subjectid",
      "geilimit" -> "max",
      "geititle" -> "Template:Martians-week-new",
      "prop" -> "info|revisions",
      "action" -> "query",
      "continue" -> ""
    )

    val improvedParams = Map(
      "format" -> "json",
      "generator" -> "embeddedin",
      "inprop" -> "subjectid",
      "geilimit" -> "max",
      "geititle" -> "Template:Martians-week-improved",
      "prop" -> "info|revisions",
      "action" -> "query",
      "continue" -> ""
    )

    Seq(
      HttpStub(newParams, newResponse),
      HttpStub(improvedParams, improvedResponse)
    )
  }
}
