package org.scalawiki.wlx.stat

import org.apache.pekko.event.NoLogging
import org.scalawiki.ActionBot
import org.scalawiki.dto.{Image, Namespace, Page, Revision, User}
import org.scalawiki.dto.cmd.Action
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.stat.reports.AuthorsStat
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._

class AuthorsContributedSpec extends Specification {

  val contest = Contest.WLMUkraine(2015)

  def monument(id: String, name: String) =
    new Monument(id = id, name = name, listConfig = Some(WlmUa))

  def monuments(
      n: Int,
      regionId: String,
      namePrefix: String,
      startId: Int = 1
  ): Seq[Monument] =
    (startId until startId + n).map(i => monument(s"$regionId-xxx-000$i", namePrefix + i))

  /** Answers list=users, where a missing user and an invalid name come back without an id, and talk
    * page texts: the old talk page of a renamed account redirects to the new one.
    */
  class UsersBot(
      renamed: Map[String, String] = Map.empty,
      redirect: String = "#REDIRECT [[User talk:%s]]"
  ) extends ActionBot {
    private val queue = new ConcurrentLinkedQueue[Seq[String]]()
    def requested: Seq[Seq[String]] = queue.asScala.toSeq
    override def log = NoLogging
    override def run(
        action: Action,
        context: Map[String, String],
        limit: Option[Long]
    ): Future[Iterable[Page]] = {
      val params = action.pairs.toMap
      Future.successful(params.get("ususers") match {
        case Some(users) =>
          val names = users.split('|').toSeq
          queue.add(names)
          names.map { name =>
            val exists =
              !(name.startsWith("Missing") || name.contains("#") || renamed.contains(name))
            Page(
              None,
              Some(Namespace.USER),
              name,
              Seq(Revision(user = Some(User(Option.when(exists)(1L), Some(name)))))
            )
          }
        case None =>
          params("titles").split('|').toSeq.flatMap { title =>
            renamed.get(title.stripPrefix("User talk:")).map { to =>
              Page(
                Some(1L),
                Some(Namespace.USER_TALK),
                title,
                Seq(Revision.one(redirect.format(to)))
              )
            }
          }
      })
    }
  }

  "authorsContributed" should {
    "work on no monuments" in {
      val output = new AuthorsStat
      val monumentDb = new MonumentDB(contest, Seq.empty)
      val table = output.authorsContributedTable(
        Seq.empty,
        new ImageDB(contest, Seq.empty, monumentDb),
        Some(monumentDb)
      )

      table.headers === Seq("Region", "0 years total")
      table.data === Seq(Seq("Total", "0"))
    }

    "work on no images" in {
      val output = new AuthorsStat

      val monumentDb = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val table = output.authorsContributedTable(
        Seq.empty,
        new ImageDB(contest, Seq.empty, monumentDb),
        Some(monumentDb),
        listAuthors = false
      )

      table.headers === Seq("Region", "0 years total")
      table.data === Seq(
        Seq("Автономна Республіка Крим", "0"),
        Seq("Вінницька область", "0"),
        Seq("Волинська область", "0"),
        Seq("Total", "0")
      )
    }

    "work with images" in {
      val output = new AuthorsStat

      val images = Seq(
        Image(
          "File:Img11.jpg",
          monumentIds = List("01-xxx-0001"),
          author = Some("FromCrimea")
        ),
        Image(
          "File:Img51.jpg",
          monumentIds = List("05-xxx-0001"),
          author = Some("FromPodillya1")
        ),
        Image(
          "File:Img52.jpg",
          monumentIds = List("05-xxx-0001"),
          author = Some("FromPodillya2")
        ),
        Image(
          "File:Img71.jpg",
          monumentIds = List("07-xxx-0001"),
          author = Some("FromVolyn")
        ),
        Image(
          "File:Img12.jpg",
          monumentIds = List("01-xxx-0001"),
          author = Some("FromCrimea")
        )
      )

      val monumentDb = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val table = output.authorsContributedTable(
        Seq.empty,
        new ImageDB(contest, images, monumentDb),
        Some(monumentDb),
        listAuthors = false
      )

      table.headers === Seq("Region", "0 years total")
      table.data === Seq(
        Seq("Автономна Республіка Крим", "1"),
        Seq("Вінницька область", "2"),
        Seq("Волинська область", "1"),
        Seq("Total", "4")
      )
    }

    "link the all-time count to the year-less author list page" in {
      val monumentDb = new MonumentDB(contest, monuments(1, "05", "Podillya"))
      val images = Seq(
        Image("File:Img51.jpg", monumentIds = List("05-xxx-0001"), author = Some("FromPodillya"))
      )

      val table = new AuthorsStat().authorsContributedTable(
        Seq.empty,
        new ImageDB(contest, images, monumentDb),
        Some(monumentDb)
      )

      val listPage = "Commons:Wiki Loves Monuments in Ukraine/Автори Вінницька область"
      AuthorsStat.authorsListPage(contest, "Вінницька область") === listPage
      table.data.head === Seq("Вінницька область", s"[[$listPage|1]]")
    }

    "message the authors' accounts, and list those without one as text" in {
      val accounts = Map("Ykvach" -> "Ykvach", "Artemka" -> "Artemco", "Artemco" -> "Artemco")
      AuthorsStat.authorsList(Seq("Artemco", "Artemka", "Ykvach", "roma.wem"), accounts) ===
        """# {{#target:User_talk:Artemco|}}
          |# {{#target:User_talk:Ykvach|}}
          |# roma.wem""".stripMargin
    }

    "find the authors that have a Commons account" in {
      val bot = new UsersBot
      val existing = (1 to 60).map(i => s"Author_$i")
      val accounts = Await.result(
        AuthorsStat.accounts(bot, existing ++ Seq("Missing one", "bad#name", "roma.wem")),
        5.seconds
      )

      accounts === (existing.map(_.replace('_', ' ')) :+ "Roma.wem").map(name => name -> name).toMap
      bot.requested.size === 2
    }

    "find the account an author was renamed to" in {
      val bot = new UsersBot(renamed = Map("Artemka" -> "Artemco", "Donbass360" -> "Сергей Орлик"))
      Await.result(
        AuthorsStat.accounts(bot, Seq("Artemka", "Donbass360", "Missing one")),
        5.seconds
      ) ===
        Map("Artemka" -> "Artemco", "Donbass360" -> "Сергей Орлик")
    }

    "follow a redirect however it is spelled" in {
      Seq(
        "#redirect[[User talk:%s]]",
        "#Redirect [[user_talk:%s|talk]]",
        "#ПЕРЕНАПРАВЛЕННЯ [[Обговорення користувача:%s]]",
        "  #перенаправлення[[:User talk:%s#top]]"
      ).map { redirect =>
        val bot = new UsersBot(renamed = Map("Artemka" -> "Artemco"), redirect = redirect)
        Await.result(AuthorsStat.accounts(bot, Seq("Artemka")), 5.seconds)
      } must contain(be_===(Map("Artemka" -> "Artemco"))).forall
    }

    "count an author renamed since once, under their account" in {
      val monumentDb = new MonumentDB(contest, monuments(1, "05", "Podillya"))
      val images = Seq(
        Image("File:Before.jpg", monumentIds = List("05-xxx-0001"), author = Some("Artemka")),
        Image("File:After.jpg", monumentIds = List("05-xxx-0001"), author = Some("Artemco"))
      )
      val db = new ImageDB(contest, images, monumentDb)
      val stat = ContestStat(contest, contest.year, Some(monumentDb), db, db, Seq(db))
        .withAccounts(Map("Artemka" -> "Artemco", "Artemco" -> "Artemco"))

      val table = new AuthorsStat().authorsContributedTable(
        stat.dbsByYear,
        stat.totalImageDb,
        Some(monumentDb),
        listAuthors = false
      )

      table.data === Seq(Seq("Вінницька область", "1", "1"), Seq("Total", "1", "1"))
    }

    "put every report's authors under their current accounts" in {
      val monumentDb = new MonumentDB(contest, monuments(1, "05", "Podillya"))
      val images = Seq(
        Image("File:Old.jpg", monumentIds = List("05-xxx-0001"), author = Some("Artemka")),
        Image("File:Panoramio.jpg", monumentIds = List("05-xxx-0001"), author = Some("roma.wem")),
        Image("File:Unknown.jpg", monumentIds = List("05-xxx-0001"), author = Some(""))
      )
      val db = new ImageDB(contest, images, monumentDb)
      val stat = ContestStat(contest, contest.year, Some(monumentDb), db, db, Seq(db))
      stat.authors.toSet === Set("Artemka", "roma.wem")

      // the lookup finds only the name it was asked about, the old one
      val renamed = stat.withAccounts(Map("Artemka" -> "Artemco"))

      (renamed.currentYearImageDb eq renamed.dbsByYear.last) must beTrue
      Seq(renamed.currentYearImageDb, renamed.totalImageDb).map(_.authors) must contain(
        be_===(Set("Artemco", "roma.wem", ""))
      ).forall
      AuthorsStat.authorsList(
        AuthorsStat.authorsByRegion(renamed.totalImageDb).flatMap(_._2),
        renamed.accounts.get
      ) ===
        """# {{#target:User_talk:Artemco|}}
          |# roma.wem""".stripMargin
    }

    "look up again only the authors not cached as accounts" in {
      val cache = Files.createTempDirectory("accounts").resolve("commons-accounts.txt")
      val authors = Seq("Ykvach", "roma.wem", "Missing one", "Artemka")
      val renamed = Map("Artemka" -> "Artemco")

      val first = new UsersBot(renamed)
      Await.result(AuthorsStat.accounts(first, authors, Some(cache)), 5.seconds) ===
        Map("Ykvach" -> "Ykvach", "Roma.wem" -> "Roma.wem", "Artemka" -> "Artemco")
      Files.readAllLines(cache).asScala === Seq("Artemka\tArtemco", "Roma.wem", "Ykvach")

      val second = new UsersBot(renamed)
      Await.result(AuthorsStat.accounts(second, authors :+ "Newcomer", Some(cache)), 5.seconds) ===
        Map(
          "Ykvach" -> "Ykvach",
          "Roma.wem" -> "Roma.wem",
          "Artemka" -> "Artemco",
          "Newcomer" -> "Newcomer"
        )
      second.requested.flatten.toSet === Set("Missing one", "Newcomer")
      Files.readAllLines(cache).asScala === Seq(
        "Artemka\tArtemco",
        "Newcomer",
        "Roma.wem",
        "Ykvach"
      )
    }

    "not count a file with no author as an author" in {
      val monumentDb = new MonumentDB(contest, monuments(1, "05", "Podillya"))
      val images = Seq(
        Image("File:Img51.jpg", monumentIds = List("05-xxx-0001"), author = Some("FromPodillya")),
        Image("File:Img52.jpg", monumentIds = List("05-xxx-0001"), author = None),
        Image("File:Img53.jpg", monumentIds = List("05-xxx-0001"), author = Some(""))
      )

      val table = new AuthorsStat().authorsContributedTable(
        Seq(new ImageDB(contest, images, monumentDb)),
        new ImageDB(contest, images, monumentDb),
        Some(monumentDb),
        listAuthors = false
      )

      table.data === Seq(Seq("Вінницька область", "1", "1"), Seq("Total", "1", "1"))
    }

    "work with images 2 years" in {
      val output = new AuthorsStat

      val images1 = Seq(
        Image(
          "File:Img11y1f1.jpg",
          monumentIds = List("01-xxx-0001"),
          author = Some("FromCrimea")
        ),
        Image(
          "File:Img11y1f2.jpg",
          monumentIds = List("01-xxx-0001"),
          author = Some("FromCrimea")
        ),
        Image(
          "File:Img51y1f1.jpg",
          monumentIds = List("05-xxx-0001"),
          author = Some("FromPodillya1")
        ),
        Image(
          "File:Img51y1f2.jpg",
          monumentIds = List("05-xxx-0001"),
          author = Some("FromPodillya2")
        ),
        Image(
          "File:Img71y1f1.jpg",
          monumentIds = List("07-xxx-0001"),
          author = Some("FromVolyn")
        )
      )

      val images2 = Seq(
        Image(
          "File:Img11y2f1.jpg",
          monumentIds = List("01-xxx-0001"),
          author = Some("FromCrimea1")
        ),
        Image(
          "File:Img12y2f1.jpg",
          monumentIds = List("01-xxx-0002"),
          author = Some("FromCrimea2")
        ),
        Image(
          "File:Img52y2f1.jpg",
          monumentIds = List("05-xxx-0002"),
          author = Some("FromPodillya1")
        ),
        Image(
          "File:Img52y2f1.jpg",
          monumentIds = List("05-xxx-0002"),
          author = Some("FromPodillya2")
        ),
        Image(
          "File:Img72y2f1.jpg",
          monumentIds = List("07-xxx-0002"),
          author = Some("FromVolyn")
        )
      )

      val monumentDb = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val table = output.authorsContributedTable(
        Seq(images1, images2).zipWithIndex.map { case (images, i) =>
          new ImageDB(Contest.WLMUkraine(2014 + i), images, monumentDb)
        },
        new ImageDB(contest, images1 ++ images2, monumentDb),
        Some(monumentDb),
        listAuthors = false
      )

      table.headers === Seq("Region", "2 years total", "2015", "2014")
      table.data === Seq(
        Seq("Автономна Республіка Крим", "3") ++ Seq("1", "2").reverse,
        Seq("Вінницька область", "2") ++ Seq("2", "2").reverse,
        Seq("Волинська область", "1") ++ Seq("1", "1").reverse,
        Seq("Total", "6") ++ Seq("4", "5").reverse
      )
    }
  }
}
