package org.scalawiki.bots.stat

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.caching.scaladsl.Cache
import org.scalawiki.MwBot
import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.list._
import org.scalawiki.dto.cmd.query.prop._
import org.scalawiki.dto.cmd.query.{Generator, Query}
import org.scalawiki.dto.{Namespace, Page}

import java.util.Collections
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import scala.collection.SortedSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CatScan {

  val system = ActorSystem()

  val cache: Cache[String, Set[String]] = LfuCache(system)

  val total = Collections.newSetFromMap[String](new ConcurrentHashMap())
  val noUkWiki = Collections.newSetFromMap[String](new ConcurrentHashMap())
  val noUkWikiCats = Collections.newSetFromMap[String](new ConcurrentHashMap())
  val allCats = Collections.newSetFromMap[String](new ConcurrentHashMap())

  val categories = new AtomicInteger()

  val wikipedia = "wikipedia.org"
  val enWikiHost = "en." + wikipedia

  val cats = Map(
    "en" -> "Category:Crimea",
    "fr" -> "Catégorie:Crimée",
    "it" -> "Categoria:Crimea",
    "el" -> "Κατηγορία:Κριμαία",
    "es" -> "Categoría:Crimea",
    "be-x-old" -> "Катэгорыя:Крым",
    "be" -> "Катэгорыя:Крым",
    "de" -> "Kategorie:Krim",
    "pl" -> "Kategoria:Krym",
    "crh" -> "Kategoriya:Qırım",
    "uk" -> "Категорія:Крим",
    "ru" -> "Категория:Крым"
  )

  def main(args: Array[String]) {
    val lang = "ru"
    val host = lang + ".wikipedia.org"
    val title = cats(lang)

    val bot = MwBot.fromHost(host)

    val countFuture = getCountCached(bot, title)

    countFuture.map { count =>
      bot.system.log.info("Final count:" + count)

      //    SortedSet(total.asScala.toSeq: _*).foreach(t => println(s"* [[$t]]"))
//        SortedSet(noUkWiki.asScala.toSeq: _*).foreach(t => println(s"* [[$t]]"))
    }
  }

  def getCountCached(bot: MwBot, title: String): Future[Set[String]] = {
    cache.get(title).getOrElse(getCount(bot, title))
  }

  def getCount(bot: MwBot, title: String): Future[Set[String]] = {
    bot.system.log.info(s"Enter: $title, cache size: ${cache.size}")
    cache(
      title,
      { () =>
        Future {
          Set.empty[String]
        }
      }
    )
    val subCats =
      catsWithLinks(title, namespaces = Set(Namespace.CATEGORY), bot).flatMap { pages =>
        bot.system.log.info(s"Category $title has ${pages.size} subcats")

        val noUkCats = pages.filter(!_.langLinks.contains("uk")).map(_.title)
        noUkWikiCats.addAll(noUkCats.toSeq.asJava)

        allCats.addAll(pages.map(_.title).toSeq.asJava)

        val futures = pages.map { page =>
          getCountCached(bot, page.title)

        }
        Future.reduce(futures)((s1, s2) => s1 ++ s2)
      }

    val thisCat =
      catsWithLinks(title, namespaces = Set(Namespace.MAIN), bot).map { articles =>
        import scala.collection.JavaConverters._

        val size = articles.size
        val titles: Set[String] = articles.map(_.title).toSet
        total.addAll(titles.asJava)

        val noUk = articles.filter(!_.langLinks.contains("uk")).map(_.title)
        noUkWiki.addAll(noUk.toSeq.asJava)

        val processed = categories.addAndGet(1)

        bot.system.log.info(
          s"Total is ${total.size}, processed = $processed (${processed * 100.0 / cache.size} %) Category $title has $size articles"
        )

        if (processed == cache.size) {
          println(cache.keys.mkString("All cats:\n", "\n", "\n"))

          println("== no UkWiki pages ==")
          SortedSet(noUkWiki.asScala.toSeq: _*).foreach(t => println(s"* [[$t]]"))

          println("== no UkWiki cats ==")
          SortedSet(noUkWikiCats.asScala.toSeq: _*).foreach(t => println(s"* [[:$t]]"))

        }
        titles
      }
    val sum = Seq(subCats, thisCat)
    Future.reduce(sum)((s1, s2) => s1 ++ s2)
  }

  def catsWithLinks(
      title: String,
      namespaces: Set[Int],
      bot: MwBot
  ): Future[Iterable[Page]] = {
    val action = Action(
      Query(
        Prop(
          LangLinks(LlLimit("max"))
        ),
        Generator(
          ListArgs
            .toDsl(
              "categorymembers",
              Some(title),
              None,
              namespaces,
              Some("max")
            )
            .get
        )
      )
    )

    bot.run(action)
  }
}
