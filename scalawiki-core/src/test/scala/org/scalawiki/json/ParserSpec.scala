package org.scalawiki.json

import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.list.{EmbeddedIn, ListParam}
import org.scalawiki.dto.cmd.query.prop.{Categories, ClLimit, LangLinks, LlLimit, Prop, Revisions}
import org.scalawiki.dto.cmd.query.{PageIdsParam, Query, TitlesParam}
import org.scalawiki.dto.{MwException, Page}
import org.scalawiki.util.TestUtils._
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

import scala.util.Failure

class ParserSpec extends Specification {

  val pageStr = """{"pageid": 123, "ns": 4, "title": "PageTitle" }"""

  val queryType = "embeddedin"
  val queryContinue = "eicontinue"

  val queryStr = s""""query": {"$queryType": [$pageStr] }"""

  val emptyAction = Action(Query(ListParam(EmbeddedIn())))

  "One page query" should {
    val limitsStr = s"""{"limits": {"embeddedin": 500}, $queryStr}"""

    val parser = new Parser(emptyAction)
    val pagesOpt = parser.parse(limitsStr).toOption

    "contain page" in {
      checkPages(pagesOpt)
    }

    "not have continue" in {
      parser.continue === Map.empty
    }
  }

  "New Multipage query" should {
    val queryContinueStr =
      s"""{$queryStr, "continue": {"continue": "-||", "eicontinue": "qcValue"}}"""
    val parser = new Parser(emptyAction)
    val pagesOpt = parser.parse(queryContinueStr).toOption

    "contain page" in {
      checkPages(pagesOpt)
    }

    "have continue" in {
      parser.continue === Map("continue" -> "-||", "eicontinue" -> "qcValue")
    }
  }

  "Legacy Multipage query" should {
    // TODO query-continue is not supported, but we should report it then
    val queryContinueStr =
      s"""{"query-continue": {"$queryType": {"$queryContinue": "qcValue" }}, $queryStr}"""

    val parser = new Parser(emptyAction)
    val pagesOpt = parser.parse(queryContinueStr).toOption

    "contain page" in {
      checkPages(pagesOpt)
    }

    "have continue" in {
      parser.continue === Map.empty
    }
  }

  def checkPages(pagesOpt: Option[Seq[Page]]): MatchResult[Any] = {
    pagesOpt
      .map { pages =>
        pages must have size 1
        checkPage(pages(0))
      }
      .getOrElse(pagesOpt must beSome)
  }

  def checkPageOpt(pageOpt: Option[Page]): MatchResult[Any] = {
    pageOpt.map(checkPage).getOrElse(pageOpt must beSome)
  }

  def checkPage(page: Page): MatchResult[Any] = {
    page.id === Some(123)
    page.ns === Some(4)
    page.title === "PageTitle"
  }

  "parser" should {
    "parse page with lang links" in {
      val s = resourceAsString("/org/scalawiki/query/langLinks.json")

      val action = Action(
        Query(
          Prop(
            LangLinks(LlLimit("max"))
          ),
          TitlesParam(Seq("Article"))
        )
      )

      val parser = new Parser(action)
      val page = parser.parse(s).get.head

      val langLinks = page.langLinks
      langLinks.size === 6

      langLinks === Map(
        "de" -> "Artikel",
        "eo" -> "Artikolo",
        "fr" -> "Article",
        "it" -> "Articolo",
        "nl" -> "Artikel",
        "pt" -> "Artigo"
      )
    }

    "parse page categories" in {
      val s =
        """{"continue": {"clcontinue": "11|Lakes_in_Armenia", "continue": "||"},
          | "query": {"pages": {
          |   "11": {"pageid": 11, "ns": 6, "title": "File:Lake.jpg", "categories": [
          |     {"ns": 14, "title": "Category:Images from Wiki Loves Earth 2026"},
          |     {"ns": 14, "title": "Category:Images from Wiki Loves Earth 2026 in Armenia"}]},
          |   "12": {"pageid": 12, "ns": 6, "title": "File:Desert.jpg"}}}}""".stripMargin

      val action = Action(Query(Prop(Categories(ClLimit("max"))), PageIdsParam(Seq(11, 12))))
      action.pairs.toMap === Map("action" -> "query", "prop" -> "categories", "cllimit" -> "max", "pageids" -> "11|12")

      val parser = new Parser(action)
      val pages = parser.parse(s).get

      pages.map(p => p.title -> p.categories).toMap === Map(
        "File:Lake.jpg" -> Seq(
          "Category:Images from Wiki Loves Earth 2026",
          "Category:Images from Wiki Loves Earth 2026 in Armenia"
        ),
        "File:Desert.jpg" -> Nil
      )
      parser.continue === Map("clcontinue" -> "11|Lakes_in_Armenia", "continue" -> "||")
    }

    "merge the categories of a page split by continue" in {
      val first = Page(Some(11L), Some(6), "File:Lake.jpg", categories = Seq("Category:A"))
      val next = Page(Some(11L), Some(6), "File:Lake.jpg", categories = Seq("Category:B"))
      first.appendLists(next).categories === Seq("Category:A", "Category:B")
    }

    "parse mwerror" in {
      val action = Action(
        Query(
          Prop(Revisions()),
          TitlesParam(Seq("PageTitle")),
          PageIdsParam(Seq(123456))
        )
      )

      val json =
        """{
        "servedby": "mw1202",
        "error": {
          "code": "multisource",
          "info": "Cannot use 'pageids' at the same time as 'titles'",
          "*": "See https://en.wikipedia.org/w/api.php for API usage"
        }
      }"""

      val parser = new Parser(action)
      val result = parser.parse(json)

      result.isFailure === true
      val mw = result match { case Failure(mw: MwException) => mw }
      mw.code === "multisource"
      mw.info === "Cannot use 'pageids' at the same time as 'titles'"
    }
  }

  "Page ids query" should {
    // the API answers a page id it no longer has (a deleted page) without a title
    "leave out a missing page id" in {
      val json =
        """{"batchcomplete": "", "query": {"pages": {
          "99999999999": {"pageid": 99999999999, "missing": ""},
          "136018389": {"pageid": 136018389, "ns": 6, "title": "File:X.jpg"}}}}"""
      val pages = new Parser(Action(Query(PageIdsParam(Seq(99999999999L, 136018389L))))).parse(json).get
      pages.flatMap(_.id) === Seq(136018389L)
    }
  }
}
