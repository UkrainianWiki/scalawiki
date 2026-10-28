package org.scalawiki.bots

import java.net.URLDecoder

import org.scalawiki.MwBot
import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.prop.rvprop.{Content, RvProp}
import org.scalawiki.dto.cmd.query.prop.{Prop, Revisions}
import org.scalawiki.dto.cmd.query.{Query, TitlesParam}
import org.scalawiki.dto.{Image, Page}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.util.pimpFuture
import scala.io.Source

/** Converts links to Wikimedia Commons files to short links (with file id) and tries to add image
  * author and license
  */
object ShortLinksBot {
  val commons = MwBot.fromHost(MwBot.commons)
  val ukWiki = MwBot.fromHost(MwBot.ukWiki)

  def getPage(title: String): Future[Page] = {
    val action = Action(
      Query(
        TitlesParam(Seq(title)),
        Prop(
          Revisions(RvProp(Content))
        )
      )
    )

    commons
      .run(action)
      .flatMap { commonsPages =>
        val commonsPage = commonsPages.head
        if (commonsPage.missing) {
          ukWiki.run(action).map(_.head)
        } else Future.successful(commonsPage)
      }
      .recoverWith { case e =>
        Future.successful(Page(title = "Error! " + e))
      }
  }

  def getPageLicense(page: Page): Option[String] = {
    for (
      id <- page.id;
      text <- page.revisions.headOption.flatMap(_.content)
    ) yield {

      val author = Image.getAuthorFromPage(text)

      val license = text
        .split("\\s|\\||\\{|\\}")
        .map(_.toLowerCase)
        .find { s =>
          s.startsWith("cc-") ||
          s.startsWith("gfdl") ||
          s.startsWith("wikimapia")
        }
        .getOrElse("???")

      val readableLicense = license
        .replace("cc-by-sa-", "CC BY-SA ")
        .replace("cc-zero", "CC0 1.0")
        .replace("gfdl-self", "GFDL")
        .replace("wikimapia", "CC BY-SA 3.0")

      s"https://commons.wikimedia.org/?curid=$id © $author, $readableLicense"
    }
  }

  def getLineInfo(line: String): Future[String] = {
    val s = line.indexOf("File:")
    val title = line.substring(s).trim
    getPage(title).map { page =>
      getPageLicense(page).getOrElse("Error with " + title)
    }
  }

  def getFileSubstring(line: String): Future[String] = {
    val replaced = line.replace("%D0%A4%D0%B0%D0%B9%D0%BB:", "File:")
    val start = replaced.indexOf("File:")
    if (start >= 0) {
      val decoded = URLDecoder.decode(replaced.substring(start), "UTF-8")
      getPage(decoded.trim)
        .map(page => getPageLicense(page).getOrElse(line))
        .recoverWith { case e =>
          Future.successful("Error! " + e)
        }
    } else Future.successful(line)
  }

  def main(args: Array[String]) {
    val lines = Source.fromFile("arch.txt").getLines().toSeq

    val parallel = false
    val updatedLines = if (parallel) {
      Future.sequence(lines.map(getFileSubstring)).await
    } else {
      lines.map(line => getFileSubstring(line).await)
    }
    println(updatedLines.mkString("\n"))
  }
}
