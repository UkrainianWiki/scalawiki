package org.scalawiki.bots.museum

import org.jsoup.Jsoup
import org.jsoup.nodes.Document.OutputSettings
import org.jsoup.safety.Safelist

import scala.compat.Platform

object HtmlParser {

  /** Replaces special whitespace characters (short/nonbreaking space) with regular space
    * @param s
    * @return
    */

  def replaceWs(s: String): String =
    s.replace('\u00a0', ' ')
      .replace('\u200b', ' ')
      .replace("&nbsp;", " ")

  def trimmedLines(html: String): Seq[String] = {
    val text = htmlText(html)
    text
      .split("\n")
      .map(replaceWs _ andThen (_.trim))
      .filter(_.nonEmpty)
      .toList
  }

  /** Tries to get page text from html code.
    * @param html
    * @return
    */
  def htmlText(html: String): String = {
    val tags2Nl = Jsoup.clean(
      html,
      "",
      Safelist.none().addTags("p"),
      new OutputSettings().prettyPrint(true)
    )
    Jsoup
      .clean(
        tags2Nl,
        "",
        Safelist.none(),
        new OutputSettings().prettyPrint(false)
      )
      .split(Platform.EOL)
      .map(_.trim)
      .mkString(Platform.EOL)
  }
}
