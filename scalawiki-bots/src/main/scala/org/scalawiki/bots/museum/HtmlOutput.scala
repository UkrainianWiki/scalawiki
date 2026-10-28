package org.scalawiki.bots.museum

import better.files.{File => SFile}
import org.scalawiki.dto.Image
import org.scalawiki.dto.markup.Gallery

object HtmlOutput {

  def makeGalleries(entries: Seq[Entry]): Seq[String] = {
    def makeToC: String = {
      val navItems = entries.zipWithIndex.map { case (e, i) =>
        val link = s"e$i.html"
        val title = s"${e.dir} (${e.images.size})"
        s"""<li><a href="$link"> $title </a></li>"""
      }

      navItems.mkString(
        """<nav role="navigation" class="table-of-contents">
           <ol>""",
        "\n",
        """</ol>
          </nav>
        """
      )
    }

    val head =
      """<head>
          <meta charset="UTF-8">
          <link rel="stylesheet" media="screen" href="main.css">
        </head>"""

    val nav = makeToC

    val galleries = entries.zipWithIndex.map { case (e, i) =>
      s"""<h1 id="e$i">${e.dir}</h1>""" +
        e.text.map(t => s"<br> $t <br>".replace("\n", "<br>")).getOrElse("") +
        makeEntryGallery(e)
    }

    galleries.map(g => "<html>" + head + "<body>\n" + nav + g + "\n</body></html>")
  }

  def makeEntryGallery(entry: Entry): String = {
    Gallery.asHtml(
      entry.images.map { entry =>
        new Image(
          entry.filePath,
          url = Some(SFile(entry.filePath).uri.toString)
        )
      },
      entry.images.map(_.sourceDescription.getOrElse(""))
    )
  }
}
