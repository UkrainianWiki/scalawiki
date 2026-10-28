package org.scalawiki.wlx.stat.reports

import org.scalawiki.MwBot
import org.scalawiki.wlx.DeletedImagesCsv
import org.scalawiki.wlx.DeletedImagesCsv.DeletedImage
import org.scalawiki.wlx.dto.Contest

import scala.concurrent.Future

/** The contest files deleted on Commons after the statistics cached them, as the CSV cache's syncs
  * found them ([[DeletedImagesCsv]]): one page,
  * `Commons:<contest type> in <country>/Deleted images`, with a section per year of the run, newest
  * first, and one for the other files of the contest template.
  */
object DeletedImages {

  def pageName(contest: Contest): String =
    s"Commons:${contest.contestType.name} in ${contest.country.name}/Deleted images"

  /** The page's text, from the deleted-files CSVs in `csvDir`. */
  def text(contest: Contest, years: Seq[Int], csvDir: String): String = {
    val byYear = years.sorted.reverse.map { year =>
      year.toString -> DeletedImagesCsv.read(
        DeletedImagesCsv.filename(contest.campaign, year, csvDir)
      )
    }
    val other = "Other files with the contest template" ->
      DeletedImagesCsv.read(DeletedImagesCsv.totalFilename(contest.campaign, csvDir))
    val sections = (byYear :+ other).filter(_._2.nonEmpty)
    val total = sections.map(_._2.size).sum

    s"Files of ${contest.contestType.name} in ${contest.country.name} that were deleted from Commons after the " +
      s"statistics cached them, as the daily sync found them: $total.\n" +
      sections.map { case (title, rows) =>
        s"\n== $title (${rows.size}) ==\n" + table(rows)
      }.mkString
  }

  private def table(rows: Seq[DeletedImage]): String = {
    val sorted = rows.sortBy(r => (r.detected.toEpochDay * -1, r.image.title))
    "{| class=\"wikitable sortable\"\n! File !! Author !! Uploaded !! Monument !! Found deleted\n" +
      sorted.map { r =>
        val i = r.image
        val author = i.author.filterNot(_.isBlank).fold("")(a => s"[[User:$a|$a]]")
        Seq(
          s"[[:${i.title}]]",
          author,
          i.date.map(_.toLocalDate.toString).getOrElse(""),
          i.monumentIds.mkString(", "),
          r.detected.toString
        ).mkString("|-\n| ", " || ", "\n")
      }.mkString +
      "|}\n"
  }

  def updateWiki(contest: Contest, years: Seq[Int], csvDir: String, bot: MwBot): Future[Any] =
    bot.page(pageName(contest)).edit(text(contest, years, csvDir), Some("updating"))
}
