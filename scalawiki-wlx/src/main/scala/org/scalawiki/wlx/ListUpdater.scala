package org.scalawiki.wlx

import org.scalawiki.MwBot
import org.scalawiki.dto.markup.SwTemplate
import org.scalawiki.edit.{PageUpdateTask, PageUpdater}
import org.scalawiki.wikitext.SwebleParser
import org.scalawiki.wlx.dto.{Monument, SpecialNomination}
import org.scalawiki.wlx.stat.ContestStat
import org.sweble.wikitext.engine.config.WikiConfig
import org.sweble.wikitext.engine.utils.DefaultConfigEnWp
import org.sweble.wikitext.parser.nodes.WtTemplate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ListUpdater {

  /** Update every list page for `monumentDb`. [[PageUpdater]] processes the pages serially; callers
    * chain the returned future so one list pass finishes before the next starts.
    */
  def updateLists(
      monumentDb: MonumentDB,
      monumentUpdater: MonumentUpdater
  ): Future[Unit] = {
    val task = new ListUpdaterTask(MwBot.ukWiki, monumentDb, monumentUpdater)
    val updater = new PageUpdater(task)
    updater.update().map(_ => ())
  }

  /** Run `monumentUpdater` over the separate wiki pages that hold special nomination (thematic)
    * monument lists. [[updateLists]] never visits them because they are not part of the per-region
    * `monumentDb`. Mirrors `ReporterRegistry.fillSpecialNominationLists`.
    */
  def updateSpecialNominationLists(
      stat: ContestStat,
      monumentUpdater: MonumentUpdater
  ): Future[Unit] = {
    val nominations = SpecialNomination.nominations.filter(_.listTemplate.nonEmpty)

    SpecialNomination.getMonumentsMap(nominations, stat).flatMap { monumentsMap =>
      val passes = for {
        nomination <- nominations
        listTemplate <- nomination.listTemplate
        monuments = monumentsMap.getOrElse(nomination, Nil)
        if monuments.nonEmpty
      } yield { () =>
        val nominationContest = stat.contest.copy(
          uploadConfigs = stat.contest.uploadConfigs match {
            case head +: tail => head.copy(listTemplate = listTemplate) +: tail
            case empty        => empty
          }
        )
        updateLists(
          new MonumentDB(nominationContest, monuments.toSeq),
          monumentUpdater
        )
      }

      // one nomination's list pass at a time, mirroring the old serial loop
      passes.foldLeft(Future.unit)((acc, run) => acc.flatMap(_ => run()))
    }
  }
}

trait MonumentUpdater {
  def needsUpdate(m: Monument): Boolean
  def updatedParams(m: Monument): Map[String, String]
}

class ListUpdaterTask(
    val host: String,
    monumentDb: MonumentDB,
    monumentUpdater: MonumentUpdater
) extends PageUpdateTask
    with SwebleParser {

  val config: WikiConfig = DefaultConfigEnWp.generate

  val titles = pagesToFill(monumentDb)

  val uploadConfig = monumentDb.contest.uploadConfigs.head

  override def updatePage(title: String, pageText: String): (String, String) = {
    val template = uploadConfig.listTemplate
    val wlxParser = new WlxTemplateParser(uploadConfig.listConfig, title)
    var updated: Int = 0

    def mapper(wtTemplate: WtTemplate) = {
      val swTemplate = new SwTemplate(wtTemplate)
      val monument = wlxParser.templateToMonument(swTemplate.template)

      if (needsUpdate(monument)) {
        updated += 1

        val updates = monumentUpdater.updatedParams(monument)

        updates.foreach { case (name, value) =>
          swTemplate.setTemplateParam(name, value)
        }
      }
    }

    val noComments = pageText
      .replace("<!--", "<x-comment>")
      .replace("-->", "</x-comment>")

    val newText = replace(
      noComments,
      { case t: WtTemplate if getTemplateName(t) == template => t },
      mapper
    )

    val withComments = newText
      .replace("<x-comment>", "<!--")
      .replace("</x-comment>", "-->")

    val comment = s"updated $updated monument(s)"
    (withComments, comment)
  }

  def needsUpdate(m: Monument): Boolean = monumentUpdater.needsUpdate(m)

  def pagesToFill(monumentDb: MonumentDB): Set[String] = {

    val monumentsToFill = monumentDb.monuments.filter(needsUpdate)

    println(s"NewIds: ${monumentsToFill.size}")

    val titles = monumentsToFill.map(_.page).toSet

    println(s"pages: ${titles.size}")
    titles
  }
}
