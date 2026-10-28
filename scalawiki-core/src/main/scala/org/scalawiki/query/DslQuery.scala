package org.scalawiki.query

import org.scalawiki.MwBot
import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.{MwException, PageList}
import org.scalawiki.json.Parser

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class QueryProgress(
    pages: Long,
    done: Boolean,
    action: Action,
    bot: MwBot,
    context: Map[String, String] = Map.empty
)

class DslQuery(
    val action: Action,
    val bot: MwBot,
    context: Map[String, String] = Map.empty
) {

  import scala.concurrent.ExecutionContext.Implicits.global

  var startTime: Long = 0

  def run(
      continue: Map[String, String] = Map("continue" -> ""),
      pages: PageList = new PageList(),
      limit: Option[Long] = None
  ): Future[PageList] = {

    val params =
      action.pairs ++ Seq("format" -> "json", "utf8" -> "") ++ continue

    if (startTime == 0)
      startTime = System.nanoTime()

    onProgress(pages.size)

    implicit val success: retry.Success[String] =
      retry.Success[String] { body =>
        val trimmed = body.trim
        trimmed.startsWith("{") || trimmed.startsWith("[")
      }

    retry.Backoff()(odelay.Timer.default)(() => bot.post(params.toMap)) flatMap { body =>
      val parser = new Parser(action)

      parser.parse(body) match {
        case Success(newPages) =>
          pages.addPages(newPages)

          val newContinue = parser.continue
          if (newContinue.isEmpty || limit.exists(_ <= pages.size)) {

            onProgress(pages.size, done = true)
            Future.successful(pages)
          } else {
            run(newContinue, pages, limit)
          }

        case Failure(mwEx: MwException) =>
          val withParams = mwEx.copy(params = params.toMap)
          if (withParams.expected)
            bot.log.warning(s"${bot.host} ${withParams.code}: ${withParams.info}")
          else
            bot.log.error(s"${bot.host} exception $withParams, body: $body")
          Future.failed(withParams)
        case Failure(ex) =>
          bot.log.error(s"${bot.host} exception $ex, body: $body")
          Future.failed(MwException(ex.getMessage, ex.getMessage, params.toMap))
      }
    }
  }

  def onProgress(pages: Long, done: Boolean = false): Unit = {
    if (done) {
      val estimatedTime = (System.nanoTime() - startTime) / Math.pow(10, 9)

      bot.log.info(
        s"${bot.host} Action completed with $pages pages in $estimatedTime seconds,  $action.pairs"
      )
    } else {
      bot.log.info(s"${bot.host} pages: $pages action: $action.pairs")
    }

    val progress = new QueryProgress(pages, done, action, bot, context)

    bot.system.eventStream.publish(progress)
  }
}
