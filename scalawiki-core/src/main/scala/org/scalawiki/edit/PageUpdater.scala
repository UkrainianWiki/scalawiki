package org.scalawiki.edit

import java.time.ZonedDateTime

import org.scalawiki.WithBot
import org.scalawiki.dto.MwException

import scala.concurrent._
import scala.util.{Failure, Success, Try}

object PageUpdater {

  /** How many times to re-read a page and re-apply the edit after MediaWiki rejects it with an edit
    * conflict before giving up on that page.
    */
  val conflictRetries = 3
}

class PageUpdater(task: PageUpdateTask) extends WithBot {

  import scala.concurrent.ExecutionContext.Implicits.global

  def host = task.host

  /** Update every page from [[PageUpdateTask.titles]], one at a time.
    *
    * The pages are processed serially on purpose:
    *   - [[https://www.mediawiki.org/wiki/API:Etiquette API:Etiquette]] asks bots to make requests
    *     one after another, not in parallel;
    *   - the host connection pool is deliberately tiny
    *     (`pekko.http.host-connection-pool.max-connections = 2`).
    *
    * Firing an edit future per page at once (as this did previously) submits far more than
    * `max-open-requests` (32) concurrent requests to that pool, so all but the first few fail
    * straight away with a `BufferOverflowException` and the run stops after a handful of edits.
    * Chaining the pages keeps at most one request in flight and lets the whole batch finish.
    *
    * A failure on one page is logged and skipped, not propagated, so one bad page cannot abort the
    * rest of the batch.
    *
    * @return
    *   the per-page outcome, in processing order
    */
  def update(): Future[Seq[(String, Try[Any])]] = {
    val titles = task.titles.toVector
    val total = titles.size

    val done = titles.zipWithIndex.foldLeft(
      Future.successful(Vector.empty[(String, Try[Any])])
    ) { case (acc, (title, index)) =>
      acc.flatMap { results =>
        println(s"Processing page: $title, ${index + 1} of $total")
        updatePage(title)
          .map(Success(_): Try[Any])
          .recover { case e => Failure(e) }
          .map(outcome => results :+ (title -> outcome))
      }
    }

    done.foreach { results =>
      val (successful, errors) = results.partition(_._2.isSuccess)
      println(s"Successful page updates: ${successful.size}")
      println(s"Errors in  page updates: ${errors.size}")
      errors.foreach { case (title, outcome) => println(s"  $title: $outcome") }
    }

    done
  }

  def updatePage(title: String): Future[Any] =
    updatePage(title, PageUpdater.conflictRetries)

  /** The moment we started looking at the page, sent as `starttimestamp` so MediaWiki can reject
    * the save if the page was deleted in the meantime. Overridable so tests get a deterministic
    * value.
    */
  protected def now(): ZonedDateTime = ZonedDateTime.now()

  /** Read the page, apply [[PageUpdateTask.updatePage]], and save the result against the exact
    * revision that was read.
    *
    * The current revision is fetched via `prop=revisions` (not `action=raw`) so we also get its id
    * and timestamp. Those go back to `action=edit` as `baserevid` / `basetimestamp` (+
    * `starttimestamp`): if another user or bot edited the page between our read and our write,
    * MediaWiki rejects the save with an `editconflict` / `pagedeleted` error instead of silently
    * overwriting that change. We then re-read the now-current page, re-run the task against it and
    * try again — up to `retriesLeft` times. Re-applying is safe: the task rewrites individual
    * template parameters of the monuments it cares about and leaves everything else (including the
    * other editor's changes) untouched.
    */
  def updatePage(title: String, retriesLeft: Int): Future[Any] = {
    val startTimestamp = now()
    bot
      .page(title)
      // `limit = None`: fetch only the current revision, not the whole history
      // with content.
      .revisions(
        Set.empty[Int],
        Set("ids", "content", "timestamp"),
        limit = None
      )
      .flatMap { pages =>
        // rvdir defaults to "older", so the first revision is the current one.
        val revision = pages.headOption.flatMap(_.revisions.headOption)

        revision.flatMap(_.content) match {
          case None =>
            // No current revision: the page is missing or was deleted. Don't
            // create it from empty text — surface it as an error for this page.
            Future.failed(
              new NoSuchElementException(
                s"$title: no current revision (page missing or deleted), skipping"
              )
            )

          case Some(pageText) =>
            val (newText: String, comment: String) =
              task.updatePage(title, pageText)

            bot
              .page(title)
              .edit(
                newText,
                Some(comment),
                basetimestamp = revision.flatMap(_.timestamp),
                baseRevId = revision.flatMap(_.revId),
                startTimestamp = Some(startTimestamp)
              )
              .recoverWith {
                case e: MwException if e.conflict && retriesLeft > 0 =>
                  println(
                    s"$title: edit conflict (${e.code}), re-reading and retrying " +
                      s"($retriesLeft attempt(s) left)"
                  )
                  updatePage(title, retriesLeft - 1)
              }
        }
      }
  }

}
