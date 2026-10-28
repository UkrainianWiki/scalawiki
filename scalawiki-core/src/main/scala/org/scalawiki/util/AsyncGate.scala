package org.scalawiki.util

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

/** Shares a concurrency budget between callers without blocking dispatcher threads. */
final class AsyncGate(maxConcurrent: Int) {
  require(maxConcurrent > 0)
  private val lock = new Object
  private val waiting = mutable.Queue.empty[() => Unit]
  private var running = 0

  def submit[T](operation: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val result = Promise[T]()
    val start = () => {
      val future =
        try operation
        catch { case NonFatal(e) => Future.failed(e) }
      future.onComplete { outcome =>
        lock.synchronized { running -= 1 }
        result.complete(outcome)
        drain()
      }
    }
    lock.synchronized { waiting.enqueue(start) }
    drain()
    result.future
  }

  private def drain(): Unit = {
    var next: () => Unit = null
    do {
      next = lock.synchronized {
        if (running >= maxConcurrent || waiting.isEmpty) null
        else {
          running += 1
          waiting.dequeue()
        }
      }
      if (next != null) next()
    } while (next != null)
  }
}
