package org.scalawiki.util

import org.specs2.mutable.Specification

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

class WriteWatcherSpec extends Specification {

  sequential

  import ExecutionContext.Implicits.global

  "disabled WriteWatcher" should {
    "run the op immediately and not track it" in {
      WriteWatcher.reset()
      val ran = new AtomicInteger(0)
      val f = WriteWatcher.submit("x")(() => Future { ran.incrementAndGet(); 1 })
      Await.result(f, 2.seconds) === 1
      WriteWatcher.completedCount === 0L
      WriteWatcher.awaitQuiescence() === Nil
    }
  }

  "enabled WriteWatcher" should {

    "throttle concurrent writes to maxConcurrent and complete them all" in {
      WriteWatcher.reset()
      WriteWatcher.enable()
      try {
        val inFlight = new AtomicInteger(0)
        val peak = new AtomicInteger(0)
        val gates = (1 to 20).map(_ => Promise[Int]())

        val futures = gates.map { p =>
          WriteWatcher.submit("w") { () =>
            val n = inFlight.incrementAndGet()
            peak.updateAndGet(m => math.max(m, n))
            p.future.map { v => inFlight.decrementAndGet(); v }
          }
        }

        // nothing finishes until we release the gates
        Thread.sleep(200)
        gates.zipWithIndex.foreach { case (p, i) => p.success(i) }

        val failures = WriteWatcher.awaitQuiescence(settle = 500.millis)
        Await.result(Future.sequence(futures), 5.seconds).sum === (0 until 20).sum
        failures === Nil
        WriteWatcher.completedCount === 20L
        peak.get() must be_<=(4)
      } finally WriteWatcher.reset()
    }

    "record and surface a failed write" in {
      WriteWatcher.reset()
      WriteWatcher.enable()
      try {
        val boom = new RuntimeException("boom")
        val f = WriteWatcher.submit("bad")(() => Future.failed[Int](boom))
        Await.result(f.failed, 2.seconds) === boom

        val failures = WriteWatcher.awaitQuiescence(settle = 300.millis)
        failures.map(_._1) === Seq("bad")
        failures.map(_._2) === Seq(boom)
      } finally WriteWatcher.reset()
    }

    "drain writes submitted from an earlier write's completion callback" in {
      WriteWatcher.reset()
      WriteWatcher.enable()
      try {
        val count = new AtomicInteger(0)
        def chain(n: Int): Future[Int] =
          WriteWatcher.submit(s"w$n") { () =>
            Future {
              count.incrementAndGet()
              if (n > 0) { chain(n - 1); () }
              n
            }
          }
        Await.result(chain(30), 5.seconds) === 30
        WriteWatcher.awaitQuiescence(settle = 500.millis) === Nil
        count.get() === 31
        WriteWatcher.completedCount === 31L
        WriteWatcher.inFlightCount === 0
      } finally WriteWatcher.reset()
    }

    "report (completed, submitted) progress while draining" in {
      WriteWatcher.reset()
      WriteWatcher.enable()
      try {
        val gates = (1 to 8).map(_ => Promise[Int]())
        val futures = gates.map(p => WriteWatcher.submit("w")(() => p.future))

        WriteWatcher.submittedCount === 8L

        @volatile var lastDone = -1L
        @volatile var lastSubmitted = -1L
        val progress = Future {
          WriteWatcher.awaitQuiescence(
            settle = 300.millis,
            onProgress = (done, submitted) => { lastDone = done; lastSubmitted = submitted }
          )
        }

        Thread.sleep(150)
        lastDone === 0L
        gates.take(5).zipWithIndex.foreach { case (p, i) => p.success(i) }
        Thread.sleep(300)
        lastDone must be_>=(5L)
        gates.drop(5).zipWithIndex.foreach { case (p, i) => p.success(i) }

        Await.result(progress, 5.seconds) === Nil
        Await.result(Future.sequence(futures), 5.seconds)
        lastDone === 8L
        lastSubmitted === 8L
      } finally WriteWatcher.reset()
    }

    "propagate a benign failure to the caller but not record it" in {
      WriteWatcher.reset()
      WriteWatcher.enable()
      try {
        val conflict = new RuntimeException("editconflict")
        val f =
          WriteWatcher.submit("conflicted", benign = _ => true)(() => Future.failed[Int](conflict))
        Await.result(f.failed, 2.seconds) === conflict

        val failures = WriteWatcher.awaitQuiescence(settle = 300.millis)
        failures === Nil
        WriteWatcher.completedCount === 1L
      } finally WriteWatcher.reset()
    }
  }
}
