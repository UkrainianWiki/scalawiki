package org.scalawiki.wlx.stat.progress

import me.tongfei.progressbar.{ProgressBar, ProgressBarBuilder, ProgressBarStyle}
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** A running unit of work whose item count is known (or becomes known). Handed to the body of
  * [[Progress.bar]] / [[Progress.barF]]. All methods are cheap and thread-safe, so callers can
  * drive them from `Future` callbacks.
  */
trait ProgressTask {

  /** Advance the counter by `n` items. */
  def step(n: Long = 1): Unit

  /** Set the counter to an absolute value (useful when the caller already tracks a running total,
    * e.g. an `AtomicInteger`).
    */
  def stepTo(n: Long): Unit

  /** (Re)set the total once it is known. `n <= 0` marks the task indeterminate. */
  def total(n: Long): Unit

  /** A short trailing note shown after the bar (e.g. the current sub-step). */
  def msg(s: String): Unit
}

/** Console progress for the stats CLI.
  *
  * Detailed logging goes to `logs/scalawiki.log` (see
  * `scalawiki-core/src/main/resources/logback.xml`); this object is the *only* thing that writes
  * progress to the screen. It degrades cleanly:
  *
  *   - interactive terminal, progress enabled -> a live [[me.tongfei.progressbar.ProgressBar]] on
  *     stderr (items done/total, speed, elapsed, ETA);
  *   - otherwise (fully redirected/CI, or `--no-progress`) -> throttled INFO lines to the log, no
  *     screen output.
  *
  * Both the bar and the user-facing [[note]] lines render on stderr, so they never mix with
  * report/wikitext written to stdout (`run-stats.sh > report.txt` captures only the report).
  */
object Progress {

  private val log = LoggerFactory.getLogger("org.scalawiki.wlx.progress")

  @volatile private var enabled: Boolean = true

  /** Whether a real terminal is attached to stderr, where the bar renders.
    *
    * `System.console()` needs *both* stdin and stdout on a tty, so it goes null under
    * `run-stats.sh > report.txt` even though stderr is still a terminal — hence the env fallback.
    * In CI / fully-redirected runs none of these hold and we degrade to log lines. Cached: this
    * doesn't change over a run.
    */
  private lazy val interactive: Boolean = {
    def env(name: String) = Option(System.getenv(name)).exists(_.nonEmpty)
    val ci = env("CI") || env("GITHUB_ACTIONS") || env("BUILD_NUMBER")
    !ci && (
      System.console() != null ||
        Option(System.getenv("TERM")).exists(t => t.nonEmpty && t != "dumb") ||
        env("WT_SESSION") // Windows Terminal
    )
  }

  private val openBars = new ConcurrentLinkedQueue[ProgressBar]()

  /** Enable or disable the live display (from `--no-progress`). Off still logs progress to the
    * file.
    */
  def configure(on: Boolean): Unit = enabled = on

  private def live: Boolean = enabled && interactive

  // -- important, user-facing lines -----------------------------------------

  /** Print a line the user should see regardless of verbosity (phase results, the publish summary).
    * Goes to stderr — alongside the progress bar and away from report/wikitext on stdout — and to
    * the log file.
    */
  def note(line: String): Unit = {
    log.info(line)
    // A newline first so the line doesn't land on top of a live bar.
    if (live) System.err.println()
    System.err.println(line)
  }

  // -- indeterminate phases ------------------------------------------------

  def phase[T](label: String)(body: => T): T = {
    val t0 = System.nanoTime()
    emitStart(label)
    try {
      val r = body
      emitDone(label, t0, None)
      r
    } catch {
      case NonFatal(e) => emitDone(label, t0, Some(e)); throw e
    }
  }

  def phaseF[T](
      label: String
  )(body: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val t0 = System.nanoTime()
    emitStart(label)
    val f =
      try body
      catch { case NonFatal(e) => Future.failed(e) }
    f.onComplete(res => emitDone(label, t0, res.failed.toOption))
    f
  }

  // -- determinate bars --------------------------------------------------

  def bar[T](label: String, total: Long)(body: ProgressTask => T): T = {
    val t0 = System.nanoTime()
    emitStart(label)
    val task = newTask(label, total)
    try {
      val r = body(task)
      task.close()
      emitDone(label, t0, None)
      r
    } catch {
      case NonFatal(e) => task.close(); emitDone(label, t0, Some(e)); throw e
    }
  }

  def barF[T](label: String, total: Long)(
      body: ProgressTask => Future[T]
  )(implicit ec: ExecutionContext): Future[T] = {
    val t0 = System.nanoTime()
    emitStart(label)
    val task = newTask(label, total)
    val f =
      try body(task)
      catch { case NonFatal(e) => Future.failed(e) }
    f.onComplete { res =>
      task.close()
      emitDone(label, t0, res.failed.toOption)
    }
    f
  }

  /** Close any progress bar still open (safety net for the CLI's `finally`). */
  def close(): Unit = {
    var pb = openBars.poll()
    while (pb != null) {
      try pb.close()
      catch { case NonFatal(_) => }
      pb = openBars.poll()
    }
  }

  // -- internals -------------------------------------------------------

  private def emitStart(label: String): Unit = {
    log.info(s"START $label")
    if (live) System.err.println(s"[>] $label")
  }

  private def emitDone(label: String, startNanos: Long, error: Option[Throwable]): Unit = {
    val secs = (System.nanoTime() - startNanos) / 1e9
    error match {
      case None =>
        log.info(f"DONE  $label ($secs%.1fs)")
        if (live) System.err.println(f"[OK] $label ($secs%.1fs)")
      case Some(e) =>
        log.warn(f"FAILED $label ($secs%.1fs): $e")
        if (live) System.err.println(f"[!!] $label ($secs%.1fs): ${e.getMessage}")
    }
  }

  private trait CloseableTask extends ProgressTask { def close(): Unit }

  private val barLegendShown = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** The me.tongfei bar renders `<name> 42% [===>   ] 42/100 (0:00:12 / 0:00:41) 3.5/s` — the
    * parenthesised pair is *time elapsed / estimated time remaining*, with no labels. Spell it out
    * once, the first time a bar appears.
    */
  private def showBarLegend(): Unit =
    if (barLegendShown.compareAndSet(false, true))
      System.err.println(
        "    bar key:  done/total  |  elapsed / remaining  |  items per second"
      )

  private def newTask(label: String, total: Long): CloseableTask =
    if (live) { showBarLegend(); new BarTask(label, total) }
    else new LogTask(label, total)

  /** Backed by a real progress bar on stderr. */
  private class BarTask(label: String, total: Long) extends CloseableTask {
    private val pb: ProgressBar =
      new ProgressBarBuilder()
        .setTaskName(label)
        .setInitialMax(if (total <= 0) -1 else total)
        .setStyle(ProgressBarStyle.ASCII)
        .setUpdateIntervalMillis(300)
        .showSpeed()
        .build()
    openBars.add(pb)

    def step(n: Long): Unit = pb.stepBy(n)
    def stepTo(n: Long): Unit = pb.stepTo(n)
    def total(n: Long): Unit = pb.maxHint(if (n <= 0) -1 else n)
    def msg(s: String): Unit = pb.setExtraMessage(if (s.isEmpty) "" else s" $s")
    def close(): Unit = {
      openBars.remove(pb)
      try pb.close()
      catch { case NonFatal(_) => }
    }
  }

  /** Fallback: throttled progress lines to the log (file), ~1 every 2s. */
  private class LogTask(label: String, initialTotal: Long) extends CloseableTask {
    private val current = new AtomicLong(0L)
    @volatile private var max: Long = initialTotal
    @volatile private var extra: String = ""
    @volatile private var lastEmitNanos: Long = 0L
    private val startNanos = System.nanoTime()

    def step(n: Long): Unit = { current.addAndGet(n); maybeEmit() }
    def stepTo(n: Long): Unit = { current.set(n); maybeEmit() }
    def total(n: Long): Unit = { max = n; maybeEmit() }
    def msg(s: String): Unit = extra = s
    def close(): Unit = emit()

    private def maybeEmit(): Unit = {
      val now = System.nanoTime()
      if (now - lastEmitNanos > 2e9) { lastEmitNanos = now; emit() }
    }

    private def emit(): Unit = {
      val c = current.get()
      val secs = (System.nanoTime() - startNanos) / 1e9
      val rate = if (secs > 0) c / secs else 0.0
      val totalStr = if (max > 0) s"/$max" else ""
      val pctEta =
        if (max > 0 && rate > 0) {
          val pct = 100.0 * c / max
          val etaSecs = ((max - c) / rate).toLong
          f" ($pct%.0f%%) ETA ${fmtDuration(etaSecs)}"
        } else ""
      val extraStr = if (extra.nonEmpty) s" - $extra" else ""
      log.info(f"$label: $c$totalStr$pctEta ${rate}%.0f/s$extraStr")
    }
  }

  private def fmtDuration(totalSecs: Long): String = {
    val s = math.max(0L, totalSecs)
    f"${s / 60}%d:${s % 60}%02d"
  }
}
