package org.scalawiki.wlx.stat.reports

import org.scalawiki.util.{DryRun, WriteWatcher}
import org.scalawiki.wlx.stat.progress.Progress
import org.scalawiki.wlx.stat.{ContestStat, StatConfig}

import scala.concurrent.ExecutionContext.Implicits.global

/** Turns gathered contest data into published output: runs every configured
  * report, then blocks until every wiki write it triggered has settled.
  *
  * Each report step is isolated (via [[ReporterRegistry]]) so one failing step
  * no longer aborts the rest; recovered edit conflicts are not counted as
  * failures.
  */
class ReportRunner(stat: ContestStat, config: StatConfig) {

  /** Run the reports, wait for publishing, print the `=== Publish summary ===`.
    *
    * @return the number of failures (report steps that threw + wiki writes that
    *         errored). 0 means a clean run.
    */
  def run(): Int = {
    val stepErrors =
      Progress.bar("Generating reports", 0L) { task =>
        new ReporterRegistry(stat, config, Some(task)).output()
      }
    // Publishing is the long pole on a cached run: dozens of throttled edits
    // draining a few at a time. Drive a bar off WriteWatcher's counters.
    val writeFailures =
      Progress.bar(
        if (DryRun.isEnabled) "Saving dry-run output" else "Publishing edits",
        WriteWatcher.submittedCount
      ) { task =>
        WriteWatcher.awaitQuiescence(onProgress = (done, submitted) => {
          task.total(submitted)
          task.stepTo(done)
        })
      }

    if (stepErrors.nonEmpty || writeFailures.nonEmpty) {
      Progress.note("\n=== Publish summary: INCOMPLETE ===")
      if (stepErrors.nonEmpty) {
        Progress.note(s"report steps that failed: ${stepErrors.size}")
        stepErrors.foreach { case (name, e) => Progress.note(s"  - $name: $e") }
      }
      if (writeFailures.nonEmpty) {
        Progress.note(
          s"wiki writes that errored: ${writeFailures.size} " +
            "(edit conflicts the list updater retries are excluded)"
        )
        writeFailures.foreach { case (desc, e) => Progress.note(s"  - $desc: $e") }
      }
    } else if (DryRun.isEnabled) {
      Progress.note(
        s"\n=== Dry run: OK (${DryRun.skippedCount} wiki writes not published; " +
          s"edit text saved under ${DryRun.outputDir.getOrElse("")}) ==="
      )
    } else {
      Progress.note(
        s"\n=== Publish summary: OK (${WriteWatcher.completedCount} wiki writes) ==="
      )
    }

    stepErrors.size + writeFailures.size
  }
}
