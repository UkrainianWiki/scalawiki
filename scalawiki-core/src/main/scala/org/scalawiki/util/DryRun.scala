package org.scalawiki.util

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicLong

/** Process-wide "dry run" switch for outbound wiki writes.
  *
  * Once [[enable]]d, page edits are not sent: the text that would have been
  * published is saved to `<dir>/<host>/<title>.wiki` instead (a later edit of
  * the same page overwrites the file, as on the wiki), and file uploads are
  * skipped and only logged. Reads are unaffected, so a run still fetches and
  * builds everything it normally would. Disabled by default.
  */
object DryRun {

  @volatile private var dir: Option[Path] = None
  private val skipped = new AtomicLong(0L)

  def enable(outputDir: String): Unit = {
    dir = Some(Paths.get(outputDir))
    skipped.set(0L)
  }

  /** Disable and forget the count. Only useful for tests. */
  def reset(): Unit = {
    dir = None
    skipped.set(0L)
  }

  def isEnabled: Boolean = dir.isDefined

  def outputDir: Option[Path] = dir

  /** Wiki writes (edits + uploads) not published since [[enable]]. */
  def skippedCount: Long = skipped.get()

  /** Save the text of an unpublished edit; returns the file written. */
  def saveEdit(host: String, title: String, text: String): Path = {
    val base = dir.getOrElse(throw new IllegalStateException("DryRun is not enabled"))
    val file = base.resolve(fileName(host)).resolve(fileName(title) + ".wiki")
    Files.createDirectories(file.getParent)
    Files.write(file, text.getBytes(StandardCharsets.UTF_8))
    skipped.incrementAndGet()
    file
  }

  def skipUpload(): Unit = skipped.incrementAndGet()

  /** A page title as a single file name: subpage slashes, the namespace colon
    * and other characters Windows rejects become `_`. */
  private[util] def fileName(s: String): String =
    s.replaceAll("""[\\/:*?"<>|\p{Cntrl}]""", "_")
}
