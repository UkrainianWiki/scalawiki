package org.scalawiki.dto

case class MwException(
    code: String,
    info: String,
    params: Map[String, String] = Map.empty
) extends RuntimeException(
      s"MediaWiki Error: code: $code, info: $info, params: $params"
    ) {

  /** The page moved on under us since we read it — retrying the same edit just replays the stale
    * base revision, so the caller must re-read and rebuild (see `org.scalawiki.edit.PageUpdater`).
    */
  def conflict: Boolean = MwException.conflictCodes.contains(code)

  /** A transient server-side condition a backoff + retry clears on its own: replication lag, rate
    * limiting, a brief read-only window, an expired token.
    */
  def transient: Boolean = MwException.transientCodes.contains(code)

  /** Routine on a long batch run: recovered by the retry / re-read machinery, so worth a concise
    * log line rather than an error with a stack trace.
    */
  def expected: Boolean = conflict || transient
}

object MwException {

  /** API `error.code` values that mean another edit landed between our read and our write
    * (`editconflict`), the page was deleted (`pagedeleted`), or it was created by someone else
    * while we prepared a create (`articleexists`). Retrying unchanged replays the same stale base
    * revision, so the caller must re-read the page and rebuild the edit. See
    * `org.scalawiki.edit.PageUpdater`.
    */
  val conflictCodes: Set[String] =
    Set("editconflict", "pagedeleted", "articleexists")

  /** API `error.code` values for transient server-side conditions that a backoff + retry (or, for
    * `badtoken`, a token refresh + retry) clears:
    *   - `maxlag` — database replication lag
    *   - `ratelimited` — the bot hit its rate limit
    *   - `readonly` — the wiki is briefly in read-only mode
    *   - `badtoken` — the cached CSRF token expired mid-run
    */
  val transientCodes: Set[String] =
    Set("maxlag", "ratelimited", "readonly", "badtoken")

  /** Codes that are a normal, self-correcting part of a long run rather than a fault to surface
    * with a stack trace.
    */
  val expectedCodes: Set[String] = conflictCodes ++ transientCodes
}
