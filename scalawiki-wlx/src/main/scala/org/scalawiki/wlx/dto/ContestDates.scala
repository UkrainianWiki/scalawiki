package org.scalawiki.wlx.dto

import com.typesafe.config.Config

import java.time.{LocalDate, ZoneId, ZoneOffset, ZonedDateTime}
import scala.util.Try

/** Key calendar dates for one contest year, read from the `dates.<year>` block of the campaign
  * `.conf` (see `wlm_ua.conf` / `wle_ua.conf`). Every field is optional: a missing block or key
  * means "not recorded, fall back to a default".
  *
  * The on-wiki rules state each cut-off as end-of-day in the contest's local time. That zone comes
  * from a `timezone` key (on the `dates` block, overridable per year); it defaults to UTC for
  * campaigns that don't set one.
  *
  * @param uploadStart
  *   first day of the on-wiki submission window (Регламент п. 5.2)
  * @param uploadEnd
  *   last day of the submission window; used as the "cache was accurate until" cut-off when
  *   `--csv-cache-resync` re-checks a frozen past year
  * @param latestAllowedPicturedDate
  *   photos for the main nominations must be created on or before this date (Регламент п. 9.1 / п.
  *   5.2 — a wartime security rule). Absent for the pre-2022 years, which had no such restriction.
  * @param zone
  *   time zone the dates above are end-of-day in
  */
case class ContestDates(
    uploadStart: Option[LocalDate] = None,
    uploadEnd: Option[LocalDate] = None,
    latestAllowedPicturedDate: Option[LocalDate] = None,
    zone: ZoneId = ZoneOffset.UTC
) {

  private def endOfDay(d: LocalDate): ZonedDateTime =
    d.atTime(23, 59, 59).atZone(zone)

  def uploadEndInstant: Option[ZonedDateTime] = uploadEnd.map(endOfDay)

  def latestAllowedPicturedInstant: Option[ZonedDateTime] =
    latestAllowedPicturedDate.map(endOfDay)
}

object ContestDates {

  private def dateAt(config: Config, key: String): Option[LocalDate] =
    if (config.hasPath(key)) Try(LocalDate.parse(config.getString(key))).toOption
    else None

  def zoneOf(config: Config, fallback: ZoneId = ZoneOffset.UTC): ZoneId =
    if (config.hasPath("timezone"))
      Try(ZoneId.of(config.getString("timezone"))).getOrElse(fallback)
    else fallback

  /** @param config
    *   the `dates.<year>` block
    * @param defaultZone
    *   zone to use when the year block has no `timezone` of its own
    */
  def fromConfig(config: Config, defaultZone: ZoneId = ZoneOffset.UTC): ContestDates =
    ContestDates(
      uploadStart = dateAt(config, "upload-start"),
      uploadEnd = dateAt(config, "upload-end"),
      latestAllowedPicturedDate = dateAt(config, "latest-allowed-pictured-date"),
      zone = zoneOf(config, defaultZone)
    )
}
