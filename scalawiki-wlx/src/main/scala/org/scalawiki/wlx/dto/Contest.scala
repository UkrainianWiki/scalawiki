package org.scalawiki.wlx.dto

import java.time.ZonedDateTime

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions, ConfigResolveOptions}
import org.scalawiki.wlx.stat.rating.RateConfig

import scala.util.Try

/** Represents Wiki Loves X contest
  *
  * @param contestType
  * @param country
  * @param year
  * @param startDate
  * @param endDate
  * @param uploadConfigs
  * @param specialNominations
  */
case class Contest(
    contestType: ContestType,
    country: Country,
    year: Int,
    startDate: String = "",
    endDate: String = "",
    uploadConfigs: Seq[UploadConfig] = Nil,
    specialNominations: Seq[SpecialNomination] = Nil,
    rateConfig: RateConfig = RateConfig(),
    config: Option[Config] = None
) extends HasImagesCategory {

  def campaign: String = contestType.code + "-" + country.code

  def name: String = s"${contestType.name} $year" + countryName.fold("")(" in " + _)

  def countryName: Option[String] =
    if (country != NoAdmDivision)
      Some(country.name)
    else
      None

  /** @return
    *   Name of category containing contest images
    */
  override def imagesCategory: String =
    s"Category:Images from $name".replaceAll(" ", "_")

  /** @return
    *   name of template that monument lists consist of
    */
  def listTemplate: Option[String] =
    uploadConfigs.headOption.map(_.listTemplate)

  /** @return
    *   name of template that marks a contest image with monument id
    */
  def fileTemplate: Option[String] =
    uploadConfigs.headOption.map(_.fileTemplate)

  def listsHost: Option[String] =
    uploadConfigs.head.listsHost
      .orElse(
        country.languageCodes.headOption.map(_ + ".wikipedia.org")
      )

  /** Calendar dates for `forYear` (defaults to this contest's year) from the `dates.<year>` block
    * of the campaign `.conf`, when present. Dates are end-of-day in `dates.timezone` (per-year
    * `timezone` overrides it; UTC when neither is set).
    */
  def dates(forYear: Int = year): Option[ContestDates] =
    config
      .filter(_.hasPath(s"dates.$forYear"))
      .map { c =>
        val campaignZone = ContestDates.zoneOf(c.getConfig("dates"))
        ContestDates.fromConfig(c.getConfig(s"dates.$forYear"), campaignZone)
      }

}

object Contest {

  private val opts = ConfigParseOptions.defaults.setAllowMissing(false)

  def load(name: String): Option[Contest] = {
    Try {
      ConfigFactory.load(name, opts, ConfigResolveOptions.defaults)
    }.map(fromConfig)
      .getOrElse {
        val Campaign = "(\\w+)_(\\w+).conf".r
        name match {
          case Campaign(typeCode, countryCode) =>
            for (
              contestType <- ContestType.byCode(typeCode);
              country <- Country.byCode(countryCode)
            ) yield Contest(contestType, country, ZonedDateTime.now.getYear)
          case _ => None
        }
      }
  }

  def byCampaign(campaign: String): Option[Contest] = {
    load(campaign.replace("-", "_") + ".conf")
  }

  /** The campaign's contest, pinned to `year` and `rateConfig`.
    *
    * @throws IllegalArgumentException
    *   if `campaign` does not resolve to a contest
    */
  def byCampaign(campaign: String, year: Int, rateConfig: RateConfig = RateConfig()): Contest =
    byCampaign(campaign)
      .getOrElse(throw new IllegalArgumentException(s"Unknown campaign: $campaign"))
      .copy(year = year, rateConfig = rateConfig)

  def fromConfig(config: Config): Option[Contest] = {
    val (typeStr, countryStr, year) = (
      config.getString("type"),
      config.getString("country"),
      ZonedDateTime.now.getYear
    )

    val uploadConfig = UploadConfig.fromConfig(config)

    for (
      contestType <- ContestType.byCode(typeStr.toLowerCase);
      country <- Country.fromJavaLocales
        .find(country => country.name == countryStr || country.code == countryStr)
    )
      yield new Contest(
        contestType = contestType,
        country = country,
        year = year,
        uploadConfigs = Seq(uploadConfig),
        config = Some(config),
        specialNominations = SpecialNomination.nominations
          .filter(n => n.years.contains(year))
      )
  }

  def ESPCUkraine(
      year: Int,
      startDate: String = "01-09",
      endDate: String = "30-09"
  ) =
    new Contest(
      ContestType.ESPC,
      Country.Ukraine,
      year,
      startDate,
      endDate,
      Nil
    )

  def WLMUkraine(year: Int): Contest =
    load("wlm_ua.conf").get.copy(year = year)

  def WLEUkraine(year: Int): Contest =
    load("wle_ua.conf").get.copy(year = year)

}
