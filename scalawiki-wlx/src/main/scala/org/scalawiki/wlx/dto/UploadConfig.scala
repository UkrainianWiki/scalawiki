package org.scalawiki.wlx.dto

import com.typesafe.config.Config
import org.scalawiki.wlx.dto.lists.ListConfig

/** @param campaign
  *   name of upload campaign (https://commons.wikimedia.org/wiki/Commons:Upload_campaigns)
  * @param listTemplate
  *   name of template that monument lists consist of
  * @param fileTemplate
  *   name of template that marks a contest image with monument id
  * @param listConfig
  *   configuration of monument list fields
  */
case class UploadConfig(
    campaign: String,
    listTemplate: String,
    fileTemplate: String,
    listConfig: ListConfig,
    listsHost: Option[String] = None
)

object UploadConfig {
  def fromConfig(c: Config): UploadConfig = {

    val (campaign, listTemplate, fileTemplate) = (
      c.getString("campaign"),
      c.getString("listTemplate"),
      c.getString("fileTemplate")
    )

    val listConfig = ListConfig.fromConfig(c)

    new UploadConfig(campaign, listTemplate, fileTemplate, listConfig)
  }
}
