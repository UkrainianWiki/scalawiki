package org.scalawiki.wlx.dto.lists

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.immutable.ListMap

/** Maps constant English field names to possibly localized field names in monument lists
  */
trait ListConfig {
  def templateName: String

  def namesMap: Map[String, String]

}

case class ListConfigImpl(templateName: String, namesMap: Map[String, String]) extends ListConfig

object ListConfig {

  import scala.collection.JavaConverters._

  def load(name: String): ListConfig = {
    fromConfig(ConfigFactory.load(name))
  }

  def fromConfig(c: Config): ListConfig = {

    val names = c.getConfigList("names")
    val kvs = names.asScala.map { n =>
      val entry = n.entrySet().iterator().next()
      entry.getKey -> entry.getValue.unwrapped().toString
    }

    new ListConfigImpl(c.getString("listTemplate"), ListMap(kvs.toSeq: _*))
  }

  val WleUa = load("wle_ua.conf")
  val WlmUa = load("wlm_ua.conf")
  val WleTh = load("wle_th.conf")
}

class OtherTemplateListConfig(val templateName: String, base: ListConfig) extends ListConfig {
  override def namesMap: Map[String, String] = base.namesMap
}

object EmptyListConfig extends ListConfig {
  override def templateName: String = "???"

  override def namesMap: Map[String, String] = Map.empty
}
