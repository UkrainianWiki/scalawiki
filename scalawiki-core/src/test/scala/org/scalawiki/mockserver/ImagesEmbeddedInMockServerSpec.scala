package org.scalawiki.mockserver

import org.scalawiki.query.QueryLibrary
import org.scalawiki.util.TestUtils.resourceAsString
import spray.util.pimpFuture

class ImagesEmbeddedInMockServerSpec
    extends BaseMockServerSpec
    with QueryLibrary {

  "images" should {
    "successfully get images" in {
      val response =
        resourceAsString("/org/scalawiki/query/embeddedinIiPropRvProp.json")

      val action = Map(
        "action" -> "query",
        "geititle" -> "Template:UkrainianNaturalHeritageSite",
        "prop" -> "info|revisions|imageinfo",
        "rvprop" -> "ids|content|timestamp|user|userid",
        "iiprop" -> "timestamp|user|size",
        "generator" -> "embeddedin",
        "geilimit" -> "max"
      )

      stubOk(action, response)

      val bot = getBot

      val future = bot.run(
        imagesByGenerator(generatorWithTemplate("UkrainianNaturalHeritageSite"))
      )

      val info = future.await
      info should not(beEmpty)
      info.size === 50
      info.map(
        _.text.exists(_.contains("UkrainianNaturalHeritageSite"))
      ) === List.fill(50)(true)
    }

    "successfully get images new rvslots format" in {
      val response = resourceAsString(
        "/org/scalawiki/query/embeddedinIiPropRvPropRvSlots.json"
      )
      val action = Map(
        "action" -> "query",
        "geititle" -> "Template:UkrainianNaturalHeritageSite",
        "prop" -> "info|revisions|imageinfo",
        "rvprop" -> "ids|content|timestamp|user|userid",
        "iiprop" -> "timestamp|user|size",
        "generator" -> "embeddedin",
        "geilimit" -> "max",
        "rvslots" -> "main"
      )

      stubOk(action, response)

      val bot = getBot

      val future = bot.run(
        imagesByGenerator(
          generatorWithTemplate("UkrainianNaturalHeritageSite"),
          rvSlots = Some("main")
        )
      )

      val info = future.await
      info should not(beEmpty)
      info.size === 50
      info.map(
        _.text.exists(_.contains("UkrainianNaturalHeritageSite"))
      ) === List.fill(50)(true)
      info.map(_.images.nonEmpty) === List.fill(50)(true)
    }
  }
}
