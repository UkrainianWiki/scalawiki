package org.scalawiki.wlx.stat

import com.typesafe.config.ConfigFactory
import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.SpecialNomination
import org.specs2.mutable.Specification

class SpecialNominationsSpec extends Specification {

  "SpecialNominations" should {
    "load empty" in {
      SpecialNomination.load("wle_ua.conf") === Seq.empty
    }

    "load all" in {

      val expected = Seq(
        new SpecialNomination(
          "Музичні пам'ятки в Україні",
          Some("WLM-рядок"),
          Seq("Template:WLM-music-navbar")
        ),
        new SpecialNomination(
          "Замки і фортеці України",
          Some("WLM-рядок"),
          Seq("Template:WLM замки і фортеці")
        ),
        new SpecialNomination(
          "Кримськотатарські пам'ятки в Україні",
          Some("WLM-рядок"),
          Seq(
            "Вікіпедія:Вікі любить пам'ятки/Кримськотатарські пам'ятки в Україні"
          )
        ),
        new SpecialNomination(
          "Пам'ятки національно-визвольної боротьби",
          Some("WLM-рядок"),
          Seq(
            "Вікіпедія:Вікі любить пам'ятки/Пам'ятки національно-визвольної боротьби"
          )
        ),
        new SpecialNomination(
          "Грецькі пам'ятки в Україні",
          Some("WLM-рядок"),
          Seq("Вікіпедія:Вікі любить пам'ятки/Грецькі пам'ятки в Україні")
        ),
        new SpecialNomination(
          "Вірменські пам'ятки в Україні",
          Some("WLM-рядок"),
          Seq("Вікіпедія:Вікі любить пам'ятки/Вірменські пам'ятки в Україні")
        ),
        new SpecialNomination(
          "Бібліотеки",
          Some("WLM-рядок"),
          Seq("Вікіпедія:Вікі любить пам'ятки/Бібліотеки")
        ),
        new SpecialNomination(
          "Українські пам'ятки Першої світової війни",
          Some("WLM-рядок"),
          Seq(
            "Вікіпедія:Вікі любить пам'ятки/Українські пам'ятки Першої світової війни"
          )
        ),
        new SpecialNomination(
          "Цивільні споруди доби Гетьманщини",
          Some("WLM-рядок"),
          Seq("Template:WLM цивільні споруди доби Гетьманщини")
        ),
        new SpecialNomination(
          "Млини",
          Some("WLM-рядок"),
          Seq("Template:WLM млини та вітряки"),
          Seq(2019, 2020, 2021),
          idPrefix = Some("88")
        ),
        new SpecialNomination(
          "Єврейська спадщина",
          Some("WLM-рядок"),
          Seq("Template:WLM єврейська спадщина"),
          Seq(2019, 2020, 2021),
          idPrefix = Some("99")
        ),
        new SpecialNomination(
          "Віа Регіа",
          Some("ВЛП-рядок"),
          Nil,
          Seq(2020, 2021),
          Nil
        ),
        new SpecialNomination(
          "Квіти України",
          Some("WLM-рядок"),
          Seq("Template:WLM Квіти України"),
          Seq(2021),
          idPrefix = Some("98")
        ),
        new SpecialNomination(
          "Національно-визвольні",
          Some("WLM-рядок"),
          Seq("Template:WLM національно-визвольні"),
          Seq(2021),
          idPrefix = Some("97")
        ),
        new SpecialNomination(
          "Пам'ятки Подесення",
          Some("WLM-рядок"),
          Seq("Template:WLM Пам'ятки Подесення"),
          Seq(2021),
          idPrefix = Some("95")
        ),
        new SpecialNomination(
          "Аерофото",
          None,
          Nil,
          Seq(2021),
          Nil,
          Some("WLM2021-UA-Aero")
        )
      )

      SpecialNomination
        .load("wlm_ua.conf")
        .map(_.copy(cities = Nil))
        .filterNot(sn => sn.years.nonEmpty && sn.years.min >= 2022)
        .map(sn => sn.copy(years = sn.years.filterNot(_ >= 2022))) === expected
    }

    "load the video nomination" in {
      SpecialNomination
        .load("wlm_ua.conf")
        .find(_.name == "Відео")
        .map(sn => (sn.mediaType, sn.years)) ===
        Some((Some(SpecialNomination.Video), Seq(2024, 2025, 2026)))
    }

    "read the id prefixes" in {
      val config = ConfigFactory.parseString(
        """nominations: [{name: "Єврейська спадщина", idPrefix: "99"}, {name: "Бібліотеки"}]"""
      )
      SpecialNomination.fromConfig(config).map(_.idPrefix) === Seq(Some("99"), None)
    }

    "give each nomination of a year its own id prefix" in {
      val nominations = SpecialNomination.load("wlm_ua.conf")
      forall(2012 to 2030) { year =>
        val prefixes = nominations.filter(_.inYear(year)).flatMap(_.idPrefix)
        prefixes.diff(prefixes.distinct) must beEmpty
      }
    }

    "name the id prefixes of a year's nominations" in {
      val nominations = SpecialNomination.load("wlm_ua.conf")
      SpecialNomination.byIdPrefix(nominations, 2025).map { case (p, n) => p -> n.name } === Map(
        "00" -> "Війна руйнує пам'ятки",
        "93" -> "Польська спадщина",
        "94" -> "Німецька спадщина",
        "96" -> "Пам'ятки дерев'яної архітектури України",
        "99" -> "Єврейська спадщина"
      )
      SpecialNomination.byIdPrefix(nominations, 2026).keySet === Set(
        "00",
        "90",
        "92",
        "93",
        "94",
        "96",
        "99"
      )
    }

    "name the id prefixes of any year's nominations" in {
      val byPrefix = SpecialNomination.byIdPrefix(SpecialNomination.load("wlm_ua.conf"))
      byPrefix.keySet === Set("00", "88", "90", "92", "93", "94", "95", "96", "97", "98", "99")
      byPrefix("88").name === "Млини"
    }

    "reject an unknown media type" in {
      val config =
        ConfigFactory.parseString("""nominations: [{name: "Аудіо", mediaType: "audio"}]""")
      SpecialNomination.fromConfig(config) must throwA[IllegalArgumentException]
    }
  }

  "matchesFile" should {
    val video = SpecialNomination("Відео", None, Nil, mediaType = Some(SpecialNomination.Video))
    val film = SpecialNomination("Плівка", None, Nil, fileTemplate = Some("WLM2026-UA-film"))

    "take any video for a video nomination" in {
      video.matchesFile(Image("File:Church.ogv", mediaType = Some("VIDEO"))) must beTrue
      video.matchesFile(Image("File:Church.jpg", mediaType = Some("BITMAP"))) must beFalse
    }

    "take files with the nomination's file template" in {
      film.matchesFile(
        Image("File:Church.jpg", specialNominations = Set("WLM2026-UA-film"))
      ) must beTrue
      film.matchesFile(Image("File:Church.webm", mediaType = Some("VIDEO"))) must beFalse
    }
  }
}
