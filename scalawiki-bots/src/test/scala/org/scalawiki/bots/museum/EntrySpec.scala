package org.scalawiki.bots.museum

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.FicusConfig
import org.specs2.mutable.Specification

class EntrySpec extends Specification {

  val entry0 = Entry("dir", article = Some("article"))

  "fromRow" should {
    "get dir" in {
      Entry.fromRow(Seq("dir")) === Entry("dir")
    }

    "get dir and article" in {
      Entry.fromRow(Seq("dir", "article")) === Entry("dir", Some("article"))
    }

    "get dir, article and wlmId" in {
      Entry.fromRow(Seq("dir", "article", "wlmId")) === Entry(
        "dir",
        Some("article"),
        Some("wlmId")
      )
    }

    "get dir, article, wlmId and extra column" in {
      Entry.fromRow(Seq("dir", "article", "wlmId", "something else")) === Entry(
        "dir",
        Some("article"),
        Some("wlmId")
      )
    }

    "get dir, empty article and empty wlmId" in {
      Entry.fromRow(Seq("dir", " ", " ")) === Entry("dir")
    }

    "get dir, article and empty wlmId" in {
      Entry.fromRow(Seq("dir", "article", " ")) === Entry(
        "dir",
        Some("article")
      )
    }
  }

  "imagesMaps" should {
    "be empty when no images" in {
      Entry("dir").imagesMaps === Seq.empty
    }

    "default article to dir" in {
      Entry(
        "dir",
        images = Seq(
          EntryImage("image", Some("description"))
        )
      ).genImageFields.imagesMaps === Seq(
        Map(
          "title" -> "dir 1",
          "file" -> "image",
          "description" -> "{{uk|description, [[:uk:dir|]]}}",
          "source-description" -> "description"
        )
      )
    }

    "use article" in {
      Entry(
        "dir",
        Some("article"),
        images = Seq(
          EntryImage("image", Some("description"))
        )
      ).genImageFields.imagesMaps === Seq(
        Map(
          "title" -> "article 1",
          "file" -> "image",
          "description" -> "{{uk|description, [[:uk:article|]]}}",
          "source-description" -> "description"
        )
      )
    }

    "use parent wlm id" in {
      Entry(
        "dir",
        Some("article"),
        images = Seq(
          EntryImage("image", Some("description"))
        ),
        wlmId = Some("parent-wlm-id")
      ).genImageFields.imagesMaps === Seq(
        Map(
          "title" -> "article 1",
          "file" -> "image",
          "description" -> "{{uk|description, [[:uk:article|]]}} {{Monument Ukraine|parent-wlm-id}}",
          "source-description" -> "description"
        )
      )
    }

    "override parent wlm id" in {
      val entry = Entry(
        "dir",
        Some("article"),
        images = Seq(
          EntryImage("image1", Some("description1"), wlmId = None),
          EntryImage(
            "image2",
            Some("description2"),
            wlmId = Some("specific-wlm-id")
          )
        ),
        wlmId = Some("parent-wlm-id")
      ).genImageFields

      val maps = entry.imagesMaps

      maps.size === 2

      maps.head === Map(
        "title" -> "article 1",
        "file" -> "image1",
        "description" -> "{{uk|description1, [[:uk:article|]]}} {{Monument Ukraine|parent-wlm-id}}",
        "source-description" -> "description1"
      )

      maps.last === Map(
        "title" -> "article 2",
        "file" -> "image2",
        "description" -> "{{uk|description2, [[:uk:article|]]}} {{Monument Ukraine|specific-wlm-id}}",
        "source-description" -> "description2",
        "wlm-id" -> "specific-wlm-id"
      )
    }
  }

  "to/fromConfig" should {

    def roundTrip(entry: Entry, dir: String): Entry =
      Entry.fromConfig(entry.toConfig, dir)

    "map dir to article" in {
      import net.ceedubs.ficus.Ficus._

      val cfg: FicusConfig = Entry("dir").toConfig
      cfg.as[String]("article") === "dir"
      cfg.as[Seq[String]]("images") === Seq.empty
    }

    "map dir" in {
      roundTrip(entry0, "dir") === entry0
    }

    "read image" in {
      val entry = entry0
        .copy(
          images = Seq(
            EntryImage("image", Some("description"))
          )
        )
        .genImageFields

      roundTrip(entry, "dir") === entry
    }

    "read parent wlmId" in {
      val entry = entry0
        .copy(
          wlmId = Some("wlm-id"),
          images = Seq(
            EntryImage("image", Some("description"))
          )
        )
        .genImageFields

      roundTrip(entry, "dir") === entry
    }

    "read entry and image wlmId" in {
      val entry = entry0
        .copy(
          wlmId = Some("parent-wlm-id"),
          images = Seq(
            EntryImage("image", Some("description"), wlmId = Some("image-wlm"))
          )
        )
        .genImageFields

      roundTrip(entry, "dir") === entry
    }

    "read uploadTitle" in {
      val origEntry = Entry(
        "dir",
        article = Some("article"),
        images = Seq(
          EntryImage("image1", Some("description1"))
        )
      ).genImageFields

      val str =
        origEntry.toConfig.root().render().replace("article 1", "article 2")
      val cfg = ConfigFactory.parseString(str)
      val entry = Entry.fromConfig(cfg, "dir")
      val image = entry.images.head
      image.uploadTitle === Some("article 2")
    }

    "read wiki-description" in {
      val origEntry = Entry(
        "dir",
        article = Some("article"),
        images = Seq(
          EntryImage("image1", Some("description1"))
        )
      ).genImageFields

      val str = origEntry.toConfig
        .root()
        .render()
        .replace("{{uk|description1", "{{uk|description2")
      val cfg = ConfigFactory.parseString(str)
      val entry = Entry.fromConfig(cfg, "dir")
      val image = entry.images.head
      image.wikiDescription === Some("{{uk|description2, [[:uk:article|]]}}")
    }
  }

  "diff reporter" should {

    "be quite" in {
      val image =
        EntryImage("image", Some("description1"), Some("wiki-description1"))
      val entry = entry0.copy(wlmId = Some("wlm-id"), images = Seq(image))

      entry.diff(entry) === Seq.empty
    }

    "tell image entry field change" in {

      val image = EntryImage(
        "image",
        Some("description1"),
        wikiDescription = Some("wiki-description1")
      )
      val entry = entry0.copy(wlmId = Some("wlm-id"), images = Seq(image))

      val changed =
        entry.copy(images = Seq(image.copy(wikiDescription = Some("wiki-description2"))))

      entry.diff(changed) === Seq(
        Diff(
          "images[0].wikiDescription",
          Some("wiki-description1"),
          Some("wiki-description2")
        )
      )
    }

    "tell entry field change" in {
      val entry = entry0.copy(wlmId = Some("wlm-id1"))
      val changed = entry.copy(wlmId = Some("wlm-id2"))
      entry.diff(changed) === Seq(
        Diff("wlmId", Some("wlm-id1"), Some("wlm-id2"))
      )
    }

    "tell all fields change" in {

      val image1 = EntryImage(
        "image1",
        Some("description1"),
        Some("upload-title1"),
        Some("wiki-description1"),
        Some("wlm-id11")
      )
      val entry1 = Entry("dir1", Some("article1"), Some("wlm-id1"), Seq(image1))

      val image2 = EntryImage(
        "image2",
        Some("description2"),
        Some("upload-title2"),
        Some("wiki-description2"),
        Some("wlm-id12")
      )
      val entry2 = Entry("dir2", Some("article2"), Some("wlm-id2"), Seq(image2))

      entry1.diff(entry2) === Seq(
        Diff("dir", "dir1", "dir2"),
        Diff("article", Some("article1"), Some("article2")),
        Diff("wlmId", Some("wlm-id1"), Some("wlm-id2")),
        Diff("images[0].filePath", "image1", "image2"),
        Diff(
          "images[0].uploadTitle",
          Some("upload-title1"),
          Some("upload-title2")
        ),
        Diff(
          "images[0].sourceDescription",
          Some("description1"),
          Some("description2")
        ),
        Diff(
          "images[0].wikiDescription",
          Some("wiki-description1"),
          Some("wiki-description2")
        ),
        Diff("images[0].wlmId", Some("wlm-id11"), Some("wlm-id12"))
      )
    }
  }

  "update from" should {

    "change nothing" in {

      val image1 = EntryImage(
        "image1",
        Some("description1"),
        Some("upload-title1"),
        Some("wiki-description1"),
        Some("wlm-id11")
      )
      val entry1 = Entry("dir1", Some("article1"), Some("wlm-id1"), Seq(image1))

      val image2 = EntryImage(
        "image2",
        Some("description2"),
        Some("upload-title2"),
        Some("wiki-description2"),
        Some("wlm-id12")
      )
      val entry2 = Entry("dir2", Some("article2"), Some("wlm-id2"), Seq(image2))

      val updated = entry1.updateFrom(
        entry2,
        Set.empty,
        Set.empty
      )
      updated === entry1
    }

    "change all" in {

      val image1 = EntryImage(
        "image1",
        Some("description1"),
        Some("upload-title1"),
        Some("wiki-description1"),
        Some("wlm-id11")
      )
      val entry1 = Entry("dir1", Some("article1"), Some("wlm-id1"), Seq(image1))

      val image2 = EntryImage(
        "image2",
        Some("description2"),
        Some("upload-title2"),
        Some("wiki-description2"),
        Some("wlm-id12")
      )
      val entry2 = Entry("dir2", Some("article2"), Some("wlm-id2"), Seq(image2))

      val updated = entry1.updateFrom(
        entry2,
        Set("dir", "article", "wlmId"),
        Set(
          "filePath",
          "uploadTitle",
          "sourceDescription",
          "wikiDescription",
          "wlmId"
        )
      )
      updated === entry2
    }
  }
}
