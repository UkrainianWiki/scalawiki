package org.scalawiki.wlx.stat.reports

import org.scalawiki.dto.Image
import org.scalawiki.wlx.DeletedImagesCsv
import org.scalawiki.wlx.DeletedImagesCsv.DeletedImage
import org.scalawiki.wlx.dto.Contest
import org.specs2.mutable.Specification

import java.nio.file.Files
import java.time.{LocalDate, ZonedDateTime}

class DeletedImagesSpec extends Specification {

  private val contest = Contest.WLMUkraine(2026)

  private def deleted(title: String, id: Long, author: String, detected: String) =
    DeletedImage(
      Image(
        title,
        pageId = Some(id),
        author = Some(author),
        monumentIds = Seq("07-101-0001"),
        date = Some(ZonedDateTime.parse("2026-09-03T10:00:00Z"))
      ),
      LocalDate.parse(detected)
    )

  "the deleted images page" should {
    "list each year's deleted files, newest year and newest found first, then the template's others" in {
      val dir = Files.createTempDirectory("deleted-images").toString
      DeletedImagesCsv.write(
        DeletedImagesCsv.filename(contest.campaign, 2026, dir),
        Seq(
          deleted("File:A.jpg", 1L, "Alice", "2026-10-01"),
          deleted("File:B.jpg", 2L, "Bob", "2026-10-20")
        )
      )
      DeletedImagesCsv.write(
        DeletedImagesCsv.filename(contest.campaign, 2024, dir),
        Seq(
          deleted("File:Old.jpg", 3L, "Carol", "2026-10-21")
        )
      )
      DeletedImagesCsv.write(
        DeletedImagesCsv.totalFilename(contest.campaign, dir),
        Seq(
          deleted("File:Other.jpg", 4L, "Dan", "2026-10-22")
        )
      )

      val text = DeletedImages.text(contest, Seq(2024, 2025, 2026), dir)

      DeletedImages.pageName(contest) === "Commons:Wiki Loves Monuments in Ukraine/Deleted images"
      text must contain("statistics cached them, as the daily sync found them: 4.")
      text must contain(
        "[[:File:B.jpg]] || [[User:Bob|Bob]] || 2026-09-03 || 07-101-0001 || 2026-10-20"
      )
      val order = Seq(
        "== 2026 (2) ==",
        "File:B.jpg",
        "File:A.jpg",
        "== 2024 (1) ==",
        "File:Old.jpg",
        "== Other files with the contest template (1) ==",
        "File:Other.jpg"
      ).map(text.indexOf(_))
      order must not(contain(-1))
      order === order.sorted
      text must not(contain("== 2025"))
    }

    "say there are none when no file was found deleted" in {
      val dir = Files.createTempDirectory("deleted-images").toString
      DeletedImages.text(contest, Seq(2026), dir) must endWith("as the daily sync found them: 0.\n")
    }
  }
}
