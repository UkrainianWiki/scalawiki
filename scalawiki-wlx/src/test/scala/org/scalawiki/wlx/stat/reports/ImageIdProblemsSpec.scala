package org.scalawiki.wlx.stat.reports

import org.scalawiki.dto.Image
import org.scalawiki.wlx.dto.{Contest, Monument, SpecialNomination}
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.specs2.mutable.Specification

class ImageIdProblemsSpec extends Specification {

  private val contest = Contest.WLMUkraine(2026)
  private val monumentDb = new MonumentDB(contest, Seq(new Monument(id = "01-101-0001", name = "m")))

  private val nominations = Seq(
    new SpecialNomination("Плівка 2026", None, Nil, Seq(2026), fileTemplate = Some("WLM2026-UA-film")),
    new SpecialNomination("Відео", None, Nil, Seq(2024, 2025, 2026), mediaType = Some(SpecialNomination.Video))
  )

  private val problems = new ImageIdProblems(monumentDb, nominations, nominationIds = Set("80-361-0001"))

  private def img(title: String, ids: String*): Image = Image(title, monumentIds = ids)

  private def titles(images: Seq[Image]): Seq[String] = images.map(_.title)

  private val images = Seq(
    img("File:Ok.jpg", "01-101-0001"),
    img("File:Bad.jpg", "01-101-9999"),
    img("File:OneOfTwoBad.jpg", "01-101-0001", "01-101-9999"),
    img("File:NoId.jpg"),
    img("File:NominationList.jpg", "80-361-0001"),
    img("File:OldNominationList.jpg", "99-101-0001"),
    img("File:FilmBadId.jpg", "01-101-9999").copy(specialNominations = Set("WLM2026-UA-film")),
    img("File:FilmNoId.jpg").copy(specialNominations = Set("WLM2026-UA-film")),
    img("File:Video.webm"),
    img("File:Ineligible.jpg", "01-101-9999").copy(categories = Set("Obviously ineligible submissions for WLM 2026"))
  )

  "bad ids" should {
    "list images with an id of no monument, but no special nomination images" in {
      titles(problems.badIds(new ImageDB(contest, images, Some(monumentDb)))) ===
        Seq("File:Bad.jpg", "File:OneOfTwoBad.jpg")
    }
  }

  "missing ids" should {
    "list images with no id, but no special nomination images" in {
      titles(problems.missingIds(new ImageDB(contest, images, Some(monumentDb)))) === Seq("File:NoId.jpg")
    }

    "leave out file-placed nomination images only in that nomination's years" in {
      val db2020 = new ImageDB(contest.copy(year = 2020), images, Some(monumentDb))
      titles(problems.missingIds(db2020)) === Seq("File:NoId.jpg", "File:FilmNoId.jpg", "File:Video.webm")
    }
  }
}
