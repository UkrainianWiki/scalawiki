package org.scalawiki.wlx.stat.reports

import org.scalawiki.wlx.dto.Monument
import org.specs2.mutable.Specification

class SpecialNominationsDuplicatesSpec extends Specification {

  private val lists = "Вікіпедія:Вікі любить пам'ятки/"

  private def monument(id: String, page: String, name: String = "Церква") =
    new Monument(id = id, name = name, page = lists + page)

  "SpecialNominations.duplicatedIds" should {
    "find an id on two pages of one nomination, rows in list order" in {
      val monuments = Seq(
        monument("99-245-0001", "Єврейська спадщина/Львівська область", "Меморіал"),
        monument("46-101-0001", "Єврейська спадщина/Львівська область"),
        monument("99-245-0001", "Єврейська спадщина/Чернівецька область", "Кладовище")
      )
      SpecialNominations.duplicatedIds(monuments).map { case (id, ms) => id -> ms.map(_.name) } ===
        Seq("99-245-0001" -> Seq("Меморіал", "Кладовище"))
    }

    "find an id repeated on the same page" in {
      val monuments = Seq(
        monument(
          "26-212-0355",
          "Пам'ятки дерев'яної архітектури України/Івано-Франківська область/1"
        ),
        monument(
          "26-212-0355",
          "Пам'ятки дерев'яної архітектури України/Івано-Франківська область/1"
        )
      )
      SpecialNominations.duplicatedIds(monuments).map(_._1) === Seq("26-212-0355")
    }

    "leave out rows without an id, and ids used once" in {
      val monuments = Seq(
        monument("", "Полтавська область"),
        monument(" ", "Полтавська область"),
        monument("53-101-0001", "Полтавська область")
      )
      SpecialNominations.duplicatedIds(monuments) must beEmpty
    }

    "sort ids" in {
      val monuments = Seq("02", "01", "02", "01").map(n => monument(s"99-000-00$n", "Бібліотеки"))
      SpecialNominations.duplicatedIds(monuments).map(_._1) === Seq("99-000-0001", "99-000-0002")
    }
  }

  "SpecialNominations.duplicatedIdsText" should {
    val duplicates = SpecialNominations.duplicatedIds(
      Seq(
        monument(
          "99-245-0001",
          "Єврейська спадщина/Львівська область",
          "[[Меморіал (Львів)|Меморіал]]"
        ),
        monument("99-245-0001", "Єврейська спадщина/Чернівецька область", "Кладовище\n<br/> старе")
      )
    )
    val text = SpecialNominations.duplicatedIdsText(
      Seq("Єврейська спадщина" -> duplicates, "Бібліотеки" -> Nil),
      "Wiki Loves Monuments 2026 in Ukraine"
    )

    "have a section only for a nomination with duplicates" in {
      text must contain("== Єврейська спадщина ==")
      text must not(contain("== Бібліотеки =="))
    }

    "link names and list pages to uk.wikipedia" in {
      text must contain("[[:uk:Меморіал (Львів)|Меморіал]]")
      text must contain(
        s"[[:uk:${lists}Єврейська спадщина/Чернівецька область|Єврейська спадщина/Чернівецька область]]"
      )
    }

    "keep each name on one line, so it stays in its table cell" in {
      text must contain("Кладовище <br/> старе")
    }

    "put ids on several lists under their own heading" in {
      text must contain("=== In different lists (1) ===")
      text must not(contain("=== Repeated within one list"))
    }

    "link a list once for an id repeated in it, with its count, and show a repeated name once" in {
      val page = "Пам'ятки дерев'яної архітектури України/Івано-Франківська область/1"
      val sameList = SpecialNominations.duplicatedIds(
        Seq(
          monument("26-212-0355", page, "Гуцульська ґражда"),
          monument("26-212-0355", page, "Гуцульська ґражда")
        )
      )
      val t = SpecialNominations.duplicatedIdsText(Seq("Дерев'яна архітектура" -> sameList), "WLM")
      t must contain("=== Repeated within one list (1) ===")
      t must not(contain("=== In different lists"))
      s"\\[\\[:uk:${java.util.regex.Pattern.quote(lists + page)}\\|".r.findAllIn(t).size === 1
      t must contain("×2")
      "Гуцульська ґражда".r.findAllIn(t).size === 1
    }

    "say so when no nomination has duplicates" in {
      SpecialNominations.duplicatedIdsText(Seq("Бібліотеки" -> Nil), "WLM") must contain(
        "No duplicated ids."
      )
    }
  }
}
