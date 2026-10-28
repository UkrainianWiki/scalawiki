package org.scalawiki.wlx.stat.generic

import org.scalawiki.wlx.MonumentDB
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa
import org.scalawiki.wlx.stat.Stats
import org.specs2.mutable.Specification

class MonumentStatSpec extends Specification {

  val contest = Contest.WLMUkraine(2015)

  def monument(id: String, name: String) =
    new Monument(id = id, name = name, listConfig = Some(WlmUa))

  def monuments(
      n: Int,
      regionId: String,
      namePrefix: String,
      startId: Int = 1
  ): Seq[Monument] =
    (startId until startId + n).map(i => monument(s"$regionId-xxx-000$i", namePrefix + i))

  "with articles stat" should {
    "work with no regions, no articles" in {
      ok
    }

    "work with no articles" in {
      val db = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val table = Stats.withArticles(db)

      table.headers === Seq(
        "Region KOATUU code",
        "Region name",
        "All",
        "With Articles",
        "%"
      )
      table.total === Seq("Total", "Total", 14, 0, 0)
      table.rows === List(
        List("01", "Автономна Республіка Крим", 2, 0, 0),
        List("05", "Вінницька область", 5, 0, 0),
        List("07", "Волинська область", 7, 0, 0)
      )
    }

    "work with articles" in {
      val db = new MonumentDB(
        contest,
        monuments(1, "01", "Crimea") ++ monuments(2, "01", "[[Crimea]]", 2) ++
          monuments(5, "05", "Podillya") ++ monuments(
            1,
            "05",
            "[[Podillya]]",
            6
          ) ++
          monuments(7, "07", "Volyn") ++ monuments(7, "07", "[[Volyn]]", 8)
      )

      val table = Stats.withArticles(db)

      table.headers === Seq(
        "Region KOATUU code",
        "Region name",
        "All",
        "With Articles",
        "%"
      )
      table.total === Seq("Total", "Total", 23, 10, 43)
      table.rows === List(
        List("01", "Автономна Республіка Крим", 3, 2, 67),
        List("05", "Вінницька область", 6, 1, 17),
        List("07", "Волинська область", 14, 7, 50)
      )
    }

  }

}
