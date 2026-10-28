package org.scalawiki.wlx.stat

import org.scalawiki.dto.Image
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa
import org.scalawiki.wlx.dto.{Contest, Monument}
import org.scalawiki.wlx.stat.reports.MonumentsPicturedByRegion
import org.specs2.mutable.Specification

class MonumentsPicturedByRegionSpec extends Specification {

  val startYear = 2015
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

  def getStat(
      monuments: Seq[Monument],
      images: Seq[Seq[Image]] = Seq(Seq.empty)
  ): ContestStat = {
    val monumentDb = new MonumentDB(contest, monuments)

    def imageDb(images: Seq[Image]) = new ImageDB(contest, images, monumentDb)

    val imagesDbs = images.map(imageDb)

    ContestStat(
      contest,
      startYear,
      Some(monumentDb),
      imagesDbs.last,
      imageDb(images.flatten),
      imagesDbs.init
    )
  }

  "monuments pictured" should {
    "no monuments" in {
      val stat = getStat(Seq.empty)
      val table = new MonumentsPicturedByRegion(stat).table

      table.headers === Seq(
        "Region",
        "Objects in lists",
        "0 years total",
        "0 years percentage"
      )
      table.data === Seq(
        Seq("Total", "0", "0", "0")
      )
    }

    "no monuments pictured" in {
      val _monuments =
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")

      val stat = getStat(_monuments)

      val table = new MonumentsPicturedByRegion(stat).table

      table.headers === Seq(
        "Region",
        "Objects in lists",
        "0 years total",
        "0 years percentage"
      )
      table.data === Seq(
        Seq("Автономна Республіка Крим", "2", "0", "0"),
        Seq("Вінницька область", "5", "0", "0"),
        Seq("Волинська область", "7", "0", "0"),
        Seq("Total", "14", "0", "0")
      )
    }

    "monuments pictured" in {
      val monumentDb = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val image = Image(
        "File:Img3.jpg",
        monumentIds = List("01-xxx-0001"),
        pageId = Some(1L)
      )

      val totalImageDb = new ImageDB(contest, Seq(image), monumentDb)
      val imageDbs = Seq.empty

      val table = new MonumentsPicturedByRegion(
        imageDbs,
        totalImageDb,
        monumentDb
      ).table

      table.headers === Seq(
        "Region",
        "Objects in lists",
        "0 years total",
        "0 years percentage"
      )
      table.data === Seq(
        Seq("Автономна Республіка Крим", "2", "1", "50"),
        Seq("Вінницька область", "5", "0", "0"),
        Seq("Волинська область", "7", "0", "0"),
        Seq("Total", "14", "1", "7")
      )
    }

    "two years monuments pictured" in {
      val monumentDb = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val images = Seq(
        Seq(
          Image(
            "File:Img1.jpg",
            monumentIds = List("01-xxx-0001"),
            pageId = Some(1L)
          )
        ),
        Seq(
          Image(
            "File:Img2.jpg",
            monumentIds = List("05-xxx-0001"),
            pageId = Some(2L)
          ),
          Image(
            "File:Img3.jpg",
            monumentIds = List("07-xxx-0001"),
            pageId = Some(3L)
          )
        )
      )

      val totalImageDb = new ImageDB(contest, images.flatten, monumentDb)
      val imageDbs = images.zipWithIndex.map { case (img, i) =>
        new ImageDB(Contest.WLMUkraine(2014 + i), img, monumentDb)
      }

      val table = new MonumentsPicturedByRegion(
        imageDbs,
        totalImageDb,
        monumentDb
      ).table

      table.headers === Seq(
        "Region",
        "Objects in lists",
        "2 years total",
        "2 years percentage",
        "2015 Objects",
        "2015 Pictures",
        "2015 newly pictured",
        "2014 Objects",
        "2014 Pictures"
      )
      table.data === Seq(
        Seq("Автономна Республіка Крим", "2", "1", "50") ++ Seq(
          Seq("1", "1"),
          Seq("0", "0", "0")
        ).reverse.flatten,
        Seq("Вінницька область", "5", "1", "20") ++ Seq(
          Seq("0", "0"),
          Seq(
            "1",
            "1",
            "[[Commons:Wiki Loves Monuments 2015 in Ukraine/Monuments newly pictured by region in Вінницька область|1]]"
          )
        ).reverse.flatten,
        Seq("Волинська область", "7", "1", "14") ++ Seq(
          Seq("0", "0"),
          Seq(
            "1",
            "1",
            "[[Commons:Wiki Loves Monuments 2015 in Ukraine/Monuments newly pictured by region in Волинська область|1]]"
          )
        ).reverse.flatten,
        Seq("Total", "14", "3", "21") ++ Seq(
          Seq("1", "1"),
          Seq("2", "2", "2")
        ).reverse.flatten
      )
    }

    "4 years monuments pictured" in {
      val monumentDb = new MonumentDB(
        contest,
        monuments(2, "01", "Crimea") ++
          monuments(5, "05", "Podillya") ++
          monuments(7, "07", "Volyn")
      )

      val images = Seq(
        Image(
          "File:Img1.jpg",
          monumentIds = List("01-xxx-0001"),
          pageId = Some(1L)
        ),
        Image(
          "File:Img2.jpg",
          monumentIds = List("05-xxx-0001"),
          pageId = Some(2L)
        ),
        Image(
          "File:Img3.jpg",
          monumentIds = List("07-xxx-0001"),
          pageId = Some(3L)
        ),
        Image(
          "File:Img4.jpg",
          monumentIds = List("01-xxx-0002"),
          pageId = Some(4L)
        )
      )

      val totalImageDb = new ImageDB(contest, images, monumentDb)
      val imageDbs = images.zipWithIndex.map { case (img, i) =>
        new ImageDB(Contest.WLMUkraine(2012 + i), Seq(img), monumentDb)
      }

      val table = new MonumentsPicturedByRegion(
        imageDbs,
        totalImageDb,
        monumentDb
      ).table

      table.headers === Seq(
        "Region",
        "Objects in lists",
        "4 years total",
        "4 years percentage",
        "2015 Objects",
        "2015 Pictures",
        "2015 newly pictured",
        "2014 Objects",
        "2014 Pictures",
        "2013 Objects",
        "2013 Pictures",
        "2012 Objects",
        "2012 Pictures"
      )
      val data = table.data.toSeq
      data.size === 4

      data(0) === Seq("Автономна Республіка Крим", "2", "2", "100") ++ Seq(
        Seq("1", "1"),
        Seq("0", "0"),
        Seq("0", "0"),
        Seq(
          "1",
          "1",
          "[[Commons:Wiki Loves Monuments 2015 in Ukraine/Monuments newly pictured by region in Автономна Республіка Крим|1]]"
        )
      ).reverse.flatten
      data(1) === Seq("Вінницька область", "5", "1", "20") ++ Seq(
        Seq("0", "0"),
        Seq("1", "1"),
        Seq("0", "0"),
        Seq("0", "0", "0")
      ).reverse.flatten
      data(2) === Seq("Волинська область", "7", "1", "14") ++ Seq(
        Seq("0", "0"),
        Seq("0", "0"),
        Seq("1", "1"),
        Seq("0", "0", "0")
      ).reverse.flatten
      data(3) === Seq("Total", "14", "4", "28") ++ Seq(
        Seq("1", "1"),
        Seq("1", "1"),
        Seq("1", "1"),
        Seq("1", "1", "1")
      ).reverse.flatten
    }
  }
}
