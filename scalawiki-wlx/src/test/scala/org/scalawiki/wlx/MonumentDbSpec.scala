package org.scalawiki.wlx

import org.scalawiki.wlx.dto.lists.ListConfig
import org.scalawiki.wlx.dto.{Contest, Country, Monument}
import org.specs2.mutable.Specification

class MonumentDbSpec extends Specification {

  val Ukraine = Country.Ukraine
  val contest = Contest.WLMUkraine(2014)

  val monuments = Ukraine.regionIds.flatMap { regionId =>
    (1 to regionId.toInt).map { i =>
      Monument(
        id = regionId + "-001-" + f"$i%04d",
        name = "Monument in " + Ukraine.regionName(regionId),
        listConfig = Some(ListConfig.WleUa)
      )
    }
  }

  def specialNominationMonuments(prefix: String) = Ukraine.regionIds.flatMap { regionId =>
    (1 to regionId.toInt).map { i =>
      Monument(
        id = prefix + "-" + regionId + "1-" + f"$i%04d",
        name = "Special Nomination Monument in " + Ukraine.regionName(regionId),
        listConfig = Some(ListConfig.WlmUa)
      )
    }
  }

  "monument db" should {
    "contain monuments ids" in {
      val db = new MonumentDB(contest, monuments.toSeq)

      db.ids.size === monuments.size
      db.ids === monuments.map(_.id)
    }

    "contain special nomination monuments ids 88" in {
      val monuments = specialNominationMonuments("88")
      val db = new MonumentDB(contest, monuments.toSeq)

      db.ids.size === monuments.size
      db.ids === monuments.map(_.id)
    }

    "contain special nomination monuments ids 99" in {
      val monuments = specialNominationMonuments("99")
      val db = new MonumentDB(contest, monuments.toSeq)

      db.ids.size === monuments.size
      db.ids === monuments.map(_.id)
    }

    "group monuments by regions" in {
      val db = new MonumentDB(contest, monuments.toSeq)

      val regions = db._byRegion.keySet

      for (region <- regions) yield {
        db._byRegion(region).size === region.toInt
      }

      regions === Ukraine.regionIds
    }

    "group special nomination monuments by regions 88" in {
      val db = new MonumentDB(contest, specialNominationMonuments("88").toSeq)

      val regions = db._byRegion.keySet

      for (region <- regions) yield {
        db._byRegion(region).size === region.toInt
      }

      regions === Ukraine.regionIds
    }

    "group special nomination monuments by regions 99" in {
      val db = new MonumentDB(contest, specialNominationMonuments("99").toSeq)

      val regions = db._byRegion.keySet

      for (region <- regions) yield {
        db._byRegion(region).size === region.toInt
      }

      regions === Ukraine.regionIds
    }

    "get city for Kyiv" in {
      val regionId = "80-391"
      val kyiv = Ukraine.byIdAndName(regionId, "Київ").head
      val places = Seq("Київ", "м. Київ", "[[Київ]]", "м. [[Київ]]")

      val monuments = places.zipWithIndex.map { case (city, index) =>
        new Monument(name = "name", id = s"$regionId-$index", city = Some(city))
      }
      val mdb = new MonumentDB(contest, monuments)

      monuments.map { m =>
        val actual = mdb.getAdmDivision(m.id)
        actual aka s"${m.city}" must beSome(kyiv)
      }
    }
  }
}
