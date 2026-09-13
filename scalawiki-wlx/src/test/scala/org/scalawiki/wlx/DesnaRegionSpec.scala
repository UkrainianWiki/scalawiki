package org.scalawiki.wlx

import org.scalawiki.wlx.stat.reports.DesnaRegionSpecialNomination
import org.specs2.mutable.Specification

class DesnaRegionSpec extends Specification {

  "Desna region" should {
    val desna = DesnaRegionSpecialNomination()
//    "detect oblasts" in {
//      desna.oblastNames === Seq("Київська", "Чернігівська", "Сумська")
//      val oblasts = desna.oblasts
//      oblasts.size === 3
//      oblasts.map(_.name) === Seq("Київська", "Чернігівська", "Сумська").map(_ + " область")
//    }

    "detect raions" in {
      val places = desna.places
      val raions = places.map(desna.getRaion)
      raions.map(_.size).distinct === List(1)
    }

    "detect places" in {
      val rows = desna.places
      val places = rows.map(desna.getPlace)
      places.map(_.size).distinct === List(1, 2, 3, 5, 7)
    }

    "Погреби" in {
      val places =
        desna.getPlace(List("Київська", "Броварський", "Зазимська", "Погреби"))
      places.size === 1
    }

    "Остер" in {
      val places = desna.getPlace(
        List("Чернігівська", "Чернігівський", "Остерська", "Остер")
      )
      places.size === 1
    }

    "Новгород-Сіверський" in {
      val places = desna.getRaion(
        List("Чернігівська", "Новгород-Сіверський", "Семенівська", "Лісківщина")
      )
      places.size === 1
    }

    "Козелецький район" in {
      val places = desna.getRaion(
        List("Чернігівська", "Козелецький", "Козелецька", "Тарасів")
      )
      places.size === 1
    }

    "Бірки" in {
      val places = desna.getPlace(
        List("Чернігівська", "Чернігівський", "Остерська", "Бірки")
      )
      places.size === 1
    }
  }
}
