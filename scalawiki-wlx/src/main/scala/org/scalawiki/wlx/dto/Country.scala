package org.scalawiki.wlx.dto

import java.util.Locale

import scala.collection.immutable.SortedSet

trait AdmDivision {
  def code: String

  def shortCode: String = KatotthTypes.shortRegionCode(code, regionType)

  def name: String

  lazy val namesList: Seq[String] =
    parent().map(_.namesList).getOrElse(Nil) :+ fullName

  lazy val shortNamesList: Seq[String] =
    parent().map(_.shortNamesList).getOrElse(Nil) :+ name

  def regionType: Option[RegionType]

  def level: Int

  override def toString = s"$name ($code)"

  def regions: Seq[AdmDivision]

  def allSubregions: Seq[AdmDivision] = {
    Seq(this) ++ regions.flatMap(_.allSubregions)
  }

  def mapByCode: Map[String, AdmDivision] = {
    allSubregions.map { place =>
      place.code -> place
    }.toMap
  }

  def parent: () => Option[AdmDivision]

  def regionId(monumentId: String): String = {
    if (monumentId.contains("-")) {
      monumentId.split("-").take(2).mkString
    } else {
      monumentId.take(4)
    }
  }

  def regionName(regId: String): String = if (regId == code) name else ""

  def byMonumentId(monumentId: String): Option[AdmDivision] = {
    if (regionId(monumentId) == shortCode) {
      Some(this)
    } else {
      None
    }
  }

  def byName(name: String): Seq[AdmDivision] = {
    byName(name, 0, Int.MaxValue)
  }

  def byName(name: String, level: Int, max: Int): Seq[AdmDivision] =
    byLowerName(name.toLowerCase, level, max)

  // The searched name is lowercased once up front, not again at every node.
  private def byLowerName(lowerName: String, level: Int, max: Int): Seq[AdmDivision] = {
    Seq(this).filter(_.name.toLowerCase == lowerName) ++
      (if (level < max) {
         regions.flatMap(_.byLowerName(lowerName, level + 1, max))
       } else Nil)
  }

  def byRegion(monumentIds: Set[String]): Map[AdmDivision, Set[String]] = {
    val entries =
      monumentIds.flatMap(id => byMonumentId(id).map(adm => id -> adm))

    entries
      .groupBy { case (id, adm) => adm }
      .mapValues(_.toMap.keySet)
      .toMap
  }

  def byIdAndName(
      regionId: String,
      rawName: String,
      cityType: Option[String] = None
  ): Seq[AdmDivision] = {
    val cleanName = AdmDivision.cleanName(rawName)

    val candidates = byMonumentId(regionId)
      .map { region =>
        val here = region.byName(cleanName)
        if (here.isEmpty) {
          region.parent().map(_.byName(cleanName)).getOrElse(Nil)
        } else {
          here
        }
      }
      .getOrElse(Nil)

    val types = cityType.fold(
      KoatuuTypes.nameToType(rawName).toSet.filterNot(_.code == "Р")
    ) { code =>
      KoatuuTypes.codeToType
        .get(code)
        .map(Set(_))
        .getOrElse(KoatuuTypes.nameToType(code).toSet.filterNot(_.code == "Р"))
    }

    if (candidates.size > 1) {
      val byType = if (types.nonEmpty) {
        candidates.filter(c => c.regionType.exists(types.contains))
      } else {
        candidates
      }

      if (byType.size > 1) {
        val byParent = byType.filter { c =>
          val parentName = c.parent().map(_.name).getOrElse("").toLowerCase()
          rawName.toLowerCase().contains(parentName)
        }
        if (byParent.size == 1) {
          byParent
        } else {
          byType
        }
      } else {
        byType
      }
    } else {
      candidates
    }
  }

  def withParents(parent: () => Option[AdmDivision] = () => None): AdmDivision

  def regionsWithParents(): Seq[AdmDivision] = {
    regions.map(_.withParents(() => Some(this)))
  }

  def fullName: String = name + regionType.flatMap(_.nameSuffix).getOrElse("")
}

trait AdmRegion extends AdmDivision {
  lazy val regionIds: SortedSet[String] = SortedSet(
    regions.map(_.shortCode): _*
  )

  lazy val regionNames: Seq[String] =
    regions.sortBy(_.shortCode).map(_.fullName)

  lazy val regionById: Map[String, AdmDivision] =
    regions.groupBy(_.shortCode).mapValues(_.head).toMap

  override def regionName(regId: String) =
    byMonumentId(regId).map(_.fullName).getOrElse("")

  override def byMonumentId(monumentId: String): Option[AdmDivision] = {
    regionById.get(regionId(monumentId)).flatMap { region =>
      region.byMonumentId(monumentId).orElse(Some(region))
    }
  }
}

object NoAdmDivision extends Country("", "")

case class Country(
    code: String,
    name: String,
    val languageCodes: Seq[String] = Nil,
    var regions: Seq[AdmDivision] = Nil,
    val codeToRegion: Map[String, AdmDivision] = Map.empty
) extends AdmRegion {

  val parent: () => Option[AdmDivision] = () => None

  def withoutLangCodes: Country = copy(languageCodes = Nil)

  override def regionId(monumentId: String): String = {
    monumentId.take(2)
  }

  override def withParents(
      parent: () => Option[AdmDivision] = () => None
  ): AdmDivision = {
    regions = regionsWithParents()
    this
  }

  override def regionType: Option[RegionType] = None

  override def level: Int = 0
}

case class Region(
    code: String,
    name: String,
    var regions: Seq[AdmDivision] = Nil,
    var parent: () => Option[AdmDivision] = () => None,
    regionType: Option[RegionType] = None,
    level: Int = 0
) extends AdmRegion {

  override def withParents(
      parent: () => Option[AdmDivision] = () => None
  ): AdmDivision = {
    this.parent = parent
    this.regions = regionsWithParents()
    this
  }

  override def fullName: String = name + regionType
    .filterNot(_.noSuffix.contains(name))
    .flatMap(_.nameSuffix)
    .getOrElse("")
}

case class NoRegions(
    code: String,
    name: String,
    var parent: () => Option[AdmDivision] = () => None,
    regionType: Option[RegionType] = None,
    level: Int = 0
) extends AdmDivision {

  val regions: Seq[AdmDivision] = Nil

  override def withParents(
      parent: () => Option[AdmDivision] = () => None
  ): AdmDivision = {
    this.parent = parent
    this
  }

}

object AdmDivision {
  def apply(
      code: String,
      name: String,
      regions: Seq[AdmDivision],
      parent: () => Option[AdmDivision],
      regionType: Option[RegionType],
      level: Int = 0
  ): AdmDivision = {
    if (regions.isEmpty) {
      NoRegions(code, name, parent, regionType, level)
    } else {
      Region(code, name, regions, parent, regionType, level)
    }
  }

  def cleanName(raw: String): String = {
    raw
      .replace("р-н", "район")
      .replace("сільська рада", "")
      .replace("селищна рада", "")
      .replace("[[", "")
      .replace("]]", "")
      .replace("&nbsp;", " ")
      .replace('\u00A0', ' ')
      .replace("м.", "")
      .replace("місто", "")
      .replace("с.", "")
      .replace("С.", "")
      .replace(".", "")
      .replace("село", "")
      .replace("сел.", "")
      .replace("смт", "")
      .replace("Смт", "")
      .replace("с-ще", "")
      .replace("с-щ", "")
      .replace("'''", "")
      .replace("''", "")
      .replace(",", "")
      .replace("’", "'")
      .replace("”", "'")
      .split("\\(")
      .head
      .split("\\|")
      .head
      .trim
  }

}

object Country {

  val Azerbaijan = new Country("AZ", "Azerbaijan", Seq("az"))

  val Ukraine: Country =
    new Country("UA", "Ukraine", Seq("uk"), Koatuu.regions(() => Some(Ukraine)))

  val customCountries = Seq(Ukraine)

  def langMap: Map[String, Seq[String]] = {
    Locale.getAvailableLocales
      .groupBy(_.getCountry)
      .map { case (countryCode, locales) =>
        val langs = locales.toSeq.flatMap { locale =>
          Option(locale.getLanguage)
            .filter(_.nonEmpty)
        }.distinct
        countryCode -> langs
      }
  }

  def fromJavaLocales: Seq[Country] = {

    val countryLangs = langMap

    val fromJava = Locale.getISOCountries
      .map { countryCode =>
        val langCodes = countryLangs.getOrElse(countryCode, Seq.empty)

        val locales =
          langCodes.map(langCode => new Locale(langCode, countryCode))

        val locale = locales.headOption.getOrElse(new Locale("", countryCode))

        val langs = locales.map(_.getDisplayLanguage(Locale.ENGLISH)).distinct

        new Country(
          locale.getCountry,
          locale.getDisplayCountry(Locale.ENGLISH),
          langCodes
        )
      }
      .filterNot(_.code == "ua")

    Seq(Ukraine) ++ fromJava
  }

  lazy val countryMap: Map[String, Country] =
    (fromJavaLocales ++ customCountries)
      .groupBy(_.code.toLowerCase)
      .mapValues(_.head)
      .toMap

  def byCode(code: String): Option[Country] = countryMap.get(code.toLowerCase)
}
