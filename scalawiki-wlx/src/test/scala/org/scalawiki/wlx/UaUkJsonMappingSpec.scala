package org.scalawiki.wlx

import org.specs2.mutable.Specification

class UaUkJsonMappingSpec extends Specification {

  // Load once for all tests
  lazy val mapping: UaUkMapping = UaUkJsonMapping.load("monuments_config/ua_uk.json")

  "UaUkJsonMapping.load" should {

    "parse fieldMap with a simple source→dest entry" in {
      // "назва" → "name" from fields array
      mapping.fieldMap must haveKey("назва")
      mapping.fieldMap("назва") must contain("name")
    }

    "parse fieldMap with a duplicate source (галерея → commonscat AND gallery)" in {
      mapping.fieldMap must haveKey("галерея")
      mapping.fieldMap("галерея") must containAllOf(Seq("commonscat", "gallery"))
    }

    "represent empty-dest entries as identity (source → Seq(source))" in {
      // "паспорт" has dest="" in ua_uk.json
      mapping.fieldMap must haveKey("паспорт")
      mapping.fieldMap("паспорт") must_== Seq("паспорт")
    }.pendingUntilFixed("ignore unmapped fields")

    "parse sqlMap with Field type entry" in {
      // adm2 → {type:Field, value:rayon}
      mapping.sqlMap must haveKey("adm2")
      mapping.sqlMap("adm2") must_== SqlEntry("Field", "rayon")
    }

    "parse sqlMap with Text type entry" in {
      // adm0 → {type:Text, value:ua}
      mapping.sqlMap must haveKey("adm0")
      mapping.sqlMap("adm0") must_== SqlEntry("Text", "ua")
    }

    "parse sqlMap with Raw type entry" in {
      // adm1 → {type:Raw, value:LOWER(`iso_oblast`)}
      mapping.sqlMap must haveKey("adm1")
      mapping.sqlMap("adm1").entryType must_== "Raw"
    }

    "preserve sql_data key insertion order in sqlKeyOrder" in {
      // actual first key in ua_uk.json sql_data is "country"
      mapping.sqlKeyOrder.head must_== "country"
      mapping.sqlKeyOrder must contain("adm0")
      mapping.sqlKeyOrder must contain("adm2")
      val adm0Idx = mapping.sqlKeyOrder.indexOf("adm0")
      val adm2Idx = mapping.sqlKeyOrder.indexOf("adm2")
      adm0Idx must be_>=(0)
      adm2Idx must be_>=(0)
    }
  }

  "UaUkJsonMapping.applyMapping" should {

    "rename a source field to its dest (назва → name)" in {
      val row = Map("назва" -> "Церква Святого Миколая")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("name")
      result("name") must_== "Церква Святого Миколая"
      result must not haveKey ("назва")
    }

    "emit multiple columns for duplicate-source field (галерея → commonscat + gallery)" in {
      val row = Map("галерея" -> "Churches in Kyiv")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("commonscat")
      result must haveKey("gallery")
      result("commonscat") must_== "Churches in Kyiv"
      result("gallery") must_== "Churches in Kyiv"
      result must not haveKey ("галерея")
    }

    "remap via sql_data Field (rayon → adm2)" in {
      // район → rayon (Level 1), rayon → adm2 (Level 2 Field)
      val row = Map("район" -> "Шевченківський")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("adm2")
      result("adm2") must_== "Шевченківський"
      result must not haveKey ("rayon")
      result must not haveKey ("район")
    }

    "inject Text literal (adm0 = ua, lang = uk)" in {
      val row = Map("ID" -> "14-101-0001")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("adm0")
      result("adm0") must_== "ua"
      result must haveKey("lang")
      result("lang") must_== "uk"
    }

    "skip Raw entry (adm1 not injected as literal)" in {
      val row = Map("iso" -> "UA-30")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result.get("adm1") must beNone
    }

    "keep unmapped field under original name" in {
      val row = Map("unknownField" -> "someValue")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("unknownField")
      result("unknownField") must_== "someValue"
    }.pendingUntilFixed("ignore unmapped fields")

    "sql_data Field entry whose value is absent from row produces no column (case A)" in {
      // sql_data has Field entries for "source" and "changed" whose values
      // are not produced by any fields mapping — neither appears in output
      val row = Map("ID" -> "14-101-0001")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must not haveKey ("source")
      result must not haveKey ("changed")
    }

    "keep empty-dest source field under its original name (general rule, tested via паспорт and наказ)" in {
      val row = Map("паспорт" -> "12345", "наказ" -> "№100")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("паспорт")
      result("паспорт") must_== "12345"
      result must haveKey("наказ")
      result("наказ") must_== "№100"
    }.pendingUntilFixed("ignore unmapped fields")

    "handle collision: case D — sqlKey already in row, drop old key" in {
      val row = Map("галерея" -> "SomeCat")
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result must haveKey("commonscat")
      result("commonscat") must_== "SomeCat"
    }

    "full round-trip: realistic monument row" in {
      val row = Map(
        "ID" -> "14-101-0001",
        "назва" -> "Будинок Городецького",
        "район" -> "Печерський",
        "iso" -> "UA-30",
        "широта" -> "50.4501",
        "довгота" -> "30.5234",
        "фото" -> "Horodetsky House.jpg"
      )
      val result = UaUkJsonMapping.applyMapping(row, mapping)
      result("id") must_== "14-101-0001"
      result("name") must_== "Будинок Городецького"
      result("adm2") must_== "Печерський"
      result("lat") must_== "50.4501"
      result("lon") must_== "30.5234"
      result("image") must_== "Horodetsky House.jpg"
      result("adm0") must_== "ua"
      result("lang") must_== "uk"
    }
  }

  "UaUkJsonMapping.headerColumns" should {

    "put sql_data Field+Text keys first in insertion order, then remaining alphabetically" in {
      val rows = Seq(
        Map("id" -> "1", "name" -> "A", "unknownZ" -> "z"),
        Map("id" -> "2", "adm0" -> "ua", "unknownA" -> "a")
      )
      val headers = UaUkJsonMapping.headerColumns(rows, mapping)
      val sqlNonRaw = mapping.sqlKeyOrder.filter(k =>
        mapping.sqlMap.get(k).exists(e => e.entryType == "Field" || e.entryType == "Text")
      )
      headers.take(sqlNonRaw.size) must_== sqlNonRaw
      val remaining = headers.drop(sqlNonRaw.size)
      remaining must_== remaining.sorted
    }

    "not include Raw sql_data keys in the sql-first section" in {
      val rows = Seq(Map("id" -> "1"))
      val headers = UaUkJsonMapping.headerColumns(rows, mapping)
      val sqlNonRaw = mapping.sqlKeyOrder.filter(k =>
        mapping.sqlMap.get(k).exists(e => e.entryType == "Field" || e.entryType == "Text")
      )
      // adm1 is Raw — must not be in the first section of headers
      mapping.sqlMap.get("adm1").map(_.entryType) must beSome("Raw")
      val firstSection = headers.take(sqlNonRaw.size)
      firstSection must not contain ("adm1")
    }
  }
}
