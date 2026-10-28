package org.scalawiki.wlx

import play.api.libs.json._

import scala.io.{Codec, Source}

/** A single sql_data entry.
  * @param entryType
  *   "Field" (rename an existing key), "Text" (inject literal), or "Raw" (skip)
  * @param value
  *   field name (for Field) or literal value (for Text/Raw)
  */
case class SqlEntry(entryType: String, value: String)

/** The parsed ua_uk.json mapping.
  *
  * @param fieldMap
  *   source template param name → list of dest column names. Empty-dest entries in JSON are stored
  *   as Seq(source) (identity). Duplicate sources accumulate dest names in order of appearance.
  * @param sqlMap
  *   sql_data key → SqlEntry (all types: Field, Text, Raw)
  * @param sqlKeyOrder
  *   sql_data keys in JSON insertion order (for stable CSV headers)
  */
case class UaUkMapping(
    fieldMap: Map[String, Seq[String]],
    sqlMap: Map[String, SqlEntry],
    sqlKeyOrder: Seq[String]
)

object UaUkJsonMapping {

  /** Load ua_uk.json from the classpath.
    * @param resourcePath
    *   e.g. "monuments_config/ua_uk.json"
    */
  def load(resourcePath: String): UaUkMapping = {
    val source = Source.fromResource(resourcePath)(Codec.UTF8)
    val raw =
      try source.mkString
      finally source.close()
    val json = Json.parse(raw)

    // --- Level 1: fields array ---
    // Build fieldMap: source → Seq[dest]. Empty dest → Seq(source) (identity).
    // Duplicate sources accumulate dest names in order of appearance.
    val fields = (json \ "fields").as[JsArray].value
    val fieldMapBuilder = scala.collection.mutable.LinkedHashMap
      .empty[String, scala.collection.mutable.ListBuffer[String]]

    fields.foreach { entry =>
      val src = (entry \ "source").as[String]
      val dest = (entry \ "dest").as[String]
      if (dest.nonEmpty) { // empty dest → identity
        fieldMapBuilder
          .getOrElseUpdate(src, scala.collection.mutable.ListBuffer.empty)
          .append(dest)
      }
    }

    // Note: for duplicate sources (e.g. галерея), dest names are ordered by appearance in the fields array.
    val fieldMap: Map[String, Seq[String]] =
      fieldMapBuilder.map { case (k, v) => k -> v.toSeq }.toMap

    // --- Level 2: sql_data object (preserve insertion order) ---
    // IMPORTANT: Play JSON's JsObject stores fields as an internal Seq[(String, JsValue)],
    // and Json.parse preserves the document order in that Seq.
    // .fields returns an IndexedSeq in that order.
    // Do NOT convert through Map at any point — doing so will lose insertion order.
    val sqlDataObj = (json \ "sql_data").as[JsObject]
    val sqlEntries: Seq[(String, SqlEntry)] = sqlDataObj.fields.toSeq.map { case (key, obj) =>
      val entryType = (obj \ "type").as[String]
      val value = (obj \ "value").as[String]
      key -> SqlEntry(entryType, value)
    }

    val sqlMap: Map[String, SqlEntry] = sqlEntries.toMap
    val sqlKeyOrder: Seq[String] = sqlEntries.map(_._1)

    UaUkMapping(fieldMap, sqlMap, sqlKeyOrder)
  }

  /** Apply two-level mapping to one monument row.
    *
    * Step 1 (Level 1 — fields): For each (key, value) in row: if fieldMap contains key → emit
    * (dest, value) for each dest in fieldMap(key) else → emit (key, value) unchanged
    *
    * Step 2 (Level 2 — sql_data Field entries): case A: sqlEntry.value absent from intermediateRow
    * → skip case B: sqlKey == sqlEntry.value → no-op case C: sqlKey not yet in intermediateRow →
    * rename sqlEntry.value → sqlKey case D: sqlKey already in intermediateRow → keep sqlKey, drop
    * sqlEntry.value
    *
    * Step 3 (Level 2 — sql_data Text entries): Inject sqlKey → literal value into every row.
    *
    * Raw entries are skipped in all steps.
    */
  def applyMapping(
      row: Map[String, String],
      mapping: UaUkMapping
  ): Map[String, String] = {

    // Step 1
    var intermediate = Map.empty[String, String]
    for ((key, value) <- row) {
      mapping.fieldMap.get(key) match {
        case Some(dests) =>
          dests.foreach { dest => intermediate = intermediate.updated(dest, value) }
        case None => // intermediate = intermediate.updated(key, value)
      }
    }

    // Step 2 — Field (in sql_data insertion order)
    for (sqlKey <- mapping.sqlKeyOrder) {
      mapping.sqlMap.get(sqlKey).filter(_.entryType == "Field").foreach { entry =>
        if (intermediate.contains(entry.value)) {
          if (sqlKey != entry.value) {
            if (!intermediate.contains(sqlKey)) {
              // case C: rename
              intermediate = intermediate.updated(sqlKey, intermediate(entry.value)) - entry.value
            } else {
              // case D: sqlKey already present, just drop old key
              intermediate = intermediate - entry.value
            }
          }
          // case B: sqlKey == entry.value → no-op
        }
        // case A: entry.value absent → skip
      }
    }

    // Step 3 — Text (in sql_data insertion order)
    for (sqlKey <- mapping.sqlKeyOrder) {
      mapping.sqlMap.get(sqlKey).filter(_.entryType == "Text").foreach { entry =>
        intermediate = intermediate.updated(sqlKey, entry.value)
      }
    }

    intermediate
  }

  /** Compute CSV header columns in deterministic order.
    *
    * Order:
    *   1. sql_data keys (Field + Text only, excluding Raw) in sqlKeyOrder insertion order. Raw
    *      sql_data keys are NOT included in section 1.
    *   2. All remaining keys found across all rows, alphabetically sorted. "Remaining" = any row
    *      key NOT in sqlKeySet (the Field+Text set). Raw sql_data keys (e.g. "adm1") are not in
    *      sqlKeySet, so if a row has an "adm1" key it falls into the alphabetical section — not the
    *      sql first section.
    *
    * @param mappedRows
    *   rows that have already had applyMapping applied
    * @param mapping
    *   the UaUkMapping (for sqlKeyOrder)
    */
  def headerColumns(
      mappedRows: Iterable[Map[String, String]],
      mapping: UaUkMapping
  ): Seq[String] = {
    // Section 1: sql_data keys that are Field or Text (not Raw), in insertion order
    val sqlKeys = mapping.sqlKeyOrder.filter { k =>
      mapping.sqlMap.get(k).exists(e => e.entryType == "Field" || e.entryType == "Text")
    }
    val sqlKeySet = sqlKeys.toSet

    // Section 2: all row keys not covered by sqlKeySet, sorted alphabetically
    val allKeys = mappedRows.flatMap(_.keys).toSet
    val remaining = (allKeys -- sqlKeySet).toSeq.sorted

    sqlKeys ++ remaining
  }
}
