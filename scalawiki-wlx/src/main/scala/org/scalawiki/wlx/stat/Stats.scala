package org.scalawiki.wlx.stat

import org.scalawiki.wlx.MonumentDB
import org.scalawiki.wlx.dto.{AdmDivision, Monument}
import org.scalawiki.wlx.stat.generic._

object Stats {

  def withArticles(monumentDb: MonumentDB) = {

    new Records[Monument, String](
      data = monumentDb.allMonuments,
      rowGrouping = MonumentsByRegion,
      columnAggregations = Seq(
        All,
        MonumentsWithArticles,
        new PercentageAggregation("%", MonumentsWithArticles)
      ),
      rowOrdering = new StringColumnOrdering(0),
      rowKeyMapping = Some(new RegionNameById(monumentDb.contest.country))
    )
  }

}

object MonumentsByRegion extends Grouping[Monument, String]("Region KOATUU code", _.regionId)

object MonumentsWithArticles
    extends Aggregation[Monument, Int](
      "With Articles",
      _.count(_.name.contains("[["))
    )

object All extends Aggregation[Any, Int]("All", _.size)

class RegionNameById(country: AdmDivision)
    extends Mapping[String, String]("Region name", country.regionName)
