package org.scalawiki.wlx.stat.generic

import org.scalawiki.dto.markup.Table

class Aggregation[-T, +V](val name: String, aggFunc: Iterable[T] => V)
    extends Function[Iterable[T], V] {

  override def apply(t: Iterable[T]): V = aggFunc(t)
}

class Grouping[T, V](val name: String, groupFunc: T => V)
    extends Function[Iterable[T], Map[V, Iterable[T]]] {

  override def apply(t: Iterable[T]): Map[V, Iterable[T]] = t.groupBy(groupFunc)
}

class Mapping[-T, +V](val name: String, mapFunc: T => V) extends Function[T, V] {

  override def apply(t: T): V = mapFunc(t)
}

class PercentageAggregation[T](
    name: String,
    valueAggregation: Aggregation[T, Int]
) extends Aggregation[T, Long](
      name,
      t => (valueAggregation(t) * 100.0 / t.size).round
    )

class StringColumnOrdering(column: Int, reverse: Boolean = false) extends Ordering[Seq[Any]] {
  override def compare(x: Seq[Any], y: Seq[Any]): Int =
    x(column).toString
      .compareTo(
        y(column).toString
      ) * (if (reverse) -1 else 1)
}

class LongColumnOrdering(column: Int, reverse: Boolean = false) extends Ordering[Seq[Any]] {

  override def compare(x: Seq[Any], y: Seq[Any]): Int =
    x(column).toString.toLong
      .compareTo(
        y(column).toString.toLong
      ) * (if (reverse) -1 else 1)
}

class Records[T, RK](
    data: Iterable[T],
    rowGrouping: Grouping[T, RK],
    columnAggregations: Seq[Aggregation[T, Any]],
    rowOrdering: Ordering[Seq[Any]],
    rowKeyMapping: Option[Mapping[RK, Any]] = None
) {
  val byRowKey: Map[RK, Iterable[T]] = rowGrouping(data)

  val rows = byRowKey
    .map { case (rk, tSeq) =>
      Seq(rk) ++
        rowKeyMapping.map(_(rk)).toSeq ++
        columnAggregations.map(_(tSeq))
    }
    .toSeq
    .sorted(rowOrdering)

  val total: Seq[Any] = Seq("Total") ++ rowKeyMapping.map(_ => "Total").toSeq ++
    columnAggregations.map(_(data))

  val headers: Seq[String] = Seq(rowGrouping.name) ++
    rowKeyMapping.map(_.name).toSeq ++
    columnAggregations.map(_.name)

  def rowsWithTotal(totalAtTheEnd: Boolean = true): Seq[Seq[Any]] =
    if (totalAtTheEnd)
      rows ++ Seq(total)
    else
      Seq(total) ++ rows

  def asWiki(title: String, totalAtTheEnd: Boolean = true) = {
    val strData = rowsWithTotal(totalAtTheEnd).map(_.map(_.toString))
    new Table(headers, strData, title)
  }

}
