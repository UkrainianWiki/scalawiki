package org.scalawiki.bots

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

abstract class OpTable() {

  def name: String

  def dateCol: Int

  def amountCol: Int

  def descrCol: Int

  def ops: Seq[Seq[String]]

  def opDescr(op: Seq[String]) = op(amountCol) + " " + op(descrCol)

  def opDate(op: Seq[String]) = op(dateCol)

  def opAmountStr(op: Seq[String]) = op(amountCol)

  def opAmount(op: Seq[String]) = toDouble(op(amountCol))

  def toDouble(s: String) = s match {
    case x if x.nonEmpty => x.toDouble
    case _               => 0.0
  }

  lazy val byDate = ops.groupBy(opDate)
  lazy val byAmount = ops.groupBy(opAmountStr)

  def byDateAndAmount(date: String, amount: String): Seq[Seq[String]] =
    byDate(date).filter(op => opAmountStr(op) == amount)

  def sum = ops.map(opAmount).sum

  def sumPlus = ops.map(opAmount).filter(_ > 0).sum

  def sumByDate: Map[String, Double] =
    byDate.mapValues(_.map(opAmount).filter(_ > 0).sum).toMap

  def diffByDate(that: OpTable) = {
    val thisMap = this.sumByDate
    val thatMap = that.sumByDate
    val allKeys = (thisMap.keySet ++ thatMap.keySet).toSeq.sorted

    allKeys
      .map { date =>
        val thisV = thisMap.getOrElse(date, 0.0)
        val thatV = thatMap.getOrElse(date, 0.0)

        if (thisV - thatV <= 0.01)
          "" // s"the same: $thisV"
        else {
          val tab = List.fill(10)(" ").mkString("")
          s"$date Diff: ${thisV - thatV}; $name :$thisV; ${that.name}: $thatV \n" +
            this.name + ":\n" +
            this.byDate
              .getOrElse(date, Nil)
              .map(op => tab + opDescr(op))
              .mkString("\n") + "\n" +
            that.name + ":\n" +
            that.byDate
              .getOrElse(date, Nil)
              .map(op => tab + that.opDescr(op))
              .mkString("\n")
        }
      }
      .filter(_.nonEmpty)
      .mkString("\n")
  }

  def diff[T](f: OpTable => Double, that: OpTable) = {
    val thisV = f(this)
    val thatV = f(that)

    if (thisV == thatV)
      "the same"
    else
      s"Diff: ${thisV - thatV}; $name:$thisV; ${that.name}: $thatV"
  }

}

class AppOps(val rows: Seq[Seq[String]]) extends OpTable() {

  override def name = "App"

  def indexOf(name: String) = rows.head.indexOf(name)

  override val dateCol = indexOf("Date")
  override val amountCol = indexOf("Amount")
  override val descrCol = indexOf("Description")

  override val ops = rows.tail.sortBy(_(dateCol))

}

case class Exchange(date: String, uah: Double, usd: Double, rate: Double)

class BankOps(val rows: Seq[Seq[String]]) extends OpTable() {

  override def name = "Bank"

  def indexOf(name: String) = rows.head.indexOf(name)

  override val dateCol = indexOf("Дата операцiї")
  override val amountCol = indexOf("Дебет")
  override val descrCol = indexOf("Призначення платежу")
  val dateCol2 = indexOf("Дата документа")
  val creditCol = indexOf("Кредит")
  def credit(op: Seq[String]) = toDouble(op(creditCol))

  override val ops = mapDates(rows.tail)

  val byDate2 = ops.groupBy(_(dateCol2))

  private def mapDates(rows: Seq[Seq[String]]) = {
    def dateConv(s: String) =
      s.split(" ").head.split("\\.").reverse.mkString("-")

    def rowConv(row: Seq[String]) = row.zipWithIndex.map {
      case (date, i) if Set(dateCol, dateCol2).contains(i) =>
        dateConv(date)
      case (x, _) => x
    }

    rows.map(rowConv)
  }

  def rate(op: Seq[String]): Option[Exchange] = {
    val uah = credit(op)
    val descr = op(descrCol)
    for (
      usd <- "(\\d+\\.\\d+) USD".r
        .findFirstMatchIn(descr)
        .map(_.group(1).toDouble)
    ) yield Exchange(op(dateCol), uah, usd, uah / usd)
  }

  def rates = ops.flatMap(rate)
}

object CsvComparator {
  val UA_CSV = new DefaultCSVFormat {
    override val delimiter = ';'
  }

  def bankRaw(path: String) = CSVReader.open(path, "cp1251")(UA_CSV).all()
  def appRaw(path: String) = CSVReader.open(path).all()

  val bankDateExample = "18.01.2016 15:00"
  val appDateExample = "2016-12-30"

  /** usage: CsvComparator <bank statement csv (cp1251, ';')> <app export csv> */
  def main(args: Array[String]): Unit = {
    require(args.length == 2, "usage: CsvComparator <bank statement csv> <app export csv>")
    val bankOps = new BankOps(bankRaw(args(0)))
    val appOps = new AppOps(appRaw(args(1)))

    val sumDiff = appOps.diff(_.sumPlus, bankOps)
    println(sumDiff)

    println(appOps.diffByDate(bankOps))
    bankOps.rates.foreach(println)
  }
}
