package org.scalawiki.dto.cmd

import java.time.ZonedDateTime

import org.scalawiki.Timestamp
import org.scalawiki.dto.cmd.query.Query

trait Parameter[+T] extends Equals {
  def name: String

  def summary: String

  def pairs: Seq[(String, String)]

  def flatten: Seq[Parameter[Any]] = Seq(this)
}

abstract class EnumParameter[ARG <: EnumArg[ARG]](
    val name: String,
    val summary: String
) extends Parameter[EnumArg[ARG]] {

  var allArgs: Seq[EnumArg[ARG]] = Seq.empty

  def args: Seq[EnumArg[ARG]] = Seq(arg)

  def arg: EnumArg[ARG] = args.head

  override def pairs: Seq[(String, String)] = {
    Seq(name -> args.map(_.name).mkString("|")) ++ args.flatMap(_.pairs)
  }

  def byPF[T](pf: PartialFunction[EnumArg[ARG], T]) = args.collectFirst(pf)

  override def flatten = Seq(this)

}

abstract class ListParameter[T] extends Parameter[T] {

  def args: Iterable[T]

  override def pairs: Seq[(String, String)] = {
    Seq(name -> args.mkString("|"))
  }
}

abstract class SingleParameter[T] extends Parameter[T] {

  def arg: T

  override def pairs: Seq[(String, String)] = {
    Seq(name -> arg.toString)
  }
}

abstract class StringListParameter(val name: String, val summary: String)
    extends ListParameter[String]

abstract class IntListParameter(val name: String, val summary: String) extends ListParameter[Int]

abstract class LongListParameter(val name: String, val summary: String) extends ListParameter[Long]

abstract class IdListParameter(val name: String, val summary: String) extends ListParameter[Long]

abstract class StringParameter(val name: String, val summary: String)
    extends SingleParameter[String]

abstract class IntParameter(val name: String, val summary: String) extends SingleParameter[Int]

abstract class LongParameter(val name: String, val summary: String) extends SingleParameter[Long]

abstract class IdParameter(val name: String, val summary: String) extends SingleParameter[Long]

abstract class DateTimeParameter(val name: String, val summary: String)
    extends SingleParameter[ZonedDateTime] {

  override def pairs: Seq[(String, String)] = Seq(name -> Timestamp.format(arg))
}

abstract class BooleanParameter(val name: String, val summary: String)
    extends SingleParameter[Boolean]

abstract class ByteArrayParameter(val name: String, val summary: String)
    extends SingleParameter[Array[Byte]]

trait ArgWithParams[P <: Parameter[Any], T <: EnumArg[T]] extends EnumArg[T] {
  def params: Seq[P] = Seq.empty

  def byType[X: Manifest]: Seq[X] =
    params.collect {
      case x if manifest[X].runtimeClass.isInstance(x) => x.asInstanceOf[X]
    }

  def byPF[P1](pf: PartialFunction[P, P1]): Seq[P1] = params.collect(pf)

  override def pairs: Seq[(String, String)] = params.flatMap(_.pairs)
}

trait EnumArg[+T <: EnumArg[T]] {
  //  def param: EnumParameter[T]
  def name: String

  def summary: String

  def pairs: Seq[(String, String)] = Seq.empty
}

abstract class EnumArgument[T <: EnumArg[T]](
    val name: String,
    val summary: String = ""
) extends EnumArg[T]

trait ActionArg extends EnumArg[ActionArg] {
  /*val param = ActionParam*/
}

case class Action(override val arg: ActionArg) extends EnumParameter[ActionArg]("action", "") {
  def query: Option[Query] = args.collectFirst { case q: Query => q }
  override def toString = pairs.toString()
}
