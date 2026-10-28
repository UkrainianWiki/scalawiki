package org.scalawiki.dto.markup

import de.fau.cs.osr.ptk.common.ast.RtData
import org.sweble.wikitext.parser.nodes.{WtTemplate, WtTemplateArgument}

import scala.collection.JavaConverters._
import scala.collection.mutable

case class SwTemplate(wtNode: WtTemplate) extends SwNode {

  val name: String = getText(wtNode.getName).trim
  val args: mutable.Buffer[WtTemplateArgument] =
    wtNode.getArgs.asScala.collect { case arg: WtTemplateArgument => arg }
  val template = getTemplate

  def getTemplate: Template = {
    val argsMap = args.zipWithIndex.map { case (arg, index) =>
      val name =
        if (arg.hasName)
          getText(arg.getName).trim
        else
          (index + 1).toString

      val value = getText(arg.getValue).trim

      name -> value
    }.toMap

    Template(name, argsMap)
  }

  def getArg(name: String): Option[WtTemplateArgument] =
    args.find(p => p.getName.isResolved && p.getName.getAsString.trim == name)

  def setTemplateParam(name: String, value: String): Unit = {
    getArg(name) match {
      case Some(arg) =>
        val orig = getText(arg.getValue)

        val padded =
          if (orig.startsWith(" ") && !value.startsWith(" ")) " " + value
          else value
        val withNl =
          if (orig.endsWith("\n") && !value.endsWith("\n")) padded + "\n"
          else padded

        val f = NodeFactory
        val node = f.value(f.list(f.text(withNl)))
        arg.setValue(node)

      case None =>
        addTemplateParam(name, value)
    }
  }

  /** Appends a new named parameter to the template. Used when the list template gained a field
    * (e.g. WLM UA lists' new `бали` rating field) that a given row does not carry yet, so
    * [[getArg]] finds nothing to update.
    */
  def addTemplateParam(name: String, value: String): Unit = {
    val f = NodeFactory
    val arg = f.tmplArg(
      f.name(f.list(f.text(name))),
      f.value(f.list(f.text(" " + value + "\n")))
    )
    // Round-trip data: separator before the name, " =" between name and value,
    // "" after. Monument list rows put one parameter per line, so start a new
    // line ourselves unless the preceding value already ends with one (which
    // would otherwise glue "| name =" onto the previous line).
    val prevEndsWithNl =
      args.lastOption.exists(a => getText(a.getValue).endsWith("\n"))
    arg.setRtd(
      if (prevEndsWithNl) "| " else "\n| ",
      RtData.SEP,
      " =",
      RtData.SEP,
      ""
    )
    wtNode.getArgs.add(arg)
    // keep `args` (which getArg reads) in sync with the AST so a later
    // setTemplateParam for this name updates it instead of appending a duplicate
    args += arg
  }
}
