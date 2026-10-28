package org.scalawiki.wlx

import org.scalawiki.dto.markup.Template
import org.specs2.mutable.Specification

class WlxTemplateParserSpec extends Specification {

  def dataToWiki(data: Seq[Seq[(String, String)]]): String = {
    val templates = data.map { params =>
      new Template("templateName", params.toMap)
    }

    templates.map(_.text).mkString("\n")
  }

  "parser" should {

    "parse id and name fields" in {

      val data = Seq(
        Seq("_ID" -> "id1", "_name" -> "name1"),
        Seq("_ID" -> "id2", "_name" -> "name2")
      )

      val text = dataToWiki(data)

      val parser = new WlxTemplateParser(IdNameConfig, "page")
      val monuments = parser.parse(text)

      monuments.size === 2
      monuments.map(_.page).toSet === Set("page")
      monuments.map(m => Seq(m.id, m.name)) === data.map(_.toMap.values.toSeq)
    }

    "parse id and name and pass other fields" in {

      val data = Seq(
        Seq("_ID" -> "id1", "_name" -> "name1", "_f1" -> "d11", "_f2" -> "d12"),
        Seq("_ID" -> "id2", "_name" -> "name2", "_f1" -> "d21", "_f2" -> "d22")
      )

      val text = dataToWiki(data)

      val parser = new WlxTemplateParser(IdNameConfig, "page")
      val monuments = parser.parse(text)

      monuments.size === 2
      monuments.map(_.page).toSet === Set("page")
      monuments.map(m => Seq(m.id, m.name, m.otherParams("_f1"), m.otherParams("_f2"))) === data
        .map(_.toMap.values.toSeq)
    }
  }
}
