package org.scalawiki.wlx

import org.scalawiki.dto.markup.Table
import org.scalawiki.util.TestUtils._
import org.scalawiki.wlx.dto.lists.ListConfig.WleTh
import org.specs2.mutable.Specification

class WlxTableParserSpec extends Specification {

  "parser" should {

    "parse name field and generate id" in {

      val headers =
        Seq("_name")

      val data = Seq(
        Seq("name1"),
        Seq("name2")
      )

      val table = new Table(headers, data)
      val parser = new WlxTableParser(NameConfig)
      val monuments = parser.parse(table.asWiki)

      monuments.size === 2
      monuments.map(m => Seq(m.name)) === data
      monuments.map(_.id) === Seq("1", "2")
    }

    "parse id and name fields" in {

      val headers =
        Seq("_ID", "_name")

      val data = Seq(
        Seq("id1", "name1"),
        Seq("id2", "name2")
      )

      val table = new Table(headers, data)
      val parser = new WlxTableParser(IdNameConfig)
      val monuments = parser.parse(table.asWiki)

      monuments.size === 2
      monuments.map(m => Seq(m.id, m.name)) === data
    }

    "parse id and name and pass other fields" in {

      val headers =
        Seq("_ID", "_name", "_f1", "_f2")

      val data = Seq(
        Seq("id1", "name1", "d11", "d12"),
        Seq("id2", "name2", "d21", "d22")
      )

      val table = Table(headers, data)
      val parser = new WlxTableParser(IdNameConfig)
      val monuments = parser.parse(table.asWiki)

      monuments.size === 2
      monuments.map(m => Seq(m.id, m.name, m.otherParams("_f1"), m.otherParams("_f2"))) === data
    }

    "parse thailand" in {
      val wiki = resourceAsString("/org/scalawiki/wlx/thailand_wle_table.wiki")
      val parser = new WlxTableParser(WleTh)
      val monuments = parser.parse(wiki)

      monuments.size === 148
      monuments.map(_.id) === (1 to 148).map(_.toString)
    }
  }
}
