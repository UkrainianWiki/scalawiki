package org.scalawiki.wikitext

import org.scalawiki.dto.markup.Table
import org.sweble.wikitext.engine.config.WikiConfig
import org.sweble.wikitext.engine.utils.DefaultConfigEnWp
import org.sweble.wikitext.parser.nodes._

object TableParser extends SwebleParser {

  val config: WikiConfig = DefaultConfigEnWp.generate

  val headerFunc: PartialFunction[WtNode, WtTableHeader] = { case h: WtTableHeader =>
    h
  }

  val cellFunc: PartialFunction[WtNode, WtTableCell] = { case c: WtTableCell =>
    c
  }

  val headerOrCell = cellFunc orElse headerFunc

  def parse(wiki: String): Table = {

    val page = parsePage("Some title", wiki).getPage

    findNode(page, { case t: WtTableImplicitTableBody => t })
      .map { tableBody =>
        val rows = collectNodes(tableBody, { case r: WtTableRow => r })

        val headers = rows.headOption.toSeq.flatMap { head =>
          nodesToText(head, headerFunc)
        }

        val dataRows = if (headers.isEmpty) rows else rows.drop(1)

        val items = dataRows
          .map { row =>
            nodesToText(row, headerOrCell)
          }
          .filter(_.nonEmpty)

        new Table(headers, items, "", "")

      }
      .getOrElse(new Table(Seq.empty, Seq.empty, "", ""))
  }

}
