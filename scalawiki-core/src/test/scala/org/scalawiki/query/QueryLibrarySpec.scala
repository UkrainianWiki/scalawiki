package org.scalawiki.query

import org.scalawiki.dto.cmd.Action
import org.specs2.mutable.Specification

class QueryLibrarySpec extends Specification with QueryLibrary {

  private def iiprop(action: Action): Option[String] = action.pairs.toMap.get("iiprop")

  "imagesByIds" should {
    "request the media type only when asked" in {
      iiprop(imagesByIds(Seq(1L))) === Some("timestamp|user|size")
      iiprop(imagesByIds(Seq(1L), withMediaType = true)) === Some("timestamp|user|size|mediatype")
    }
  }
}
