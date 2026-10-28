package org.scalawiki.edit

trait PageUpdateTask {
  def host: String
  def titles: Iterable[String]

  /** Produce the new wikitext (and edit summary) for `title` from its current `text`.
    *
    * Must be idempotent with respect to its own output: [[PageUpdater]] may re-run this against a
    * freshly read revision if the save loses an edit conflict, so applying it to already-updated
    * text has to yield the same result (rewrite specific values in place; never append or
    * accumulate).
    */
  def updatePage(title: String, text: String): (String, String)
}
