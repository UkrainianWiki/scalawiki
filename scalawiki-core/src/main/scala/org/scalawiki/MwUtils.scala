package org.scalawiki

object MwUtils {

  /** Convenience method for normalizing MediaWiki titles. (Converts all spaces to underscores and
    * makes the first letter caps).
    *
    * @param s
    *   the string to normalize
    * @return
    *   the normalized string
    */
  def normalize(s: String): String = s.capitalize.replaceAll(" ", "_")

}
