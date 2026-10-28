package org.scalawiki.bots

import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern

import better.files.File.{Order, PathMatcherSyntax}
import better.files.{File => SFile}
import org.scalawiki.AlphaNumOrdering

import scala.collection.mutable
import scala.io.{Codec, Source}

/** Save and load lines to/from file
  */
object FileUtils {

  val nl = System.lineSeparator

  val byAlphaNumName = Ordering.by[SFile, String](_.name)(AlphaNumOrdering)

  /** Read lines from file
    *
    * @param filename
    *   file to read
    * @param codec
    *   character encoding/decoding preferences, default is [[scala.io.Codec.defaultCharsetCodec()]]
    * @return
    *   lines from the file
    */
  def read(filename: String)(implicit codec: Codec): mutable.Buffer[String] =
    Source.fromFile(filename).getLines.toBuffer

  /** Save lines to file, overwriting it
    *
    * @param filename
    *   file to write to
    * @param lines
    *   lines to save
    * @param codec
    *   character encoding/decoding preferences, default is [[scala.io.Codec.defaultCharsetCodec()]]
    * @return
    */
  def write(filename: String, lines: Seq[String])(implicit codec: Codec): Path =
    write(filename, lines.mkString(nl))

  def write(filename: String, content: String)(implicit codec: Codec): Path =
    Files.write(Paths.get(filename), content.getBytes(codec.charSet))

  def writeWithBackup(file: SFile, content: String)(implicit codec: Codec) = {
    if (file.exists) {
      val backup = backupName(file)
      if (file.contentAsString != content) {
        file.moveTo(file.parent / backup)
      }
    }
    file.overwrite(content)
  }

  def backupName(file: SFile): String = {
    val pattern = Pattern.quote(file.pathAsString) + "\\.(\\d+)"
    val maybeLast = file.parent
      .glob(pattern)(PathMatcherSyntax.regex)
      .toSeq
      .sortBy(_.name)(AlphaNumOrdering)
      .lastOption
    val number = maybeLast.fold(1) { last =>
      pattern.r
        .findFirstMatchIn(last.pathAsString)
        .fold(1)(_.group(1).toInt + 1)
    }
    file.name + "." + number
  }

  /** @param dir
    *   directory
    * @return
    *   subdirectories, sorted by name
    */
  def subDirs(dir: SFile): Seq[SFile] =
    list(dir, _.isDirectory)

  /** @param dir
    *   directory
    * @return
    *   regular files, sorted by name
    */
  def getFiles(dir: SFile): Seq[SFile] =
    list(dir, _.isRegularFile)

  /** @param dir
    *   directory
    * @param predicate
    * @return
    *   directory members filtered by predicate
    */
  def list(dir: SFile, predicate: SFile => Boolean)(implicit
      order: Order = byAlphaNumName
  ): Seq[SFile] =
    dir.list.filter(predicate).toSeq.sorted(order)

  def isImage(f: SFile): Boolean = hasExt(f, Set(".jpg", ".tif"))

  def isDoc(f: SFile): Boolean = hasExt(f, Set(".doc", ".docx"))

  def isHtml(f: SFile): Boolean = hasExt(f, Set(".htm", ".html"))

  def hasExt(file: SFile, extensions: Set[String]): Boolean =
    extensions.contains(file.extension.getOrElse("."))

}
