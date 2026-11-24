package org.scalawiki.bots.museum

import java.nio.file.{FileSystem, FileSystems}

import better.files.File.Order
import better.files.{File => SFile}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import org.scalawiki.MwBot
import org.scalawiki.bots.FileUtils._
import org.scalawiki.dto.Site
import org.scalawiki.dto.markup.Table
import org.scalawiki.wikitext.TableParser
import org.scalawiki.wlx.dto.Monument
import org.scalawiki.wlx.dto.lists.ListConfig.WlmUa

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Pereiaslav(conf: Config, fs: FileSystem = FileSystems.getDefault) {

  val home = conf.getString("home")

  val lang = conf.getString("lang")

  def ukWiki = MwBot.fromHost(conf.getString("host"))

  val tablePage = conf.getString("table-page")

  val wlmPage = conf.getString("wlm-page")

  val convertDocs = conf.getOrElse[Boolean]("convert-docs", false)

  val sep = fs.getSeparator

  private val homeDir = SFile(fs.getPath(home))

  def getEntries: Future[Seq[Entry]] = {
    for (entries <- fromWikiTable(tablePage)) yield entries.map { entry =>
      val objDir = homeDir / entry.dir
      val files = list(objDir, isImage)
      val paths = files.map(_.pathAsString)

      val descriptions =
        getImagesDescr(objDir, paths).map(Option.apply).padTo(paths.size, None)
      val images = files.zip(descriptions).map { case (f, d) =>
        EntryImage(f.pathAsString, d, size = Some(f.size))
      }

      entry.copy(images = images, text = getArticleText(objDir))
    }
  }

  def fromWikiTable(tablePage: String): Future[Seq[Entry]] = {
    for (text <- ukWiki.pageText(tablePage)) yield {
      val table = TableParser.parse(text)
      table.data.map(Entry.fromRow).toSeq
    }
  }

  def getImagesDescr(dir: SFile, files: Seq[String]): Seq[String] = {
    docsToHtml(dir)
    val descFile = list(dir, isHtml)(Order.bySize).headOption.toSeq
    descFile.flatMap { file =>
      val content = file.contentAsString
      val lines = HtmlParser.trimmedLines(content)
      val filenames =
        files.map(path => SFile(path).nameWithoutExtension.toLowerCase)
      lines.filter { line =>
        filenames.exists(line.toLowerCase.startsWith) || line.head.isDigit
      }
    }
  }

  def docsToHtml(dir: SFile): Unit = {
    if (convertDocs) {
      val docs = list(dir, isDoc).map(_.toJava)
      ImageListParser.docToHtml(docs)
    }
  }

  def getArticleText(dir: SFile): Option[String] = {
    list(dir, isHtml)(Order.bySize).lastOption.map { file =>
      HtmlParser.htmlText(file.contentAsString)
    }
  }

  def makeTable(dirs: Seq[String]) = {

    val headers = Seq("dir", "article", "wlmId")
    val data = dirs.map(d => Seq(d, d, ""))
    val string = new Table(headers, data).asWiki
    ukWiki.page(conf.getString("test-page")).edit("test", multi = false)
  }

  def makeUploadFiles(entries: Seq[Entry]): Unit = {
    entries.foreach { entry =>
      val file = homeDir / entry.dir / "upload.conf"
      val text = entry.toConfigString
      if (!file.exists) {
        println(s"Creating ${file.pathAsString}")
        file.overwrite(text)
      }
    }
  }

  def readUploadFiles(entries: Seq[Entry]): Seq[Entry] = {
    entries.flatMap { entry =>
      val file = homeDir / entry.dir / "upload.conf"
      if (file.exists) {
        val cfg = ConfigFactory.parseFile(file.toJava)
        val loaded = Entry.fromConfig(cfg, entry.dir)
        Some(loaded)
      } else None
    }
  }

  def makeGallery(entries: Seq[Entry]): Unit = {
    HtmlOutput.makeGalleries(entries).zipWithIndex.foreach { case (h, i) =>
      SFile(s"$home/e$i.html").overwrite(h)
    }
  }

  def wlm(): Seq[Monument] = {
    val ukWiki = MwBot.fromHost(MwBot.ukWiki)
    val text = ukWiki.await(ukWiki.pageText(wlmPage))

    Monument.monumentsFromText(text, wlmPage, WlmUa.templateName, WlmUa).toSeq
  }

  def stat(entries: Seq[Entry]) = {

    val objects = entries.size
    val images = entries.map(_.images.size).sum
    //    val bytes = entries.map(_.images.flatMap(_.size).sum).sum
    val bytes = entries.map(_.images.map(i => SFile(i.filePath).size).sum).sum
    val Mb = bytes / (1024 * 1024)

    println(s"Objects: $objects, images: $images, MBytes: $Mb")
  }

  def run() = {

    getEntries.map { original =>
      val genOrinal = original.map(_.genImageFields)

      val loaded = readUploadFiles(original)
      val genLoaded = loaded.map(_.genImageFields)

      stat(genLoaded)

      val commons = Site.commons
      val bot = MwBot.fromSite(commons)

      val images = genLoaded.flatMap(_.images)
      val count = images.size

      images.zipWithIndex.foreach { case (image, i) =>
        val file = SFile(image.filePath)
        val size = file.size

        val fileTitle = image.uploadTitle.get + file.extension.getOrElse("")
        val fileFileTitle = "File:" + fileTitle
        val url = commons.pageUrl(fileFileTitle, urlEncode = false)
        val link = s"""    <li> <a href="$url">$url</a>"""
        println(link)
//          print(s"Uploading image $i/$count $fileTitle ${size / 1024} KB ")
      // val result = upload(image, bot).await

      // println(result)
      }

    }
  }

  def upload(entry: EntryImage, bot: MwBot): Future[String] = {
    val page = ImageTemplate.makeInfoPage(
      entry.uploadTitle.get,
      entry.wikiDescription.get,
      ""
    )
    val file = SFile(entry.filePath)
    val fileTitle = entry.uploadTitle.get + file.extension.getOrElse("")

    bot
      .page("File:" + fileTitle)
      .upload(entry.filePath, Some(page), ignoreWarnings = true)
  }

  //      genLoaded.zip(loaded).foreach {
  //        case (o, l) =>
  //          val diffs = o.genImageFields.diff(l)
  //          if (diffs.nonEmpty) {
  //            println(o.dir)
  //            diffs.foreach(println)
  //          }
  //      }

  // val withSourceDescr = l.updateFrom(o, Set.empty, Set("sourceDescription"))

  // FileUtils.writeWithBackup(homeDir / l.dir / "upload.conf", withSourceDescr.toConfigString)
  //      }
  //      loaded.foreach {
  //        entry =>
  //          val diffs = entry.genImageFields.diff(entry)
  //          if (diffs.nonEmpty) {
  //            println(entry.dir)
  //            diffs.foreach(println)
  //          }
  //      }
  //    }

  //    getEntries.map { entries =>
  //      makeGallery(entries)
  //      makeUploadFiles(entries)
  //    } onFailure { case e => println(e) }
  //  }

}

object Pereiaslav {

  def main(args: Array[String]) {

    //    val localhost = Site.localhost
    //    val bot = MwBot.get(localhost)

    val conf = ConfigFactory.load("pereiaslav.conf")
    val pereiaslav = new Pereiaslav(conf)
    pereiaslav.run()
  }
}
