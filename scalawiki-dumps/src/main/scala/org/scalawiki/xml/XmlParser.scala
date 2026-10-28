package org.scalawiki.xml

import java.io.{Reader, InputStream, StringReader, File}
import javax.xml.stream.{XMLStreamReader, XMLStreamConstants, XMLInputFactory}

import org.codehaus.stax2.{LocationInfo, XMLStreamReader2, XMLInputFactory2}
import org.scalawiki.Timestamp
import org.scalawiki.dto._
import org.scalawiki.dto.filter.PageFilter
import org.scalawiki.dto.Image

import scala.collection.{Iterator, AbstractIterator}

case class SiteInfo(
    name: Option[String],
    db: Option[String],
    generator: Option[String]
)

class XmlParser(
    val parser: XMLStreamReader2 with LocationInfo,
    val pageFilter: Page => Boolean = PageFilter.all
) extends Iterable[Page] {

  private var _siteInfo: Option[SiteInfo] = None
  private var _namespaces: Map[Int, String] = Map.empty
  private var parsedSiteInfo: Boolean = false
  private var _pageStartingByteOffset: Long = -1

  def pageStartingByteOffset = _pageStartingByteOffset

  override def iterator = {

    readSiteInfo()

    val iterator = new AbstractIterator[Page] {

      override def hasNext = findElementStart("page")

      override def next() = readPage().getOrElse(Iterator.empty.next())

    }

    iterator.filter(pageFilter)
  }

  def siteInfo = {
    readSiteInfo()
    _siteInfo
  }

  def namespaces = {
    readSiteInfo()
    _namespaces
  }

  def close() =
    parser match {
      case stax2Parser: XMLStreamReader2 => stax2Parser.closeCompletely()
      case _                             => parser.close()
    }

  private def readSiteInfo() = {
    if (!parsedSiteInfo) {
      if (findElementStart("siteinfo", "page")) {
        val sitename = readElement("sitename")
        val dbname = readElement("dbname")
        val generator = readElement("generator")

        _siteInfo = Some(SiteInfo(sitename, dbname, generator))
        readNamespaces()
      }
      parsedSiteInfo = true
    }
  }

  private def readNamespaces(): Unit = {
    if (findElementStart("namespaces")) {

      while (findElementStart("namespace", parent = "namespaces")) {

        val key = parser.getAttributeValue("", "key").toInt
        val prefix = parser.getElementText

        _namespaces += (key -> prefix)
      }
      findElementEnd("namespaces")
    }
  }

  private def readPage(): Option[Page] =
    if (findElementStart("page")) {
      _pageStartingByteOffset = parser.getStartingByteOffset
      for (
        title <- readElement("title");
        ns <- readElement("ns").map(_.toInt);
        id <- readElement("id").map(_.toLong)
      ) yield {
        val revisions = readRevisions(id)
        val images = readImageInfo()
        new Page(Some(id), Some(ns), title, revisions.toSeq)
      }
    } else
      None

  private def readRevisions(pageId: Long): Seq[Revision] = {
    // TODO streaming and filtering
    var revisions = Seq.empty[Revision]
    while (findElementStart("revision", next = "upload", parent = "page")) {

      readElement("id").map(_.toLong).foreach { id =>
        val parentId = readElement("parentid", "timestamp").map(_.toLong)
        val timestamp = readElement("timestamp").map(Timestamp.parse)

        val user = readUser()

        val comment = readElement("comment")
        val text = readElement("text")
        val sha1 = readElement("sha1")

        findElementEnd("revision")

        revisions :+=
          Revision(
            revId = Some(id),
            pageId = Some(pageId),
            parentId = parentId,
            user = user,
            timestamp = timestamp,
            comment = comment,
            content = text,
            sha1 = sha1
          )
      }
    }
    revisions
  }

  def readUser(): Option[Contributor] = {
    findElementStart("contributor")

    val user: Option[Contributor] = (
      for (
        username <- readElement(
          "username",
          next = "ip",
          parent = "contributor"
        );
        userId <- readElement("id", "ip", "contributor").map(_.toInt)
      )
        yield new User(Some(userId), Some(username))
    )
      .orElse(
        readElement("ip", parent = "contributor").map(new IpContributor(_))
      )
    user
  }

  private def readImageInfo(): Option[Image] = {
    //    // TODO seq
    //    if (findElementStart("upload", parent = "page")) {
    //      val user = readUser()
    //      Some(Image())
    //    } else
    None
  }

  private def readElement(
      name: String,
      next: String = "",
      parent: String = ""
  ): Option[String] = {
    if (findElementStart(name, next, parent))
      Some(parser.getElementText)
    else
      None
  }

  private def findElementStart(
      name: String,
      next: String = "",
      parent: String = ""
  ): Boolean = {
    parser.getEventType match {
      case XMLStreamConstants.START_ELEMENT if parser.getLocalName == name =>
        true
      case XMLStreamConstants.START_ELEMENT if parser.getLocalName == next =>
        false
      case XMLStreamConstants.END_ELEMENT if parser.getLocalName == parent =>
        false
      case _ =>
        while (parser.hasNext) {
          val event = parser.next()
          event match {
            case XMLStreamConstants.START_ELEMENT if parser.getLocalName == name =>
              return true
            case XMLStreamConstants.START_ELEMENT if parser.getLocalName == next =>
              return false
            case XMLStreamConstants.END_ELEMENT if parser.getLocalName == parent =>
              return false
            case _ =>
          }
        }
        false
    }
  }

  private def findElementEnd(name: String) = {
    if (parser.getEventType != XMLStreamConstants.END_ELEMENT || parser.getLocalName != name)
      while (
        parser
          .next() != XMLStreamConstants.END_ELEMENT || parser.getLocalName != name
      ) {}
  }

}

object XmlParser {

  val xmlInputFactory = newXmlInputFactory

  def newXmlParser(
      xmlReader: XMLStreamReader,
      pageFilter: Page => Boolean = PageFilter.all
  ) =
    new XmlParser(
      xmlReader.asInstanceOf[XMLStreamReader2 with LocationInfo],
      pageFilter
    )

  def parseFile(
      filename: String,
      pageFilter: Page => Boolean = PageFilter.all
  ) =
    newXmlParser(
      xmlInputFactory.createXMLStreamReader(new File(filename)),
      pageFilter
    )

  def parseInputStream(
      is: InputStream,
      pageFilter: Page => Boolean = PageFilter.all
  ) =
    newXmlParser(xmlInputFactory.createXMLStreamReader(is), pageFilter)

  def parseReader(
      reader: Reader,
      pageFilter: Page => Boolean = PageFilter.all
  ) =
    newXmlParser(xmlInputFactory.createXMLStreamReader(reader), pageFilter)

  def parseString(data: String, pageFilter: Page => Boolean = PageFilter.all) =
    parseReader(new StringReader(data), pageFilter)

  //  def parseUrl(url: URL) = new XmlParser(xmlInputFactory.createXMLStreamReader(url))

  def newXmlInputFactory: XMLInputFactory2 = {
    val xmlInputFactory =
      XMLInputFactory.newInstance().asInstanceOf[XMLInputFactory2]
    xmlInputFactory.configureForSpeed()
    xmlInputFactory.setProperty(
      XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES,
      false
    )
    xmlInputFactory
  }
}
