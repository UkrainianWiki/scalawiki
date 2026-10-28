package org.scalawiki.wlx.stat.reports

import org.jfree.data.category.DefaultCategoryDataset
import org.scalawiki.{ActionBot, MwBot}
import org.scalawiki.dto.{Image, User}
import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.{Query, TitlesParam}
import org.scalawiki.dto.cmd.query.list.{ListParam, Users, UsUsers}
import org.scalawiki.dto.cmd.query.prop.{Prop, Revisions}
import org.scalawiki.dto.cmd.query.prop.rvprop.{Content, RvProp}
import org.scalawiki.dto.markup.Table
import org.scalawiki.wlx.dto.Contest
import org.scalawiki.wlx.stat.{ContestStat, StatConfig}
import org.scalawiki.wlx.{ImageDB, MonumentDB}
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.Try

class AuthorsStat(val uploadImages: Boolean = false) {

  var userImages: Option[String] = None

  val charts = new Charts()

  def authorsStat(data: ContestStat, bot: MwBot, gallery: Boolean) = {
    new AuthorMonuments(data, gallery = gallery, commons = Some(bot))
      .updateWiki(bot)
  }

  def authorsContributed(
      imageDbs: Seq[ImageDB],
      totalImageDb: ImageDB,
      monumentDb: Option[MonumentDB]
  ): String = {

    val table = authorsContributedTable(imageDbs, totalImageDb, monumentDb)

    val header = "\n==Authors contributed==\n"
    header + table.asWiki + userImages.getOrElse("")
  }

  def authorsContributedPerRegion(
      imageDb: ImageDB,
      bot: MwBot,
      accounts: Map[String, String]
  ): Future[Unit] =
    Future
      .traverse(AuthorsStat.authorsByRegion(imageDb)) { case (regionName, authors) =>
        bot
          .page(AuthorsStat.authorsListPage(imageDb.contest, regionName))
          .edit(AuthorsStat.authorsList(authors, accounts))
      }
      .map(_ => ())

  // a file whose page names no author groups under "", which is nobody to count
  private def named(authors: Set[String]): Set[String] = authors.filterNot(_.isBlank)

  def authorsContributedTable(
      imageDbs: Seq[ImageDB],
      totalImageDb: ImageDB,
      monumentDb: Option[MonumentDB],
      listAuthors: Boolean = true
  ): Table = {
    val contest = monumentDb.map(_.contest).getOrElse(imageDbs.head.contest)
    val categoryName = contest.imagesCategory

    val imageDbsByYear = imageDbs.groupBy(_.contest.year)
    val yearSeq = imageDbsByYear.keys.toSeq.sorted.reverse

    val numYears = yearSeq.size

    val dataset = new DefaultCategoryDataset()

    val yearDbs = yearSeq.flatMap { year =>
      imageDbsByYear(year).headOption
    }
    val dbs = Seq(totalImageDb) ++ yearDbs

    val columns = Seq("Region") ++
      Seq(s"$numYears years total") ++
      yearSeq.map(_.toString)

    val perRegion = monumentDb.fold(Seq.empty[Seq[String]]) { db =>
      val country = db.contest.country
      db.regionIds.map { regionId =>
        val regionName = country.regionName(regionId)

        val shortRegionName = regionName
          .replaceAll("область", "")
          .replaceAll("Автономна Республіка", "АР")

        yearDbs.zipWithIndex.foreach { case (yearDb, i) =>
          dataset.addValue(
            named(yearDb.authorsByRegion(regionId)).size,
            yearSeq(i),
            shortRegionName
          )
        }

        Seq(regionName) ++
          Seq {
            val count = named(totalImageDb.authorsByRegion(regionId)).size.toString
            if (listAuthors) {
              val listPage = AuthorsStat.authorsListPage(contest, regionName)
              s"[[$listPage|$count]]"
            } else count
          } ++
          yearDbs.map(yearDb => named(yearDb.authorsByRegion(regionId)).size.toString)
      }
    }

    val totalData = Seq("Total") ++ dbs.map(db => named(db.authors).size.toString)

    val rows = perRegion ++ Seq(totalData)

    val filenamePrefix = contest.name.replace("_", "")

    val ids = yearSeq.map(year => named(imageDbsByYear(year).head.authors))
    val idsSize = ids.map(_.size)

    userImages = Some(
      authorsStatImages(
        filenamePrefix,
        categoryName,
        yearSeq,
        dataset,
        ids,
        idsSize,
        uploadImages
      )
    )

    new Table(columns, rows, "Authors contributed")
  }

  def authorsImages(
      byAuthor: Map[String, Seq[Image]],
      monumentDb: Option[MonumentDB]
  ): String = {

    val sections = byAuthor
      .mapValues(images =>
        monumentDb.fold(images)(db => images.filter(_.monumentId.fold(false)(db.ids.contains)))
      )
      .collect {
        case (user, images) if images.nonEmpty =>
          val userLink = s"[[User:$user|$user]]"
          val header = s"== $userLink ==\n"
          val descriptions = images
            .map(i => i.mpx + " МПкс (" + i.resolution.getOrElse("") + ")")

          header + Image.gallery(images.map(_.title), descriptions)
      }

    sections.mkString("__TOC__\n", "\n", "")
  }

  def authorsStatImages(
      filenamePrefix: String,
      categoryName: String,
      yearSeq: Seq[Int],
      dataset: DefaultCategoryDataset,
      ids: Seq[Set[String]],
      idsSize: Seq[Int],
      uploadImages: Boolean = true
  ): String = {

    val images =
      s"\n[[File:${filenamePrefix}AuthorsByYearTotal.png|$categoryName, Authors by year overall|left]]" +
        s"\n[[File:${filenamePrefix}AuthorsByYearPie.png|$categoryName, Authors by year pie chart|left]]" +
        s"\n[[File:${filenamePrefix}AuthorsByYear.png|$categoryName, Authors by year by regions|left]]" +
        "\n<br clear=\"all\">"

    if (uploadImages) {

      val chart = charts.createChart(dataset, "Регіон")
      val byRegionFile = filenamePrefix + "AuthorsByYear"
      charts.saveCharts(chart, byRegionFile, 900, 1200)
      MwBot
        .fromHost(MwBot.commons)
        .page(byRegionFile + ".png")
        .upload(byRegionFile + ".png")

      val chartTotal =
        charts.createChart(charts.createTotalDataset(yearSeq, idsSize), "")

      val chartTotalFile = filenamePrefix + "AuthorsByYearTotal.png"
      charts.saveAsPNG(chartTotal, chartTotalFile, 900, 200)
      MwBot.fromHost(MwBot.commons).page(chartTotalFile).upload(chartTotalFile)

      val intersectionFile = filenamePrefix + "AuthorsByYearPie"
      charts.intersectionDiagram(
        "Унікальність авторів за роками",
        intersectionFile,
        yearSeq,
        ids,
        900,
        800
      )
      MwBot
        .fromHost(MwBot.commons)
        .page(intersectionFile + ".png")
        .upload(intersectionFile + ".png")
    }
    images
  }
}

object AuthorsStat {

  /** The page listing a region's all-time authors. It has no year in its title: Regional statistics
    * links its all-time author count here, so writing to a per-year title left the linked page
    * stale.
    */
  def authorsListPage(contest: Contest, regionName: String): String =
    s"Commons:${contest.contestType.name} in ${contest.country.name}/Автори $regionName"

  /** Each region's all-time authors, sorted. */
  def authorsByRegion(imageDb: ImageDB): Seq[(String, Seq[String])] =
    imageDb.monumentDb.fold(Seq.empty[(String, Seq[String])]) { monumentDb =>
      monumentDb.regionIds.map { regionId =>
        val authors = imageDb
          .authorsByRegion(regionId)
          .toSeq
          .sorted
          .map(_.replace("Участник:", ""))
          .filterNot(_.isBlank)
        monumentDb.contest.country.regionName(regionId) -> authors
      }
    }

  /** The authors list, a MassMessage target list of their [[accounts]]. An author with no Commons
    * account (a Panoramio or Flickr name, a name written out) is listed as plain text: still
    * counted, but with no talk page to message.
    */
  def authorsList(authors: Seq[String], accounts: Map[String, String]): String =
    authors
      .map(author => accounts.get(userName(author)).toRight(author))
      .distinct
      .sortBy(_.merge)
      .map {
        case Right(account) => s"# {{#target:User_talk:$account|}}"
        case Left(name)     => s"# $name"
      }
      .mkString("\n")

  /** The name an author is counted under: their current Commons account, or else the name as
    * written.
    */
  def accountOf(accounts: Map[String, String])(author: String): String =
    accounts.getOrElse(userName(author), author)

  private val logger = LoggerFactory.getLogger(classOf[AuthorsStat])

  /** Where [[accounts]] keeps the accounts it found: beside the image CSVs, and off with them. */
  def accountsCache(config: StatConfig): Option[Path] =
    Option.when(config.csvCache)(Paths.get(config.csvCacheDir, "commons-accounts.txt"))

  /** The current Commons account of each of `authors` that has one, by their [[userName]]: their
    * own, or the one it was renamed to. Commons never deletes an account, so these are kept in
    * `cacheFile` and not looked up again. A name with no account is looked up on every run: it may
    * be registered since.
    */
  def accounts(
      bot: ActionBot,
      authors: Seq[String],
      cacheFile: Option[Path] = None
  ): Future[Map[String, String]] = {
    val names = authors.map(userName).distinct
    val cached = cacheFile.fold(Map.empty[String, String])(readAccounts)
    val uncached = names.filterNot(cached.contains)
    for {
      existing <- lookUpAccounts(bot, uncached)
      renamed <- lookUpRenames(bot, uncached.filterNot(existing))
    } yield {
      val found = existing.map(name => name -> name).toMap ++ renamed
      if (found.nonEmpty) cacheFile.foreach(writeAccounts(_, cached ++ found))
      val wanted = names.toSet
      (cached ++ found).filter { case (name, _) => wanted(name) }
    }
  }

  // a line per account, `old<TAB>new` for a renamed one
  private def readAccounts(file: Path): Map[String, String] =
    if (!Files.exists(file)) Map.empty
    else
      Try {
        Files
          .readAllLines(file, StandardCharsets.UTF_8)
          .asScala
          .filter(_.nonEmpty)
          .map { line =>
            line.split('\t') match {
              case Array(old, renamed) => old -> renamed
              case _                   => line -> line
            }
          }
          .toMap
      }.recover { case e =>
        logger.warn(s"[accounts] could not read $file, looking up every author: $e")
        Map.empty[String, String]
      }.get

  private def writeAccounts(file: Path, accounts: Map[String, String]): Unit =
    Try {
      Option(file.getParent).foreach(Files.createDirectories(_))
      val lines = accounts.toSeq.sorted.map { case (name, account) =>
        if (name == account) name else s"$name\t$account"
      }
      val tmp = file.resolveSibling(file.getFileName.toString + ".tmp")
      Files.write(tmp, lines.asJava, StandardCharsets.UTF_8)
      Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    }.failed.foreach(e => logger.warn(s"[accounts] could not write $file: $e"))

  // one request per 50 names, one at a time
  private def inBatches[T](
      names: Seq[String]
  )(lookUp: Seq[String] => Future[Map[String, T]]): Future[Map[String, T]] =
    names.grouped(50).foldLeft(Future.successful(Map.empty[String, T])) { (acc, slice) =>
      acc.flatMap(found => lookUp(slice).map(found ++ _))
    }

  private def lookUpAccounts(bot: ActionBot, names: Seq[String]): Future[Set[String]] =
    inBatches(names) { slice =>
      bot.run(Action(Query(ListParam(Users(UsUsers(slice)))))).map { pages =>
        // a missing user comes back without an id, and so does an invalid name
        pages
          .flatMap(_.lastRevisionUser)
          .collect { case user: User if user.id.isDefined => user.login }
          .flatten
          .map(name => name -> name)
          .toMap
      }
    }.map(_.keySet)

  private val talkPrefix = "User talk:"
  // the target's page name, after its namespace
  private val redirect =
    "(?iu)^\\s*#(?:redirect|перенаправлення)\\s*\\[\\[\\s*:?[^:\\]|]+:([^\\]|#/]+)".r

  /** The accounts that `names` were renamed to: a renamed account's old talk page redirects to its
    * new one.
    */
  private def lookUpRenames(bot: ActionBot, names: Seq[String]): Future[Map[String, String]] =
    inBatches(names) { slice =>
      bot
        .run(
          Action(Query(TitlesParam(slice.map(talkPrefix + _)), Prop(Revisions(RvProp(Content)))))
        )
        .map { pages =>
          pages.flatMap { page =>
            page.revisions.headOption.flatMap(_.content).flatMap(redirect.findFirstMatchIn).map {
              m =>
                page.title.stripPrefix(talkPrefix) -> userName(m.group(1))
            }
          }.toMap
        }
    }.flatMap { renamed =>
      // the redirect may be to a page that isn't an account's
      lookUpAccounts(bot, renamed.values.toSeq.distinct).map(existing =>
        renamed.filter { case (_, to) => existing(to) }
      )
    }

  /** The name as MediaWiki stores it: spaces for underscores, the first letter capitalized. */
  def userName(author: String): String = author.replace('_', ' ').trim.capitalize
}
