package org.scalawiki.wlx.query

import org.scalawiki.dto.cmd.Action
import org.scalawiki.dto.cmd.query.list._
import org.scalawiki.dto.cmd.query.prop.{CategoryInfo, IiLimit, ImageInfo, Prop, Revisions}
import org.scalawiki.dto.cmd.query.prop.iiprop.{IiProp, User => IiUser}
import org.scalawiki.dto.cmd.query.prop.rvprop.{
  RvProp,
  Content => RvContent,
  Ids => RvIds,
  Timestamp => RvTimestamp
}
import org.scalawiki.dto.cmd.query.{Generator, PageIdsParam, Query, TitlesParam}
import org.scalawiki.dto.{Image, Namespace}
import org.scalawiki.query.QueryLibrary
import org.scalawiki.wlx.dto.{Contest, SpecialNomination}
import org.scalawiki.wlx.query.ImageQuery.PageRevInfo
import org.scalawiki.{ActionBot, MwBot}
import org.scalawiki.util.AsyncGate

import java.time.ZonedDateTime
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ImageQuery {

  def imagesFromCategory(contest: Contest): Future[Iterable[Image]]

  def imagesWithTemplate(contest: Contest): Future[Iterable[Image]]

  def imagesWithTemplateByIds(contest: Contest, pageIds: Set[Long]): Future[Iterable[Image]]

  /** Page id + latest revision (id, timestamp) of every file carrying the contest file template.
    * Cheap (no imageinfo, no content) — used to diff a CSV cache: new / changed / deleted files.
    */
  def imageIdsWithTemplate(contest: Contest): Future[Seq[PageRevInfo]]

  /** Page id + latest revision (id, timestamp) of every file in the contest images category,
    * without imageinfo. Cheap — used to diff against a CSV cache.
    */
  def imageIdsFromCategory(contest: Contest): Future[Seq[PageRevInfo]]

  /** Number of files MediaWiki records for the contest images category (`categoryinfo.files`), or
    * `None` if the category page is missing / has no `categoryinfo`. This counter is maintained by
    * the job queue and can lag the real membership in either direction, so it is only a sanity
    * check on the completeness of an [[imageIdsFromCategory]] sweep, never an exact count.
    */
  def categoryFileCount(contest: Contest): Future[Option[Long]]

  /** Of `pageIds`, those the wiki no longer has: deleted files. A file only moved out of a
    * category, or renamed, keeps its page id. Fails if a request does.
    */
  def missingPageIds(pageIds: Set[Long]): Future[Set[Long]]

}

class ImageQueryApi(bot: ActionBot, batchGate: AsyncGate = ImageQuery.batchGate)
    extends ImageQuery
    with QueryLibrary {

  /** File templates of every special nomination, regardless of contest year.
    *
    * Recognition must not depend on `contest.year`: a photo uploaded for the 2024 interior
    * nomination still carries `{{WLM2024-UA-interior}}` when it turns up in an all-time /
    * prior-year fetch, and downstream logic (e.g.
    * [[org.scalawiki.wlx.stat.rating.NumberOfInteriorImagesBonus]]) needs to see it. Consumers that
    * care about a specific year filter by exact template name.
    */
  private val allSpecialNominationTemplates: Set[String] =
    SpecialNomination.nominations.flatMap(_.fileTemplate).toSet

  private def categoryGenerator(contest: Contest): Generator = Generator(
    CategoryMembers(
      CmTitle(contest.imagesCategory),
      CmNamespace(Seq(Namespace.FILE)),
      CmLimit("max")
    )
  )

  override def imagesFromCategory(contest: Contest): Future[Iterable[Image]] =
    imagesByGenerator(contest, categoryGenerator(contest))

  override def imageIdsFromCategory(contest: Contest): Future[Seq[PageRevInfo]] =
    imageIdsByGenerator(categoryGenerator(contest))

  override def categoryFileCount(contest: Contest): Future[Option[Long]] =
    bot
      .run(
        Action(
          Query(
            TitlesParam(Seq(contest.imagesCategory)),
            Prop(CategoryInfo)
          )
        )
      )
      .map(_.headOption.flatMap(_.categoryInfo).map(_.files))

  /** The API answers a page id it no longer has with `{"pageid": N, "missing": ""}`, without a
    * title, which the parser leaves out: an id the answer doesn't list is missing.
    */
  override def missingPageIds(pageIds: Set[Long]): Future[Set[Long]] =
    Future
      .traverse(pageIds.toSeq.sorted.grouped(50).toSeq) { slice =>
        batchGate
          .submit(bot.run(Action(Query(PageIdsParam(slice)))))
          .map(pages => slice.toSet -- pages.flatMap(_.id))
      }
      .map(_.flatten.toSet)

  override def imagesWithTemplate(contest: Contest): Future[Iterable[Image]] =
    contest.fileTemplate
      .map { template =>
        imagesByGenerator(contest, generatorWithTemplate(template, Set(Namespace.FILE)))
      }
      .getOrElse(Future.successful(Nil))

  override def imageIdsWithTemplate(contest: Contest): Future[Seq[PageRevInfo]] =
    contest.fileTemplate
      .map { template =>
        imageIdsByGenerator(generatorWithTemplate(template, Set(Namespace.FILE)))
      }
      .getOrElse(Future.successful(Nil))

  override def imagesWithTemplateByIds(
      contest: Contest,
      pageIds: Set[Long]
  ): Future[Iterable[Image]] = {
    bot.log.info(s"imagesWithTemplateByIds pageIds size: ${pageIds.size}")
    val blockSize = 50
    val fetched = new AtomicInteger(0)
    val specialNominationTemplates = allSpecialNominationTemplates
    // Start only a small window, and share the budget with every other image
    // query. A limit per year alone multiplies when several years sync together.
    val windows = pageIds.toSeq.sorted.grouped(blockSize).grouped(2)
    def next(acc: Vector[Image]): Future[Vector[Image]] =
      if (!windows.hasNext) Future.successful(acc)
      else {
        val batch = windows.next().toVector
        Future
          .traverse(batch) { idsSlice =>
            batchGate
              .submit {
                bot.run(imagesByIds(idsSlice, withMetadata = true, withMediaType = true))
              }
              .map { pages =>
                bot.log.info(s"Fetched ${fetched.addAndGet(pages.size)} of ${pageIds.size}")
                pages.flatMap(Image.fromPage(contest.fileTemplate, specialNominationTemplates))
              }
          }
          .flatMap(results => next(acc ++ results.flatten))
      }
    next(Vector.empty).flatMap(withAuthors)
  }

  private def imagesByGenerator(contest: Contest, generator: Generator): Future[Iterable[Image]] = {
    val specialNominationTemplates = allSpecialNominationTemplates
    for {
      pages <- bot.run(imagesByGenerator(generator, withMetadata = true, withMediaType = true))
      images <- withAuthors(
        pages.flatMap(Image.fromPage(contest.fileTemplate, specialNominationTemplates)).toSeq
      )
    } yield images
  }

  private def withAuthors(images: Seq[Image]): Future[Seq[Image]] =
    withCreditTemplateAuthors(images).flatMap(withOriginalUploaders)

  /** Credits a file whose author is a photographer's credit template (`{{Yuriy Kvach}}`) to the
    * user its page links to: one more request per 50 such templates. A template that links to
    * nobody leaves the file to its uploader.
    */
  private def withCreditTemplateAuthors(images: Seq[Image]): Future[Seq[Image]] = {
    def normalized(name: String) = name.replace('_', ' ').capitalize
    val templates = images.flatMap(_.author).flatMap(Image.creditTemplate).map(normalized).distinct
    if (templates.isEmpty) Future.successful(images)
    else
      Future
        .traverse(templates.grouped(50).toSeq) { slice =>
          batchGate.submit {
            bot.run(
              Action(
                Query(TitlesParam(slice.map("Template:" + _)), Prop(Revisions(RvProp(RvContent))))
              )
            )
          }
        }
        .map { results =>
          // a redirect's text links to its target, which is the user's page all the same
          val userOf = results.flatten.flatMap { page =>
            page.revisions.headOption
              .flatMap(_.content)
              .flatMap(Image.userIn)
              .map(page.title.stripPrefix("Template:") -> _)
          }.toMap
          images.map { image =>
            image.author.flatMap(Image.creditTemplate).map(normalized).fold(image) { template =>
              image.copy(author = userOf.get(template).map(Image.renamed))
            }
          }
        }
  }

  /** Credits a file whose page names no author (or names them as `Я`, "me") to its original
    * uploader, see [[Image.authorFromUploader]]. That needs the file's whole upload history, which
    * the image queries above don't fetch: one more request per 50 such files, usually none.
    */
  private def withOriginalUploaders(images: Seq[Image]): Future[Seq[Image]] = {
    val unauthored = images.filter(_.author.isEmpty).flatMap(_.pageId)
    if (unauthored.isEmpty) Future.successful(images)
    else
      Future
        .traverse(unauthored.grouped(50).toSeq) { slice =>
          batchGate.submit {
            bot.run(
              Action(Query(PageIdsParam(slice), Prop(ImageInfo(IiProp(IiUser), IiLimit("max")))))
            )
          }
        }
        .map { results =>
          // imageinfo lists the versions newest first
          val originalUploader = results.flatten.flatMap { page =>
            page.id.zip(page.images.lastOption.flatMap(_.uploader).flatMap(_.login))
          }.toMap
          images.map { image =>
            val author =
              if (image.author.isDefined) None
              else image.pageId.flatMap(originalUploader.get).flatMap(Image.authorFromUploader)
            author.fold(image)(a => image.copy(author = Some(a)))
          }
        }
  }

  private def imageIdsByGenerator(generator: Generator): Future[Seq[PageRevInfo]] = {
    val action = Action(
      Query(
        generator,
        Prop(Revisions(RvProp(RvIds, RvTimestamp)))
      )
    )
    bot.run(action).map { pages =>
      pages.flatMap { page =>
        // Emit an entry for every page that has a pageId, even when its latest
        // revision is revision-deleted (no exposed revid/timestamp): otherwise a
        // still-existing file would look deleted to a CSV-cache diff.
        page.id.map { pageId =>
          val rev = page.revisions.headOption
          PageRevInfo(pageId, rev.flatMap(_.revId), rev.flatMap(_.timestamp))
        }
      }.toIndexedSeq
    }
  }

}

object ImageQuery {

  // Process-wide: CachedBot and live queries share the same Commons pool.
  private[query] val batchGate = new AsyncGate(2)

  /** Page id + its latest revision (id and timestamp). The cheap change token used to diff a CSV
    * image cache against the wiki. `revId` / `timestamp` are empty when the current revision is
    * revision-deleted.
    */
  case class PageRevInfo(
      pageId: Long,
      revId: Option[Long] = None,
      timestamp: Option[ZonedDateTime] = None
  )

  def create(implicit bot: ActionBot = MwBot.fromHost(MwBot.commons)): ImageQuery =
    new ImageQueryApi(bot)

}
