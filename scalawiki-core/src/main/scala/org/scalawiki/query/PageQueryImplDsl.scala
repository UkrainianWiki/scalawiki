package org.scalawiki.query

import java.nio.file.{Files, Paths}

import org.scalawiki.MwBot
import java.time.ZonedDateTime

import org.scalawiki.dto.cmd._
import org.scalawiki.dto.cmd.edit._
import org.scalawiki.dto.cmd.query._
import org.scalawiki.dto.cmd.query.list._
import org.scalawiki.dto.cmd.query.prop._
import org.scalawiki.dto.cmd.query.prop.rvprop.RvProp
import org.scalawiki.dto.{MwException, Namespace, Page}
import org.scalawiki.json.MwReads._
import org.scalawiki.util.WriteWatcher
import retry.Success

import scala.concurrent.Future

class PageQueryImplDsl(
    query: Either[Set[Long], Set[String]],
    bot: MwBot,
    context: Map[String, String] = Map.empty
) extends PageQuery
    with SinglePageQuery {

  override def withContext(context: Map[String, String]) =
    new PageQueryImplDsl(query, bot, context)

  override def revisions(
      namespaces: Set[Int],
      props: Set[String],
      continueParam: Option[(String, String)],
      limit: Option[String]
  ): Future[Iterable[Page]] = {

    import org.scalawiki.dto.cmd.query.prop.rvprop._

    val pages = query.fold(
      ids => PageIdsParam(ids.toSeq),
      titles => TitlesParam(titles.toSeq)
    )

    // `limit == None` means "current revision only": omit `rvlimit` (MediaWiki
    // then returns just the latest revision and no `rvcontinue`) and cap
    // DslQuery at the number of pages asked for, so it cannot page backwards
    // through the whole history with content even if a continuation appears.
    val revisionParams: Seq[RvParam] =
      RvProp(RvPropArgs.byNames(props.toSeq): _*) +: limit.map(RvLimit(_)).toSeq

    val action = Action(
      Query(
        pages,
        Prop(Info(), Revisions(revisionParams: _*))
      )
    )

    val runLimit =
      if (limit.isEmpty) Some(query.fold(_.size, _.size).toLong) else None

    bot.run(action, context, runLimit)
  }

  override def revisionsByGenerator(
      generator: String,
      generatorPrefix: String,
      namespaces: Set[Int],
      props: Set[String],
      continueParam: Option[(String, String)],
      limit: String,
      titlePrefix: Option[String]
  ): Future[Iterable[Page]] = {

    val pageId: Option[Long] = query.left.toOption.map(_.head)
    val title: Option[String] = query.right.toOption.map(_.head)

    val generatorArg =
      ListArgs.toDsl(generator, title, pageId, namespaces, Some(limit))
    val queryParams = pagesParam(pageId, title, generatorArg) ++ Seq(
      Prop(
        Info(),
        Revisions(RvProp(RvPropArgs.byNames(props.toSeq): _*))
      ),
      Generator(
        ListArgs.toDsl(generator, title, pageId, namespaces, Some(limit)).get
      )
    )

    val action = Action(Query(queryParams: _*))
    bot.run(action, context)
  }

  override def imageInfoByGenerator(
      generator: String,
      generatorPrefix: String,
      namespaces: Set[Int],
      props: Set[String],
      continueParam: Option[(String, String)],
      limit: String,
      titlePrefix: Option[String]
  ): Future[Iterable[Page]] = {
    import org.scalawiki.dto.cmd.query.prop.iiprop._

    val pageId: Option[Long] = query.left.toOption.map(_.head)
    val title: Option[String] = query.right.toOption.map(_.head)

    val generatorArg =
      ListArgs.toDsl(generator, title, pageId, namespaces, Some(limit))
    val queryParams = pagesParam(pageId, title, generatorArg) ++ Seq(
      Prop(
        ImageInfo(
          IiProp(IiPropArgs.byNames(props.toSeq): _*)
        )
      ),
      Generator(generatorArg.get)
    )

    val action = Action(Query(queryParams: _*))
    bot.run(action, context)
  }

  private def pagesParam(
      pageId: Option[Long],
      title: Option[String],
      generatorArg: Option[GeneratorArg]
  ) = {
    val pagesInGenerator = generatorArg.exists(
      _.pairs
        .map(_._1)
        .exists(p => p.endsWith("title") || p.endsWith("pageid"))
    )
    if (pagesInGenerator) Seq.empty[QueryParam[String]]
    else {
      title.map(t => TitlesParam(Seq(t))).toSeq ++ pageId
        .map(id => PageIdsParam(Seq(id)))
        .toSeq
    }
  }

  override def edit(
      text: String,
      summary: Option[String] = None,
      section: Option[String] = None,
      token: Option[String] = None,
      multi: Boolean = false,
      basetimestamp: Option[ZonedDateTime] = None,
      baseRevId: Option[Long] = None,
      startTimestamp: Option[ZonedDateTime] = None
  ) = {

    val page = query.fold(
      ids => PageId(ids.head),
      titles => Title(titles.head)
    )

    // `basetimestamp` / `baserevid` make MediaWiki reject the edit with an
    // `editconflict` error if the page's current revision has moved past the one
    // we read, instead of silently clobbering the intervening edit;
    // `starttimestamp` catches the page being deleted meanwhile (`pagedeleted`).
    val conflictParams: Seq[EditParam[Any]] =
      basetimestamp.map(BaseTimestamp(_)).toSeq ++
        baseRevId.map(BaseRevId(_)).toSeq ++
        startTimestamp.map(StartTimestamp(_)).toSeq

    val action = Action(Edit(Seq[EditParam[Any]](page, Text(text)) ++ conflictParams: _*))

    val baseParams = action.pairs.toMap ++
      Map(
        "action" -> "edit",
        "format" -> "json",
        "utf8" -> "",
        "bot" -> "x",
        "assert" -> "bot"
      ) ++ section
        .map(s => "section" -> s)
        .toSeq ++ summary.map(s => "summary" -> s).toSeq

    import scala.concurrent.ExecutionContext.Implicits.global

    // The CSRF token is resolved per attempt, not captured once: over a long
    // batch run MediaWiki's edit token expires, and every later edit then fails
    // with `badtoken`. On that error drop the bot's cached token so the retry
    // (and every page after it) fetches a fresh one.
    def performEdit(): Future[String] = {
      val editToken = token.getOrElse(bot.token)
      val params = baseParams + ("token" -> editToken)
      bot.log.info(s"Request ${bot.host} edit page: $page, summary: $summary")
      val response =
        if (multi) bot.postMultiPart(editResponseReads, params)
        else bot.post(editResponseReads, params)
      response
        .map { s =>
          bot.log.info(s"Response ${bot.host} edit page: $page: $s")
          s
        }
        .recoverWith {
          case e: MwException if e.code == "badtoken" && token.isEmpty =>
            bot.log.warning(
              s"${bot.host} edit page: $page: stale CSRF token, refreshing"
            )
            bot.invalidateToken()
            Future.failed(e)
        }
    }

    implicit def stringSuccess: Success[String] = Success(_ == "Success")
    // Don't burn the whole backoff budget replaying an edit that lost a race:
    // an edit-conflict error won't clear until the page is re-read and the edit
    // rebuilt, which is the caller's job (see PageUpdater).
    val isConflict: PartialFunction[Throwable, Boolean] = {
      case e: MwException => e.conflict
      case _              => false
    }
    val policy = retry.FailFast(retry.Backoff()(odelay.Timer.default))(isConflict)
    // Many callers (contest report generators) fire edits and discard the
    // future. Route it through WriteWatcher so, when the CLI has enabled it,
    // the edits are throttled to a safe concurrency, failures are logged
    // instead of vanishing, and the process can wait for every write to finish
    // before it exits. When not enabled this runs the edit immediately, as before.
    // Edit conflicts are `benign` here: FailFast surfaces them on purpose so the
    // caller (PageUpdater) can re-read and retry, so they must not be counted as
    // dropped writes.
    if (org.scalawiki.util.DryRun.isEnabled) {
      // Still routed through WriteWatcher so the run's write count and progress
      // stay meaningful; nothing is sent (not even a token request).
      WriteWatcher.submit(s"dry-run edit ${bot.host} / $page")(() =>
        Future {
          val title = query.fold(ids => s"pageid-${ids.head}", _.head)
          val file = org.scalawiki.util.DryRun.saveEdit(bot.host, title, text)
          bot.log.info(s"Dry run: not publishing ${bot.host} edit page: $page, saved to $file")
          "Success"
        }
      )
    } else
      WriteWatcher.submit(s"edit ${bot.host} / $page", isConflict)(() =>
        policy(() => performEdit())
      )
  }

  override def upload(
      filename: String,
      text: Option[String] = None,
      comment: Option[String] = None,
      ignoreWarnings: Boolean = false
  ): Future[String] = {
    val page = query.right.toOption.fold(filename)(_.head)
    if (org.scalawiki.util.DryRun.isEnabled) {
      return WriteWatcher.submit(s"dry-run upload ${bot.host} / $page")(() =>
        Future.successful {
          org.scalawiki.util.DryRun.skipUpload()
          bot.log.info(s"Dry run: not uploading $filename to ${bot.host} as $page")
          "Success"
        }
      )(scala.concurrent.ExecutionContext.Implicits.global)
    }
    val token = bot.token
    val fileContents = Files.readAllBytes(Paths.get(filename))
    val params = Map(
      "action" -> "upload",
      "filename" -> page,
      "token" -> token,
      "format" -> "json",
      "comment" -> "update",
      "filesize" -> fileContents.size.toString,
      "assert" -> "user",
      "assert" -> "bot"
    ) ++
      text.map("text" -> _) ++
      comment.map("comment" -> _) ++
      (if (ignoreWarnings) Seq("ignorewarnings" -> "true") else Seq.empty)

    WriteWatcher.submit(s"upload ${bot.host} / $page")(() =>
      bot.postFile(uploadResponseReads, params, "file", filename)
    )(scala.concurrent.ExecutionContext.Implicits.global)
  }

  override def whatTranscludesHere(
      namespaces: Set[Int],
      continueParam: Option[(String, String)]
  ): Future[Iterable[Page]] = {
    val pages = query.fold(
      ids => EiPageId(ids.head),
      titles => EiTitle(titles.head)
    )

    val action = Action(
      Query(
        ListParam(
          EmbeddedIn(
            pages,
            EiLimit("max"),
            EiNamespace(namespaces.toSeq)
          )
        )
      )
    )

    bot.run(action, context)
  }

  override def categoryMembers(
      namespaces: Set[Int],
      continueParam: Option[(String, String)]
  ): Future[Iterable[Page]] = {
    val pages = query.fold(
      ids => CmPageId(ids.head),
      titles => CmTitle(titles.head)
    )

    val cmTypes = namespaces
      .filter(_ == Namespace.CATEGORY)
      .map(_ => CmTypeSubCat) ++
      namespaces.filter(_ == Namespace.FILE).map(_ => CmTypeFile)

    val cmParams = Seq(
      pages,
      CmLimit("max"),
      CmNamespace(namespaces.toSeq)
    ) ++ (if (cmTypes.nonEmpty)
            Seq(CmType(cmTypes.toSeq: _*))
          else
            Seq.empty)

    val action = Action(
      Query(
        ListParam(
          CategoryMembers(cmParams: _*)
        )
      )
    )

    bot.run(action, context)
  }
}
