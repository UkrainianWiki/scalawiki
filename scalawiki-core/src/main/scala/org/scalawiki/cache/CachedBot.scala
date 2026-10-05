package org.scalawiki.cache

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileSystemException,
  Files,
  StandardCopyOption
}
import java.security.MessageDigest

import org.rogach.scallop.ScallopConf
import org.scalawiki.dto.cmd.Action
import org.scalawiki.{MwBot, MwBotImpl}
import org.scalawiki.dto.{MwException, Page, Site}
import org.scalawiki.http.HttpClient

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Cache {

  /** Root directory for all persistent caches, relative to the working dir. */
  val defaultRoot: File = new File("http-cache")
}

/** A plain filesystem key -> file cache.
  *
  * Each entry is one file under `<root>/<name>/` (root defaults to `http-cache/`),
  * named by the SHA-256 hex of the key, holding the response body verbatim
  * (UTF-8). This replaced a ChronicleMap-backed store: the file cache is smaller,
  * faster to read, plain text, and needs no `--add-opens` / `--add-exports` JVM
  * flags to run on JDK 17+.
  *
  * Writes go through a temp file + atomic rename, so a reader never sees a
  * half-written entry; per-key locking keeps a key's value function to a single
  * run per process. Concurrent processes may still both compute a cold key, but
  * the rename makes that safe.
  */
class Cache(name: String, persistent: Boolean = true, root: File = Cache.defaultRoot) {

  private val dir: File = new File(root, name)

  private val memory: TrieMap[String, String] = TrieMap.empty[String, String]

  /** Per-key locks so a given key's value function runs at most once per
    * process, the way `ChronicleMap.computeIfAbsent` used to guarantee.
    * (Across processes the atomic rename below still keeps readers consistent.)
    */
  private val locks: TrieMap[String, AnyRef] = TrieMap.empty[String, AnyRef]

  if (persistent) dir.mkdirs()

  private def lockFor(key: String): AnyRef =
    locks.getOrElseUpdate(key, new Object)

  private def fileFor(key: String): File = {
    val digest = MessageDigest.getInstance("SHA-256").digest(key.getBytes(StandardCharsets.UTF_8))
    new File(dir, digest.map("%02x".format(_)).mkString)
  }

  def containsKey(key: String): Boolean =
    if (persistent) fileFor(key).isFile else memory.contains(key)

  def remove(key: String): Unit = lockFor(key).synchronized {
    if (persistent) Files.deleteIfExists(fileFor(key).toPath)
    else memory.remove(key)
  }

  def computeIfAbsent(key: String, fn: String => String): String =
    lockFor(key).synchronized {
      if (!persistent) memory.getOrElseUpdate(key, fn(key))
      else {
        val target = fileFor(key)
        readFile(target).getOrElse {
          val value = fn(key)
          writeAtomically(target, value)
          value
        }
      }
    }

  /** An existing cache file's content, or `None` if it is absent or could not
    * be read — e.g. a concurrent eviction deleted it between the check and the
    * read; the caller then simply recomputes. */
  private def readFile(target: File): Option[String] =
    try {
      if (target.isFile)
        Some(new String(Files.readAllBytes(target.toPath), StandardCharsets.UTF_8))
      else None
    } catch {
      case _: IOException => None
    }

  private def writeAtomically(target: File, value: String): Unit = {
    val tmp = File.createTempFile(target.getName, ".tmp", dir)
    try {
      Files.write(tmp.toPath, value.getBytes(StandardCharsets.UTF_8))
      try {
        Files.move(
          tmp.toPath,
          target.toPath,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE
        )
      } catch {
        case _: AtomicMoveNotSupportedException =>
          Files.move(tmp.toPath, target.toPath, StandardCopyOption.REPLACE_EXISTING)
        case _: FileSystemException if target.isFile =>
        // Another process wrote an equivalent entry first (and on Windows may
        // still hold it open, blocking the replace). Its content will do.
      }
    } finally {
      // No-op after a successful move; cleans up if write/move threw.
      Files.deleteIfExists(tmp.toPath)
    }
  }

}

// TODO async compute, do not enter twice
/** An [[MwBotImpl]] that memoizes API responses in a [[Cache]]. By default it
  * sends its requests through the shared bot's session for `site` (see
  * [[CachedBot.sessionHttp]]), so a logged-in run's cache misses get the bot's
  * API limits, not an anonymous user's. */
class CachedBot(
    site: Site,
    name: String,
    persistent: Boolean,
    http: HttpClient
) extends MwBotImpl(site, http) {

  def this(site: Site, name: String, persistent: Boolean) =
    this(site, name, persistent, CachedBot.sessionHttp(site))


  val cache = new Cache(name, persistent)

  override def run(
      action: Action,
      context: Map[String, String] = Map.empty,
      limit: Option[Long] = None
  ): Future[Iterable[Page]] = {
    val future = super.run(action, context, limit)

    future recoverWith { case ex: MwException =>
      val key = paramsKey(ex.params)
      cache.remove(key)
      super.run(action, context, limit)
    }
  }

  def paramsKey(params: Map[String, String]) =
    params.toIndexedSeq.sortBy(_._1).toString()

  override def post(params: Map[String, String]): Future[String] = {
    val key = paramsKey(params)

    if (cache.containsKey(key)) {
      log.info(s"cached $host POST equivalent to: ${getUri(params)}")
    }

    try {
      val fn = (_: String) => Await.result(super.post(params), 30.minutes)
      val value = cache.computeIfAbsent(key, fn)

      // computeIfAbsent may return a previously cached error/rate-limit body (not
      // valid JSON) just as easily as a freshly fetched one. Don't let a non-JSON
      // response linger in the persistent cache: evict it so the next attempt
      // (this retry included) re-fetches from the network instead of replaying
      // the same failure forever.
      if (!CachedBot.looksLikeJson(value)) {
        cache.remove(key)
      }

      Future.successful(value)
    } catch {
      case t: Throwable =>
        Future.failed(t)
    }
  }
}

object CachedBot {

  /** The HTTP client of the shared bot for `site` ([[MwBot.fromSite]], logged
    * in from the environment when credentials are set): login is a session
    * cookie held by that client's cookie jar, and a fresh client would have an
    * empty one. */
  def sessionHttp(site: Site): HttpClient =
    MwBot.fromSite(site) match {
      case bot: MwBotImpl => bot.http
      case _              => HttpClient.get(MwBot.system)
    }

  def looksLikeJson(body: String): Boolean = {
    val trimmed = body.trim
    trimmed.startsWith("{") || trimmed.startsWith("[")
  }

  class CachedArgs(arguments: Seq[String]) extends ScallopConf(arguments) {
    val cache = opt[String](descr = "cache directory")
    verify()
  }

  def main(args: Array[String]): Unit = {
    val parsed = new CachedArgs(args)

    val cacheDir = parsed.cache()
    val dir = new File(cacheDir)
    if (!dir.isDirectory) {
      throw new IllegalArgumentException(s"Cache directory $cacheDir is absent")
    }
    val entries = Option(dir.listFiles())
      .getOrElse(Array.empty[File])
      .filter(f => f.isFile && !f.getName.endsWith(".tmp"))
    val valueSizes = entries.map(_.length()).toSeq

    println("entries: " + entries.length)
    if (valueSizes.nonEmpty) {
      println(
        s"values: total size: ${valueSizes.sum / (1024 * 1024)} MB, avg size: ${valueSizes.sum / (valueSizes.size * 1024)} KB"
      )
    }
  }
}
