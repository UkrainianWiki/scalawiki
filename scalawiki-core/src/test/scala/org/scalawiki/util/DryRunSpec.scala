package org.scalawiki.util

import org.specs2.mutable.Specification

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.concurrent.Await
import scala.concurrent.duration._

class DryRunSpec extends Specification with MockBotSpec {

  sequential

  "DryRun" should {

    "save an edit's text instead of sending any request" in {
      val dir = Files.createTempDirectory("dry-run-spec")
      DryRun.enable(dir.toString)
      try {
        // no HTTP stubs: any request (edit, token fetch) would throw
        val bot = getBot()
        Await.result(bot.page("Commons:Foo/Bar baz").edit("some text", Some("updating")), 5.seconds) === "Success"

        val file = dir.resolve(host).resolve("Commons_Foo_Bar baz.wiki")
        new String(Files.readAllBytes(file), StandardCharsets.UTF_8) === "some text"
        DryRun.skippedCount === 1L
      } finally DryRun.reset()
    }

    "skip an upload without reading the file or sending any request" in {
      val dir = Files.createTempDirectory("dry-run-spec")
      DryRun.enable(dir.toString)
      try {
        val bot = getBot()
        Await.result(bot.page("File:Chart.png").upload("no-such-file.png"), 5.seconds) === "Success"
        DryRun.skippedCount === 1L
      } finally DryRun.reset()
    }

    "turn a page title into a single safe file name" in {
      DryRun.fileName("""Commons:A/B\C*?"<>|""") === "Commons_A_B_C______"
    }
  }
}
