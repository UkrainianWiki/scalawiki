package org.scalawiki.util

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import org.specs2.mutable.Specification

class AtomicFileSpec extends Specification {
  "AtomicFile" should {
    "keep the previous cache and remove temporary data after a failed write" in {
      val dir = Files.createTempDirectory("atomic-cache")
      val cache = dir.resolve("images.csv")
      Files.write(cache, "previous complete cache".getBytes(UTF_8))
      AtomicFile.write(cache) { temporary =>
        Files.write(temporary, "partial replacement".getBytes(UTF_8))
        throw new RuntimeException("disk full")
      } must throwA[RuntimeException]("disk full")
      new String(Files.readAllBytes(cache), UTF_8) must_== "previous complete cache"
      val files = Files.list(dir)
      try files.count() must_== 1L
      finally files.close()
    }

    "replace the complete file on success" in {
      val cache = Files.createTempDirectory("atomic-cache").resolve("images.csv")
      Files.write(cache, "old".getBytes(UTF_8))
      AtomicFile.write(cache)(p => { Files.write(p, "complete replacement".getBytes(UTF_8)); () })
      new String(Files.readAllBytes(cache), UTF_8) must_== "complete replacement"
    }
  }
}
