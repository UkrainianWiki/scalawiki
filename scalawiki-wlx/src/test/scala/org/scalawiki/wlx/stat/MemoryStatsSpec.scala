package org.scalawiki.wlx.stat

import org.specs2.mutable.Specification

import scala.concurrent.duration._

class MemoryStatsSpec extends Specification {

  "MemoryStats" should {

    "sum up heap and GC activity after a collection" in {
      MemoryStats.install()
      val garbage = (1 to 200).map(_ => new Array[Byte](1 << 16))
      garbage.size === 200
      System.gc()

      // GC notifications arrive asynchronously
      eventually(retries = 50, sleep = 100.millis) {
        MemoryStats.summary() must beMatching(
          """Memory: heap peak [1-9]\d* MB, max after GC \d+ MB, limit \d+ MB \(committed \d+ MB\); GC: [1-9]\d* collections, \d+\.\d s.*"""
        )
      }
    }
  }
}
