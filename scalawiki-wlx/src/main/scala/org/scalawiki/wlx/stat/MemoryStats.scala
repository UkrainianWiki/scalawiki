package org.scalawiki.wlx.stat

import com.sun.management.GarbageCollectionNotificationInfo
import org.slf4j.LoggerFactory

import java.lang.management.{ManagementFactory, MemoryType, MemoryUsage}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicLong
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Tracks the JVM's memory use over a run and sums it up in one line, so an
  * unattended (e.g. server) run records how much memory it actually needed.
  *
  * Heap usage is taken from GC notifications: the heap just before a
  * collection approximates the peak; the heap just after one bounds what the
  * run really retained (an upper bound - a young collection leaves old
  * garbage behind). Size `-Xmx` from "max after GC" plus headroom.
  */
object MemoryStats {

  private val logger = LoggerFactory.getLogger(getClass)

  private val peakBeforeGc = new AtomicLong(0L)
  private val maxAfterGc = new AtomicLong(0L)
  @volatile private var installed = false

  private lazy val heapPools: Set[String] =
    ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP).map(_.getName).toSet

  /** Start listening for GC events. Idempotent. */
  def install(): Unit = synchronized {
    if (!installed) {
      installed = true
      val listener = new NotificationListener {
        override def handleNotification(n: Notification, handback: Any): Unit =
          if (n.getType == GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION) {
            val gc = GarbageCollectionNotificationInfo.from(n.getUserData.asInstanceOf[CompositeData]).getGcInfo
            val before = heapUsed(gc.getMemoryUsageBeforeGc)
            val after = heapUsed(gc.getMemoryUsageAfterGc)
            peakBeforeGc.accumulateAndGet(before, (a, b) => math.max(a, b))
            maxAfterGc.accumulateAndGet(after, (a, b) => math.max(a, b))
          }
      }
      ManagementFactory.getGarbageCollectorMXBeans.asScala.foreach {
        case emitter: NotificationEmitter => emitter.addNotificationListener(listener, null, null)
        case _                            =>
      }
    }
  }

  private def heapUsed(usage: java.util.Map[String, MemoryUsage]): Long =
    usage.asScala.iterator.collect { case (pool, u) if heapPools.contains(pool) => u.getUsed }.sum

  /** e.g. `Memory: heap peak 2860 MB, max after GC 2587 MB, limit 4060 MB
    * (committed 3588 MB); GC: 94 collections, 6.1 s, process peak RSS 3900 MB`.
    * The RSS part is only available on Linux. */
  def summary(): String = {
    val heap = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
    val gcs = ManagementFactory.getGarbageCollectorMXBeans.asScala.toSeq
    val gcCount = gcs.map(_.getCollectionCount).filter(_ >= 0).sum
    val gcMillis = gcs.map(_.getCollectionTime).filter(_ >= 0).sum
    val peak = math.max(peakBeforeGc.get, heap.getUsed)
    val rss = peakRss.fold("")(bytes => s", process peak RSS ${mb(bytes)}")
    s"Memory: heap peak ${mb(peak)}, max after GC ${mb(maxAfterGc.get)}, " +
      s"limit ${mb(Runtime.getRuntime.maxMemory)} (committed ${mb(heap.getCommitted)}); " +
      f"GC: $gcCount collections, ${gcMillis / 1000.0}%.1f s$rss"
  }

  /** Print [[summary]] to stderr and the log. */
  def report(): Unit = {
    val s = summary()
    Console.err.println(s)
    logger.info(s)
  }

  private def mb(bytes: Long): String = f"${bytes / 1048576.0}%.0f MB"

  /** Peak resident set size from `/proc/self/status` (Linux only). */
  private def peakRss: Option[Long] =
    Try(Files.readAllLines(Paths.get("/proc/self/status")).asScala.collectFirst {
      case line if line.startsWith("VmHWM:") => line.trim.split("\\s+")(1).toLong * 1024
    }).toOption.flatten
}
