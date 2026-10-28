package org.scalawiki.util

import java.nio.file.{Files, Path, StandardCopyOption}

/** An interrupted/failed write leaves the previous file intact. */
object AtomicFile {
  def write(path: Path)(writeTemporary: Path => Unit): Unit = {
    val target = path.toAbsolutePath
    Files.createDirectories(target.getParent)
    val temporary =
      Files.createTempFile(target.getParent, target.getFileName.toString + ".", ".tmp")
    try {
      writeTemporary(temporary)
      Files.move(
        temporary,
        target,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
    } finally Files.deleteIfExists(temporary)
  }
}
