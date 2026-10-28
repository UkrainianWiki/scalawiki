package org.scalawiki.xml

import java.io.{IOException, FilterInputStream, InputStream}

/** Reads just the specified blocks from the underlying input stream, skipping over everything else.
  * @param in
  *   underlying input stream
  * @param blocks
  *   blocks to read
  */
class SkippingInputStream(in: InputStream, val blocks: Seq[Block]) extends FilterInputStream(in) {

  private var offset: Long = 0L
  private var mark: Long = -1

  private var block = blocks.headOption.getOrElse(Block(0, Long.MaxValue))
  private var blockIndex = 0
  private var markedBlockIndex = -1

  override def available(): Int = super.available()

  override def mark(readlimit: Int): Unit = {
    in.mark(readlimit)
    mark = offset
    markedBlockIndex = blockIndex
  }

  override def skip(n: Long): Long = {
    val result: Long = in.skip(n)
    offset += result
    result
  }

  override def markSupported(): Boolean = super.markSupported()

  override def close(): Unit = super.close()

  override def read(): Int = {
    val b = new Array[Byte](1)
    if (read(b, 0, 1) != -1)
      b(0)
    else
      -1
  }

  override def read(b: Array[Byte]): Int = read(b, 0, b.length)

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    var bytesRead = 0
    var lastRead = 0

    while (lastRead >= 0 && bytesRead < len) {
      if (block.contains(offset, len - bytesRead)) {
        // read fully
        lastRead = in.read(b, off + bytesRead, len - bytesRead)
      } else if (block.contains(offset)) {
        // read partially
        lastRead = in.read(b, off + bytesRead, (block.end - offset + 1).toInt)
      } else if (offset < block.offset) {
        // skip to current block
        in.skip(block.offset - offset)
        offset = block.offset
        lastRead = 0
      } else if (blockIndex < blocks.size - 1) {
        // read next block
        blockIndex += 1
        block = blocks(blockIndex)
        lastRead = 0
      } else {
        lastRead = -1
      }
      if (lastRead > 0) {
        bytesRead += lastRead
        offset += bytesRead
      }
    }
    if (bytesRead == 0) {
      bytesRead = -1
    }

    bytesRead
  }

  override def reset(): Unit = {
    if (!in.markSupported) {
      throw new IOException("Mark not supported")
    }
    if (mark == -1) {
      throw new IOException("Mark not set")
    }

    in.reset()
    offset = mark
    blockIndex = markedBlockIndex
    block = blocks(blockIndex)
  }
}
