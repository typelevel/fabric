/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.cryo

import java.nio.ByteBuffer
import scala.annotation.tailrec

object ByteBufferPool extends ObjectPool[ByteBuffer] {

  /**
    * The size of buffers created in the pool. This defaults to 1,000, but will double automatically when it overflows
    * during use and then try again. Existing ByteBuffers will be cleared.
    */
  var ByteBufferSize: Int = 1000

  /**
    * Whether to allocate ByteBuffers as direct. This defaults to true.
    */
  var ByteBufferDirect: Boolean = true

  /**
    * Automatically resizes the queue to double each time it overflows
    */
  var AutoResize: Boolean = true

  override protected def create(): ByteBuffer =
    if (ByteBufferDirect) {
      ByteBuffer.allocateDirect(ByteBufferSize)
    } else {
      ByteBuffer.allocate(ByteBufferSize)
    }

  override protected def resetForPool(bb: ByteBuffer): Option[ByteBuffer] = {
    bb.rewind()
    Some(bb)
  }

  @tailrec
  final def resizePool(atLeast: Int): Unit =
    if (ByteBufferSize >= atLeast) {
      dispose()
    } else {
      synchronized {
        ByteBufferSize += ByteBufferSize
      }
      resizePool(atLeast)
    }
}
