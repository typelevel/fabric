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

import fabric.Platform

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait ObjectPool[T] {
  private val _created = new AtomicInteger(0)
  private val _active = new AtomicInteger(0)
  private val _queued = new AtomicInteger(0)

  def created: Int = _created.get()
  def active: Int = _active.get()
  def queued: Int = _queued.get()

  private val queue = new ConcurrentLinkedQueue[T]

  protected def create(): T

  protected def prepareForUse(value: T): T = value

  protected def resetForPool(value: T): Option[T] = Some(value)

  protected def dispose(value: T): Unit = assert(value != null)

  private def get(): T = Option(queue.poll()) match {
    case Some(value) =>
      _queued.decrementAndGet()
      _active.incrementAndGet()
      prepareForUse(value)
    case None =>
      val value = create()
      _created.incrementAndGet()
      _active.incrementAndGet()
      prepareForUse(value)
  }

  private def restore(value: T): Unit = resetForPool(value) match {
    case Some(value) =>
      queue.add(value)
      _active.decrementAndGet()
      _queued.incrementAndGet()
      ()
    case None =>
      _active.decrementAndGet()
      ()
  }

  def use[Return](f: T => Return): Return = {
    val value = get()
    try f(value)
    finally restore(value)
  }

  def ensureAvailable(size: Int): Unit =
    if (_queued.get() < size) {
      val value = create()
      _created.incrementAndGet()
      _queued.incrementAndGet()
      queue.add(value)

      ensureAvailable(size)
    } else {
      ()
    }

  @tailrec
  final def waitForNoActive(delay: FiniteDuration = 100.millis): Unit =
    if (active == 0) {
      ()
    } else {
      Platform.sleep(delay)
      waitForNoActive(delay)
    }

  def dispose(): Unit = {
    waitForNoActive()
    Option(queue.poll()) match {
      case Some(value) =>
        dispose(value)
        dispose()
      case None =>
        _created.set(0)
        _active.set(0)
        _queued.set(0)
    }
  }
}
