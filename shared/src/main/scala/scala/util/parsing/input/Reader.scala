/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util.parsing.input

/** An interface for streams of values that have positions.
 */
abstract class Reader[+T] {

   /** Returns the first element of the reader
    */
  def first: T

  /** Returns an abstract reader consisting of all elements except the first
   *
   * @return If `atEnd` is `true`, the result will be `this`;
   *         otherwise, it's a `Reader` containing more elements.
   */
  def rest: Reader[T]

  /** Returns an abstract reader consisting of all elements except the first `n` elements.
   */
  def drop(n: Int): Reader[T] = {
    var r: Reader[T] = this
    var cnt = n
    while (cnt > 0) {
      r = r.rest; cnt -= 1
    }
    r
  }

  /** The position of the first element in the reader.
   */
  def pos: Position

  /** `true` iff there are no more elements in this reader.
   */
  def atEnd: Boolean
}

class CharOffsetReader(val source: CharSequence, val offset: Int) extends Reader[Char] {
  def this(s: CharSequence) = this(s, 0)

  override def first = source.charAt(offset)
  override def rest = new CharOffsetReader(source, offset+1)
  override def atEnd = offset == source.length
  // override def charAt(i: Int): Char = source.charAt(i)
  def subSequence: CharSequence = new SubSequence(source, offset)
  def subSequence(length: Int): CharSequence = new SubSequence(source, offset, length)
}
