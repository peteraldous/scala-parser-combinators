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
package util.parsing.combinator

import scala.util.matching.Regex
import scala.util.parsing.input._
import scala.language.implicitConversions

/** The ''most important'' differences between `RegexParsers` and
 *  [[scala.util.parsing.combinator.Parsers]] are:
 *
 *  - `Elem` is defined to be [[scala.Char]]
 *  - There's an implicit conversion from [[java.lang.String]] to `Parser[String]`,
 *    so that string literals can be used as parser combinators.
 *  - There's an implicit conversion from [[scala.util.matching.Regex]] to `Parser[String]`,
 *    so that regex expressions can be used as parser combinators.
 *  - The parsing methods call the method `skipWhitespace` (defaults to `true`) and, if true,
 *    skip any whitespace before each parser is called.
 *  - Protected val `whiteSpace` returns a regex that identifies whitespace.
 *
 *  For example, this creates a very simple calculator receiving `String` input:
 *
 *  {{{
 *  object Calculator extends RegexParsers {
 *    def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
 *    def factor: Parser[Double] = number | "(" ~> expr <~ ")"
 *    def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
 *      case number ~ list => list.foldLeft(number) {
 *        case (x, "*" ~ y) => x * y
 *        case (x, "/" ~ y) => x / y
 *      }
 *    }
 *    def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
 *      case number ~ list => list.foldLeft(number) {
 *        case (x, "+" ~ y) => x + y
 *        case (x, "-" ~ y) => x - y
 *      }
 *    }
 *
 *    def parse(input: String): Double = parseAll(expr, input) match {
 *      case Success(result, _) => result
 *      case failure : NoSuccess => scala.sys.error(failure.msg)
 *    }
 *  }
 *  }}}
 */
trait RegexParsers extends Parsers {

  type Elem = Char

  override type Input = CharOffsetReader

  /** A parser that matches a literal string */
  implicit def literal(s: String): Parser[String] = new Parser[String] {
    override def parse(in: Input) = {
      var reader = in
      var i = 0
      while (i < s.length && !(reader.atEnd) && s.charAt(i) == reader.first) {
        i += 1
        reader = reader.rest
      }
      if (i == s.length)
        Success(in.subSequence(i).toString, reader, None)
      else {
        val found = if (reader.atEnd) "end of source" else "'"+reader.first+"'"
        Failure("'"+s+"' expected but "+found+" found", in)
      }
    }
  }

  /** A parser that matches a regex string */
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    override def parse(in: Input) = {
      (r findPrefixMatchOf in.subSequence) match {
        case Some(matched) =>
          Success(in.subSequence(matched.end).toString,
                  in.drop(matched.end),
                  None)
        case None =>
          val found = if (in.atEnd) "end of source" else "'"+in.first+"'"
          // TODO should this advance? The original code seems to just drop whitespace
          Failure("string matching regex '"+r+"' expected but "+found+" found", in)
      }
    }
  }

  /** `positioned` decorates a parser's result with the start position of the input it consumed.
   * If whitespace is being skipped, then it is skipped before the start position is recorded.
   *
   * @param p a `Parser` whose result conforms to `Positional`.
   * @return A parser that has the same behaviour as `p`, but which marks its result with the
   *         start position of the input it consumed after whitespace has been skipped, if it
   *         didn't already have a position.
   */
  override def positioned[T <: Positional](p: => Parser[T]): Parser[T] = {
    val pp = super.positioned(p)
    new Parser[T] {
      override def parse(in: Input) = {
        pp(in)
      }
    }
  }

  /**
    * @inheritdoc
    *
    * This parser additionally skips whitespace if `skipWhitespace` returns true.
    */
  override def err(msg: String) = super.err(msg)

  /**
   * A parser generator delimiting whole phrases (i.e. programs).
   *
   * `phrase(p)` succeeds if `p` succeeds and no input is left over after `p`.
   *
   * @param p the parser that must consume all input for the resulting parser
   *          to succeed.
   *
   * @return  a parser that has the same result as `p`, but that only succeeds
   *          if `p` consumed all the input.
   */
  override def phrase[T](p: Parser[T]): Parser[T] =
    super.phrase(p <~ "".r)

  /** Parse some prefix of reader `in` with parser `p`. */
  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] =
    p(in)

  /** Parse some prefix of character sequence `in` with parser `p`. */
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    p(new CharSequenceReader(in))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] =
    p(new PagedSeqReader(PagedSeq.fromReader(in)))

  /** Parse all of reader `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: Reader[Char]): ParseResult[T] =
    parse(phrase(p), in)

  /** Parse all of reader `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: java.io.Reader): ParseResult[T] =
    parse(phrase(p), in)

  /** Parse all of character sequence `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    parse(phrase(p), in)
}
