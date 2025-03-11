/*
 * Copyright 2011 Arktekk
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uritemplate

object Syntax extends Syntax

trait Syntax {
  implicit def varString(s: String): Var = Var(s)
}

case class Var(s: String) {
  def :=[V: CanBeVar](v: V): (String, Option[Variable]) = (s, CanBeVar[V].canBe(v))
}

object CanBeVar extends CanBeVars {
  def apply[V](implicit canBe: CanBeVar[V]) = canBe
}

trait CanBeVar[-V] {
  def canBe(v: V): Option[Variable]
}

trait CanBeVars extends LowerPriorityCanBeVars {
  implicit val stringCanBe: CanBeVar[String] = new CanBeVar[String] {
    def canBe(v: String) = Option(v).map(vv => SequentialVar(Seq(vv)))
  }

  implicit val tuple2CanBe: CanBeVar[(String, String)] = new CanBeVar[(String, String)] {
    def canBe(v: (String, String)) = Option(v).map(vv => AssociativeVar(Seq(vv)))
  }

  implicit val seqStringCanBe: CanBeVar[Seq[String]] = new CanBeVar[Seq[String]] {
    def canBe(v: Seq[String]) = Option(v).map(SequentialVar.apply)
  }

  implicit def optionCanBe[C: CanBeVar]: CanBeVar[Option[C]] = new CanBeVar[Option[C]] {
    def canBe(v: Option[C]) = v.flatMap(CanBeVar[C].canBe)
  }

  implicit val optionNothingCanBe: CanBeVar[Option[Nothing]] = new CanBeVar[Option[Nothing]] {
    def canBe(v: Option[Nothing]) = v
  }
}

trait LowerPriorityCanBeVars {
  implicit val seqTupleCanBe: CanBeVar[Seq[(String, String)]] = new CanBeVar[Seq[(String, String)]] {
    def canBe(v: Seq[(String, String)]) = Option(v).map(AssociativeVar.apply)
  }
}

sealed trait Variable
case class SequentialVar(variable: Seq[String])            extends Variable
case class AssociativeVar(variable: Seq[(String, String)]) extends Variable

object URITemplate {
  def apply(s: String): URITemplate                 = parse(s).fold(sys.error, identity)
  def parse(s: String): Either[String, URITemplate] = URITemplateParser.parse(s)
}

object Allow {
  def U(s: String): String  = URITemplateParser.u(s).map(_.expanded).mkString
  def UR(s: String): String = URITemplateParser.ur(s).map(_.expanded).mkString
}

case class URITemplate(expansions: List[Expansion]) {
  def expand(variables: Map[String, Option[Variable]]): String = expansions.map(_.expand(variables)).mkString
  def expand(variables: (String, Option[Variable])*): String   = expand(Map(variables: _*))
}

sealed trait Expansion {
  def expand(variables: Map[String, Option[Variable]]): String
}

sealed trait Lit                     extends Expansion {
  def expand(variables: Map[String, Option[Variable]]): String = expanded
  def expanded: String
}
case class Encoded(expanded: String) extends Lit
case class Unencoded(char: Char)     extends Lit       {
  def expanded = "%" + char.toInt.toHexString.toUpperCase
}

case class VarSpec(name: String, modifier: Option[Modifier])

object Expression {

  def expand(
      variables: Map[String, Option[Variable]],
      variableList: List[VarSpec],
      first: String,
      sep: String,
      named: Boolean,
      ifemp: String,
      allow: String => String
  ) = {
    val expanded = variableList.flatMap { case VarSpec(n, modifier) =>
      def name(s: String) = if (named) n + "=" + s else s

      variables.getOrElse(n, None).flatMap {
        case SequentialVar(Seq()) | AssociativeVar(Seq()) => None

        case SequentialVar(Seq("")) => Some(if (named) n + ifemp else ifemp)

        case SequentialVar(variable) =>
          Some(modifier match {
            case None                 => name(variable.map(allow).mkString(","))
            case Some(Prefix(length)) => name(variable.map(v => allow(v.take(length))).mkString(","))
            case Some(Explode)        => variable.map(n => name(allow(n))).mkString(sep)
          })

        case AssociativeVar(variable) =>
          Some(modifier match {
            case None            => name(variable.flatMap { case (k, v) => Seq(allow(k), allow(v)) }.mkString(","))
            case Some(Explode)   => variable.map { case (k, v) => allow(k) + "=" + allow(v) }.mkString(sep)
            case Some(Prefix(_)) => throw new IllegalArgumentException("Not allowed by RFC-6570")
          })
      }
    }
    if (expanded.isEmpty) "" else expanded.mkString(first, sep, "")
  }
}

sealed abstract class Expression(first: String, sep: String, named: Boolean, ifemp: String, allow: String => String)
    extends Expansion {
  def variableList: List[VarSpec]
  def expand(variables: Map[String, Option[Variable]]): String =
    Expression.expand(variables, variableList, first, sep, named, ifemp, allow)
}
case class Simple(variableList: List[VarSpec]) extends Expression("", ",", false, "", Allow.U)
case class Reserved(variableList: List[VarSpec])          extends Expression("", ",", false, "", Allow.UR)
case class Fragment(variableList: List[VarSpec])          extends Expression("#", ",", false, "", Allow.UR)
case class Label(variableList: List[VarSpec])             extends Expression(".", ".", false, "", Allow.U)
case class PathSegment(variableList: List[VarSpec])       extends Expression("/", "/", false, "", Allow.U)
case class PathParameter(variableList: List[VarSpec])     extends Expression(";", ";", true, "", Allow.U)
case class Query(variableList: List[VarSpec])             extends Expression("?", "&", true, "=", Allow.U)
case class QueryContinuation(variableList: List[VarSpec]) extends Expression("&", "&", true, "=", Allow.U)

sealed trait Modifier
case class Prefix(maxLength: Int) extends Modifier
case object Explode               extends Modifier

object URITemplateParser {
  import scala.util.parsing.combinator.RegexParsers
  import scala.util.parsing.input.CharSequenceReader

  def parse(s: String): Either[String, URITemplate] = {
    val syntax = new URITemplateParsers
    import syntax._
    phrase(template)(new CharSequenceReader(s)) match {
      case Success(result, _) => Right(result)
      case n                  => Left(n.toString)
    }
  }

  def u(s: String): List[Lit] = {
    val syntax = new URITemplateParsers
    import syntax._
    phrase(U.*)(new CharSequenceReader(s)) match {
      case Success(result, _) => result
      case n                  => sys.error(n.toString)
    }
  }

  def ur(s: String): List[Lit] = {
    val syntax = new URITemplateParsers
    import syntax._
    phrase(`U+R`.*)(new CharSequenceReader(s)) match {
      case Success(result, _) => result
      case n                  => sys.error(n.toString)
    }
  }

  class URITemplateParsers extends RegexParsers {

    override def skipWhitespace = false

    lazy val template = (expression | literal).* ^^ { expansions => URITemplate(expansions) }

    lazy val `U+R` = (pctEncoded ^^ (Encoded.apply) | reserved | unreserved) | anyChar ^^ (Unencoded.apply)
    lazy val U     = unreserved | anyChar ^^ (Unencoded.apply)

    lazy val literal: Parser[Lit] =
      pctEncoded ^^ Encoded.apply | reserved | unreserved | ucschar | iprivate | unencodedLit
    lazy val unencodedLit: Parser[Unencoded] = {
      List(
        %(0x21),
        %(0x23, 0x24),
        %(0x26),
        %(0x28, 0x3b),
        %(0x3d),
        %(0x3f, 0x5b),
        %(0x5d),
        %(0x5f),
        %(0x61, 0x7a),
        %(0x7e)
      ).reduce(_ | _) ^^ (Unencoded.apply)
    }

    lazy val expression: Parser[Expression] = "{" ~> operator.? ~ variableList <~ "}" ^^ {
      case Some(op) ~ vars => op.apply(vars)
      case _ ~ vars        => Simple(vars)
    }

    lazy val operator: Parser[List[VarSpec] => Expression] = {
      List(
        "+" ^^^ (Reserved.apply(_)),
        "#" ^^^ (Fragment.apply(_)),
        "." ^^^ (Label.apply(_)),
        "/" ^^^ (PathSegment.apply(_)),
        ";" ^^^ (PathParameter.apply(_)),
        "?" ^^^ (Query.apply(_)),
        "&" ^^^ (QueryContinuation.apply(_)),
        opReserve
      ).reduce(_ | _)
    }

    lazy val opReserve: Parser[Nothing] = ("=" | "," | "!" | "@" | "|") >> { c => failure(c + " is reserved") }

    lazy val variableList = repsep(varspec, ",")
    lazy val varspec      = varname ~ modifierLevel4.? ^^ { case name ~ mod => VarSpec(name, mod) }
    lazy val varname      = varchar ~ rep("." | varchar) ^^ { case head ~ tail => head + tail.mkString }
    lazy val varchar      = ALPHA | DIGIT | "_" | pctEncoded

    lazy val modifierLevel4 = prefix | explode
    lazy val prefix         = ":" ~> "[1-9]".r ~ "\\d{0,3}".r ^^ { case a ~ b => Prefix((a + b).toInt) }
    lazy val explode        = "*" ^^^ Explode

    lazy val ALPHA  = "[A-Za-z]".r
    lazy val DIGIT  = "[0-9]".r
    lazy val HEXDIG = "[0-9A-Fa-f]".r

    lazy val pctEncoded = "%" ~ HEXDIG ~ HEXDIG ^^ { case pct ~ h1 ~ h2 => pct + h1 + h2 }
    lazy val unreserved = (ALPHA | DIGIT | "-" | "." | "_" | "~") ^^ Encoded.apply
    lazy val reserved   = (genDelims | subDelims) ^^ Encoded.apply
    lazy val genDelims  = ":" | "/" | "?" | "#" | "[" | "]" | "@"
    lazy val subDelims  = "!" | "$" | "&" | "'" | "(" | ")" | "*" | "+" | "," | ";" | "="

    lazy val ucschar  =
      List(
        %(0xa0, 0xd7ff),
        %(0xf900, 0xfdcf),
        %(0xfdf0, 0xffef),
        %(0x10000, 0x1fffd),
        %(0x20000, 0x2fffd),
        %(0x30000, 0x3fffd),
        %(0x40000, 0x4fffd),
        %(0x50000, 0x5fffd),
        %(0x60000, 0x6fffd),
        %(0x70000, 0x7fffd),
        %(0x80000, 0x8fffd),
        %(0x90000, 0x9fffd),
        %(0xa0000, 0xafffd),
        %(0xb0000, 0xbfffd),
        %(0xc0000, 0xcfffd),
        %(0xd0000, 0xdfffd),
        %(0xe1000, 0xefffd)
      ).reduce(_ | _) ^^ (Unencoded.apply)
    lazy val iprivate =
      (%(0xe000, 0xf8ff) | %(0xf0000, 0xffffd) | %(0x100000, 0x10fffd)) ^^ (Unencoded.apply)

    lazy val anyChar          = elem("anyChar", _ != 26.toChar)
    def %(v: Int)             = elem(v.toHexString.toUpperCase, _.toInt == v)
    def %(from: Int, to: Int) =
      elem(from.toHexString.toUpperCase + "-" + to.toHexString.toUpperCase, c => c.toInt >= from && c.toInt <= to)
  }
}
