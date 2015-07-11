package uritemplate

object Syntax extends Syntax

trait Syntax {
  import scala.language.implicitConversions
  implicit def varString(s:String) = Var(s)
}

case class Var(s:String){
  def :=[V : CanBeVar](v:V):(String, Option[Variable]) = (s, CanBeVar[V].canBe(v))
}

object CanBeVar extends CanBeVars {
  def apply[V](implicit canBe:CanBeVar[V]) = canBe
}

trait CanBeVar[-V] {
  def canBe(v:V):Option[Variable]
}

trait CanBeVars extends LowerPriorityCanBeVars {
  implicit val stringCanBe = new CanBeVar[String]{
    def canBe(v: String) = Option(v).map(vv => SequentialVar(Seq(vv)))
  }

  implicit val tuple2CanBe = new CanBeVar[(String, String)]{
    def canBe(v: (String, String)) = Option(v).map(vv => AssociativeVar(Seq(vv)))
  }

  implicit val seqStringCanBe = new CanBeVar[Seq[String]]{
    def canBe(v: Seq[String]) = Option(v).map(SequentialVar)
  }

  implicit def optionCanBe[C : CanBeVar] = new CanBeVar[Option[C]]{
    def canBe(v: Option[C]) = v.flatMap(CanBeVar[C].canBe)
  }

  implicit val optionNothingCanBe = new CanBeVar[Option[Nothing]]{
    def canBe(v: Option[Nothing]) = v
  }
}

trait LowerPriorityCanBeVars {
  implicit val seqTupleCanBe = new CanBeVar[Seq[(String, String)]]{
    def canBe(v: Seq[(String, String)]) = Option(v).map(AssociativeVar)
  }
}

sealed trait Variable
case class SequentialVar(variable:Seq[String]) extends Variable
case class AssociativeVar(variable:Seq[(String, String)]) extends Variable

object URITemplate {
  def apply(s:String):URITemplate = parse(s).fold(sys.error, identity)
  def parse(s:String) = URITemplateParser.parse(s)
}

object Allow {
  def U(s:String):String = URITemplateParser.u(s).map(_.expanded).mkString
  def UR(s:String)= URITemplateParser.ur(s).map(_.expanded).mkString
}

case class URITemplate(expansions: List[Expansion]){
  def expand(variables:Map[String, Option[Variable]]) = expansions.map(_.expand(variables)).mkString  
  def expand(variables:(String, Option[Variable])*):String = expand(Map(variables:_*))
}

sealed trait Expansion {
  def expand(variables:Map[String, Option[Variable]]):String
}

sealed trait Lit extends Expansion {
  def expand(variables: Map[String, Option[Variable]]) = expanded
  def expanded:String 
}
case class Encoded(expanded:String) extends Lit
case class Unencoded(char:Char) extends Lit {
  def expanded = "%" + char.intValue().toHexString.toUpperCase
}

case class VarSpec(name: String, modifier: Option[Modifier])

object Expression {
  
  def expand(variables:Map[String, Option[Variable]], variableList:List[VarSpec], first:String, sep:String, named:Boolean, ifemp:String, allow:String => String) = {
    val expanded = variableList.flatMap{ case VarSpec(n, modifier) =>
      
      def name(s:String) = if(named) n+"="+s else s
      
      variables.getOrElse(n, None).flatMap{          
        case SequentialVar(Seq()) | AssociativeVar(Seq()) => None

        case SequentialVar(Seq("")) => Some(if(named)n+ifemp else ifemp)

        case SequentialVar(variable) => Some(modifier match {
          case None => name(variable.map(allow).mkString(","))              
          case Some(Prefix(length)) => name(variable.map(v => allow(v.take(length))).mkString(","))              
          case Some(Explode) => variable.map(n => name(allow(n))).mkString(sep)            
        })

        case AssociativeVar(variable) => Some(modifier match {
          case None => name(variable.flatMap{ case (k,v) => Seq(allow(k), allow(v)) }.mkString(","))
          case Some(Explode) => variable.map{ case (k, v) => allow(k)+"="+allow(v)}.mkString(sep)
          case Some(Prefix(_)) => throw new IllegalArgumentException("Not allowed by RFC-6570")
        })
      }
    }
    if(expanded.isEmpty) "" else expanded.mkString(first, sep, "")
  }  
}

sealed abstract class Expression(first:String, sep:String, named:Boolean, ifemp:String, allow:String => String) extends Expansion {
  def variableList:List[VarSpec]
  def expand(variables: Map[String, Option[Variable]]) = Expression.expand(variables, variableList, first, sep, named, ifemp, allow)
}
case class Simple(variableList:List[VarSpec])            extends Expression("",  ",", false, "",  Allow.U)
case class Reserved(variableList:List[VarSpec])          extends Expression("",  ",", false, "",  Allow.UR)
case class Fragment(variableList:List[VarSpec])          extends Expression("#", ",", false, "",  Allow.UR)
case class Label(variableList:List[VarSpec])             extends Expression(".", ".", false, "",  Allow.U)
case class PathSegment(variableList:List[VarSpec])       extends Expression("/", "/", false, "",  Allow.U)
case class PathParameter(variableList:List[VarSpec])     extends Expression(";", ";", true,  "",  Allow.U)
case class Query(variableList:List[VarSpec])             extends Expression("?", "&", true,  "=", Allow.U)
case class QueryContinuation(variableList:List[VarSpec]) extends Expression("&", "&", true,  "=", Allow.U)

sealed trait Modifier
case class Prefix(maxLength: Int) extends Modifier
case object Explode extends Modifier

object URITemplateParser {
  import util.parsing.combinator.RegexParsers
  import util.parsing.input.{CharSequenceReader}  
  
  def parse(s:String):Either[String, URITemplate] = {
    val syntax = new URITemplateParsers
    import syntax._
    phrase(template)(new CharSequenceReader(s)) match {
      case Success(result, _) => Right(result)
      case n                  => Left(n.toString)
    } 
  }
  
  def u(s:String):List[Lit] = {
    val syntax = new URITemplateParsers
    import syntax._
    phrase(U.*)(new CharSequenceReader(s)) match {
      case Success(result, _) => result
      case n                  => sys.error(n.toString)
    }
  }
  
  def ur(s:String):List[Lit] = {
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
    
    lazy val `U+R` = (pctEncoded ^^ Encoded | reserved | unreserved) | anyChar ^^ Unencoded 
    lazy val U     = unreserved | anyChar ^^ Unencoded    
    
    lazy val literal:Parser[Lit] =
      ( pctEncoded ^^ Encoded
      | reserved 
      | unreserved
      | ucschar
      | iprivate 
      | unencodedLit )
    
    lazy val unencodedLit =
      ( %(0x21) | %(0x23, 0x24) | %(0x26) | %(0x28, 0x3B) | %(0x3D) | %(0x3F, 0x5B)
      | %(0x5D) | %(0x5F) | %(0x61, 0x7A) | %(0x7E)) ^^ Unencoded
    
    lazy val expression = "{" ~> operator.? ~ variableList <~ "}" ^^ { 
      case Some(op) ~ vars => op(vars)
      case _        ~ vars => Simple(vars)
    }
    
    lazy val operator = 
      ( "+" ^^^ Reserved 
      | "#" ^^^ Fragment 
      | "." ^^^ Label 
      | "/" ^^^ PathSegment 
      | ";" ^^^ PathParameter 
      | "?" ^^^ Query 
      | "&" ^^^ QueryContinuation 
      | opReserve )
    
    lazy val opReserve = ("=" | "," | "!" | "@" | "|") >> { c => failure(c + " is reserved") }
    
    lazy val variableList = repsep(varspec, ",")
    lazy val varspec = varname ~ modifierLevel4.? ^^ { case name ~ mod => VarSpec(name, mod) }
    lazy val varname = varchar ~ rep("." | varchar) ^^ { case head ~ tail => head + tail.mkString }
    lazy val varchar = ALPHA | DIGIT | "_" | pctEncoded
    
    lazy val modifierLevel4 = prefix | explode
    lazy val prefix = ":" ~> "[1-9]".r ~ "\\d{0,3}".r ^^ { case a ~ b => Prefix((a + b).toInt) }
    lazy val explode = "*" ^^^ Explode
    
    lazy val ALPHA = "[A-Za-z]".r
    lazy val DIGIT = "[0-9]".r
    lazy val HEXDIG = "[0-9A-Fa-f]".r
    
    lazy val pctEncoded = "%" ~ HEXDIG ~ HEXDIG ^^ { case pct ~ h1 ~ h2 => pct + h1 + h2 }
    lazy val unreserved = (ALPHA | DIGIT | "-" | "." | "_" | "~") ^^ Encoded
    lazy val reserved = (genDelims | subDelims) ^^ Encoded
    lazy val genDelims = ":" | "/" | "?" | "#" | "[" | "]" | "@"
    lazy val subDelims = "!" | "$" | "&" | "'" | "(" | ")" | "*" | "+" | "," | ";" | "="
    
    lazy val ucschar = 
      ( %(0xA0,0xD7FF)      | %(0xF900,0xFDCF)   | %(0xFDF0,0xFFEF)
      | %(0x10000, 0x1FFFD) | %(0x20000,0x2FFFD) | %(0x30000,0x3FFFD)
      | %(0x40000, 0x4FFFD) | %(0x50000,0x5FFFD) | %(0x60000,0x6FFFD)
      | %(0x70000, 0x7FFFD) | %(0x80000,0x8FFFD) | %(0x90000,0x9FFFD)
      | %(0xA0000, 0xAFFFD) | %(0xB0000,0xBFFFD) | %(0xC0000,0xCFFFD)
      | %(0xD0000, 0xDFFFD) | %(0xE1000,0xEFFFD)) ^^ Unencoded
    
    lazy val iprivate = 
      ( %(0xE000,0xF8FF) | %(0xF0000,0xFFFFD) | %(0x100000,0x10FFFD) ) ^^ Unencoded

    lazy val anyChar = elem("anyChar", _ != 26.toChar)
    def %(v:Int) = elem(v.toHexString.toUpperCase, _.toInt == v)
    def %(from:Int, to:Int) = elem(from.toHexString.toUpperCase+"-"+to.toHexString.toUpperCase, c =>  c.toInt >= from && c.toInt <= to)
  }
}


