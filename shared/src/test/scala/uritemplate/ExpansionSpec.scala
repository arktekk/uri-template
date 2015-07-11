package uritemplate

import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

trait ExpansionSpec extends PropSpec {

  val variables:Map[String, Option[Variable]]

  override def suiteName = name
  
  def renderVariables = {
    val keyLength = variables.keySet.map(_.length()).max
    val strings = variables.map{ case (key, value) =>
      val padded:String = key + " " * (keyLength - key.length()) 
      padded + " := " + value
    }
    strings.mkString("\t","\n\t", "")
  }

  def name:String

  def example(name:String)(examples:(String, String)*){
    property(name){
      val table = Table(("Expression", "Expansion"), examples :_*)
      forAll(table){ (exp, ex) =>        
        assert((URITemplate(exp) expand variables) === ex)
      }
    }
  }
  
}