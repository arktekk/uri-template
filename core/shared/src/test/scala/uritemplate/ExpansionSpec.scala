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

import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatest.propspec.AnyPropSpec

trait ExpansionSpec extends AnyPropSpec {

  val variables: Map[String, Option[Variable]]

  override def suiteName = name

  def renderVariables = {
    val keyLength = variables.keySet.map(_.length()).max
    val strings   = variables.map { case (key, value) =>
      val padded: String = key + " " * (keyLength - key.length())
      padded + " := " + value
    }
    strings.mkString("\t", "\n\t", "")
  }

  def name: String

  def example(name: String)(examples: (String, String)*): Unit = {
    property(name) {
      val table = Table(("Expression", "Expansion"), examples: _*)
      forAll(table) { (exp, ex) =>
        assert((URITemplate(exp) expand variables) === ex)
      }
    }
  }

}
