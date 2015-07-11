package uritemplate

import Syntax._

class Level1Examples extends ExpansionSpec {

  def name = "Level 1 examples"

  val variables = Map(
    "var"   := "value",
    "hello" := "Hello World!"
  )
  
  example("Simple string expansion                         (Sec 3.2.2)")(
    ("{var}", "value"),
    ("{hello}", "Hello%20World%21")
  )
}