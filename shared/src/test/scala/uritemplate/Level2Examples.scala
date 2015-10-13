package uritemplate

import Syntax._

class Level2Examples extends ExpansionSpec {
  
  def name = "Level 2 examples"
  
  val variables = Map(
    "var"   := "value",
    "hello" := "Hello World!",
    "path"  := "/foo/bar"
  )
  
  example("+ Reserved string expansion                     (Sec 3.2.3)")(
    ("{+var}",            "value"),
    ("{+hello}",          "Hello%20World!"),
    ("{+path}/here",      "/foo/bar/here"),
    ("here?ref={+path}",  "here?ref=/foo/bar")
  )
  
  example("# Fragment expansion, crosshatch-prefixed       (Sec 3.2.4)")(
    ("X{#var}",    "X#value"),
    ("X{#hello}",  "X#Hello%20World!")
  )  
}