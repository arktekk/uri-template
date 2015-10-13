package uritemplate

import Syntax._

class Level3Examples extends ExpansionSpec {
  def name = "Level 3 examples"
  
  val variables = Map(
    "var"   := "value",
    "hello" := "Hello World!",
    "empty" := "",
    "path"  := "/foo/bar",
    "x"     := "1024",
    "y"     := "768"
  )
  
  example("String expansion with multiple variables        (Sec 3.2.2)")(
    ("map?{x,y}",             "map?1024,768"),
    ("{x,hello,y}",           "1024,Hello%20World%21,768")
  )
  
  example("+ Reserved expansion with multiple variables    (Sec 3.2.3)")(
    ("{+x,hello,y}",          "1024,Hello%20World!,768"),
    ("{+path,x}/here",        "/foo/bar,1024/here")
  )
  
  example("# Fragment expansion with multiple variables    (Sec 3.2.4)")(
    ("{#x,hello,y}",          "#1024,Hello%20World!,768"),
    ("{#path,x}/here",        "#/foo/bar,1024/here")
  )
  
  example(". Label expansion, dot-prefixed                 (Sec 3.2.5)")(
    ("X{.var}",               "X.value"),
    ("X{.x,y}",               "X.1024.768")
  )
  
  example("/ Path segments, slash-prefixed                 (Sec 3.2.6)")(
    ("{/var}",                "/value"),
    ("{/var,x}/here",         "/value/1024/here")
  )
  
  example("; Path-style parameters, semicolon-prefixed     (Sec 3.2.7)")(
    ("{;x,y}",                ";x=1024;y=768"),
    ("{;x,y,empty}",          ";x=1024;y=768;empty")
  )
  
  example("? Form-style query, ampersand-separated         (Sec 3.2.8)")(
    ("{?x,y}",                "?x=1024&y=768"),
    ("{?x,y,empty}",          "?x=1024&y=768&empty=")
  )
  
  example("& Form-style query continuation                 (Sec 3.2.9)")(
    ("?fixed=yes{&x}",        "?fixed=yes&x=1024"),
    ("{&x,y,empty}",          "&x=1024&y=768&empty=")
  )
}