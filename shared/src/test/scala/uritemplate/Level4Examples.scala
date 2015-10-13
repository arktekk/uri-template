package uritemplate

import Syntax._

class Level4Examples extends ExpansionSpec {
  
  def name = "Level 4 examples"
  
  val variables = Map(
    "var"   := "value",
    "hello" := "Hello World!",
    "path"  := "/foo/bar",
    "list"  := List("red", "green", "blue"),
    "keys"  := List(("semi",";"),("dot","."),("comma",","))
  )
  
  example("String expansion with value modifiers           (Sec 3.2.2)")(
    ("{var:3}",               "val"),
    ("{var:30}",              "value"),
    ("{list}",                "red,green,blue"),
    ("{list*}",               "red,green,blue"),
    ("{keys}",                "semi,%3B,dot,.,comma,%2C"),
    ("{keys*}",               "semi=%3B,dot=.,comma=%2C")
  )
  
  example("+ Reserved expansion with value modifiers       (Sec 3.2.3)")(
    ("{+path:6}/here",        "/foo/b/here"),
    ("{+list}",               "red,green,blue"),
    ("{+list*}",              "red,green,blue"),
    ("{+keys}",               "semi,;,dot,.,comma,,"),
    ("{+keys*}",              "semi=;,dot=.,comma=,")
  )
  
  example("# Fragment expansion with value modifiers       (Sec 3.2.4)")(
    ("{#path:6}/here",        "#/foo/b/here"),
    ("{#list}",               "#red,green,blue"),
    ("{#list*}",              "#red,green,blue"),
    ("{#keys}",               "#semi,;,dot,.,comma,,"),
    ("{#keys*}",              "#semi=;,dot=.,comma=,")
  )
  
  example(". Label expansion, dot-prefixed                 (Sec 3.2.5)")(
    ("X{.var:3}",             "X.val"),
    ("X{.list}",              "X.red,green,blue"),
    ("X{.list*}",             "X.red.green.blue"),
    ("X{.keys}",              "X.semi,%3B,dot,.,comma,%2C"),
    ("X{.keys*}",             "X.semi=%3B.dot=..comma=%2C")
  )
  
  example("/ Path segments, slash-prefixed                 (Sec 3.2.6)")(
    ("{/var:1,var}",          "/v/value"),
    ("{/list}",               "/red,green,blue"),
    ("{/list*}",              "/red/green/blue"),
    ("{/list*,path:4}",       "/red/green/blue/%2Ffoo"),
    ("{/keys}",               "/semi,%3B,dot,.,comma,%2C"),
    ("{/keys*}",              "/semi=%3B/dot=./comma=%2C")
  )
  
  example("; Path-style parameters, semicolon-prefixed     (Sec 3.2.7)")(
    ("{;hello:5}",            ";hello=Hello"),
    ("{;list}",               ";list=red,green,blue"),
    ("{;list*}",              ";list=red;list=green;list=blue"),
    ("{;keys}",               ";keys=semi,%3B,dot,.,comma,%2C"),
    ("{;keys*}",              ";semi=%3B;dot=.;comma=%2C")
  )
  
  example("? Form-style query, ampersand-separated         (Sec 3.2.8)")(
    ("{?var:3}",              "?var=val"),
    ("{?list}",               "?list=red,green,blue"),
    ("{?list*}",              "?list=red&list=green&list=blue"),
    ("{?keys}",               "?keys=semi,%3B,dot,.,comma,%2C"),
    ("{?keys*}",              "?semi=%3B&dot=.&comma=%2C")
  )
  
  example("& Form-style query continuation                 (Sec 3.2.9)")(
    ("{&var:3}",              "&var=val"),
    ("{&list}",               "&list=red,green,blue"),
    ("{&list*}",              "&list=red&list=green&list=blue"),
    ("{&keys}",               "&keys=semi,%3B,dot,.,comma,%2C"),
    ("{&keys*}",              "&semi=%3B&dot=.&comma=%2C")
  )
}