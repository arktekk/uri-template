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

import Syntax._

class ExpressionExpansionSpec extends ExpansionSpec {

  def name = "Expression Expansion Specification"

  val variables = Map(
    "count" := List("one", "two", "three"),
    "dom" := List("example", "com"),
    "dub" := "me/too",
    "hello" := "Hello World!",
    "half" := "50%",
    "var" := "value",
    "who" := "fred",
    "base" := "http://example.com/home/",
    "path" := "/foo/bar",
    "list" := List("red", "green", "blue"),
    "keys" := List(("semi", ";"), ("dot", "."), ("comma", ",")),
    "v" := "6",
    "x" := "1024",
    "y" := "768",
    "empty" := "",
    "empty_keys" := Nil,
    "undef" := None
  )

  example("3.2.2.  Simple String Expansion: {var}")(
    ("{var}", "value"),
    ("{hello}", "Hello%20World%21"),
    ("{half}", "50%25"),
    ("O{empty}X", "OX"),
    ("O{undef}X", "OX"),
    ("{x,y}", "1024,768"),
    ("{x,hello,y}", "1024,Hello%20World%21,768"),
    ("?{x,empty}", "?1024,"),
    ("?{x,undef}", "?1024"),
    ("?{undef,y}", "?768"),
    ("{var:3}", "val"),
    ("{var:30}", "value"),
    ("{list}", "red,green,blue"),
    ("{list*}", "red,green,blue"),
    ("{keys}", "semi,%3B,dot,.,comma,%2C"),
    ("{keys*}", "semi=%3B,dot=.,comma=%2C")
  )

  example("3.2.3.  Reserved expansion: {+var}")(
    ("{+var}", "value"),
    ("{+hello}", "Hello%20World!"),
    ("{+half}", "50%25"),
    ("{base}index", "http%3A%2F%2Fexample.com%2Fhome%2Findex"),
    ("{+base}index", "http://example.com/home/index"),
    ("O{+empty}X", "OX"),
    ("O{+undef}X", "OX"),
    ("{+path}/here", "/foo/bar/here"),
    ("here?ref={+path}", "here?ref=/foo/bar"),
    ("up{+path}{var}/here", "up/foo/barvalue/here"),
    ("{+x,hello,y}", "1024,Hello%20World!,768"),
    ("{+path,x}/here", "/foo/bar,1024/here"),
    ("{+path:6}/here", "/foo/b/here"),
    ("{+list}", "red,green,blue"),
    ("{+list*}", "red,green,blue"),
    ("{+keys}", "semi,;,dot,.,comma,,"),
    ("{+keys*}", "semi=;,dot=.,comma=,")
  )

  example("3.2.4.  Fragment expansion: {#var}")(
    ("{#var}", "#value"),
    ("{#hello}", "#Hello%20World!"),
    ("{#half}", "#50%25"),
    ("foo{#empty}", "foo#"),
    ("foo{#undef}", "foo"),
    ("{#x,hello,y}", "#1024,Hello%20World!,768"),
    ("{#path,x}/here", "#/foo/bar,1024/here"),
    ("{#path:6}/here", "#/foo/b/here"),
    ("{#list}", "#red,green,blue"),
    ("{#list*}", "#red,green,blue"),
    ("{#keys}", "#semi,;,dot,.,comma,,"),
    ("{#keys*}", "#semi=;,dot=.,comma=,")
  )

  example("3.2.5.  Label expansion with dot-prefix: {.var}")(
    ("{.who}", ".fred"),
    ("{.who,who}", ".fred.fred"),
    ("{.half,who}", ".50%25.fred"),
    ("www{.dom*}", "www.example.com"),
    ("X{.var}", "X.value"),
    ("X{.empty}", "X."),
    ("X{.undef}", "X"),
    ("X{.var:3}", "X.val"),
    ("X{.list}", "X.red,green,blue"),
    ("X{.list*}", "X.red.green.blue"),
    ("X{.keys}", "X.semi,%3B,dot,.,comma,%2C"),
    ("X{.keys*}", "X.semi=%3B.dot=..comma=%2C"),
    ("X{.empty_keys}", "X"),
    ("X{.empty_keys*}", "X")
  )

  example("3.2.6.  Path segment expansion: {/var}")(
    ("{/who}", "/fred"),
    ("{/who,who}", "/fred/fred"),
    ("{/half,who}", "/50%25/fred"),
    ("{/who,dub}", "/fred/me%2Ftoo"),
    ("{/var}", "/value"),
    ("{/var,empty}", "/value/"),
    ("{/var,undef}", "/value"),
    ("{/var,x}/here", "/value/1024/here"),
    ("{/var:1,var}", "/v/value"),
    ("{/list}", "/red,green,blue"),
    ("{/list*}", "/red/green/blue"),
    ("{/list*,path:4}", "/red/green/blue/%2Ffoo"),
    ("{/keys}", "/semi,%3B,dot,.,comma,%2C"),
    ("{/keys*}", "/semi=%3B/dot=./comma=%2C")
  )

  example("3.2.7.  Path-style parameter expansion: {;var}")(
    ("{;who}", ";who=fred"),
    ("{;half}", ";half=50%25"),
    ("{;empty}", ";empty"),
    ("{;v,empty,who}", ";v=6;empty;who=fred"),
    ("{;v,bar,who}", ";v=6;who=fred"),
    ("{;x,y}", ";x=1024;y=768"),
    ("{;x,y,empty}", ";x=1024;y=768;empty"),
    ("{;x,y,undef}", ";x=1024;y=768"),
    ("{;hello:5}", ";hello=Hello"),
    ("{;list}", ";list=red,green,blue"),
    ("{;list*}", ";list=red;list=green;list=blue"),
    ("{;keys}", ";keys=semi,%3B,dot,.,comma,%2C"),
    ("{;keys*}", ";semi=%3B;dot=.;comma=%2C")
  )

  example("3.2.8.  Form-style query expansion: {?var}")(
    ("{?who}", "?who=fred"),
    ("{?half}", "?half=50%25"),
    ("{?x,y}", "?x=1024&y=768"),
    ("{?x,y,empty}", "?x=1024&y=768&empty="),
    ("{?x,y,undef}", "?x=1024&y=768"),
    ("{?var:3}", "?var=val"),
    ("{?list}", "?list=red,green,blue"),
    ("{?list*}", "?list=red&list=green&list=blue"),
    ("{?keys}", "?keys=semi,%3B,dot,.,comma,%2C"),
    ("{?keys*}", "?semi=%3B&dot=.&comma=%2C")
  )

  example("3.2.9.  Form-style query continuation: {&var}")(
    ("{&who}", "&who=fred"),
    ("{&half}", "&half=50%25"),
    ("?fixed=yes{&x}", "?fixed=yes&x=1024"),
    ("{&x,y,empty}", "&x=1024&y=768&empty="),
    ("{&x,y,undef}", "&x=1024&y=768"),
    ("{&var:3}", "&var=val"),
    ("{&list}", "&list=red,green,blue"),
    ("{&list*}", "&list=red&list=green&list=blue"),
    ("{&keys}", "&keys=semi,%3B,dot,.,comma,%2C"),
    ("{&keys*}", "&semi=%3B&dot=.&comma=%2C")
  )
}
