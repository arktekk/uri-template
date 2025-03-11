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

class Level1Examples extends ExpansionSpec {

  def name = "Level 1 examples"

  val variables = Map(
    "var" := "value",
    "hello" := "Hello World!"
  )

  example("Simple string expansion                         (Sec 3.2.2)")(
    ("{var}", "value"),
    ("{hello}", "Hello%20World%21")
  )
}
