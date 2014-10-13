/*
Copyright 2014 sumito3478 <sumito3478@gmail.com>
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package scarango
package internal

import scala.language.experimental.macros
import scala.reflect.macros._
import scala.collection.immutable._
import scala.reflect.macros._
import scala.language.implicitConversions
import java.nio.file._

class Reflect[C <: Context](val c: C) {
  import c.universe._

  def warn(x: String) = c.warning(c.enclosingPosition, x)

  def warnAndBail(x: String) = {
    warn(x)
    sys.error(x)
  }

  implicit class AbstractFileW(self: scala.reflect.io.AbstractFile) {
    def relativePath = Paths.get("").toAbsolutePath.relativize(Paths.get(self.path)).toString
  }

  def typecheck(tree: c.Tree, expected: c.Type): c.Tree = try c.typeCheck(tree = tree, pt = expected) catch {
    case e: TypecheckException =>
      println(s"Typecheck failed! The expanded tree is:\n$tree")
      throw e
  }

  def checking(expected: c.Type)(f: => c.Tree): c.Tree = typecheck(f, expected)
}

