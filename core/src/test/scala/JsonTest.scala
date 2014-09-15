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

import utest._

object JsonTest extends TestSuite {
  case class A(x: Int, y: String, z: Option[Int])
  case class B(x: Int, y: A, z: Option[A])
  val tests = TestSuite {
    "Converter" - {
      "marshall" - {
        "marshall case class" - {
          val a1 = A(1, "1", Some(1))
          val a2 = A(1, "1", None)
          val b1 = B(1, a1, Some(a1))
          val b2 = B(1, a2, None)
          import json.Converter._
          import json.Converter.auto._
          import json.Json._
          // uTest assert macro and Shapeless derive macro both generates terms named "inst$macro$<n>" and they conflicts... I should call them separatedly
          val a1json = marshall(a1)
          val a2json = marshall(a2)
          val b1json = marshall(b1)
          val b2json = marshall(b2)
          assert(
            a1json == JObject(Map("x" -> JInt(1), "y" -> JString("1"), "z" -> JInt(1))),
            a2json == JObject(Map("x" -> JInt(1), "y" -> JString("1"))),
            b1json == JObject(Map("x" -> JInt(1), "y" -> a1json, "z" -> a1json)),
            b2json == JObject(Map("x" -> JInt(1), "y" -> a2json)))
        }
        "marshall List" - {
          import json.Converter._
          import json.Json._
          assert(marshall(List(1, 2, 3)) == JArray(Seq(JInt(1), JInt(2), JInt(3))))
        }
      }
      "unmarshall" - {
        "unmarshall case class" - {
          import json.Converter._
          import json.Converter.auto._
          import json.Json._
          val a1json = JObject(Map("x" -> JInt(1), "y" -> JString("1"), "z" -> JInt(1)))
          val a2json = JObject(Map("x" -> JInt(1), "y" -> JString("1")))
          val b1json = JObject(Map("x" -> JInt(1), "y" -> a1json, "z" -> a1json))
          val b2json = JObject(Map("x" -> JInt(1), "y" -> a2json))
          val a1 = unmarshallUnsafe[A](a1json)
          val a2 = unmarshallUnsafe[A](a2json)
          val b1 = unmarshallUnsafe[B](b1json)
          val b2 = unmarshallUnsafe[B](b2json)
          // uTest assert macro and Shapeless derive macro both generates terms named "inst$macro$<n>" and they conflicts... I should call them separatedly
          assert(
            a1 == A(1, "1", Some(1)),
            a2 == A(1, "1", None),
            b1 == B(1, a1, Some(a1)),
            b2 == B(1, a2, None))
        }
      }
    }
  }
}

