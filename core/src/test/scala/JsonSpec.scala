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
import scala.concurrent._, duration._
import internal.Implicits._
import collection._
import json._, Json._, Marshall._, UnMarshall._
import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.concurrent.PatienceConfiguration._

class JsonSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  case class A(x: Int, y: String, z: Option[Int])
  case class B(x: Int, y: A, z: Option[A])
  describe("Marshall") {
    describe("marshall") {
      it("marshall instances of case class") {
        val a1 = A(1, "1", Some(1))
        val a2 = A(1, "1", None)
        val b1 = B(1, a1, Some(a1))
        val b2 = B(1, a2, None)
        val a1json = marshall(a1)
        val a2json = marshall(a2)
        val b1json = marshall(b1)
        val b2json = marshall(b2)
        assert(
          a1json == JObject(Map("x" -> JInt(1), "y" -> JString("1"), "z" -> JInt(1)))
            && a2json == JObject(Map("x" -> JInt(1), "y" -> JString("1")))
            && b1json == JObject(Map("x" -> JInt(1), "y" -> a1json, "z" -> a1json))
            && b2json == JObject(Map("x" -> JInt(1), "y" -> a2json)))
      }
      it("marshall List") {
        val marshalled = marshall(List(1, 2, 3))
        val expected = JArray(Seq(JInt(1), JInt(2), JInt(3)))
        assert(marshalled == expected)
      }
    }
  }
  describe("UnMarshall") {
    describe("unmarshallUnsafe") {
      it("unmarshall instances of case class") {
        val a1json = JObject(Map("x" -> JInt(1), "y" -> JString("1"), "z" -> JInt(1)))
        val a2json = JObject(Map("x" -> JInt(1), "y" -> JString("1")))
        val b1json = JObject(Map("x" -> JInt(1), "y" -> a1json, "z" -> a1json))
        val b2json = JObject(Map("x" -> JInt(1), "y" -> a2json))
        val a1 = unmarshallUnsafe[A](a1json)
        val a2 = unmarshallUnsafe[A](a2json)
        val b1 = unmarshallUnsafe[B](b1json)
        val b2 = unmarshallUnsafe[B](b2json)
        assert(
          a1 == A(1, "1", Some(1))
            && a2 == A(1, "1", None)
            && b1 == B(1, a1, Some(a1))
            && b2 == B(1, a2, None))
      }
    }
  }
}
