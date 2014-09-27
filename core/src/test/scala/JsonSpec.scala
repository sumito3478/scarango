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

import json._, Json._, Marshall._, UnMarshall._
import ScarangoSpec._

class JsonSpec extends ScarangoSpec {
  describe("Marshall") {
    describe("marshall") {
      it("marshall instances of case class") {
        object Results {
          val a1json = marshall(a1)
          val a2json = marshall(a2)
          val b1json = marshall(b1)
          val b2json = marshall(b2)
          val b3json = marshall(b3)
        }
        assert(
          Results.a1json == a1json
            && Results.a2json == a2json
            && Results.b1json == b1json
            && Results.b2json == b2json
            && Results.b3json == b3json)
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
        object Results {
          val a1 = unmarshallUnsafe[A](a1json)
          val a2 = unmarshallUnsafe[A](a2json)
          val b1 = unmarshallUnsafe[B](b1json)
          val b2 = unmarshallUnsafe[B](b2json)
          val b3 = unmarshallUnsafe[B](b3json)
        }
        assert(
          Results.a1 == a1
            && Results.a2 == a2
            && Results.b1 == b1
            && Results.b2 == b2
            && Results.b3 == b3)
      }
    }
  }
}
