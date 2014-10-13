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

import internal.Implicits._
import collection._
import json._, Marshall._, UnMarshall._
import ScarangoSpec._

class CollectionSpec extends ScarangoSpec {
  describe("A Collection") {
    describe("when saves case class instances") {
      it("case class instances are saved in the collection") {
        withCollection {
          (con, col) =>
            (for {
              Document(a1id, _, _) <- col.document.save(doc = a1, createCollection = true, waitForSync = true)
              Document(a2id, _, _) <- col.document.save(doc = a2, waitForSync = true)
              Document(b1id, _, _) <- col.document.save(doc = b1, waitForSync = true)
              Document(b2id, _, _) <- col.document.save(doc = b2, waitForSync = true)
              a1found <- col.document.find[A](id = a1id)
              a2found <- col.document.find[A](id = a2id)
              b1found <- col.document.find[B](id = b1id)
              b2found <- col.document.find[B](id = b2id)
            } yield {
              assert(a1found == a1 && a2found == a2 && b1found == b1 && b2found == b2)
            }).await()
        }
      }
    }
    describe("when hash index created") {
      it("by-example query") {
        withPreparedCollection {
          case (con, col, (a1id, a2id, b1id, b2id, b3id)) =>
            (for {
              _ <- col.index.hash(fields = List("x"))
              result <- col.document.byExample(example = Map("x" -> Json.JInt(1)))
            } yield {
              assert(result.count == 4)
              val resultSet = result.result.toSet[Json]
              import shapeless.syntax.typeable._
              val ids = for {
                x <- resultSet
              } yield (for {
                x <- x.cast[Json.JObject]
                x <- x.value.get("_id")
                x <- x.cast[Json.JString]
              } yield x.value).getOrElse(sys.error(s"Wrong result $x"))
              assert(ids == Set(a1id, a2id, b1id, b2id))
              val as = for {
                result <- resultSet
                a <- unmarshall[A](result).toOption
              } yield a
              assert(as == Set(a1, a2))
              val bs = for {
                result <- resultSet
                b <- unmarshall[B](result).toOption
              } yield b
              assert(bs == Set(b1, b2))
            }).await()
        }
      }
    }
    describe("when skiplist index created") {
      it("by-example query") {
        withPreparedCollection {
          case (con, col, (a1id, a2id, b1id, b2id, b3id)) =>
            (for {
              _ <- col.index.skiplist(fields = List("x"))
              result <- col.document.byExample(example = Map("x" -> Json.JInt(1)))
            } yield {
              assert(result.count == 4)
              val resultSet = result.result.toSet[Json]
              import shapeless.syntax.typeable._
              val ids = for {
                x <- resultSet
              } yield (for {
                x <- x.cast[Json.JObject]
                x <- x.value.get("_id")
                x <- x.cast[Json.JString]
              } yield x.value).getOrElse(sys.error(s"Wrong result $x"))
              assert(ids == Set(a1id, a2id, b1id, b2id))
              val as = for {
                result <- resultSet
                a <- unmarshall[A](result).toOption
              } yield a
              assert(as == Set(a1, a2))
              val bs = for {
                result <- resultSet
                b <- unmarshall[B](result).toOption
              } yield b
              assert(bs == Set(b1, b2))
            }).await()
        }
      }
    }
    describe("when geo index created") {
      it("near query") {
        withPreparedCollection {
          case (con, col, (a1id, a2id, b1id, b2id, b3id)) =>
            (for {
              _ <- col.index.geo(fields = List("loc"))
              result <- col.document.near(latitude = 0, longitude = 0, distance = Some("distance"))
            } yield {
              assert(result.count == 3)
              val resultSet = result.result.toSet[Json]
              import shapeless.syntax.typeable._
              case class DoubleWithAlmostEquals(self: Double) {
                override def equals(that: Any): Boolean = that match {
                  case DoubleWithAlmostEquals(that) => (self - that).abs <= 0.0001
                  case that => self == that
                }
                override def toString = self.toString
              }
              val distances = for {
                x <- resultSet
              } yield (for {
                x <- x.cast[Json.JObject]
                x <- x.value.get("distance")
                x <- x.cast[Json.JNumber]
              } yield new DoubleWithAlmostEquals(x.value)).getOrElse(sys.error(s"Wrong result $x"))
              val expected = for (x <- Set(222.38985328911744, 444.779706578235)) yield new DoubleWithAlmostEquals(x)
              assert(distances == expected)
            }).await()
        }
      }
    }
  }
}
