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

import scala.concurrent._, duration._
import internal.Implicits._
import collection._
import json._, Marshall._, UnMarshall._
import org.scalatest._
import org.scalatest.concurrent._

class CollectionSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  case class A(x: Int, y: String, z: Option[Int])
  case class B(x: Int, y: A, z: Option[A])
  val a1 = A(1, "1", Some(1))
  val a2 = A(1, "1", None)
  val b1 = B(1, a1, Some(a1))
  val b2 = B(1, a2, None)
  describe("A Collection") {
    describe("when saves case class instances") {
      it("case class instances are saved in the collection") {
        val testcol = connection.Connection(executor = http.Executor())._system.testcol
        try {
          val f = for {
            Document(a1id, _, _) <- testcol.document.save(doc = a1, createCollection = true, waitForSync = true)
            Document(a2id, _, _) <- testcol.document.save(doc = a2, waitForSync = true)
            Document(b1id, _, _) <- testcol.document.save(doc = b1, waitForSync = true)
            Document(b2id, _, _) <- testcol.document.save(doc = b2, waitForSync = true)
            a1found <- testcol.document.find[A](id = a1id)
            a2found <- testcol.document.find[A](id = a2id)
            b1found <- testcol.document.find[B](id = b1id)
            b2found <- testcol.document.find[B](id = b2id)
          } yield {
            assert(a1found == a1 && a2found == a2 && b1found == b1 && b2found == b2)
          }
          val ready = f.isReadyWithin(1.minute)
          assert(ready)
        } finally {
          testcol.delete
        }
      }
    }
  }
}
