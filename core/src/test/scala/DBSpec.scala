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

class DBSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  case class A(x: Int, y: String, z: Option[Int])
  case class B(x: Int, y: A, z: Option[A])
  val a1 = A(1, "1", Some(1))
  val a2 = A(1, "1", None)
  val b1 = B(1, a1, Some(a1))
  val b2 = B(1, a2, None)
  val b3 = B(2, a1, Some(a2))
  describe("A DB") {
    describe("when query with AQL cursor") {
      it("returns results") {
        val _system = connection.Connection(executor = http.Executor())._system
        val testcol = _system.testcol
        try {
          val f = for {
            Document(a1id, _, _) <- testcol.save(doc = a1, createCollection = true, waitForSync = true)
            Document(a2id, _, _) <- testcol.save(doc = a2, waitForSync = true)
            Document(b1id, _, _) <- testcol.save(doc = b1, waitForSync = true)
            Document(b2id, _, _) <- testcol.save(doc = b2, waitForSync = true)
            Document(b3id, _, _) <- testcol.save(doc = b3, waitForSync = true)
            a1found <- testcol.document[A](id = a1id)
            a2found <- testcol.document[A](id = a2id)
            b1found <- testcol.document[B](id = b1id)
            b2found <- testcol.document[B](id = b2id)
            cursor <- _system._cursor[Document](query = db.AQL("FOR x IN testcol RETURN x"), count = true, batchSize = 2)
            results <- cursor.readAll
          } yield {
            val resultsSet = results.map(_._id).toSet
            assert(resultsSet == Set(a1id, a2id, b1id, b2id, b3id))
          }
          val ready = f.isReadyWithin(1.minute)
          assert(ready)
        } finally {
          testcol.delete
        }
      }
    }
    describe("when validate query") {
      describe("when the query string is invalid") {
        it("throws ArangoDBException") {
          val _system = connection.Connection(executor = http.Executor())._system
          val testcol = _system.testcol
          try {
            val f = for {
              Document(a1id, _, _) <- testcol.save(doc = a1, createCollection = true, waitForSync = true)
              result <- _system._query("invalid")
            } yield {
              assert(result.collections == List("testcol"))
            }
            val e = f.failed.futureValue(timeout(1.minute))
            // assert(e.isInstanceOf[ArangoException]) // TODO: this assertion fails since e is java.util.concurrent.Execution. Dispatcher should extract the cause...
          } finally {
            testcol.delete
          }
        }
      }
      describe("when the query string is valid") {
        it("returns the list of collections that the query accesses") {
          val _system = connection.Connection(executor = http.Executor())._system
          val testcol = _system.testcol
          try {
            val f = for {
              Document(a1id, _, _) <- testcol.save(doc = a1, createCollection = true, waitForSync = true)
              result <- _system._query("FOR x IN testcol RETURN x")
            } yield {
              assert(result.collections == List("testcol"))
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
}

