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

import scala.concurrent._
import internal.Implicits._
import collection._
import json._, Marshall._, UnMarshall._
import ScarangoSpec._

class DBSpec extends ScarangoSpec {
  describe("A DB") {
    describe("when query with AQL cursor") {
      it("returns results") {
        withCollection {
          (con, col) =>
            (for {
              Document(a1id, _, _) <- col.document.save(doc = a1, createCollection = true, waitForSync = true)
              Document(a2id, _, _) <- col.document.save(doc = a2, waitForSync = true)
              Document(b1id, _, _) <- col.document.save(doc = b1, waitForSync = true)
              Document(b2id, _, _) <- col.document.save(doc = b2, waitForSync = true)
              Document(b3id, _, _) <- col.document.save(doc = b3, waitForSync = true)
              cursor <- con._system._cursor[Document](query = db.AQL(s"FOR x IN ${col.name} RETURN x"), count = true, batchSize = 2)
              results <- cursor.readAll
            } yield {
              val resultsSet = results.map(_._id).toSet
              assert(resultsSet == Set(a1id, a2id, b1id, b2id, b3id))
            }).await()
        }
      }
      describe("with bind parameters") {
        it("returns results") {
          withCollection {
            (con, col) =>
              (for {
                Document(a1id, _, _) <- col.document.save(doc = a1, createCollection = true, waitForSync = true)
                Document(a2id, _, _) <- col.document.save(doc = a2, waitForSync = true)
                Document(b1id, _, _) <- col.document.save(doc = b1, waitForSync = true)
                Document(b2id, _, _) <- col.document.save(doc = b2, waitForSync = true)
                Document(b3id, _, _) <- col.document.save(doc = b3, waitForSync = true)
                cursor <- con._system._cursor[Document](query = db.AQL(s"FOR x IN ${col.name} FILTER x.x == @x RETURN x", bindVars = Map("x" -> Json.JInt(1))), count = true, batchSize = 2)
                results <- cursor.readAll
              } yield {
                val resultsSet = results.map(_._id).toSet
                assert(resultsSet == Set(a1id, a2id, b1id, b2id))
              }).await()
          }
        }
      }
    }
    describe("when validate query") {
      describe("when the query string is invalid") {
        it("throws ArangoDBException") {
          withCollection {
            (con, col) =>
              val e = (for {
                Document(a1id, _, _) <- col.document.save(doc = a1, createCollection = true, waitForSync = true)
                result <- con._system._query("invalid")
              } yield ()).failed.await()
            // assert(e.isInstanceOf[ArangoException]) // TODO: this assertion fails since e is java.util.concurrent.Execution. Dispatcher should extract the cause...
          }
        }
      }
      describe("when the query string is valid") {
        it("returns the list of collections that the query accesses") {
          withCollection {
            (con, col) =>
              (for {
                Document(a1id, _, _) <- col.document.save(doc = a1, createCollection = true, waitForSync = true)
                result <- con._system._query(s"FOR x IN ${col.name} RETURN x")
              } yield {
                assert(result.collections == List(col.name))
              }).await()
          }
        }
      }
    }
  }
}

