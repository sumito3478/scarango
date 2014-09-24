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
import scala.concurrent._, duration._
import internal.Implicits._
import collection._
import json._, Marshall._, UnMarshall._
import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.concurrent.PatienceConfiguration._

class ConnectionSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  implicit class FutureW[A](self: Future[A]) {
    def check(timeout: Timeout = Timeout(1.minute)): A = whenReady(self, timeout = timeout)(identity _)
  }
  describe("Connection") {
    describe("_create") {
      it("create new database") {
        val con = connection.Connection(executor = http.Executor())
        val create = for {
          creationResult <- con._system._create(name = "test-database")
          _ = assert(creationResult)
        } yield ()
        create.check()
        try {
          val test = for {
            dbs <- con._system._database
            _ = assert(dbs == List("_system", "test-database"))
            userDBs <- con._database.user
            _ = assert(userDBs == List("_system", "test-database"))
          } yield ()
          test.check()
        } finally {
          val delete = for {
            deletionResult <- con._system._delete(name = "test-database")
            _ = assert(deletionResult)
          } yield ()
          delete.check()
        }
      }
    }
  }
}
