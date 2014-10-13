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

class ConnectionSpec extends ScarangoSpec {
  describe("Connection") {
    describe("_create") {
      it("create new database") {
        withConnection {
          con =>
            (for {
              creationResult <- con._system._create(name = "test-database")
              _ = assert(creationResult)
            } yield ()).await()
            try {
              (for {
                dbs <- con._system._database
                _ = assert(dbs == List("_system", "test-database"))
                userDBs <- con._database.user
                _ = assert(userDBs == List("_system", "test-database"))
              } yield ()).await()
            } finally {
              (for {
                deletionResult <- con._system._delete(name = "test-database")
                _ = assert(deletionResult)
              } yield ()).await()
            }
        }
      }
    }
  }
}
