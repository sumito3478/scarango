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
  }
}
