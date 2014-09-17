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
package collection

import db._
import json._, Json._, Marshall._, UnMarshall._
import scala.concurrent._
import internal.Implicits._

case class Document(_id: String, _rev: String, _key: String)
case class EnsuringIndex(`type`: String, unique: Boolean, fields: List[String])
case class EnsuringIndexResult(isNewlyCreated: Boolean, id: String, fields: List[String], `type`: String, unique: Boolean)
case class QueryByExample(collection: String, example: Json, skip: Option[Int], limit: Option[Int])
case class QueryResult(hasMore: Boolean, count: Int, result: List[Json])
case class Collection(name: String, database: DatabaseLike) {
  import database._dispatcher
  def document[A: UnMarshall](id: String): Future[A] =
    (_dispatcher.GET / s"document/$id").dispatchRoot[A]()
  def save[A: Marshall](doc: A, createCollection: Boolean = false, waitForSync: Boolean = false): Future[Document] =
    (_dispatcher.copy(body = Some(Json.write(marshall(doc)))).POST / s"document" <<? Seq("collection" -> name, "createCollection" -> createCollection, "waitForSync" -> waitForSync)).dispatchRoot[Document]()
  def ensureHashIndex(fields: String*) =
    (_dispatcher.copy(body = Some(Json.write(marshall(EnsuringIndex(`type` = "hash", unique = false, fields = fields.toList))))).POST / s"index" <<? Seq("collection" -> name)).dispatch[EnsuringIndexResult]()
  def byExample[A](example: Map[String, Json], skip: Option[Int], limit: Option[Int]) =
    (_dispatcher.copy(body = Some(Json.write(marshall(QueryByExample(collection = name, example = Json.JObject(example), skip = skip, limit = limit))))).PUT / s"simple/by-example").dispatch[QueryResult]()
}

