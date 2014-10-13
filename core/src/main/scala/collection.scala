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
import connection._

case class Document(_id: String, _rev: String, _key: String)
case class EnsuringIndex(`type`: String, unique: Boolean, fields: List[String])
case class EnsuringIndexResult(isNewlyCreated: Boolean, id: String, fields: List[String], `type`: String, unique: Boolean)
case class QueryByExample(collection: String, example: Json, skip: Option[Int], limit: Option[Int])
case class QueryResult(hasMore: Boolean, count: Int, result: List[Json])
case class IndexCreationOptions(`type`: String, unique: Boolean, fields: List[String])
case class IndexCreationResult(id: String, `type`: String, unique: Boolean, fields: List[String], isNewlyCreated: Boolean)
case class GeoIndexCreationOptions(`type`: String, fields: List[String], geoJson: Boolean, constraint: Boolean, ignoreNull: Boolean)
case class GeoIndexCreationResult(id: String, `type`: String, unique: Boolean, fields: List[String], isNewlyCreated: Boolean, geoJson: Boolean, constraint: Boolean, ignoreNull: Boolean)
case class NearQuery(collection: String, latitude: Double, longitude: Double, distance: Option[String], skip: Option[Int], limit: Int = 100, geo: Option[String])
case class WithinQuery(collection: String, latitude: Double, longitude: Double, radius: Double, distance: Option[String], skip: Option[Int], limit: Int = 100, geo: Option[String])
case class FullTextIndexCreationOptions(`type`: String, fields: List[String], minLength: Int)
case class FullTextIndexCreationResult(id: String, `type`: String, unique: Boolean, fields: List[String], isNewlyCreated: Boolean, minLength: Int)
case class FullTextQuery(collection: String, attribute: String, query: String, skip: Option[Int], limit: Option[Int], index: Option[String])
case class Collection(name: String, database: DatabaseLike) {
  import database._dispatcher
  object document {
    def find[A: UnMarshall](id: String): Future[A] =
      (_dispatcher.GET / s"document/$id").dispatchRoot[A]()
    def save[A: Marshall](doc: A, createCollection: Boolean = false, waitForSync: Boolean = false): Future[Document] =
      (_dispatcher.copy(body = JsonContent(marshall(doc))).POST / s"document" <<? Seq("collection" -> name, "createCollection" -> createCollection, "waitForSync" -> waitForSync)).dispatchRoot[Document]()
    def ensureHashIndex(fields: String*) =
      (_dispatcher.copy(body = JsonContent(marshall(EnsuringIndex(`type` = "hash", unique = false, fields = fields.toList)))).POST / s"index" <<? Seq("collection" -> name)).dispatch[EnsuringIndexResult]()
    def byExample[A](example: Map[String, Json], skip: Option[Int] = None, limit: Option[Int] = None) =
      (_dispatcher.copy(body = JsonContent(marshall(QueryByExample(collection = name, example = Json.JObject(example), skip = skip, limit = limit)))).PUT / s"simple/by-example").dispatchRoot[QueryResult]()
    def near[A](latitude: Double, longitude: Double, distance: Option[String] = None, skip: Option[Int] = None, limit: Int = 100, geo: Option[String] = None) =
      (_dispatcher.copy(
        body = JsonContent(marshall(NearQuery(collection = name, latitude = latitude, longitude = longitude, distance = distance, skip = skip, limit = limit, geo = geo)))).PUT / s"simple/near").dispatchRoot[QueryResult]()
    def within[A](latitude: Double, longitude: Double, radius: Double, distance: Option[String] = None, skip: Option[Int] = None, limit: Int = 100, geo: Option[String] = None) =
      (_dispatcher.copy(
        body = JsonContent(marshall(WithinQuery(collection = name, latitude = latitude, longitude = longitude, radius = radius, distance = distance, skip = skip, limit = limit, geo = geo)))).PUT / s"simple/within").dispatchRoot[QueryResult]()
    def fulltext[A](attribute: String, query: String, skip: Option[Int] = None, limit: Option[Int] = None, index: Option[String] = None) =
      (_dispatcher.copy(body = JsonContent(marshall(FullTextQuery(collection = name, attribute = attribute, query = query, skip = skip, limit = limit, index = index)))).PUT / s"simple/fulltext").dispatchRoot[QueryResult]()
  }
  object index {
    def hash(fields: Seq[String], unique: Boolean = false) =
      (_dispatcher.copy(body = JsonContent(marshall(IndexCreationOptions(`type` = "hash", unique = unique, fields = fields.toList)))).POST / s"index" <<? Seq("collection" -> name)).dispatchRoot[IndexCreationResult]()
    def skiplist(fields: Seq[String], unique: Boolean = false) =
      (_dispatcher.copy(body = JsonContent(marshall(IndexCreationOptions(`type` = "skiplist", unique = unique, fields = fields.toList)))).POST / s"index" <<? Seq("collection" -> name)).dispatchRoot[IndexCreationResult]()
    def geo(fields: Seq[String], geoJson: Boolean = false, constraint: Boolean = false, ignoreNull: Boolean = false) =
      (_dispatcher.copy(
        body = JsonContent(marshall(GeoIndexCreationOptions(`type` = "geo", fields = fields.toList, geoJson = geoJson, constraint = constraint, ignoreNull = ignoreNull)))).POST / s"index" <<? Seq("collection" -> name)).dispatchRoot[GeoIndexCreationResult]()
    def fulltext(fields: Seq[String], minLength: Int) =
      (_dispatcher.copy(
        body = JsonContent(marshall(FullTextIndexCreationOptions(`type` = "fulltext", fields = fields.toList, minLength = minLength)))).POST / s"index" <<? Seq("collection" -> name)).dispatchRoot[FullTextIndexCreationResult]()
  }
  /**
   * Deletes this collection.
   */
  def delete: Future[Unit] =
    (_dispatcher.DELETE / s"collection/$name").dispatchRoot[Unit]
}

