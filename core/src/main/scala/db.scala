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
package db

import json._, Json._, Marshall._, UnMarshall._
import connection._
import collection._
import scala.language.dynamics
import scala.concurrent._

case class DatabaseInfo(name: String, id: String, path: String, isSystem: Boolean)
case class User(username: String, passwd: Option[String], active: Boolean, extra: Json)

sealed trait DatabaseLike extends Dynamic {
  val connection: Connection
  protected[this] def _api: String
  private[scarango] val _dispatcher = connection.Dispatcher(url = _api)
  def selectDynamic(name: String): Collection = Collection(name, this)
  /**
   * Retrieves information about the current database
   */
  def _info: Future[DatabaseInfo] = (_dispatcher.GET / "database/current").dispatch[DatabaseInfo]()
}

case class Database(name: String, connection: Connection) extends DatabaseLike {
  def _api = s"${connection._baseUrl}/_db/$name/_api"
}
case class DefaultDatabase(connection: Connection) extends DatabaseLike {
  def _api = s"${connection._api}"
}
case class SystemDatabase(connection: Connection) extends DatabaseLike {
  def _api = s"${connection._baseUrl}/_db/_system/_api"
  /**
   * Retrieves the list of all existing databases Note: retrieving the list of databases is only possible from within the _system database.
   */
  def _dbs: Future[List[String]] = (_dispatcher.GET / "database").dispatch[List[String]]()

  /**
   * Creates a new database
   */
  def _create(name: String, users: List[db.User] = List()): Future[Boolean] = (_dispatcher.copy(body = Some(Json.write(marshall(DBCreationOption(name = name, users = users))))).POST / "database").dispatch[Boolean]()

  /**
   * Deletes the database along with all data stored in it. Note: dropping a database is only possible from within the _system database. The _system database itself cannot be dropped.
   */
  def _delete(name: String): Future[Boolean] = (_dispatcher.DELETE / s"database/$name").dispatch[Boolean]()
}
