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
import internal.Implicits._
import scalaz._, Scalaz._

case class DatabaseInfo(name: String, id: String, path: String, isSystem: Boolean)
case class User(username: String, passwd: Option[String], active: Boolean, extra: Json)
case class AQL(source: String, bindVars: Map[String, Json] = Map())
case class CursorQuery(query: String, count: Boolean, batchSize: Int, bindVars: Map[String, Json] = Map())
case class CursorQueryResult[A](hasMore: Boolean, error: Boolean, id: Option[String], result: List[A], code: Int, count: Option[Int])
case class Cursor[A](head: List[A], tail: Option[() => Future[Cursor[A]]]) {
  /**
   * Applies a binary operator to all elements returned by this cursor, going first to last.
   */
  def foldLeft[B](init: B)(f: (B, A) => B): Future[B] = {
    def loop(c: Cursor[A], acc: B): Future[B] = c.tail match {
      case None => Future(c.head.foldLeft(acc)(f))
      case Some(x) =>
        val h = c.head.foldLeft(acc)(f)
        for {
          y <- x()
          acc <- loop(y, h)
        } yield acc
    }
    loop(this, init)
  }
  /**
   * Read all results from the cursor - this method exists mainly for testing purpose.
   */
  def readAll: Future[List[A]] =
    foldLeft(DList[A]()) {
      case (acc, x) => acc :+ x
    }.map(_.toList)
}
case class QueryValidationInput(query: String)
case class QueryValidationResult(bindVars: List[String], collections: List[String])
case class CollectionKeyOptions(
  `type`: String,
  allowUserKeys: Boolean,
  increment: Int,
  offset: Int)
case class CollectionCreationOptions(
  name: String,
  waitForSync: Boolean = false,
  doCompact: Boolean = true,
  journalSize: Option[Int] = None,
  isSystem: Boolean = false,
  isVolatile: Boolean = false,
  keyOptions: Option[CollectionKeyOptions] = None,
  `type`: Int = 2,
  numberOfShards: Int = 1,
  shardKeys: String = "_key")

sealed trait DatabaseLike extends Dynamic {
  val connection: Connection
  protected[this] def _api: String
  private[scarango] val _dispatcher = connection.Dispatcher(url = _api)

  def _collection(name: String) = Collection(name = name, database = this)

  def selectDynamic(name: String) = _collection(name = name)
  /**
   * Create an AQL query cursor and returns the results as a Cursor object
   */
  def _cursor[A: UnMarshall](query: AQL, count: Boolean = false, batchSize: Int = 1000): Future[Cursor[A]] = {
    def fail(): Nothing = throw new ArangoDriverException("hasMore = true but no id found")
    def nextCursor(prev: CursorQueryResult[A]): Future[Cursor[A]] = {
      for {
        next <- (_dispatcher.copy(body = Some(Json.write(marshall(CursorQuery(query = query.source, count = count, batchSize = batchSize, bindVars = query.bindVars))))).PUT
          / s"cursor/${prev.id.getOrElse(fail())}").dispatchRoot[CursorQueryResult[A]]
      } yield if (next.hasMore) Cursor(head = next.result, tail = Some(() => nextCursor(next))) else Cursor(head = next.result, tail = None)
    }
    for {
      first <- (_dispatcher.copy(body = Some(Json.write(marshall(CursorQuery(query = query.source, count = count, batchSize = batchSize, bindVars = query.bindVars))))).POST / s"cursor").dispatchRoot[CursorQueryResult[A]]
    } yield if (first.hasMore) Cursor(head = first.result, tail = Some(() => nextCursor(first))) else Cursor(head = first.result, tail = None)
  }
  /**
   * Validate an AQL Query string without executing it
   */
  def _query(query: String): Future[QueryValidationResult] = {
    val in = QueryValidationInput(query = query)
    for {
      result <- (_dispatcher.copy(body = Some(Json.write(marshall(in)))).POST / s"query").dispatchRoot[QueryValidationResult]
    } yield result
  }

  /**
   * Creates an new collection with a given name.
   */
  def _createCollection(options: CollectionCreationOptions): Future[Collection] = {
    for {
      _ <- (_dispatcher.copy(body = Some(Json.write(marshall(options)))).POST / s"collection").dispatchRoot[Unit]() // TODO: returns the created collection info
    } yield _collection(name = options.name)
  }
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
  def _database: Future[List[String]] = (_dispatcher.GET / "database").dispatch[List[String]]()

  /**
   * Creates a new database
   */
  def _create(name: String, users: List[db.User] = List()): Future[Boolean] = (_dispatcher.copy(body = Some(Json.write(marshall(DBCreationOption(name = name, users = users))))).POST / "database").dispatch[Boolean]()

  /**
   * Deletes the database along with all data stored in it. Note: dropping a database is only possible from within the _system database. The _system database itself cannot be dropped.
   */
  def _delete(name: String): Future[Boolean] = (_dispatcher.DELETE / s"database/$name").dispatch[Boolean]()
}
