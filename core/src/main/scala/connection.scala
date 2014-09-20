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
package connection

import scala.concurrent._

import internal.Implicits._

import com.ning.http.client._, http._
import json._, Json._, Marshall._, UnMarshall._
import scalaz._, Scalaz._
import scala.language.dynamics

private[scarango] case class Result[A](result: A)
private[scarango] case class DBCreationOption(name: String, users: List[db.User])

case class Connection(executor: Executor, host: String = "127.0.0.1", port: Int = 8529, ssl: Boolean = false, user: Option[String] = None, password: Option[String] = None) extends Dynamic {
  private[scarango] def _baseUrl = {
    val scheme = if (ssl) "https" else "http"
    s"$scheme://$host:$port"
  }
  private[scarango] def _api = s"${_baseUrl}/_api"
  /**
   * Retrieves the list of all databases the current user can access without specifying a different username or password
   */
  def _userDBs: Future[List[String]] = (Dispatcher().GET / "database/user").dispatch[List[String]]()

  val _system = db.SystemDatabase(connection = this)

  def selectDynamic(name: String) = db.Database(name = name, connection = this)

  private[scarango] case class Dispatcher(url: String = _api, method: String = "GET", body: Option[String] = None, headers: Map[String, String] = Map(), queries: Map[String, String] = Map()) {
    private[this] def dispatchRaw[A](f: Response => A): Future[A] = {
      val req = new RequestBuilder().setUrl(url).setMethod(method)
      for (body <- body)
        req.setBody(body)
      for ((k, v) <- headers)
        req.addHeader(k, v)
      for ((k, v) <- queries)
        req.addQueryParameter(k, v)
      executor(req.build, new AsyncCompletionHandler[A] {
        def onCompleted(res: Response) = f(res)
      })
    }
    private[this] def checkError(json: Json): Unit = {
      json match {
        case JObject(x) if x.get("error") == Some(JBoolean(true)) =>
          val error = unmarshall[ArangoErrorResponse](json) match {
            case -\/(e) => throw new ArangoDriverException(message = s"Unrecognized error response: ${Json.prettyPrint(json)}", cause = Some(e))
            case \/-(r) => r
          }
          throw ArangoException(error = error)
        case _ =>
      }
    }
    def dispatch[A: UnMarshall](): Future[A] =
      dispatchRaw[A] {
        res =>
          val body = res.getResponseBody("UTF-8")
          Json.read(body) match {
            case -\/(e) => throw new ArangoDriverException(s"Could not parse response: $body", cause = Some(e))
            case \/-(json) =>
              checkError(json)
              unmarshall[Result[A]](json) match {
                case -\/(e) => throw new ArangoDriverException(s"Could not convert response: $body", cause = Some(e))
                case \/-(Result(result)) => result
              }
          }
      }
    def dispatchRoot[A: UnMarshall](): Future[A] =
      dispatchRaw[A] {
        res =>
          val body = res.getResponseBody("UTF-8")
          Json.read(body) match {
            case -\/(e) => throw new ArangoDriverException(s"Could not parse response: $body", cause = Some(e))
            case \/-(json) =>
              checkError(json)
              unmarshall[A](json) match {
                case -\/(e) => throw new ArangoDriverException(s"Could not convert response: $body", cause = Some(e))
                case \/-(result) => result
              }
          }
      }
    def GET = copy(method = "GET")
    def POST = copy(method = "POST")
    def PUT = copy(method = "PUT")
    def PATCH = copy(method = "PATCH")
    def HEAD = copy(method = "HEAD")
    def DELETE = copy(method = "DELETE")
    def <:<(hs: Traversable[(String, Any)]) = copy(headers = headers ++ hs.map { case (k, v) => k -> v.toString })
    def <<?(params: Traversable[(String, Any)]) = copy(queries = queries ++ params.map { case (k, v) => k -> v.toString })
    def /(segment: String) = copy(url = s"$url/$segment")
  }
}