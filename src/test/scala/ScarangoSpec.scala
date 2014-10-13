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

import java.util.concurrent.atomic._
import org.scalatest._
import org.scalatest.time._
import org.scalatest.concurrent._
import scala.concurrent._
import scala.concurrent.duration._
import http._, db._, connection._, collection._
import json._, Json._
import Disposable._
import scalaz._
import internal.Implicits._

object ScarangoSpec {
  private val _sequence = new AtomicInteger
  case class A(x: Int, y: String, z: Option[Int], text: String)
  case class B(x: Int, y: A, z: Option[A], loc: List[Double])
}
import ScarangoSpec._

abstract class ScarangoSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  val a1 = A(1, "1", Some(1), "Nam cupide conculcatur nimis ante metutum!")
  val a2 = A(1, "1", None, "さらに思ひつゞくれば、やまとうたの道は、唯まことすくなく、あだなるすさびばかりと思ふ人もやあらむ。")
  val b1 = B(1, a1, Some(a1), List(0.002, 0))
  val b2 = B(1, a2, None, List(-0.002, 0))
  val b3 = B(2, a1, Some(a2), List(-0.004, 0))
  val a1json = JObject(Map("x" -> JInt(1), "y" -> JString("1"), "z" -> JInt(1), "text" -> JString("Nam cupide conculcatur nimis ante metutum!")))
  val a2json = JObject(Map("x" -> JInt(1), "y" -> JString("1"), "text" -> JString("さらに思ひつゞくれば、やまとうたの道は、唯まことすくなく、あだなるすさびばかりと思ふ人もやあらむ。")))
  val b1json = JObject(Map("x" -> JInt(1), "y" -> a1json, "z" -> a1json, "loc" -> JArray(Seq(JNumber(0.002), JNumber(0)))))
  val b2json = JObject(Map("x" -> JInt(1), "y" -> a2json, "loc" -> JArray(Seq(JNumber(-0.002), JNumber(0)))))
  val b3json = JObject(Map("x" -> JInt(2), "y" -> a1json, "z" -> a2json, "loc" -> JArray(Seq(JNumber(-0.004), JNumber(0)))))
  implicit class FutureW[A](self: Future[A]) {
    def await(timeOut: Span = 1.minute): A = {
      Await.ready(self, timeOut)
      self.eitherValue match {
        case Some(Left(e)) => throw e
        case Some(Right(x)) => x
        case None => throw new TimeoutException
      }
    }
  }
  implicit class ThrowableEitherW[A](self: Throwable \/ A) {
    def get = self.leftMap(throw _).getOrElse(fail("[BUG] This should not be reached!"))
  }
  def withConnection[A](f: Connection => A): A = {
    using(Executor()) {
      executor =>
        val con = Connection(executor = executor)
        f(con)
    }
  }
  def withCollection[A](f: (Connection, Collection) => A): A =
    withConnection {
      con =>
        val testcolName = s"test${_sequence.incrementAndGet()}"
        val col = con._system._createCollection(CollectionCreationOptions(name = testcolName)).await()
        try {
          f(con, col)
        } finally {
          col.delete.await()
        }
    }
  def withPreparedCollection[A](f: (Connection, Collection, (String, String, String, String, String)) => A): A =
    withCollection {
      (con, col) =>
        val (a1id, a2id, b1id, b2id, b3id) = (for {
          Document(a1id, _, _) <- col.document.save(doc = a1, createCollection = true, waitForSync = true)
          Document(a2id, _, _) <- col.document.save(doc = a2, waitForSync = true)
          Document(b1id, _, _) <- col.document.save(doc = b1, waitForSync = true)
          Document(b2id, _, _) <- col.document.save(doc = b2, waitForSync = true)
          Document(b3id, _, _) <- col.document.save(doc = b3, waitForSync = true)
        } yield (a1id, a2id, b1id, b2id, b3id)).await()
        f(con, col, (a1id, a2id, b1id, b2id, b3id))
    }
}
