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

//import java.util.concurrent.atomic._
import org.scalatest._
import org.scalatest.time._
import org.scalatest.concurrent._
//import scala.concurrent._
//import scala.concurrent.duration._
//import http._, db._, connection._, collection._
//import json._, Json._
//import Disposable._
import scala.collection._
import logging._
import internal._

class LoggingSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  describe("Logger") {
    it("outputs logs") {
      sealed trait MockLoggerImpl
      implicit object MockLoggerImpl extends LoggerImpl[MockLoggerImpl] {
        val traces = new mutable.ListBuffer[(String, String, Option[Throwable])]
        def isTraceEnabled = true
        def trace(pos: String, msg: String) = traces += ((pos, msg, None))
        def traceThrowable(pos: String, msg: String, e: Throwable) = traces += ((pos, msg, Some(e)))
        val debugs = new mutable.ListBuffer[(String, String, Option[Throwable])]
        def isDebugEnabled = true
        def debug(pos: String, msg: String) = debugs += ((pos, msg, None))
        def debugThrowable(pos: String, msg: String, e: Throwable) = debugs += ((pos, msg, Some(e)))
        val infos = new mutable.ListBuffer[(String, String, Option[Throwable])]
        def isInfoEnabled = true
        def info(pos: String, msg: String) = infos += ((pos, msg, None))
        def infoThrowable(pos: String, msg: String, e: Throwable) = infos += ((pos, msg, Some(e)))
        val warns = new mutable.ListBuffer[(String, String, Option[Throwable])]
        def isWarnEnabled = true
        def warn(pos: String, msg: String) = warns += ((pos, msg, None))
        def warnThrowable(pos: String, msg: String, e: Throwable) = warns += ((pos, msg, Some(e)))
        val errors = new mutable.ListBuffer[(String, String, Option[Throwable])]
        def isErrorEnabled = true
        def error(pos: String, msg: String) = errors += ((pos, msg, None))
        def errorThrowable(pos: String, msg: String, e: Throwable) = errors += ((pos, msg, Some(e)))
      }
      val e = new RuntimeException("this is a test exception!")
      Logger.error("test")
      Logger.errorThrowable("test", e)
      Logger.warn("test")
      Logger.warnThrowable("test", e)
      Logger.info("test")
      Logger.infoThrowable("test", e)
      Logger.debug("test")
      Logger.debugThrowable("test", e)
      Logger.trace("test")
      Logger.traceThrowable("test", e)
      def check(x: List[(String, String, Option[Throwable])]) = {
        assert(MockLoggerImpl.errors.toList.map(_._1).forall(_.startsWith("src/test/scala/LoggingSpec.scala")))
        assert(MockLoggerImpl.errors.toList.map(_._2) == List("test", "test"))
        assert(MockLoggerImpl.errors.toList.map(_._3) == List(None, Some(e)))
      }
      check(MockLoggerImpl.errors.toList)
      check(MockLoggerImpl.warns.toList)
      check(MockLoggerImpl.infos.toList)
      check(MockLoggerImpl.debugs.toList)
      check(MockLoggerImpl.traces.toList)
    }
  }
}
