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
import logging._
import logging.Defaults._
import internal._

class LoggingSpec extends FunSpec with ScalaFutures with DiagrammedAssertions {
  describe("Logger") {
    it("outputs logs") {
      // TODO: implement a mock LoggerImpl and test with it
      Logger.error("test")
      Logger.errorThrowable("test", new RuntimeException("This is a test exception!"))
      Logger.warn("test")
      Logger.warnThrowable("test", new RuntimeException("This is a test exception!"))
      Logger.info("test")
      Logger.infoThrowable("test", new RuntimeException("This is a test exception!"))
      Logger.debug("test")
      Logger.debugThrowable("test", new RuntimeException("This is a test exception!"))
      Logger.trace("test")
      Logger.traceThrowable("test", new RuntimeException("This is a test exception!"))
    }
  }
}
