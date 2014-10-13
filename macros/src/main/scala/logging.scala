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
package logging

import scala.language.experimental.macros

trait LoggerImpl[Tag] {
  def isTraceEnabled: Boolean
  def trace(pos: String, msg: String): Unit
  def traceThrowable(pos: String, msg: String, e: Throwable): Unit
  def isDebugEnabled: Boolean
  def debug(pos: String, msg: String): Unit
  def debugThrowable(pos: String, msg: String, e: Throwable): Unit
  def isInfoEnabled: Boolean
  def info(pos: String, msg: String): Unit
  def infoThrowable(pos: String, msg: String, e: Throwable): Unit
  def isWarnEnabled: Boolean
  def warn(pos: String, msg: String): Unit
  def warnThrowable(pos: String, msg: String, e: Throwable): Unit
  def isErrorEnabled: Boolean
  def error(pos: String, msg: String): Unit
  def errorThrowable(pos: String, msg: String, e: Throwable): Unit
}

sealed trait Slf4jLogger

object Slf4jLogger extends LoggerImpl[Slf4jLogger] {
  import org.slf4j._
  private[this] val underlying = LoggerFactory.getLogger("scarango")
  def isTraceEnabled = underlying.isTraceEnabled
  def trace(pos: String, msg: String) = {
    MDC.put("position", pos)
    underlying.trace(msg)
    MDC.remove("position")
  }
  def traceThrowable(pos: String, msg: String, e: Throwable) = {
    MDC.put("position", pos)
    underlying.trace(msg, e)
    MDC.remove("position")
  }
  def isDebugEnabled = underlying.isDebugEnabled
  def debug(pos: String, msg: String) = {
    MDC.put("position", pos)
    underlying.debug(msg)
    MDC.remove("position")
  }
  def debugThrowable(pos: String, msg: String, e: Throwable) = {
    MDC.put("position", pos)
    underlying.debug(msg, e)
    MDC.remove("position")
  }
  def isInfoEnabled = underlying.isInfoEnabled
  def info(pos: String, msg: String) = {
    MDC.put("position", pos)
    underlying.info(msg)
    MDC.remove("position")
  }
  def infoThrowable(pos: String, msg: String, e: Throwable) = {
    MDC.put("position", pos)
    underlying.info(msg, e)
    MDC.remove("position")
  }
  def isWarnEnabled = underlying.isWarnEnabled
  def warn(pos: String, msg: String) = {
    MDC.put("position", pos)
    underlying.warn(msg)
    MDC.remove("position")
  }
  def warnThrowable(pos: String, msg: String, e: Throwable) = {
    MDC.put("position", pos)
    underlying.warn(msg, e)
    MDC.remove("position")
  }
  def isErrorEnabled = underlying.isErrorEnabled
  def error(pos: String, msg: String) = {
    MDC.put("position", pos)
    underlying.error(msg)
    MDC.remove("position")
  }
  def errorThrowable(pos: String, msg: String, e: Throwable) = {
    MDC.put("position", pos)
    underlying.error(msg, e)
    MDC.remove("position")
  }
}

object Defaults {
  implicit def defaultLoggerImpl: LoggerImpl[Slf4jLogger] = Slf4jLogger
}
