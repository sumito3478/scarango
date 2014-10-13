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
import json._
import internal._

trait Disposable {
  def disposeInternal(): Unit

  private[this] val disposed = new AtomicBoolean(false)

  def dispose(): Unit = if (!disposed.compareAndSet(false, true)) disposeInternal()

  override def finalize() = if (!disposed.get) {
    import logging.Defaults._ // I think we can't get an instance of LoggerImpl from finalizer, use default one instead.
    Logger.warn(s"$this - calling dispose from finalizer!")
    dispose()
  }
}
object Disposable {
  def using[A <: Disposable, B](disposable: => A)(f: A => B): B =
    try f(disposable) finally disposable.dispose()
}

case class ArangoErrorResponse(error: Boolean, code: Int, errorNum: Int, errorMessage: String)

case class ArangoException(error: ArangoErrorResponse) extends Exception(error.errorMessage)

case class ArangoDriverException(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull)
