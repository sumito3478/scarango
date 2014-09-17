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
package internal

import scala.collection._
import scalaz._, Scalaz._
import shapeless._
import json._, Json._

object LabelledProductTypeClassMarshall extends LabelledProductTypeClass[Marshall] {
  def emptyProduct = new Marshall[HNil] {
    def marshall(x: HNil): JObject = JObject(Map())
  }
  def product[F, T <: HList](name: String, FHead: Marshall[F], FTail: Marshall[T]) = new Marshall[F :: T] {
    def marshall(x: F :: T): Json = {
      FTail.marshall(x.tail) match {
        case JObject(t) =>
          JObject((t + (name -> FHead.marshall(x.head))).filterNot(_._2 == JNothing))
        case t =>
          sys.error(s"[BUG] this should not be reached - $t")
      }
    }
  }
  def project[F, G](instance: => Marshall[G], to: F => G, from: G => F) = new Marshall[F] {
    def marshall(x: F): Json = instance.marshall(to(x))
  }
}
