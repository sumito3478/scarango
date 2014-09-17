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

import scalaz._, Scalaz._
import shapeless._
import json._, Json._

object LabelledProductTypeClassUnMarshall extends LabelledProductTypeClass[UnMarshall] {
  def emptyProduct = new UnMarshall[HNil] {
    def typename = "HNil"
    def unmarshall(x: Json): ValidationNel[String, HNil] = HNil.successNel
  }
  def product[F, T <: HList](name: String, FHead: UnMarshall[F], FTail: UnMarshall[T]) = new UnMarshall[F :: T] {
    def typename = "case class" // how can I get typename without writing macros by myself?
    def unmarshall(x: Json): ValidationNel[String, F :: T] = {
      x match {
        case JObject(xs) =>
          xs.get(name) match {
            case Some(field) => FHead.unmarshall(field) match {
              case Success(h) => FTail.unmarshall(x) match {
                case Success(t) => (h :: t).successNel
                case Failure(te) => te.failure
              }
              case Failure(he) => FTail.unmarshall(x) match {
                case Success(t) => he.failure
                case Failure(te) => (he append te).failure
              }
            }
            case None =>
              // try to unmarshall with JNothing
              FHead.unmarshall(JNothing) match {
                case Success(h) => FTail.unmarshall(x) match {
                  case Success(t) => (h :: t).successNel
                  case Failure(te) => te.failure
                }
                case Failure(_) =>
                  // Could not convert JNothing to F - this means $x must have $name field
                  val he = NonEmptyList(s"Cannot find field $name in $x")
                  FTail.unmarshall(x) match {
                    case Success(t) => he.failure
                    case Failure(te) => (he append te).failure
                  }
              }
          }
        case _ => failure(x)
      }
    }
  }
  def project[F, G](instance: => UnMarshall[G], to: F => G, from: G => F) = new UnMarshall[F] {
    def typename = "case class" // how can I get typename without writing macros by myself?
    def unmarshall(x: Json): ValidationNel[String, F] = instance.unmarshall(x).map(from(_))
  }
}

