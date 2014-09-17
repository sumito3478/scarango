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
package json

import scala.collection._
import scalaz._, Scalaz._
import shapeless._

// JSON AST for ArangoDB
sealed abstract class Json
object Json {
  case class JInt(value: Int) extends Json
  case class JNumber(value: Double) extends Json
  case class JBoolean(value: Boolean) extends Json
  case class JString(value: String) extends Json
  case class JArray(value: Seq[Json]) extends Json
  case class JObject(value: Map[String, Json] /* does field order matter in ArangoDB...? */ ) extends Json
  case object JNull extends Json
  case object JNothing extends Json

  def read(x: String): Throwable \/ Json = internal.JacksonBackend.read(x)

  def write(x: Json): String = internal.JacksonBackend.write(x)

  def prettyPrint(x: Json): String = internal.JacksonBackend.prettyPrint(x)
}
import Json._

// Converter between case class and JSON AST
abstract class Converter[A] {
  def typename: String
  protected[this] def failure(x: Json) = s"Cannot unmarshall $x to $typename".failureNel
  def marshall(x: A): Json
  def unmarshall(x: Json): ValidationNel[String, A]
}
object Converter extends LabelledProductTypeClassCompanion[Converter] {
  implicit object JsonConverter extends Converter[Json] {
    def typename = "Json"
    def marshall(x: Json): Json = x
    def unmarshall(x: Json): ValidationNel[String, Json] = x.successNel
  }
  implicit object IntConverter extends Converter[Int] {
    def typename = "Int"
    def marshall(x: Int): Json = JInt(x)
    def unmarshall(x: Json): ValidationNel[String, Int] = x match {
      case JInt(x) => x.successNel
      case JNumber(x) if x.toInt.toDouble == x => x.toInt.successNel
      case x => failure(x)
    }
  }
  implicit object DoubleConverter extends Converter[Double] {
    def typename = "Double"
    def marshall(x: Double): Json = JNumber(x)
    def unmarshall(x: Json): ValidationNel[String, Double] = x match {
      case JInt(x) => x.toDouble.successNel
      case JNumber(x) => x.successNel
      case x => failure(x)
    }
  }
  implicit object BooleanConverter extends Converter[Boolean] {
    def typename = "Boolean"
    def marshall(x: Boolean): Json = JBoolean(x)
    def unmarshall(x: Json): ValidationNel[String, Boolean] = x match {
      case JBoolean(x) => x.successNel
      case x => failure(x)
    }
  }
  implicit object StringConverter extends Converter[String] {
    def typename = "String"
    def marshall(x: String): Json = JString(x)
    def unmarshall(x: Json): ValidationNel[String, String] = x match {
      case JString(x) => x.successNel
      case x => failure(x)
    }
  }
  implicit def OptionConverter[A: Converter] = new Converter[Option[A]] {
    def typename = s"Option[${implicitly[Converter[A]].typename}]"
    def marshall(x: Option[A]): Json = x match {
      case Some(x) => Converter.marshall(x)
      case None => JNothing
    }
    def unmarshall(x: Json): ValidationNel[String, Option[A]] = x match {
      case JNothing | JNull => None.successNel
      case x => Converter.unmarshallRaw[A](x).map(Some(_))
    }
  }
  // I don't know the canonical way to concatenate Seq[ValidationNel[String, A]] to ValidationNel[String, Seq[A]], so I do that by myself...
  private[this] def concatenateValidationNel[A](x: TraversableOnce[ValidationNel[String, A]]): ValidationNel[String, List[A]] =
    x.foldLeft(DList[A]().successNel[String]) {
      (acc, x) =>
        acc |+| x.map(DList(_))
    }.map(_.toList)
  implicit def ListConverter[A: Converter] = new Converter[List[A]] {
    def typename = s"List[${implicitly[Converter[A]].typename}]"
    def marshall(x: List[A]): Json = JArray(x.map(Converter.marshall(_)).filterNot(_ == JNothing))
    def unmarshall(x: Json): ValidationNel[String, List[A]] = x match {
      case JArray(x) => concatenateValidationNel(x.map(Converter.unmarshallRaw[A](_)))
      case x => failure(x)
    }
  }
  implicit def MapConverter[A: Converter] = new Converter[Map[String, A]] {
    def typename = s"Map[String, ${implicitly[Converter[A]].typename}]"
    def marshall(x: Map[String, A]): Json = JObject(x.mapValues(Converter.marshall(_)).filterNot(_._2 == JNothing))
    def unmarshall(x: Json): ValidationNel[String, Map[String, A]] = x match {
      case JObject(x) =>
        for (values <- concatenateValidationNel(x.values.map(Converter.unmarshallRaw[A](_))))
          yield Map((x.keys, values).zipped.toSeq: _*)
      case _ => failure(x)
    }
  }
  implicit def LabelledProductConverter: LabelledProductTypeClass[Converter] = new LabelledProductTypeClass[Converter] {
    def emptyProduct = new Converter[HNil] {
      def typename = "HNil"
      def marshall(x: HNil): JObject = JObject(Map())
      def unmarshall(x: Json): ValidationNel[String, HNil] = HNil.successNel
    }
    def product[F, T <: HList](name: String, FHead: Converter[F], FTail: Converter[T]) = new Converter[F :: T] {
      def typename = "case class" // how can I get typename without writing macros by myself?
      def marshall(x: F :: T): Json = {
        FTail.marshall(x.tail) match {
          case JObject(t) =>
            JObject((t + (name -> FHead.marshall(x.head))).filterNot(_._2 == JNothing))
          case t =>
            sys.error(s"[BUG] this should not be reached - $t")
        }
      }
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
    def project[F, G](instance: => Converter[G], to: F => G, from: G => F) = new Converter[F] {
      def typename = "case class" // how can I get typename without writing macros by myself?
      def marshall(x: F): Json = instance.marshall(to(x))
      def unmarshall(x: Json): ValidationNel[String, F] = instance.unmarshall(x).map(from(_))
    }
  }
  case class ConversionException(messages: NonEmptyList[String]) extends Exception(messages.toList.mkString("\n"))
  def marshall[A: Converter](x: A): Json = implicitly[Converter[A]].marshall(x)
  def unmarshallRaw[A: Converter](x: Json): ValidationNel[String, A] = implicitly[Converter[A]].unmarshall(x)
  def unmarshall[A: Converter](x: Json): ConversionException \/ A = unmarshallRaw[A](x).disjunction.leftMap(ConversionException(_))
  def unmarshallUnsafe[A: Converter](x: Json): A = unmarshall[A](x) match {
    case -\/(e) => throw e
    case \/-(x) => x
  }
}

