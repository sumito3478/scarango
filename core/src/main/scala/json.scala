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

import scala.language.experimental.macros

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

abstract class Marshall[-A] {
  def marshall(x: A): Json
}
object Marshall {
  def marshall[A: Marshall](x: A): Json = implicitly[Marshall[A]].marshall(x)
  implicit object MarshallJson extends Marshall[Json] {
    override def marshall(x: Json) = x
  }
  implicit object MarshallUnit extends Marshall[Unit] {
    override def marshall(x: Unit) = JNothing
  }
  implicit object MarshallInt extends Marshall[Int] {
    override def marshall(x: Int) = JInt(x)
  }
  implicit object MarshallDouble extends Marshall[Double] {
    override def marshall(x: Double) = JNumber(x)
  }
  implicit object MarshallBoolean extends Marshall[Boolean] {
    override def marshall(x: Boolean) = JBoolean(x)
  }
  implicit object MarshallString extends Marshall[String] {
    override def marshall(x: String) = JString(x)
  }
  implicit def MarshallOption[A: Marshall] = new Marshall[Option[A]] {
    override def marshall(x: Option[A]) = x match {
      case Some(x) => Marshall.marshall(x)
      case None => JNothing
    }
  }
  implicit def MarshallSeq[A: Marshall] = new Marshall[Seq[A]] {
    override def marshall(x: Seq[A]): Json = JArray(x.map(Marshall.marshall(_)).filterNot(_ == JNothing))
  }
  implicit def MarshallMap[A: Marshall] = new Marshall[Map[String, A]] {
    override def marshall(x: Map[String, A]): Json = JObject(x.mapValues(Marshall.marshall(_)).filterNot(_._2 == JNothing))
  }
  implicit val LabelledProductTypeClassMarshall = internal.LabelledProductTypeClassMarshall

  implicit def MarshallLabelledGeneric[A](implicit ev: LabelledProductTypeClass[Marshall]): Marshall[A] = macro GenericMacros.deriveLabelledProductInstance[Marshall, A]
}

// unmarshall from JSON AST to A
abstract class UnMarshall[A] {
  def unmarshall(x: Json): ValidationNel[String, A]
  def typename: String
  protected[this] def failure(x: Json) = s"Cannot unmarshall $x to $typename".failureNel
}
case class UnMarshallException(messages: NonEmptyList[String]) extends Exception(messages.toList.mkString("\n"))
object UnMarshall {
  def unmarshallRaw[A: UnMarshall](x: Json): ValidationNel[String, A] = implicitly[UnMarshall[A]].unmarshall(x)
  def unmarshall[A: UnMarshall](x: Json): UnMarshallException \/ A = unmarshallRaw[A](x).disjunction.leftMap(UnMarshallException(_))
  def unmarshallUnsafe[A: UnMarshall](x: Json): A = unmarshall[A](x) match {
    case -\/(e) => throw e
    case \/-(x) => x
  }
  implicit object UnMarshallJson extends UnMarshall[Json] {
    def typename = "Json"
    def unmarshall(x: Json): ValidationNel[String, Json] = x.successNel
  }
  implicit object UnMarshallUnit extends UnMarshall[Unit] {
    def typename = "Unit"
    def unmarshall(x: Json): ValidationNel[String, Unit] = ().successNel
  }
  implicit object UnMarshallInt extends UnMarshall[Int] {
    def typename = "Int"
    def unmarshall(x: Json): ValidationNel[String, Int] = x match {
      case JInt(x) => x.successNel
      case JNumber(x) if x.toInt.toDouble == x => x.toInt.successNel
      case x => failure(x)
    }
  }
  implicit object UnMarshallDouble extends UnMarshall[Double] {
    def typename = "Double"
    def unmarshall(x: Json): ValidationNel[String, Double] = x match {
      case JInt(x) => x.toDouble.successNel
      case JNumber(x) => x.successNel
      case x => failure(x)
    }
  }
  implicit object UnMarshallBoolean extends UnMarshall[Boolean] {
    def typename = "Boolean"
    def unmarshall(x: Json): ValidationNel[String, Boolean] = x match {
      case JBoolean(x) => x.successNel
      case x => failure(x)
    }
  }
  implicit object UnMarshallString extends UnMarshall[String] {
    def typename = "String"
    def unmarshall(x: Json): ValidationNel[String, String] = x match {
      case JString(x) => x.successNel
      case x => failure(x)
    }
  }
  implicit def UnMarshallOption[A: UnMarshall] = new UnMarshall[Option[A]] {
    def typename = s"Option[${implicitly[UnMarshall[A]].typename}]"
    def unmarshall(x: Json): ValidationNel[String, Option[A]] = x match {
      case JNothing | JNull => None.successNel
      case x => UnMarshall.unmarshallRaw[A](x).map(Some(_))
    }
  }
  // I don't know the canonical way to concatenate Seq[ValidationNel[String, A]] to ValidationNel[String, Seq[A]], so I do that by myself...
  private[this] def concatenateValidationNel[A](x: TraversableOnce[ValidationNel[String, A]]): ValidationNel[String, List[A]] =
    x.foldLeft(DList[A]().successNel[String]) {
      (acc, x) =>
        acc |+| x.map(DList(_))
    }.map(_.toList)
  implicit def UnMarshallList[A: UnMarshall] = new UnMarshall[List[A]] {
    def typename = s"List[${implicitly[UnMarshall[A]].typename}]"
    def unmarshall(x: Json): ValidationNel[String, List[A]] = x match {
      case JArray(x) => concatenateValidationNel(x.map(UnMarshall.unmarshallRaw[A](_)))
      case x => failure(x)
    }
  }
  implicit def UnMarshallMap[A: UnMarshall] = new UnMarshall[Map[String, A]] {
    def typename = s"Map[String, ${implicitly[UnMarshall[A]].typename}]"
    def unmarshall(x: Json): ValidationNel[String, Map[String, A]] = x match {
      case JObject(x) =>
        for (values <- concatenateValidationNel(x.values.map(UnMarshall.unmarshallRaw[A](_))))
          yield Map((x.keys, values).zipped.toSeq: _*)
      case _ => failure(x)
    }
  }
  implicit val LabelledProductTypeClassUnMarshall = internal.LabelledProductTypeClassUnMarshall
  implicit def UnMarshallLabelledGeneric[A](implicit ev: LabelledProductTypeClass[UnMarshall]): UnMarshall[A] = macro GenericMacros.deriveLabelledProductInstance[UnMarshall, A]
}
