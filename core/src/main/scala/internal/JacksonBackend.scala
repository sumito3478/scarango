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

import scalaz._
import json._, Json._

private[scarango] object JacksonBackend {
  import com.fasterxml.jackson
  import jackson.core._
  import jackson.databind._
  import jackson.databind.jsontype._
  import jackson.databind.ser.std._
  import jackson.databind.deser.std._
  import jackson.databind.`type`._
  import jackson.databind.module._
  private[this] object JsonSerializer extends StdSerializer[Json](classOf[Json]) {
    override def serialize(value: Json, gen: JsonGenerator, provider: SerializerProvider): Unit = {
      value match {
        case JBoolean(x) => gen.writeBoolean(x)
        case JInt(x) => gen.writeNumber(x)
        case JNumber(x) => gen.writeNumber(x)
        case JString(x) => gen.writeString(x)
        case JArray(xs) =>
          gen.writeStartArray
          for (x <- xs) serialize(x, gen, provider)
          gen.writeEndArray
        case JObject(xs) =>
          gen.writeStartObject
          for ((k, v) <- xs) {
            gen.writeFieldName(k)
            serialize(v, gen, provider)
          }
          gen.writeEndObject
        case JNull => gen.writeNull
        case JNothing =>
      }
    }
    // is this necessary?
    override def serializeWithType(value: Json, gen: JsonGenerator, provider: SerializerProvider, typeSer: TypeSerializer): Unit = {
      typeSer.writeTypePrefixForScalar(value, gen)
      serialize(value, gen, provider)
      typeSer.writeTypeSuffixForScalar(value, gen)
    }
  }
  private[this] object JsonDeserializer extends StdDeserializer[Json](classOf[Json]) {
    override def deserialize(p: JsonParser, ctx: DeserializationContext): Json = {
      import JsonToken._
      p.getCurrentToken match {
        case START_OBJECT =>
          def loop(xs: List[(String, Json)]): JObject =
            p.nextToken match {
              case END_OBJECT => JObject(xs.reverse.toMap)
              case _ =>
                val name = p.getCurrentName
                p.nextToken
                val value = deserialize(p, ctx)
                loop((name, value) :: xs)
            }
          loop(List())
        case START_ARRAY =>
          def loop(xs: List[Json]): JArray =
            p.nextToken match {
              case END_ARRAY => JArray(xs.reverse)
              case _ => loop(deserialize(p, ctx) :: xs)
            }
          loop(List())
        case VALUE_EMBEDDED_OBJECT => throw ctx.mappingException(classOf[Json])
        case VALUE_FALSE => JBoolean(false)
        case VALUE_TRUE => JBoolean(true)
        case VALUE_NULL => JNull
        case VALUE_NUMBER_FLOAT => JNumber(p.getDoubleValue)
        case VALUE_NUMBER_INT =>
          val x = p.getLongValue
          if (x.toInt == x) JInt(x.toInt) else JNumber(x.toDouble)
        case VALUE_STRING => JString(p.getText)
        case _ => throw ctx.mappingException(classOf[Json])
      }
    }
    override def deserializeWithType(p: JsonParser, ctx: DeserializationContext, typeDeser: TypeDeserializer) = typeDeser.deserializeTypedFromScalar(p, ctx)
  }
  private[this] object Module extends SimpleModule(Version.unknownVersion) {
    addSerializer(JsonSerializer)
    setDeserializers(new SimpleDeserializers {
      override def findBeanDeserializer(ty: JavaType, config: DeserializationConfig, beanDesc: BeanDescription): JsonDeserializer[_] =
        if (classOf[Json].isAssignableFrom(ty.getRawClass)) JsonDeserializer else null
      override def findCollectionDeserializer(ty: CollectionType, config: DeserializationConfig, beanDesc: BeanDescription, elementTypeDeserializer: TypeDeserializer, elementDeserializer: JsonDeserializer[_]): JsonDeserializer[_] =
        if (classOf[JArray].isAssignableFrom(ty.getRawClass)) JsonDeserializer else null
      override def findMapDeserializer(ty: MapType, config: DeserializationConfig, beanDesc: BeanDescription, keyDeserializer: KeyDeserializer, elementTypeDeserializer: TypeDeserializer,
        elementDeserializer: JsonDeserializer[_]): JsonDeserializer[_] =
        if (classOf[JObject].isAssignableFrom(ty.getRawClass)) JsonDeserializer else null
    })
  }
  private[this] val mapper = new ObjectMapper
  mapper.registerModule(Module)

  private[this] val prettyPrintingMapper = mapper.copy.enable(SerializationFeature.INDENT_OUTPUT)

  def read(x: String): Throwable \/ Json = \/.fromTryCatchNonFatal(mapper.readValue[Json](x, classOf[Json]))

  def write(x: Json): String = mapper.writeValueAsString(x)

  def prettyPrint(x: Json): String = prettyPrintingMapper.writeValueAsString(x)
}
