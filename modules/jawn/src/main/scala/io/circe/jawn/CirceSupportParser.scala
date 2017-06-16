package io.circe.jawn

import io.circe.{ Json, JsonBigDecimal, JsonBiggerDecimal, JsonObject }
import io.circe.numbers.{ BiggerDecimal, JsonNumberParser }
import java.math.{ BigDecimal, BigInteger }
import jawn.{ Facade, FContext, SupportParser }

final object CirceSupportParser extends SupportParser[Json] {
  private[this] val jsonNumberParser: JsonNumberParser[Json] = new JsonNumberParser[Json] {
    def createNegativeZero: Json = Json.JNumber(JsonBiggerDecimal(BiggerDecimal.fromDoubleUnsafe(-0.0)))
    def createUnsignedZero: Json = Json.fromLong(0)
    def createLong(value: Long): Json = Json.fromLong(value)
    def createBigDecimal(unscaled: BigInteger, scale: Int): Json =
      Json.JNumber(JsonBigDecimal(new BigDecimal(unscaled, scale)))
    def createBiggerDecimal(unscaled: BigInteger, scale: BigInteger): Json =
      Json.JNumber(JsonBiggerDecimal(BiggerDecimal(unscaled, scale)))
    def fail: Json = null
  }

  implicit final val facade: Facade[Json] = new Facade[Json] {
    final def jnull(): Json = Json.Null
    final def jfalse(): Json = Json.False
    final def jtrue(): Json = Json.True
    final def jnum(s: String): Json = jsonNumberParser.parseUnsafe(s)
    final def jint(s: String): Json = jsonNumberParser.parseUnsafeWithIndices(s, -1, -1)
    final def jstring(s: String): Json = Json.fromString(s)

    final def singleContext(): FContext[Json] = new FContext[Json] {
      private[this] final var value: Json = null
      final def add(s: String): Unit = { value = jstring(s) }
      final def add(v: Json): Unit =  { value = v }
      final def finish: Json = value
      final def isObj: Boolean = false
    }

    final def arrayContext(): FContext[Json] = new FContext[Json] {
      private[this] final val vs = Vector.newBuilder[Json]
      final def add(s: String): Unit = { vs += jstring(s) }
      final def add(v: Json): Unit = { vs += v }
      final def finish: Json = Json.fromValues(vs.result())
      final def isObj: Boolean = false
    }

    def objectContext(): FContext[Json] = new FContext[Json] {
      private[this] final var key: String = null
      private[this] final val m = scala.collection.mutable.Map.empty[String, Json]
      private[this] final val keys = Vector.newBuilder[String]

      final def add(s: String): Unit =
        if (key == null) { key = s } else {
          if (!m.contains(key)) keys += key
          m(key) = jstring(s)
          key = null
        }
      final def add(v: Json): Unit = {
        if (!m.contains(key)) keys += key
        m(key) = v
        key = null
      }
      final def finish: Json = Json.fromJsonObject(JsonObject.fromMapAndVector(m.toMap, keys.result()))
      final def isObj: Boolean = true
    }
  }
}
