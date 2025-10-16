package TydiPackaging

import TydiPackaging.binaryMethods._

import language.experimental.macros
import magnolia1._

trait FromTydiBinary[T] {
  def fromBinary(t: TydiBinary): (T, TydiBinary)
}

object FromTydiBinary {

  implicit def optionFromTydiBinary[A](implicit A: FromTydiBinary[A]): FromTydiBinary[Option[A]] = (t: TydiBinary) => {
    val (boolVal, res) = t.splitLow(1)
    val isSome = boolVal.data == 1
    if (isSome) {
      val v = (A.fromBinary(res))
      (Some(v._1), v._2)
    } else {
      // Fixme: it's better to split the binary than to try to parse empty data, but we don't know the size of the data.
      val v = (A.fromBinary(res))
      (None, v._2)
    }
  }

  implicit val boolFromTydiBinary: FromTydiBinary[Boolean] = (t: TydiBinary) => {
    val (boolVal, res) = t.splitLow(1)
    (if (boolVal.data == 1) true else false, res)
  }

  implicit val intFromTydiBinary: FromTydiBinary[Int] = (t: TydiBinary) => {
    val (intVal, res) = t.splitLow(32)
    (intVal.data.toInt, res)
  }

  implicit val longFromTydiBinary: FromTydiBinary[Long] = (t: TydiBinary) => {
    val (longVal, res) = t.splitLow(64)
    (longVal.data.toLong, res)
  }

  implicit val doubleFromTydiBinary: FromTydiBinary[Double] = (t: TydiBinary) => {
    val (doubleVal, res) = t.splitLow(64)
    (java.lang.Double.longBitsToDouble(doubleVal.data.toLong), res)
  }

  implicit val floatFromTydiBinary: FromTydiBinary[Float] = (t: TydiBinary) => {
    val (floatVal, res) = t.splitLow(32)
    (java.lang.Float.intBitsToFloat(floatVal.data.toInt), res)
  }

  implicit val charFromTydiBinary: FromTydiBinary[Char] = (t: TydiBinary) => {
    val (charVal, res) = t.splitLow(8)
    (charVal.data.toChar, res)
  }

  // String: becomes a separate stream, so the binary is empty.
  implicit val stringFromTydiBinary: FromTydiBinary[String] = (t: TydiBinary) => {
    ("", t)
  }

  // List: becomes a separate stream, so the binary is empty.
  implicit def listFromTydiBinary[A](implicit A: FromTydiBinary[A]): FromTydiBinary[List[A]] = (t: TydiBinary) => {
    (List[A](), t)
  }

  // There is no default implementation for other types, so a compiler error will be thrown when no matching FromTydiBinary implementation is found.
  // implicit def defaultFromTydiBinary[T]: FromTydiBinary[T] = new FromTydiBinary[T] {}

  // Magnolia's boilerplate for deriving for a case class (Product)
  type Typeclass[T] = FromTydiBinary[T]

  /** Create typeclasses for sealed-traits/enums ('sum types')
   *
   * Frankly, I am not sure how this works based on the limited information of the GitHub repository.
   */
  def split[T](ctx: SealedTrait[FromTydiBinary, T]): FromTydiBinary[T] = new FromTydiBinary[T] {
    override def fromBinary(t: TydiBinary): (T, TydiBinary) = ???
  }

  def join[T](ctx: magnolia1.CaseClass[FromTydiBinary, T]): FromTydiBinary[T] = (t: TydiBinary) => {
    // 1. Define the accumulator and the initial state.
    // The accumulator will hold a list of all deserialized field values (of type Any),
    // and the TydiBinary remaining after the field has been parsed.
    type Accumulator = (List[Any], TydiBinary)

    val initialAccumulator: Accumulator = (Nil, t)

    // 2. Iterate over the parameters (fields) in their declaration order (not reversed like serialization).
    // We use foldLeft to manage the shrinking TydiBinary.
    val (fieldValuesReversed, finalRemainingBlob) =
      ctx.parameters.toList.foldLeft(initialAccumulator) {
        case ((currentValues, currentBlob), param) =>

          // param.typeclass is the implicit MyDeserializer[FieldType]
          val deserializer: FromTydiBinary[param.PType] = param.typeclass

          // 3. Deserialize the current field from the current blob.
          // This returns the deserialized field value (param.PType) and the remaining blob.
          val (fieldValue, remainingBlob) = deserializer.fromBinary(currentBlob)

          // 4. Accumulate the result (prepending to the list for efficiency).
          (fieldValue :: currentValues, remainingBlob)
      }

    // 5. The fold operation collected values in reverse order of iteration (due to ::).
    // We reverse them back to the original field order.
    val fieldValues = fieldValuesReversed.reverse

    // 6. Use ctx.construct to create the final case class instance T
    // from the sequence of field values.
    val it = fieldValues.iterator
    val instance: T = ctx.construct {
      (p) => {
        val v = it.next()
        v
      }
    }

    // 7. Return the constructed instance and the final remaining blob.
    (instance, finalRemainingBlob)
  }

  // This line is where the compiler hooks into the macro
  implicit def gen[T]: FromTydiBinary[T] = macro Magnolia.gen[T]
}

