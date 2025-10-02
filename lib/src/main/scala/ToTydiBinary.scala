package TydiPackaging

//import TydiPackaging.binaryMethods._

import language.experimental.macros
import magnolia1._

import java.time.Instant

trait ToTydiBinary[T] {
  val binSize: Int

  def toBinary(t: T): TydiBinary
}

object ToTydiBinary {

  // 1. Int: Create binary from Int.
  implicit val intToTydiBinary: ToTydiBinary[Int] = new ToTydiBinary[Int] {
    def toBinary(i: Int): TydiBinary = TydiBinary(BigInt(i), 32)
    val binSize = 32
  }

  implicit val instantToBinary: ToTydiBinary[Instant] = new ToTydiBinary[Instant] {
    def toBinary(i: Instant): TydiBinary = {
      TydiBinary(BigInt(i.toEpochMilli), binSize)
    }
    val binSize = 64
  }

  // 2. Double/Float: Converts to Int (potentially losing precision) and adds it.
  implicit val doubleToTydiBinary: ToTydiBinary[Double] = new ToTydiBinary[Double] {
    def toBinary(d: Double): TydiBinary = TydiBinary(BigInt(d.toInt), 32)
    val binSize = 32
  }

  // 3. String: becomes a separate stream, so the binary is empty.
  implicit val stringToTydiBinary: ToTydiBinary[String] = new ToTydiBinary[String] {
    def toBinary(s: String): TydiBinary =  TydiBinary.empty
    val binSize = 0
  }

  // 4. List: becomes a separate stream, so the binary is empty.
  implicit def listToTydiBinary[A](implicit A: ToTydiBinary[A]): ToTydiBinary[List[A]] =
    new ToTydiBinary[List[A]] {
      def toBinary(l: List[A]): TydiBinary = TydiBinary.empty
      val binSize = 0
    }

  // There is no default implementation for other types, so a compiler error will be thrown when no matching ToTydiBinary implementation is found.
  // implicit def defaultToTydiBinary[T]: ToTydiBinary[T] = new ToTydiBinary[T] {}

  // Magnolia's boilerplate for deriving for a case class (Product)
  type Typeclass[T] = ToTydiBinary[T]

  /** Choose which equality subtype to defer to
   *
   * Note that in addition to dispatching based on the type of the first parameter to the `equal` method, we check that the second
   * parameter is the same type.
   */
  def split[T](ctx: SealedTrait[ToTydiBinary, T]): ToTydiBinary[T] = new ToTydiBinary[T] {
    override val binSize: Int = 0 // Fixme not sure how to solve this

    override def toBinary(t: T): TydiBinary = ctx.split(t)(sub =>
      sub.typeclass.toBinary(sub.cast(t)))
  }

  def join[T](ctx: magnolia1.CaseClass[ToTydiBinary, T]): ToTydiBinary[T] =
    new ToTydiBinary[T] {
      def toBinary(t: T): TydiBinary = {
        ctx.parameters.toList.map { param =>
          // 1. Get the value of the field from the case class instance 't'.
          val fieldValue = param.dereference(t)

          // 2. Use the derived 'ToTydiBinary' instance for the field's type (param.typeclass) to get the binary representation of the field value.
          param.typeclass.toBinary(fieldValue)
        }.reduce(_.concat(_))
      }

      val binSize: Int = ctx.parameters.toList.map(_.typeclass.binSize).sum
    }

  // This line is where the compiler hooks into the macro
  implicit def gen[T]: ToTydiBinary[T] = macro Magnolia.gen[T]
}
