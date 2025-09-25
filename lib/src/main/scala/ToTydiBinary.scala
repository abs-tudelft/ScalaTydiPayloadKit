package TydiPackaging

//import TydiPackaging.binaryMethods._

import language.experimental.macros
import magnolia1._

trait ToTydiBinary[T] {
  val binSize: Int

  def toBinary(t: T): TydiBinary
}

object ToTydiBinary {

  // 1. Int: Create binary from Int.
  implicit val intToTydiBinary: ToTydiBinary[Int] = new ToTydiBinary[Int] {
    def toBinary(i: Int): TydiBinary = TydiBinary(i.asInstanceOf[BigInt], 32)
    val binSize = 32
  }

  // 2. Double/Float: Converts to Int (potentially losing precision) and adds it.
  implicit val doubleToTydiBinary: ToTydiBinary[Double] = new ToTydiBinary[Double] {
    def toBinary(d: Double): TydiBinary = TydiBinary(d.toInt.asInstanceOf[BigInt], 32)
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

  // 5. Fallback: Any other type (like Boolean, Char, etc.) contributes 0.
  implicit def defaultToTydiBinary[T]: ToTydiBinary[T] = new ToTydiBinary[T] {
    def toBinary(t: T): TydiBinary = TydiBinary.empty
    val binSize = 0
  }

  // Magnolia's boilerplate for deriving for a case class (Product)
  type Typeclass[T] = ToTydiBinary[T]

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
