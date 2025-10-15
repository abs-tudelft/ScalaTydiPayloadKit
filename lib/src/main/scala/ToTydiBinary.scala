package TydiPackaging

import TydiPackaging.binaryMethods._

import language.experimental.macros
import magnolia1._

trait ToTydiBinary[T] {
  val binSize: Int

  def toBinary(t: T): TydiBinary
}

object ToTydiBinary {

  implicit def optionToTydiBinary[A](implicit A: ToTydiBinary[A]): ToTydiBinary[Option[A]] = new ToTydiBinary[Option[A]] {
    def toBinary(o: Option[A]): TydiBinary = {
      o match {
        case Some(el) => true.toBinary.concat(A.toBinary(el))
        case None => false.toBinary.concat(TydiBinary(0, A.binSize))
      }
    }
    val binSize = 1
  }

  implicit val boolToTydiBinary: ToTydiBinary[Boolean] = new ToTydiBinary[Boolean] {
    def toBinary(b: Boolean): TydiBinary = TydiBinary(b.asInstanceOf[BigInt], binSize)
    val binSize = 1
  }

  implicit val intToTydiBinary: ToTydiBinary[Int] = new ToTydiBinary[Int] {
    def toBinary(i: Int): TydiBinary = TydiBinary(BigInt(i), binSize)
    val binSize = 32
  }

  implicit val longToTydiBinary: ToTydiBinary[Long] = new ToTydiBinary[Long] {
    def toBinary(l: Long): TydiBinary = TydiBinary(BigInt(l), binSize)
    val binSize = 64
  }

  implicit val doubleToTydiBinary: ToTydiBinary[Double] = new ToTydiBinary[Double] {
    def toBinary(d: Double): TydiBinary = {
      // There is also a "raw" version of this method that only differs in the handling of NaN values.
      val num = java.lang.Double.doubleToLongBits(d)
      TydiBinary(num, binSize)
    }
    val binSize = 64
  }

  implicit val floatToTydiBinary: ToTydiBinary[Float] = new ToTydiBinary[Float] {
    def toBinary(d: Float): TydiBinary = {
      // See note for Double
      val num = java.lang.Float.floatToIntBits(d)
      TydiBinary(num, binSize)
    }
    val binSize = 32
  }

  implicit val charToTydiBinary: ToTydiBinary[Char] = new ToTydiBinary[Char] {
    // Char is a 16-bit value because it uses UTF-16, while our binary representation is 8 bits.
    // Therefore, this conversion is not ideal. Fixme
    def toBinary(i: Char): TydiBinary = TydiBinary(BigInt(i), binSize)
    val binSize = 8
  }

  // String: becomes a separate stream, so the binary is empty.
  implicit val stringToTydiBinary: ToTydiBinary[String] = new ToTydiBinary[String] {
    def toBinary(s: String): TydiBinary =  TydiBinary.empty
    val binSize = 0
  }

  // List: becomes a separate stream, so the binary is empty.
  implicit def listToTydiBinary[A](implicit A: ToTydiBinary[A]): ToTydiBinary[List[A]] =
    new ToTydiBinary[List[A]] {
      def toBinary(l: List[A]): TydiBinary = TydiBinary.empty
      val binSize = 0
    }

  // There is no default implementation for other types, so a compiler error will be thrown when no matching ToTydiBinary implementation is found.
  // implicit def defaultToTydiBinary[T]: ToTydiBinary[T] = new ToTydiBinary[T] {}

  // Magnolia's boilerplate for deriving for a case class (Product)
  type Typeclass[T] = ToTydiBinary[T]

  /** Create typeclasses for sealed-traits/enums ('sum types')
   *
   * Frankly, I am not sure how this works based on the limited information of the GitHub repository.
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
