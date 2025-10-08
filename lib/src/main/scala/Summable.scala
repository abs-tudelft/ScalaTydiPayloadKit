package TydiPackaging

import language.experimental.macros
import magnolia1.Magnolia

trait Summable[T] {
  def sum(t: T): Int
}

object Summable {
  // Type Class Instances for primitives

  // 1. Int: Returns the value itself.
  implicit val intSummable: Summable[Int] = new Summable[Int] {
    def sum(i: Int): Int = i
  }

  // 2. Double/Float: Converts to Int (potentially losing precision) and adds it.
  implicit val doubleSummable: Summable[Double] = new Summable[Double] {
    def sum(d: Double): Int = d.toInt
  }

  // 3. String: Tries to parse the string as an Int.
  implicit val stringSummable: Summable[String] = new Summable[String] {
    def sum(s: String): Int = s.toIntOption.getOrElse(0)
  }

  // 4. List: Recursively sums the results of its elements.
  implicit def listSummable[A](implicit A: Summable[A]): Summable[List[A]] =
    new Summable[List[A]] {
      def sum(l: List[A]): Int = l.map(A.sum).sum
    }

  // 5. Fallback: Any other type (like Boolean, Char, etc.) contributes 0.
  implicit def defaultSummable[T]: Summable[T] = new Summable[T] {
    def sum(t: T): Int = 0
  }

  type Typeclass[T] = Summable[T]

  /** Defines how to combine the results of all fields in a case class. */
  def join[T](ctx: magnolia1.CaseClass[Summable, T]): Summable[T] =
    new Summable[T] {
      def sum(t: T): Int = {
        ctx.parameters.toList.map { param =>
          // 1. Get the value of the field from the case class instance 't'.
          val fieldValue = param.dereference(t)

          // 2. Use the derived 'Summable' instance for the field's type
          //    (param.typeclass) to get its sum.
          param.typeclass.sum(fieldValue)
        }.sum // 3. Sum up the results from all fields.
      }
    }

  /** The macro that hooks into the compiler to generate the Typeclass instance. */
  implicit def gen[T]: Summable[T] = macro Magnolia.gen[T]
}