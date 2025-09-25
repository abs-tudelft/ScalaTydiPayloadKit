package TydiPackaging

package object binaryMethods {
  implicit class fromIntToBinary(i: Int) extends ToTydiBinary {
    def toBinary: TydiBinary = TydiBinary(i.asInstanceOf[BigInt], 32)

    override val binSize: Int = 32
  }

  /*implicit class fromBigIntToBinary(i: BigInt) extends ToTydiBinary {
    def toBinary(size: Int): TydiBinary = TydiBinary(i, size)

    override val binSize: Int = ???
  }*/

  implicit class fromBoolToBinary(b: Boolean) extends ToTydiBinary {
    def toBinary: TydiBinary = TydiBinary(b.asInstanceOf[BigInt], 1)

    override val binSize: Int = 1
  }

  implicit class fromOptionToBinary[T <: ToTydiBinary](o: Option[T]) extends ToTydiBinary {
    def toBinary: TydiBinary = {
      o match {
        case Some(el) => true.toBinary.concat(el.toBinary)
        case None => false.toBinary.concat(TydiBinary(0, o.binSize))
      }
    }

    override val binSize: Int = o.binSize + 1
  }
}

