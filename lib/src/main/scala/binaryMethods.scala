package TydiPackaging

package object binaryMethods {
  implicit class fromIntToBinary(i: Int) {
    def toBinary: TydiBinary = TydiBinary(i.asInstanceOf[BigInt], 32)
  }

  /*implicit class fromBigIntToBinary(i: BigInt) extends ToTydiBinary {
    def toBinary(size: Int): TydiBinary = TydiBinary(i, size)

    override val binSize: Int = ???
  }*/

  implicit class fromBoolToBinary(b: Boolean) {
    def toBinary: TydiBinary = TydiBinary(if (b) BigInt(1) else BigInt(0), 1)
  }

  implicit class fromBoolSeqToBinary(b: Seq[Boolean]) {
    def toBinary: TydiBinary = {
      val binary = b.foldLeft(TydiBinary.empty) {
        case (acc, el) => acc.concat(el.toBinary)
      }
      binary
    }
  }

  val fromBinaryToBoolSeq: TydiBinary => Seq[Boolean] = (b: TydiBinary) => {
    val num = b.data
    for (i <- 0 until b.length) yield {
      val bit = num & (1 << i)
      bit != 0
    }
  }
}

