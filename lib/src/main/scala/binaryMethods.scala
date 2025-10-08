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
}

