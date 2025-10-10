package TydiPackaging

case class TydiBinary (data: BigInt, length: Int) {
  /**
   * Concatenates two binary values. The added binary is placed on the most significant end.
   * For example, 10101010.concat(01010101) becomes 01010101_10101010.
   * @param other Binary to concatenate self with.
   * @return Concatenated binary.
   */
  def concat(other: TydiBinary): TydiBinary = {
    val new_data = other.data << this.length | this.data
    TydiBinary(new_data, length + other.length)
  }

  /**
   * Splits the binary into two parts, the first part having a length of n bits.
   * The split is done from the least significant end, so 11111000.splitLow(3) becomes (000, 11111).
   * @param n Length in bits of part to split off from the LSB side.
   * @return (split, remainder)
   */
  def splitLow(n: Int): (TydiBinary, TydiBinary) = {
    val new_length = length - n
    val new_data = data >> n
    val modulus = BigInt(1) << n
    val mask = modulus - 1
    val data_masked = data & mask
    (TydiBinary(data_masked, n), TydiBinary(new_data, new_length))
  }

  /**
   * Splits the binary into two parts, the first part having a length of n bits.
   * The split is done from the most significant end, so 11111000.splitHigh(5) becomes (11111, 000).
   * @param n Length in bits of part to split off from the MSB side.
   * @return (split, remainder)
   */
  def splitHigh(n: Int): (TydiBinary, TydiBinary) = {
    val new_length = length - n
    val new_data = data >> new_length
    val modulus = BigInt(1) << new_length
    val mask = modulus - 1
    val data_masked = data & mask
    (TydiBinary(new_data, n), TydiBinary(data_masked, new_length))
  }

  def binString: String = {
    val toPad = math.max(length - data.bitLength, 0)
    val fullBinaryString = "0".repeat(toPad) + data.toString(2)
    val toCut = fullBinaryString.length - length
    "0b" + fullBinaryString.substring(toCut)
  }

  override def toString: String = binString
}

object TydiBinary {
  def apply(data: BigInt, length: Int): TydiBinary = new TydiBinary(data, length)
  def empty: TydiBinary = TydiBinary(0, 0)
}
