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

  /**
   * Splits the binary into n chunks, each chunk having a length of ceil(length / n) bits.
   * @param n Number of chunks to split the binary into.
   * @return Sequence of chunks.
   */
  def splitChunks(n: Int): Seq[TydiBinary] = {
    require(n > 0, "Number of chunks to split binary blob into must be positive.")
    val chunkSize = math.ceil(length.toDouble / n).toInt
    val chunks = (0 until n).map { i =>
      val (chunk, rest) = splitLow(chunkSize)
      (chunk, rest)
    }
    chunks.map(_._1)
  }

  /**
   * Grows the binary to a new length. The new length must be greater than or equal to the current length.
   * @param newLength New length of the binary.
   * @return New binary with the specified length.
   */
  def growTo(newLength: Int): TydiBinary = {
    require(newLength >= length, "New length must be greater than or equal to the current length.")
    if (newLength == length) return this
    TydiBinary(data, newLength)
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
