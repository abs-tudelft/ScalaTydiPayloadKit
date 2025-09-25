package TydiPackaging

case class TydiBinary (data: BigInt, length: Int) {
  def concat(other: TydiBinary): TydiBinary = {
    val new_data = data << length | other.data
    TydiBinary(new_data, length + other.length)
  }

  def split(n: Int): (TydiBinary, TydiBinary) = {
    val new_length = length - n
    val new_data = data >> n
    (TydiBinary(new_data, n), TydiBinary(data, new_length))
  }
}

object TydiBinary {
  def apply(data: BigInt, length: Int): TydiBinary = new TydiBinary(data, length)
  def empty: TydiBinary = TydiBinary(0, 0)
}
