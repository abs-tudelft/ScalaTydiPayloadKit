package TydiPackaging

case class TydiBinary (data: BigInt, length: Int) {
  def concat(other: TydiBinary): TydiBinary = {
    val new_data = data << other.length | other.data
    TydiBinary(new_data, length + other.length)
  }

  def split(n: Int): (TydiBinary, TydiBinary) = {
    val new_length = length - n
    val new_data = data >> n
    (TydiBinary(new_data, n), TydiBinary(data, new_length))
  }

  def binString: String = "0b" + data.toString(2).padTo(length, '0').substring(0, length)
}

object TydiBinary {
  def apply(data: BigInt, length: Int): TydiBinary = new TydiBinary(data, length)
  def empty: TydiBinary = TydiBinary(0, 0)
}
