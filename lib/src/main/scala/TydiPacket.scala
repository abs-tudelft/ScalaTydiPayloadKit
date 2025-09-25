package TydiPackaging

case class TydiPacket[T] (data: Option[T], last: Seq[Boolean]) {
  def to_binary: TydiBinary = ???

  def map_data[U](f: T => U): TydiPacket[U] = {
    TydiPacket(data.map(f), last)
  }
}
