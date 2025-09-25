package TydiPackaging

class TydiStream[T] (val packets: Seq[TydiPacket[T]]) {
  /** "Drill" into the structure to the iterable field referenced in [[f]], creating a new dimension in the `last` data. */
  def drill[U](f: T => Seq[U]): TydiStream[U] = {
    val d = packets.head.last.length
    // Map through existing items in our sequence of packets
    val new_packets: Seq[TydiPacket[U]] = packets.flatMap {
      // If the packet contains data
      case TydiPacket(Some(el), last) =>
        // Apply drilling function, create packets from elements of resulting iterable
        val nested = f(el)
        val length = nested.length
        val v: Seq[TydiPacket[U]] = if (nested.isEmpty) {
          // It can be that this dimension is empty, in that case return a single empty packet
          Seq(TydiPacket(None, last ++ Seq(true)))
        } else {
          nested.zipWithIndex.map {
            case (nested_el, j) => {
              val new_last = if (j == length - 1) {
                // The last element closes the lowest dimension and copies the parent's last data (which may or may not close).
                last ++ Seq(true)
              } else {
                // Every element that does not close the lowest dimension does also not close the upper dimensions.
                Seq.fill(d+1)(false)
              }
              TydiPacket(Some(nested_el), new_last)
            }
          }
        }
        v
      case TydiPacket(None, last) => Seq(TydiPacket(None, last ++ Seq(false)))
    }
    TydiStream(new_packets)
  }
}

object TydiStream {
  def apply[T](packets: Seq[TydiPacket[T]]): TydiStream[T] = new TydiStream(packets)

  def from_seq[T](seq: Seq[T]): TydiStream[T] = {
    val length = seq.length
    val packets = seq.zipWithIndex.map {
      case (el, i) => TydiPacket(Some(el), Seq(i == length - 1))
    }
    TydiStream(packets)
  }
}
