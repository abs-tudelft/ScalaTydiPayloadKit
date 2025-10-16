package TydiPackaging

import TydiPackaging.FromTydiBinary._
import TydiPackaging.binaryMethods.fromBinaryToBoolSeq

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

  def mapData[U](f: T => U): TydiStream[U] = {
    TydiStream(packets.map(_.mapData(f)))
  }

  /**
   * "Unpack" the lowest dimension of the stream to packets of lists.
   * @return New stream with the lowest dimension unwrapped.
   */
  def unpackDim(): TydiStream[List[T]] = {
    val (finalRows, _) = packets.foldLeft(List.empty[TydiPacket[List[T]]], List.empty[T]) {
      case ((rows, current), el) => {
        el.data match {
          case Some(data) => {
            // Prepend the current item to the 'current' row
            val newCurrentRow: List[T] = data :: current

            if (el.last.last) {
              // If the current packet closes the lowest dimension, finished the packet with the current row, add to the final rows and start a new row.
              (TydiPacket(Some(newCurrentRow.reverse), el.last.init) :: rows, List.empty[T])
            } else {
              // If not, continue the current row (item already added above).
              (rows, newCurrentRow)
            }
          }
          case None => {
            if (el.last.last) {
              // If the current empty packet closes the lowest dimension, prepend a packet with an empty list to the final rows and start a new row.
              (TydiPacket(Some(List.empty[T]), el.last.init) :: rows, List.empty[T])
            } else {
              // If not, prepend an empty packet.
              (TydiPacket[List[T]](None, el.last.init) :: rows, List.empty[T])
            }
          }
        }
      }
    }

    // After iteration, prepend the final `currentRow` should be empty, as all dimensions are either closed or the last item is empty.
    // In both cases, the final `currentRow` row is empty and all data is in the `finalRows`.
    // Both need to be reversed to restore the original order.
    TydiStream(finalRows.reverse)
  }

  def unpackToStrings(): TydiStream[String] = {
    val charStream = this.unpackDim()
    val stringStream = charStream.mapData(c => c.mkString)
    stringStream
  }

  /**
   * "Inject" a new dimension into the stream.
   * @param f Mapping function that creates a new packet from the old packet and the nested data sequence.
   * @param data Stream to inject into this stream.
   * @tparam U Type of the nested data.
   * @return
   */
  def inject[U](f: (T, Seq[U]) => T, data: TydiStream[U]): TydiStream[T] = {
    val restructuredData = data.unpackDim()

    // Add restructured data to own packets
    val newPackets = packets.zip(restructuredData.packets).map {
      case (p, c) => {
        p.mapData(d => f(d, c.data.get))
      }
    }

    TydiStream(newPackets)
  }

  /**
   * "Inject" a new dimension into the stream.
   * @param f Mapping function that creates a new packet from the old packet and the nested data sequence.
   * @param data Stream of string data to inject into this stream.
   * @return
   */
  def injectString(f: (T, String) => T, data: TydiStream[Char]): TydiStream[T] = {
    val restructuredData = data.unpackToStrings()

    // Add restructured data to own packets
    val newPackets = packets.zip(restructuredData.packets).map {
      case (p, c) => {
        p.mapData(d => f(d, c.data.get))
      }
    }

    TydiStream(newPackets)
  }

  def toSeq: Seq[T] = {
    packets.map(_.data.get)
  }

  def toBinaryBlobs()(implicit A: ToTydiBinary[T]): Seq[TydiBinary] = {
    packets.map(_.toBinary)
  }
}

object TydiStream {
  def apply[T](packets: Seq[TydiPacket[T]]): TydiStream[T] = new TydiStream(packets)

  def fromSeq[T](seq: Seq[T]): TydiStream[T] = {
    val length = seq.length
    val packets = seq.zipWithIndex.map {
      case (el, i) => TydiPacket(Some(el), Seq(i == length - 1))
    }
    TydiStream(packets)
  }

  def fromBinaryBlobs[T](blobs: Seq[TydiBinary], dim: Int)(implicit A: FromTydiBinary[T]): TydiStream[T] = {
    val packets = blobs.map(TydiPacket.fromTydiBinary[T](_, dim))
    TydiStream(packets)
  }
}
