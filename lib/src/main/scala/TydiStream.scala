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

  def inject[U](f: (T, Seq[U]) => T, data: TydiStream[U]): TydiStream[T] = {
    val dataIter = data.packets.iterator

    val newPackets = packets.map(packet => {
      val m = packet.data match {
        case Some(selfData) => {
          var prevLast = false
          val newSeq: Seq[U] = dataIter.takeWhile(p => {
            val nonEmpty = p.data.isDefined
            val last = p.last.last
            val continue = nonEmpty && !prevLast
            prevLast = last
            continue
          }).map(el => el.data.get).toSeq
          Some(f(selfData, newSeq))
        }
        case None => {
          dataIter.next()
          None
        }
      }
      TydiPacket(m, packet.last)
    })
    TydiStream(newPackets)
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
