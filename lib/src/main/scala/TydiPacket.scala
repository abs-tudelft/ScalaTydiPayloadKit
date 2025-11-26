package TydiPayloadKit

import binaryMethods._
import FromTydiBinary._

case class TydiPacket[T] (data: Option[T], last: Seq[Boolean]) {
  def toBinary(implicit A: ToTydiBinary[T]): TydiBinary = {
    val dataBinary = data match {
      case Some(el) => A.toBinary(el)
      case None => TydiBinary(0, A.binSize)
    }
    val strobe = data match {
      case Some(_) => true.toBinary
      case None => false.toBinary
    }
    strobe.concat(last.toBinary).concat(dataBinary)
  }

  def mapData[U](f: T => U): TydiPacket[U] = {
    TydiPacket(data.map(f), last)
  }
}

object TydiPacket {
  def fromTydiBinary[T](blob: TydiBinary, dim: Int)(implicit A: FromTydiBinary[T]): TydiPacket[T] = {
    val (strobe, rest1) = boolFromTydiBinary.fromBinary(blob)
    val (lastBin, rest2) = rest1.splitLow(dim)
    val last = fromBinaryToBoolSeq(lastBin)
    val data = if (strobe) Some(A.fromBinary(rest2)._1) else None
    TydiPacket(data, last)
  }
}
