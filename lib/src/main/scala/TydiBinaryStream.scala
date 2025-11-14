package TydiPackaging

class TydiBinaryStream (val packets: Seq[TydiBinary]) {
  def toSeq: Seq[TydiBinary] = packets

  /**
   * Condense the stream by grouping and concatenating binaries together.
   * @param n Number of packets per binary blob.
   * @param blobSize Size to expand each binary blob to.
   * @return New stream.
   */
  def group(n: Int, blobSize: Int): TydiBinaryStream = {
    TydiBinaryStream(
      packets.grouped(n).map(
        _.map(_.growTo(blobSize)).reduce(_.concat(_))
      ).toSeq
    )
  }

  /**
   * Convert the packets in this stream to a single binary blob by concatenation.
   * @return Concatenated binary blob.
   */
  def toSizedBinaryBlob(blobSize: Int): TydiBinary = {
    packets.map(_.growTo(blobSize)).reduce(_.concat(_))
  }

  def map[U](f: TydiBinary => U): Seq[U] = {
    packets.map(f)
  }

  def zip(that: TydiBinaryStream): Seq[(TydiBinary, TydiBinary)] = {
    packets.zip(that.packets)
  }
}

object TydiBinaryStream {
  def apply(packets: Seq[TydiBinary]): TydiBinaryStream = new TydiBinaryStream(packets)
  def empty: TydiBinaryStream = TydiBinaryStream(Seq.empty)

  /**
   * Create a binary stream from a sequence of binary blobs, each containing `groupSize` packets.
   * @param blobs Sequence of binary blobs.
   * @param groupSize Number of chunks to split the binary blobs into.
   * @return New stream.
   */
  def fromGrouped[T](blobs: Seq[TydiBinary], groupSize: Int): TydiBinaryStream = {
    val packets = blobs.flatMap(b => {
      b.splitChunks(groupSize)
    })
    TydiBinaryStream(packets)
  }
}
