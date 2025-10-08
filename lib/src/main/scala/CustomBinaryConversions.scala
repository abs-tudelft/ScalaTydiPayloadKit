package TydiPackaging

import java.time.Instant

object CustomBinaryConversions {
  implicit val instantToBinary: ToTydiBinary[Instant] = new ToTydiBinary[Instant] {
    def toBinary(i: Instant): TydiBinary = {
      TydiBinary(BigInt(i.toEpochMilli), binSize)
    }
    val binSize = 64
  }
}
