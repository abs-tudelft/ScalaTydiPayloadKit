package TydiPayloadKit

import java.time.Instant
import FromTydiBinary._

object CustomBinaryConversions {
  implicit val instantToBinary: ToTydiBinary[Instant] = new ToTydiBinary[Instant] {
    def toBinary(i: Instant): TydiBinary = {
      TydiBinary(BigInt(i.toEpochMilli), binSize)
    }
    val binSize = 64
  }

  implicit val instantFromBinary: FromTydiBinary[Instant] = new FromTydiBinary[Instant] {
    val binSize = 64

    def fromBinary(t: TydiBinary): (Instant, TydiBinary) = {
      val (i, rest) = longFromTydiBinary.fromBinary(t)
      (Instant.ofEpochMilli(i), rest)
    }
  }
}
