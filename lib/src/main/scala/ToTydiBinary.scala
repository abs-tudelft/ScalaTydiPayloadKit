package TydiPackaging

trait ToTydiBinary {
  val binSize: Int

  def toBinary: TydiBinary
}
