trait FromTydiBinary[T <: FromTydiBinary[T]] {
  def fromBinary: (T, TydiBinary)
}
