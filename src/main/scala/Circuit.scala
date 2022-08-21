case class Circuit(components: Array[Component]) {
  private var computedN = Option.empty[Int]
  def n: Int = computedN match
    case Some(value) => value
    case None =>
      val value = components.map(c => c.positiveNode max c.negativeNode).max
      computedN = Some(value)
      value
}
