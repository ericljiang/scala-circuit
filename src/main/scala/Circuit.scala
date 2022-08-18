case class Circuit(
  passiveComponents: Array[Component],
  fixedSources: Array[Component]
) {
  private var computedN = Option.empty[Int]
  def n: Int = computedN match
    case Some(value) => value
    case None =>
      val value = passiveComponents.map(c => c.positiveNode max c.negativeNode).max
      computedN = Some(value)
      value
}
