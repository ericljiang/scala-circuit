case class Circuit(components: Array[Component]) {
  private var computedN = Option.empty[Int]

  def numNodes: Int = computedN match {
    case Some(value) => value
    case None =>
      val value = components.map(c => c.positiveNode max c.negativeNode).max
      computedN = Some(value)
      value
  }

  def numIndependentVoltageSources: Int = components.count {
    case _: IndependentVoltageSource => true
    case _ => false
  }
}
