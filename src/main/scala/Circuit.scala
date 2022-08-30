case class Circuit(elements: Array[Element]) {
  private var computedN = Option.empty[Int]

  def numNodes: Int = computedN match {
    case Some(value) => value
    case None =>
      val value = elements.map(c => c.positiveNode max c.negativeNode).max
      computedN = Some(value)
      value
  }

  def numIndependentVoltageSources: Int = elements.count {
    case _: IndependentVoltageSource => true
    case _ => false
  }
}
