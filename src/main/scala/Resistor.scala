case class Resistor(id: String, positiveNode: Int, negativeNode: Int, resistance: Double) extends Element {
  def conductance: Double = 1 / resistance
}
