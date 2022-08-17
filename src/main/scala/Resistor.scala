case class Resistor(id: String, positiveNode: Int, negativeNode: Int, resistance: Double) extends Component {
  def conductance: Double = 1 / resistance
}
