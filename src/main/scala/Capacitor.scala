case class Capacitor(
  id: String,
  positiveNode: Int,
  negativeNode: Int,
  capacitance: Double,
  voltage: Double,
  current: Double
) extends Component
