case class Inductor(
  id: String,
  positiveNode: Int,
  negativeNode: Int,
  inductance: Double,
  voltage: Double,
  current: Double
) extends Component
