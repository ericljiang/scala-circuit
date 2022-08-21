import breeze.linalg.*

class NodalAnalysis extends Simulation {
  override def simulate(circuit: Circuit): Array[Double] = {
    val admittanceFormulation = constructMatrices(circuit)
    val Y = admittanceFormulation.y.toDenseMatrix
    val J = admittanceFormulation.j.toDenseVector
    val voltages: Vector[Double] = Y \ J
    voltages.toArray
  }

  case class AdmittanceFormulation(y: Matrix[Double], j: Vector[Double]) {
    def +(other: AdmittanceFormulation): AdmittanceFormulation =
      AdmittanceFormulation(this.y + other.y, this.j + other.j)
  }

  def constructMatrices(circuit: Circuit): AdmittanceFormulation = {
    val n = circuit.n
    val emptyMatrices = AdmittanceFormulation(CSCMatrix.zeros[Double](n, n), SparseVector.zeros[Double](n))
    circuit.components
      .map(createStamp(_, n))
      .fold(emptyMatrices)(_ + _)
  }

  def createStamp(component: Component, n: Int): AdmittanceFormulation = component match
    case capacitor: Capacitor => ???
    case currentSource: IndependentCurrentSource =>
      val vector = SparseVector.zeros[Double](n)
      if (currentSource.negativeNode != 0) {
        vector(currentSource.negativeNode - 1) = currentSource.current
      }
      if (currentSource.positiveNode != 0) {
        vector(currentSource.positiveNode - 1) = -currentSource.current
      }
      AdmittanceFormulation(CSCMatrix.zeros[Double](n, n), vector)
    case resistor: Resistor =>
      val builder = CSCMatrix.Builder[Double](n, n)
      if (resistor.negativeNode != 0) {
        builder.add(resistor.negativeNode - 1, resistor.negativeNode - 1, resistor.conductance)
      }
      if (resistor.positiveNode != 0) {
        builder.add(resistor.positiveNode - 1, resistor.positiveNode - 1, resistor.conductance)
      }
      if (!resistor.isGrounded) {
        builder.add(resistor.negativeNode - 1, resistor.positiveNode - 1, -resistor.conductance)
        builder.add(resistor.positiveNode - 1, resistor.negativeNode - 1, -resistor.conductance)
      }
      AdmittanceFormulation(builder.result, SparseVector.zeros[Double](n))
    case _ => ???
}
