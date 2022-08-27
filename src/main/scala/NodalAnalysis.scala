import breeze.linalg._

class NodalAnalysis extends Simulation {
  override def simulate(circuit: Circuit): Array[Double] = {
    val admittanceFormulation = stampMatrices(circuit, circuit.numNodes)
    val Y = admittanceFormulation.a.toDenseMatrix
    val J = admittanceFormulation.b.toDenseVector
    val voltages: Vector[Double] = Y \ J
    voltages.toArray
  }

  case class MatrixFormulation(a: Matrix[Double], b: Vector[Double]) {
    def +(other: MatrixFormulation): MatrixFormulation =
      MatrixFormulation(this.a + other.a, this.b + other.b)
  }

  def stampMatrices(circuit: Circuit, size: Int): MatrixFormulation = {
    val emptyMatrices = MatrixFormulation(CSCMatrix.zeros[Double](size, size), SparseVector.zeros[Double](size))
    circuit.components
      .map(createStamp(_, size))
      .fold(emptyMatrices)(_ + _)
  }

  def createStamp(component: Component, n: Int): MatrixFormulation = component match {
    case currentSource: IndependentCurrentSource =>
      val vector = SparseVector.zeros[Double](n)
      if (currentSource.negativeNode != 0) {
        vector(currentSource.negativeNode - 1) = currentSource.current
      }
      if (currentSource.positiveNode != 0) {
        vector(currentSource.positiveNode - 1) = -currentSource.current
      }
      MatrixFormulation(CSCMatrix.zeros[Double](n, n), vector)
    case resistor: Resistor =>
      val builder = new CSCMatrix.Builder[Double](n, n)
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
      MatrixFormulation(builder.result, SparseVector.zeros[Double](n))
    case _ => ???
  }
}
