import breeze.linalg._

class Simulation {
  def simulate(circuit: Circuit): Vector[Double] = {
    val startTime = System.nanoTime
    val Y = constructAdmittanceMatrix(circuit).toDenseMatrix
    val endTime = System.nanoTime
    println(s"${endTime - startTime} ns to compute Y")
    val J = constructCurrentSourcesVector(circuit).toDenseVector
    Y \ J
  }

  protected def constructAdmittanceMatrix(circuit: Circuit): Matrix[Double] =
    circuit.passiveComponents
      .map {
        case r: Resistor =>
          val builder = CSCMatrix.Builder[Double](circuit.n, circuit.n)
          if (r.negativeNode != 0) {
            builder.add(r.negativeNode - 1, r.negativeNode - 1, r.conductance)
          }
          if (r.positiveNode != 0) {
            builder.add(r.positiveNode - 1, r.positiveNode - 1, r.conductance)
          }
          if (!r.isGrounded) {
            builder.add(r.negativeNode - 1, r.positiveNode - 1, -r.conductance)
            builder.add(r.positiveNode - 1, r.negativeNode - 1, -r.conductance)
          }
          builder.result
      }
      .reduce(_ + _)

  protected def constructCurrentSourcesVector(circuit: Circuit): Vector[Double] =
    circuit.fixedSources
      .map {
        case currentSource: IndependentCurrentSource =>
          val vector = SparseVector.zeros[Double](circuit.n)
          if (currentSource.negativeNode != 0) {
            vector(currentSource.negativeNode - 1) = currentSource.current
          }
          if (currentSource.positiveNode != 0) {
            vector(currentSource.positiveNode - 1) = -currentSource.current
          }
          vector
      }
      .reduce(_ + _)
}