import breeze.linalg._

import scala.collection.mutable

class ModifiedNodalAnalysis extends NodalAnalysis {
  override def simulate(circuit: Circuit): Array[Double] = {
    val admittanceFormulation = stampMatrices(circuit, circuit.numIndependentVoltageSources + circuit.numNodes)
    val a = admittanceFormulation.a.toDenseMatrix
    val z = admittanceFormulation.b.toDenseVector
    val x = a \ z
    x.toArray
  }

  override def stampMatrices(circuit: Circuit, size: Int): MatrixFormulation = {
    val emptyMatrices = MatrixFormulation(CSCMatrix.zeros[Double](size, size), SparseVector.zeros[Double](size))
    val m = circuit.numIndependentVoltageSources
    val n = circuit.numNodes
    val voltageSourceIdMap = new mutable.HashMap[Int, String]
    var voltageSourceCounter = 0
    circuit.components
      .map {
        case s: IndependentVoltageSource =>
          val i = voltageSourceCounter
          voltageSourceCounter += 1
          voltageSourceIdMap(i) = s.id

          val builder = new CSCMatrix.Builder[Double](size, size)
          if (s.positiveNode != 0) {
            builder.add(s.positiveNode - 1, n + i, 1)
            builder.add(n + i, s.positiveNode - 1, 1)
          }
          if (s.negativeNode != 0) {
            builder.add(s.negativeNode - 1, n + i, -1)
            builder.add(n + i, s.negativeNode - 1, -1)
          }
          val a = builder.result

          val z = SparseVector.zeros[Double](m + n)
          z(n + i) = s.voltage
          MatrixFormulation(a, z)
        case component => super.createStamp(component, m + n)
      }
      .fold(emptyMatrices)(_ + _)
  }
}
