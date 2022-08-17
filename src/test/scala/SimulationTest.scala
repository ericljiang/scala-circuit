import breeze.linalg.*

class SimulationTest extends BaseTest {
  // "Electronic Circuit and System Simulation Methods" figure 1.5 p6
  private val circuit = Circuit(
    passiveComponents = Seq(
      Resistor("R2", 1, 0, 1),
      Resistor("R3", 1, 2, 1),
      Resistor("R4", 2, 0, 1),
      Resistor("R5", 2, 3, 1),
      Resistor("R6", 3, 0, 1),
      Resistor("R7", 3, 4, 1),
      Resistor("R8", 4, 0, 1),
    ),
    fixedSources = Seq(
      IndependentCurrentSource("I1", 0, 1, 1),
      IndependentCurrentSource("I9", 0, 4, 1)
    )
  )

  "Simulation" should "construct admittance matrix" in {
    val expected = DenseMatrix(
      (2.0, -1.0, 0.0, 0.0),
      (-1.0, 3.0, -1.0, 0.0),
      (0.0, -1.0, 3.0, -1.0),
      (0.0, 0.0, -1.0, 2.0)
    )
    val constructAdmittanceMatrix = PrivateMethod[Matrix[Double]](Symbol("constructAdmittanceMatrix"))
    assertResult(expected) {
      Simulation() invokePrivate constructAdmittanceMatrix(circuit)
    }
  }

  it should "construct fixed source vector" in {
    val expected = DenseVector(1, 0, 0, 1)
    val constructCurrentSourcesVector = PrivateMethod[Vector[Double]](Symbol("constructCurrentSourcesVector"))
    assertResult(expected) {
      Simulation() invokePrivate constructCurrentSourcesVector(circuit)
    }
  }

  it should "simulate" in {
    val startTime = System.nanoTime
    val result = Simulation().simulate(circuit)
    val endTime = System.nanoTime
    println(result)
    println(s"${endTime - startTime} ns")
  }
}
