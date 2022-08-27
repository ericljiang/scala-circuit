import breeze.linalg._

class ModifiedNodalAnalysisTest extends BaseTest {
  // https://lpsa.swarthmore.edu/Systems/Electrical/mna/MNA3.html#Generating_the_MNA_matrices
  private val case1 = Circuit(Array(
    IndependentVoltageSource("V1", 2, 1, 32),
    IndependentVoltageSource("V2", 3, 0, 20),
    Resistor("R1", 0, 1, 2),
    Resistor("R2", 2, 3, 4),
    Resistor("R3", 0, 2, 8)
  ))

  private val analysis = new ModifiedNodalAnalysis

  "Simulation" should "construct A matrix" in {
    val expected = DenseMatrix(
      (0.5, 0.0, 0.0, -1.0, 0.0),
      (0.0, 0.375, -0.25, 1.0, 0.0),
      (0.0, -0.25, 0.25, 0.0, 1.0),
      (-1.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0)
    )
    assertResult(expected) {
      val a = analysis.stampMatrices(case1, case1.numIndependentVoltageSources + case1.numNodes).a
      println(a.toDenseMatrix)
      a
    }
  }

  it should "construct fixed source vector" in {
    val expected = DenseVector(0, 0, 0, 32, 20)
    assertResult(expected) {
      analysis.stampMatrices(case1, case1.numIndependentVoltageSources + case1.numNodes).b
    }
  }

  it should "simulate" in {
    val startTime = System.nanoTime
    val result = analysis.simulate(case1)
    val endTime = System.nanoTime
    println(result.mkString("Array(", ", ", ")"))
    println(s"${endTime - startTime} ns")
  }
}
