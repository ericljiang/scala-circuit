import java.time.{Duration, Instant}

object Main extends App {
  println("Hello world!")
  // "Electronic Circuit and System Simulation Methods" figure 1.5 p6
  val circuit = Circuit(
    elements = Array(
      IndependentCurrentSource("I1", 0, 1, 1),
      Resistor("R2", 1, 0, 1),
      Resistor("R3", 1, 2, 1),
      Resistor("R4", 2, 0, 1),
      Resistor("R5", 2, 3, 1),
      Resistor("R6", 3, 0, 1),
      Resistor("R7", 3, 4, 1),
      Resistor("R8", 4, 0, 1),
      IndependentCurrentSource("I9", 0, 4, 1)
    )
  )

  val simulation = new NodalAnalysis

  (1 to 10).foreach { _ =>
    val start = Instant.now
    val result = simulation.simulate(circuit)
    println(result.mkString("Array(", ", ", ")"))
    val end = Instant.now
    println(s"${Duration.between(start, end)}")
  }
}