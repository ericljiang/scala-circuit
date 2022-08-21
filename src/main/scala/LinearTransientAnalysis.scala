class LinearTransientAnalysis extends NodalAnalysis {
  def simulateTimeStep(circuit: Circuit, deltaTime: Double): Array[Double] = {
    val equivalentCircuit = Circuit(circuit.components
      .flatMap {
        case capacitor: Capacitor => Seq(
          IndependentCurrentSource(
            id = s"${capacitor.id}.I_eq",
            positiveNode = capacitor.positiveNode,
            negativeNode = capacitor.negativeNode,
            current = capacitor.current + 2 * capacitor.capacitance / deltaTime * capacitor.voltage),
          Resistor(
            id = s"${capacitor.id}.R_eq",
            positiveNode = capacitor.positiveNode,
            negativeNode = capacitor.negativeNode,
            resistance = deltaTime / (2 * capacitor.capacitance))
        )
        case x => Seq(x)
      })
    simulate(equivalentCircuit)
  }
}