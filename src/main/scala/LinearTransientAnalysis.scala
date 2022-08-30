class LinearTransientAnalysis(var circuit: Circuit) {

  private val mna = new ModifiedNodalAnalysis

  def simulateTimeStep(deltaTime: Double): Array[Double] = {
    val equivalentCircuit = Circuit(circuit.components.flatMap(equivalentComponents(_, deltaTime)))
    val voltages = mna.simulate(equivalentCircuit).slice(0, circuit.numNodes)
    updateCircuit(voltages, deltaTime)
    voltages
  }

  private def updateCircuit(voltages: Array[Double], deltaTime: Double): Unit = {
    circuit = Circuit(circuit.components.map {
      case capacitor: Capacitor =>
        val positiveVoltage = if (capacitor.positiveNode == 0) 0 else voltages(capacitor.positiveNode - 1)
        val negativeVoltage = if (capacitor.negativeNode == 0) 0 else voltages(capacitor.negativeNode - 1)
        val newVoltage = negativeVoltage - positiveVoltage
        val newCurrent = capacitor.capacitance * (newVoltage - capacitor.voltage) / deltaTime
        capacitor.copy(
          voltage = newVoltage,
          current = newCurrent)
      case inductor: Inductor =>
        val positiveVoltage = if (inductor.positiveNode == 0) 0 else voltages(inductor.positiveNode - 1)
        val negativeVoltage = if (inductor.negativeNode == 0) 0 else voltages(inductor.negativeNode - 1)
        val newVoltage = negativeVoltage - positiveVoltage
        // trapezoidal
        val newCurrent = inductor.current + deltaTime / (2 * inductor.inductance) * (inductor.voltage + newVoltage)
        // backward Euler
        // val newCurrent = inductor.current + newVoltage * deltaTime / inductor.inductance
        inductor.copy(
          voltage = newVoltage,
          current = newCurrent)
      case x => x
    })
  }

  private def equivalentComponents(component: Component, deltaTime: Double): Seq[Component] = component match {
    case capacitor: Capacitor => Seq(
      // Norton equivalent "Electronic Circuit and System Simulation Methods" p. 22
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
    case inductor: Inductor => Seq(
      // Thevenin equivalent "Electronic Circuit and System Simulation Methods" p. 23
      IndependentVoltageSource(
        id = s"${inductor.id}.I_eq",
        positiveNode = inductor.positiveNode,
        negativeNode = 2, // TODO
        voltage = inductor.voltage + 2 * inductor.inductance / deltaTime * inductor.current),
      Resistor(
        id = s"${inductor.id}.R_eq",
        positiveNode = 2, // TODO
        negativeNode = inductor.negativeNode,
        resistance = 2 * inductor.inductance / deltaTime)
    )
    // if most components are not replaced with multiple equivalent components, this may be inefficient
    case component => Seq(component)
  }
}
