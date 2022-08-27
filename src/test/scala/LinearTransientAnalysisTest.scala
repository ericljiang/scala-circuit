import breeze.linalg._
import breeze.plot._

import java.awt.Desktop
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.double2bigDecimal

class LinearTransientAnalysisTest extends BaseTest {
  "LinearTransientAnalysis" should "simulate time step" in {
    var circuit = Circuit(Array(
      IndependentVoltageSource("V", 0, 1, 1),
      Resistor("R1", 0, 1, 1),
      Resistor("R2", 1, 2, 1),
      Resistor("R3", 0, 2, 1),
      Capacitor("C1", 1, 2, 1, 0, 0),
      Capacitor("C2", 0, 1, 1, 0, 0)
    ))

    val linearTransientAnalysis = new LinearTransientAnalysis

    val f = Figure()
    val p = f.subplot(0)

    val time = ArrayBuffer.empty[Double]
    val v1 = ArrayBuffer.empty[Double]
    val v2 = ArrayBuffer.empty[Double]

    val deltaTime = 0.001
    0.0 to 10.0 by deltaTime foreach { t =>
      val voltages = linearTransientAnalysis.simulateTimeStep(circuit, deltaTime)
      time += t.doubleValue
      v1 += voltages(0)
      v2 += voltages(1)
      println(voltages.mkString("Array(", ", ", ")"))
      circuit = Circuit(circuit.components.map {
        case capacitor: Capacitor =>
          val positiveVoltage = if (capacitor.positiveNode == 0) 0 else voltages(capacitor.positiveNode - 1)
          val negativeVoltage = if (capacitor.negativeNode == 0) 0 else voltages(capacitor.negativeNode - 1)
          val newVoltage = negativeVoltage - positiveVoltage
          val newCurrent = capacitor.capacitance * (newVoltage - capacitor.voltage) / deltaTime
          capacitor.copy(
            voltage = newVoltage,
            current = newCurrent)
        case x => x
      })
    }

//    p += plot(time.toArray, v1.toArray)
//    p += plot(time.toArray, v2.toArray)
    p += plot(time.toArray, DenseVector(v1.toArray) - DenseVector(v2.toArray))

    f.saveas("test.png")
    Desktop.getDesktop.open(new File("test.png"))
  }
}
