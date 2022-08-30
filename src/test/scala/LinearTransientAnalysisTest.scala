import breeze.linalg._
import breeze.plot._

import java.awt.{Color, Desktop}
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.double2bigDecimal

class LinearTransientAnalysisTest extends BaseTest {
  "LinearTransientAnalysis" should "simulate simple capacitor circuit" in {
    val circuit = Circuit(Array(
      IndependentVoltageSource("V", 1, 0, 1),
      Resistor("R", 0, 2, 1),
      Capacitor("C", 1, 2, 1, 0, 0)
    ))

    val linearTransientAnalysis = new LinearTransientAnalysis(circuit)

    val time = ArrayBuffer.empty[Double]
    val v1 = ArrayBuffer.empty[Double]
    val v2 = ArrayBuffer.empty[Double]

    val deltaTime = 0.001
    0.0 to 10.0 by deltaTime foreach { t =>
      val voltages = linearTransientAnalysis.simulateTimeStep(deltaTime)
      time += t.doubleValue
      v1 += voltages(0)
      v2 += voltages(1)
      println(voltages.mkString("Array(", ", ", ")"))
    }

    val f = Figure()
    val p = f.subplot(0)
    p += plot(time.toArray, v1.toArray, name = "v1")
    p += plot(time.toArray, v2.toArray, name = "v2")
    p += plot(time.toArray, DenseVector(v1.toArray) - DenseVector(v2.toArray), name = "v_C")
    p.legend = true
    p.chart.setBackgroundPaint(Color.WHITE)
    f.saveas("test.png")
    Desktop.getDesktop.open(new File("test.png"))
  }

  "LinearTransientAnalysis" should "simulate simple inductor circuit" in {
    val circuit = Circuit(Array(
      IndependentVoltageSource("V", 1, 0, 1),
      Resistor("R", 0, 2, 1),
      Inductor("L", 1, 2, 1, 0, 0)
    ))

    val linearTransientAnalysis = new LinearTransientAnalysis(circuit)

    val time = ArrayBuffer.empty[Double]
    val v1 = ArrayBuffer.empty[Double]
    val v2 = ArrayBuffer.empty[Double]

    val deltaTime = 0.001
    0.0 to 10.0 by deltaTime foreach { t =>
      val voltages = linearTransientAnalysis.simulateTimeStep(deltaTime)
      time += t.doubleValue
      v1 += voltages(0)
      v2 += voltages(1)
      println(voltages.mkString("Array(", ", ", ")"))
    }

    val f = Figure()
    val p = f.subplot(0)
    p += plot(time.toArray, v1.toArray, name = "v1")
    p += plot(time.toArray, v2.toArray, name = "v2")
    p += plot(time.toArray, DenseVector(v1.toArray) - DenseVector(v2.toArray), name = "v_I")
    p.legend = true
    p.chart.setBackgroundPaint(Color.WHITE)
    f.saveas("test.png")
    Desktop.getDesktop.open(new File("test.png"))
  }

  "LinearTransientAnalysis" should "simulate LC circuit" in {
    val circuit = Circuit(Array(
      Capacitor("C", 0, 1, 15e-6, 0, 0),
      Inductor("L", 1, 0, 1, 0, 1)
    ))

    val linearTransientAnalysis = new LinearTransientAnalysis(circuit)

    val time = ArrayBuffer.empty[Double]
    val v1 = ArrayBuffer.empty[Double]

    val deltaTime = 5e-6
    0.0 to 0.1 by deltaTime foreach { t =>
      val voltages = linearTransientAnalysis.simulateTimeStep(deltaTime)
      time += t.doubleValue
      v1 += voltages(0)
      println(voltages.mkString("Array(", ", ", ")"))
    }

    val f = Figure()
    val p = f.subplot(0)
    p += plot(time.toArray, v1.toArray, name = "v1")
    p.legend = true
    p.chart.setBackgroundPaint(Color.WHITE)
    f.saveas("test.png")
    Desktop.getDesktop.open(new File("test.png"))
  }
}
