package net.stoerr.learning2.algorithms

import net.stoerr.learning2.common.DoubleArrayVector._

/**
  * Algorithm for gradient descent that needs a function together with its gradient
  * and the first and second order directional derivation in direction of the gradient.
  */
class SecondOrderDescent(
                          /** value to f(x), gradient, d/dt f(x+t*gradient), d^2/dt^2 f(x+t*gradient) */
                          val fgdir: Array[Double] => (Double, Array[Double], Double, Double),
                          val maxSteps: Int, x0: Array[Double]
                        ) {

  var x: Array[Double] = x0
  var lastY = Double.MaxValue
  var eps = Double.MaxValue

  /** Does the descent and returns the argument vector, the best function value and the last function change */
  def descent(): (Array[Double], Double, Double) = {
    for (i <- 0 until maxSteps if math.abs(eps) > 1e-10) {
      calculateStep()
      println(i + "\teps=" + eps + "\tlastY=" + lastY)
    }
    val y = fgdir(x)
    (x, y._1, math.abs(y._1 - lastY))
  }

  def calculateStep(): Unit = {
    val (y, ygrad, ydir1, ydir2) = fgdir(x)
    x = x - (ygrad * (ydir1 / ydir2))
    eps = y - lastY
    lastY = y
  }

}
