package net.stoerr.learning2.algorithms

import net.stoerr.learning2.common.DoubleArrayVector._

/**
 * Resilient backpropagation: we only observe the signs of the gradient and change the step length
 * for each parameter
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.12.2014
 */
class RProp(val f: Array[Double] => Double, val fgrad: Array[Double] => (Double, Array[Double]),
            val maxSteps: Int, x0: Array[Double]) {

  private val etaminus = 0.5
  private val etaplus = 1.2

  var lval = Array.fill(x0.length)(0.1)
  var lastsign = Array.fill(x0.length)(0.0)
  var x = x0
  var lastY = Double.MaxValue
  var eps = Double.MaxValue

  /** Does the descent and returns the argument vector, the best function value and the last function change */
  def descent(): (Array[Double], Double, Double) = {
    for (i <- 0 until maxSteps if math.abs(eps) > 1e-10) {
      calculateStep()
      println(i + "\teps=" + eps + "\tlastY=" + lastY)
    }
    val y = f(x)
    (x, y, math.abs(y - lastY))
  }

  def calculateStep() = {
    val (y, ygrad) = fgrad(x)
    val gradsign = ygrad.signum
    val lfac = gradsign * etaminus + (lastsign + gradsign) * ((etaplus - etaminus) / 2)
    lval = lval elem_* lfac.elem_abs
    x = x - (gradsign elem_* lval)
    lastsign = gradsign
    eps = y - lastY
    lastY = y
  }

}
