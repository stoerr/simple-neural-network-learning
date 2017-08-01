package net.stoerr.learning2.exploration

import net.stoerr.learning2.common.DoubleArrayVector._

/**
  * Try to find the best algorithm that calculates the next step from the
  * gradient before the next to last step, the gradient before the last step, the gratient after the last step
  * and both of those steps. We try to find terms with which we multiply those 3 gradients and those two steps such
  * that the gradient based descent is fastest.
  * <p>
  * One idea would be to take 5 random terms for those, and find the best combination. Unfortunately, this runs
  * into a combinatorical explosion very quickly. Another idea would be to make a gradient descent over a neural
  * network calculating those parameters.
  *
  * @author Hans-Peter Stoerr
  * @since 09/2017
  */
class TestFindBestDescentTerm {

  case class Evaluator(funcWithGradient: Array[Double] => (Double, Array[Double]), start: Array[Double], numsteps: Int,
    /* f(d1,d2,g0,g1,g2) */
    stepFunc: (Array[Double], Array[Double], Array[Double], Array[Double], Array[Double]) => Array[Double]) {
    var p0 = start
    var (e0, g0) = funcWithGradient(p0)
    var d1 = g0
    var p1 = p0 + d1
    var (e1, g1) = funcWithGradient(p1)
    var d2 = g1
    var p2 = p1 + d1
    var (e2, g2) = funcWithGradient(p2)

    val firstE0 = e0

    def step() = {
      val d3 = stepFunc(d1, d2, g0, g1, g2)
      val p3 = p2 + d3
      val (e3, g3) = funcWithGradient(p3)
      p0 = p1
      e0 = e1
      g0 = g1
      d1 = d2
      p1 = p2
      e1 = e2
      g1 = g2
      d2 = d3
      p2 = p3
      e2 = e3
      g2 = g3
    }

    for (i <- 1 until numsteps) step()
    val evaluation = (Math.log(firstE0) - Math.log(e2)) / numsteps
  }

}
