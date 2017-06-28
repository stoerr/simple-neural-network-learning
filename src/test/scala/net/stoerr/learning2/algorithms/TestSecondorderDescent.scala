package net.stoerr.learning2.algorithms

import net.stoerr.learning2.common.Term
import net.stoerr.learning2.common.Term._
import org.scalatest.{FlatSpec, Matchers}

class TestSecondorderDescent extends FlatSpec with Matchers {

  val eps = 1e-7

  "SecondorderDescent" should "descent on a given function" in {
    val fterm: Term = ('x - 1.0) ** 2 + ('y - 2.0) ** 2 * 2.0 + ('x - 'y + 1) ** 4
    val vars = fterm.variables.toArray
    val fgdir: (Array[Double]) => (Double, Array[Double], Double, Double) = (x: Array[Double]) => {
      val valuation = (vars, x).zipped.toMap
      val (eval, grad, dir1, dir2) = evalWithGradientAndDirectionalDerivations(fterm, valuation)
      (eval, vars.map(grad), dir1, dir2)
    }

    val (x, y, dy) = new SecondOrderDescent(fgdir, 100, Array(101.0, 100.0)).descent()
    println(x.toList + "\t" + y + "\t" + dy)
    dy should be < eps
    y should be < fgdir(Array(0, 0))._1 + eps
  }

}
