package net.stoerr.learning2.algorithms

import org.scalatest.FunSuite
import net.stoerr.learning2.common.DValue
import net.stoerr.learning2.common.DValue._

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.12.2014
 */
class TestRProp extends FunSuite {

  val eps = 1e-7

  test("rprop") {
    def fd(args: Array[DValue]) = {
      // minimum (0,0)
      val dif = args(0) - args(1)
      dif.cosh + (args(0) * args(0) + args(1) * args(1))
    }
    val f: (Array[Double]) => Double = asDoubleFunction(fd)
    val fgrad: (Array[Double]) => (Double, Array[Double]) = asDoubleFunctionWithGradient(fd)
    val (x, y, dy) = new RProp(f, fgrad, 100, Array(1001.0, 1000.0)).descent()
    println(x.toList + "\t" + y + "\t" + dy)
  }

}
