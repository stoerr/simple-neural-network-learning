package net.stoerr.learning2.algorithms

import org.scalatest.FunSuite
import net.stoerr.learning2.common.DValue
import net.stoerr.learning2.common.DValue._

import net.stoerr.learning2.common.DoubleArrayVector._
import net.stoerr.learning2.network._

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

  test("rpropnn") {
    val nn: Combined = MatrixMultiply(2, 3) + Sigmoid(3) + MatrixMultiply(3, 1)
    val examples = new ExampleSet(2, 1)
    examples += (Array(0.0, 0) -> Array(1))
    examples += (Array(1.0, 0) -> Array(0))
    examples += (Array(0.0, 1) -> Array(0))
    examples += (Array(1.0, 1) -> Array(1))
    var params = randomVector(nn.numParameters)
    val startfunc = nn.asDoubleFunction(params)
    for ((in, out) <- examples.examples) println("" + in.toList + " => " + startfunc(in).toList + " vs. " + out.toList)


  }

}
