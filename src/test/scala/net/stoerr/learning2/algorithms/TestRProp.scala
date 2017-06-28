package net.stoerr.learning2.algorithms

import net.stoerr.learning2.common.DValue
import net.stoerr.learning2.common.DoubleArrayVector._
import net.stoerr.learning2.common.DValue._
import net.stoerr.learning2.network._
import org.scalatest._

class TestRProp extends FlatSpec with Matchers {

  val eps = 1e-7

  "RProp" should "descent on a given function" in {
    def fd(args: Array[DValue]) = {
      val (x, y) = (args(0), args(1))
      (x - DValue(1.0)) ** 2 + (y - DValue(2.0)) ** 2 * DValue(2.0) + (x - y + DValue(1.0)) ** 4
    }

    val f: (Array[Double]) => Double = asDoubleFunction(fd)
    val fgrad: (Array[Double]) => (Double, Array[Double]) = asDoubleFunctionWithGradient(fd)
    val (x, y, dy) = new RProp(f, fgrad, 100, Array(101.0, 100.0)).descent()
    println(x.toList + "\t" + y + "\t" + dy)
    dy should be < eps
    y should be < f(Array(0, 0)) + eps
  }

  it should "descent on a neural network" in {
    val nn: Combined = MatrixMultiply(2, 10) + Sigmoid(10) + MatrixMultiply(10, 1)
    val examples = new ExampleSet(2, 1)
    examples += (Array(0.0, 0) -> Array(1))
    examples += (Array(1.0, 0) -> Array(0))
    examples += (Array(0.0, 1) -> Array(0))
    examples += (Array(1.0, 1) -> Array(1))
    var startparams = randomVector(nn.numParameters)
    val startfunc: (Array[Double]) => Array[Double] = nn(startparams)
    for ((in, out) <- examples.examples) println("" + in.toList + " => " + startfunc(in).toList + " vs. " + out.toList)

    val f: (Array[Double]) => Double = examples.evaluation(nn)
    val fgrad: (Array[Double]) => (Double, Array[Double]) = examples.evaluationWithGradient(nn)
    val (bestparams, bestvalue, lastchange) = new RProp(f, fgrad, 100, startparams).descent()
    println(s"${bestvalue} lastchange ${lastchange} at ${bestparams.toList}")
    bestvalue.abs should be < eps
  }

}
