package net.stoerr.learning2.network

import scala.language.postfixOps
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import net.stoerr.learning2.common.DoubleArrayVector._

/** Set of examples for a neural net or something. */
class ExampleSet(val numInputs: Int, val numOutputs: Int) {

  val examples: mutable.Buffer[(Array[Double], Array[Double])] = new ArrayBuffer[(Array[Double], Array[Double])]()

  def +=(inAndOut: (Array[Double], Array[Double])): this.type = {
    examples += inAndOut;
    this
  }

  private def sqr(x: Double) = x * x

  def evaluation(nn: Buildingblock)(params: Array[Double]): Double =
    examples map { case (in, out) => nn.asDoubleFunction(params)(in) - out } map (_.sqr) sum

  def evaluationWithGradient(nn: Buildingblock)(params: Array[Double]): (Double, Array[Double]) = {
    val exampleGrads = examples map { case (in, out) =>
      val outDif = nn.asDoubleFunction(params)(in) - out
      val grad = nn.parameterDerivative(outDif * 2, in, params)
      (outDif.sqr, grad)
    }
    (exampleGrads.map(_._1).sum, exampleGrads.map(_._2).reduce(_ + _))
  }

}