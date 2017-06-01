package net.stoerr.learning2

import language.postfixOps
import language.implicitConversions

import common.DoubleArrayVector._

/**
  * Models a buildingblock with some inputs, some outputs and some parameters (= usually weights)
  */
trait Buildingblock {
  self =>

  val numInputs: Int
  val numOutputs: Int
  val numParameters: Int

  def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double]

  // def apply(inputs: Array[Array[Double]], parameters: Array[Double]): Array[Array[Double]] = inputs.map(this.apply(_, parameters))

  /** Partial derivative of something wrt. all parameters when partial derivative of that something wrt. all outputs is outputDerivative. */
  def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double]

  /** Partial derivative of something wrt. all inputs when partial derivative of that something wrt. all outputs is outputDerivative. */
  def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double]

}

case class Combined(first: Buildingblock, second: Buildingblock) extends Buildingblock {
  require(first.numOutputs == second.numInputs)

  override val numInputs: Int = first.numInputs
  override val numOutputs: Int = second.numOutputs
  override val numParameters: Int = first.numParameters + second.numParameters

  def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] = {
    require(inputs.length == numInputs)
    require(parameters.length == numParameters)
    val parameterSplitted = parameters.splitAt(first.numParameters)
    second(first(inputs, parameterSplitted._1), parameterSplitted._2)
  }

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] = ???

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] = ???
}

case class MatrixMultiply(numInputs: Int, numOutputs: Int) extends Buildingblock {
  override val numParameters: Int = numInputs * numOutputs

  override def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    parameters.grouped(numInputs).map(_ * inputs).toArray

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    outputDerivative.flatMap(inputs * _)

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    parameters.grouped(numInputs).toArray.zip(outputDerivative).map(Function.tupled(_ * _)).reduce(_ + _)


}
