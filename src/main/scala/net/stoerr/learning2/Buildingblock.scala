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

  def apply(inputs: Array[Array[Double]], parameters: Array[Double]): Array[Array[Double]] = inputs.map(this.apply(_, parameters))

  /** Partial derivative of something wrt. all parameters when partial derivative of that something wrt. all outputs is outputDerivative. */
  def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double]

  /** Partial derivative of something wrt. all inputs when partial derivative of that something wrt. all outputs is outputDerivative. */
  def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double]

}

case class combined(first: Buildingblock, second: Buildingblock) extends Buildingblock {
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

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double] = ???

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double] = ???
}

case class matrixMultiply(numInputs: Int, numOutputs: Int, parameters: Array[Double]) extends Buildingblock {
  require(numInputs * numOutputs == parameters.length)
  override val numParameters = parameters.length
  private val parameterMatrix = parameters.grouped(numInputs).toArray

  override def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    parameterMatrix.map(_ * parameters)

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double] =
    inputs.toBuffer.flatMap(outputDerivative * _).toArray

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double] =
    parameterMatrix map (_ * outputDerivative)
}
