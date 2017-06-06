package net.stoerr.learning2.network

import scala.language.{implicitConversions, postfixOps}
import net.stoerr.learning2.common.DoubleArrayVector._

/**
  * Models a buildingblock with some inputs, some outputs and some parameters (= usually weights)
  */
trait Buildingblock {

  val numInputs: Int
  val numOutputs: Int
  val numParameters: Int

  def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double]

  // def apply(inputs: Array[Array[Double]], parameters: Array[Double]): Array[Array[Double]] = inputs.map(this.apply(_, parameters))

  /** Partial derivative of something wrt. all parameters when partial derivative of that something wrt. all outputs is outputDerivative. */
  def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double]

  /** Partial derivative of something wrt. all inputs when partial derivative of that something wrt. all outputs is outputDerivative. */
  def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double]

  def +(other: Buildingblock) = Combined(this, other)

  def asDoubleFunction(params: Array[Double]): (Array[Double]) => Array[Double] = (in: Array[Double]) => apply(in, params)

}

case class Combined(first: Buildingblock, second: Buildingblock) extends Buildingblock {
  require(first.numOutputs == second.numInputs)

  override val numInputs: Int = first.numInputs
  override val numOutputs: Int = second.numOutputs
  override val numParameters: Int = first.numParameters + second.numParameters

  override def toString: String = first + "+" + second

  def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] = {
    require(inputs.length == numInputs)
    require(parameters.length == numParameters)
    val parameterSplitted = parameters.splitAt(first.numParameters)
    second(first(inputs, parameterSplitted._1), parameterSplitted._2)
  }

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] = {
    val parameterSplitted = parameters.splitAt(first.numParameters)
    val secondInputs = first.apply(inputs, parameterSplitted._1)
    val secondPDeriv = second.parameterDerivative(outputDerivative, secondInputs, parameterSplitted._2)
    val secondIDeriv = second.inputDerivative(outputDerivative, secondInputs, parameterSplitted._2)
    val firstPDeriv = first.parameterDerivative(secondIDeriv, inputs, parameterSplitted._1)
    firstPDeriv ++ secondPDeriv
  }

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] = {
    val parameterSplitted = parameters.splitAt(first.numParameters)
    val secondInputs = first.apply(inputs, parameterSplitted._1)
    val secondIDeriv = second.inputDerivative(outputDerivative, secondInputs, parameterSplitted._2)
    first.inputDerivative(secondIDeriv, inputs, parameterSplitted._1)
  }

}

/** Simulates a fully connected NN without the activation function (which we put into a separate block for simplicity. */
case class MatrixMultiply(numInputs: Int, numOutputs: Int) extends Buildingblock {
  override val numParameters: Int = numInputs * numOutputs

  override def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    parameters.grouped(numInputs).map(_ * inputs).toArray

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    outputDerivative.flatMap(inputs * _)

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    parameters.grouped(numInputs).toArray.zip(outputDerivative).map(Function.tupled(_ * _)).reduce(_ + _)

}

/** A sigmoid activation function, specifically tanh. As parameter we only use a shift - any multiplier can be simulated by the synapses. */
case class Sigmoid(numInputs: Int) extends Buildingblock {
  override val numOutputs: Int = numInputs
  override val numParameters: Int = numInputs

  def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] = (inputs zip parameters) map (t => Math.tanh(t._1 - t._2))

  override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    Array.tabulate(numInputs) { i =>
      val tanh = Math.tanh(inputs(i) - parameters(i))
      outputDerivative(i) * (tanh * tanh - 1)
    }

  override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double], parameters: Array[Double]): Array[Double] =
    Array.tabulate(numInputs) { i =>
      val tanh = Math.tanh(inputs(i) - parameters(i))
      outputDerivative(i) * (1 - tanh * tanh)
    }
}
