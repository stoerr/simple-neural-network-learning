package net.stoerr.learning2.network

import net.stoerr.learning2.common.DoubleArrayVector._

import scala.language.{implicitConversions, postfixOps}

/**
  * Models a buildingblock with some inputs, some outputs and some parameters (= usually weights)
  */
trait Buildingblock {

  val numInputs: Int
  val numOutputs: Int
  val numParameters: Int

  def apply(parameters: Array[Double])(inputs: Array[Double]): Array[Double]

  def +(other: Buildingblock) = Combined(this, other)

  // def apply(inputs: Array[Array[Double]], parameters: Array[Double]): Array[Array[Double]] = inputs.map(this.apply(_, parameters))

  /** Partial derivative of something wrt. all parameters when partial derivative of that something wrt. all outputs is outputDerivative. */
  def parameterGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double]

  /** Partial derivative of something wrt. all inputs when partial derivative of that something wrt. all outputs is outputDerivative. */
  def inputGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double]

  /** For efficient calculation of gradients for evaluation of a chain of units.
    *
    * @param parameters the parameters for us
    * @param chain      function of our outputs to the evaluation, the gradient of the evaluation wrt. our outputs and a gradient of the evaluation wrt. parameters of the rest of the chain
    * @return function of our inputs to the evaluation, the gradient of the evaluation wrt. our inputs and a gradient of the evaluation wrt. parameters of us <b>and</b> the rest of the chain
    */
  def gradientChain(parameters: Array[Double], chain: Array[Double] => (Double, Array[Double], Array[Double])): Array[Double] => (Double, Array[Double], Array[Double]) = {
    require(parameters.length == numParameters, "parameters " + parameters.length + " but expected " + numParameters)
    (inputs: Array[Double]) => {
      require(inputs.length == numInputs, "inputs " + inputs.length + " but expected " + numInputs)
      val outputs = this (parameters)(inputs)
      val (evaluation, evalInputGradient, evalParameterGradient) = chain(outputs)
      (evaluation, inputGradient(parameters, inputs, outputs, evalInputGradient),
        parameterGradient(parameters, inputs, outputs, evalInputGradient) ++ evalParameterGradient)
    }
  }

  /** Calculates an evaluation and its gradients.
    *
    * @param parameters       the parameters for us
    * @param evalWithGradient function of our outputs to the evaluation and the gradient of the evaluation wrt. our outputs
    * @return the evaluation, the gradient of the evaluation wrt. our parameters
    */
  def gradient(parameters: Array[Double], inputs: Array[Double], evalWithGradient: Array[Double] => (Double, Array[Double])): (Double, Array[Double]) = {
    require(inputs.length == numInputs, "inputs " + inputs.length + " but expected " + numInputs)
    require(parameters.length == numParameters, "parameters " + parameters.length + " but expected " + numParameters)
    val chain = evalWithGradient.andThen(eg => (eg._1, eg._2, Array[Double]()))
    gradientChain(parameters, chain).andThen(g => (g._1, g._3))(inputs)
  }
}

case class Combined(first: Buildingblock, second: Buildingblock) extends Buildingblock {
  require(first.numOutputs == second.numInputs)

  override val numInputs: Int = first.numInputs
  override val numOutputs: Int = second.numOutputs
  override val numParameters: Int = first.numParameters + second.numParameters

  override def toString: String = first + "+" + second

  def apply(parameters: Array[Double])(inputs: Array[Double]): Array[Double] = {
    require(inputs.length == numInputs, "inputs " + inputs.length + " but expected " + numInputs)
    require(parameters.length == numParameters, "parameters " + parameters.length + " but expected " + numParameters)
    val parameterSplitted = parameters.splitAt(first.numParameters)
    second(parameterSplitted._2)(first(parameterSplitted._1)(inputs))
  }

  /** not implemented since it would be horribly inefficient and gradientChain works otherwise. */
  override def parameterGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double] = ???

  /** not implemented since it would be horribly inefficient and gradientChain works otherwise. */
  override def inputGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double] = ???

  override
  def gradientChain(parameters: Array[Double], chain: Array[Double] => (Double, Array[Double], Array[Double])): (Array[Double]) => (Double, Array[Double], Array[Double]) = {
    require(parameters.length == numParameters, "parameters " + parameters.length + " but expected " + numParameters)
    val parameterSplitted = parameters.splitAt(first.numParameters)
    first.gradientChain(parameterSplitted._1, second.gradientChain(parameterSplitted._2, chain))
  }

}

/** Simulates a fully connected NN without the activation function (which we put into a separate block for simplicity. */
case class MatrixMultiply(numInputs: Int, numOutputs: Int) extends Buildingblock {
  override val numParameters: Int = numInputs * numOutputs

  override def apply(parameters: Array[Double])(inputs: Array[Double]): Array[Double] = {
    require(inputs.length == numInputs, "inputs " + inputs.length + " but expected " + numInputs)
    require(parameters.length == numParameters, "parameters " + parameters.length + " but expected " + numParameters)
    parameters.grouped(numInputs).map(_ * inputs).toArray
  }

  override def parameterGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double] =
    outputDerivative.flatMap(inputs * _)

  override def inputGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double] =
    parameters.grouped(numInputs).toArray.zip(outputDerivative).map(Function.tupled(_ * _)).reduce(_ + _)

}

/** A sigmoid activation function, specifically tanh. As parameter we only use a shift - any multiplier can be simulated by the synapses. */
case class Sigmoid(numInputs: Int) extends Buildingblock {
  override val numOutputs: Int = numInputs
  override val numParameters: Int = numInputs

  def apply(parameters: Array[Double])(inputs: Array[Double]): Array[Double] = {
    require(inputs.length == numInputs, "inputs " + inputs.length + " but expected " + numInputs)
    require(parameters.length == numParameters, "parameters " + parameters.length + " but expected " + numParameters)
    (inputs zip parameters) map (t => Math.tanh(t._1 - t._2))
  }

  override def parameterGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double] =
    Array.tabulate(numInputs) { i =>
      val tanh = calculatedOutputs(i)
      outputDerivative(i) * (tanh * tanh - 1)
    }

  override def inputGradient(parameters: Array[Double], inputs: Array[Double], calculatedOutputs: Array[Double], outputDerivative: Array[Double]): Array[Double] =
    Array.tabulate(numInputs) { i =>
      val tanh = calculatedOutputs(i)
      outputDerivative(i) * (1 - tanh * tanh)
    }
}
