package net.stoerr.learning2

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

  /** Partial derivative of all parameters wrt. something when partial derivative of outputs wrt that something is outputDerivative. */
  def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double]

  /** Partial derivative of inputs wrt. something when partial derivative of outputs wrt that something is outputDerivative. */
  def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double]

  def combine(other: Buildingblock): Buildingblock = new Buildingblock {
    require(self.numOutputs == other.numInputs)

    override val numInputs: Int = self.numInputs
    override val numOutputs: Int = other.numInputs
    override val numParameters: Int = self.numParameters + other.numParameters

    /** Partial derivative of all parameters wrt. something when partial derivative of outputs wrt that something is outputDerivative. */
    override def parameterDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double] = ???

    override def apply(inputs: Array[Double], parameters: Array[Double]): Array[Double] = ???

    /** Partial derivative of inputs wrt. something when partial derivative of outputs wrt that something is outputDerivative. */
    override def inputDerivative(outputDerivative: Array[Double], inputs: Array[Double]): Array[Double] = ???

  }

}
