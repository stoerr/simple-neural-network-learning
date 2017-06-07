package net.stoerr.learning2.network

import net.stoerr.learning2.common.DoubleArrayVector._
import org.scalatest._
import TestingHelper._

/** Tests Buildingblocks */
// http://www.scalatest.org/user_guide/using_matchers
class TestBuildingblock extends FlatSpec with Matchers {

  "MatrixMultiply" should "pass the explicit check" in {
    val b = MatrixMultiply(3, 2)
    val params = Array(1.0, 2, 3, 4, 5, 6)
    val in = Array(1.0, 2, 3)
    val outDeriv = Array(2.0, 1.0)
    b(in, params) should be(closeTo(Array(14.0, 32)))

    {
      val funcOfParam: Array[Double] => Double = i => b(in, i) * outDeriv
      funcOfParam(params) should be(60)
      val paramDeriv = gradient(funcOfParam, params)
      paramDeriv should be(closeTo(Array(2.0, 4, 6, 1, 2, 3)))
      b.parameterDerivative(outDeriv, in, params) should be(closeTo(paramDeriv))
    }

    {
      val funcOfInputs: Array[Double] => Double = i => b(i, params) * outDeriv
      funcOfInputs(in) should be(60)
      val inputDeriv = gradient(funcOfInputs, params)
      inputDeriv should be(closeTo(Array(6.0, 9, 12)))
      b.inputDerivative(outDeriv, in, params) should be(closeTo(inputDeriv))
    }

  }

  "MatrixMultiply" should behave like buildingBlock(MatrixMultiply(4, 3))

  "Sigmoid" should behave like buildingBlock(Sigmoid(3))

  "Combined" should behave like buildingBlock(Combined(MatrixMultiply(2, 3), Sigmoid(3)))

  def buildingBlock(b: Buildingblock): Unit = {
    val in = randomVector(b.numInputs)
    val params = randomVector(b.numParameters)
    val outDeriv = randomVector(b.numOutputs)

    it should "calculate something" in {
      b(in, params).abs shouldNot be(0.0)
    }

    it should "have correct parameterDerivative" in {
      val funcOfParam: Array[Double] => Double = i => b(in, i) * outDeriv
      funcOfParam(params) shouldNot be(0.0)
      val paramDeriv = gradient(funcOfParam, params)
      paramDeriv.abs shouldNot be(0.0)
      b.parameterDerivative(outDeriv, in, params) should be(closeTo(paramDeriv))
    }

    it should "have correct inputDerivative" in {
      val funcOfInputs: Array[Double] => Double = i => b(i, params) * outDeriv
      funcOfInputs(in) shouldNot be(0.0)
      val inputDeriv = gradient(funcOfInputs, in)
      b.inputDerivative(outDeriv, in, params) should be(closeTo(inputDeriv))
    }
  }

}