package net.stoerr.learning2.network

import net.stoerr.learning2.common.DoubleArrayVector._
import net.stoerr.learning2.network.TestingHelper._
import org.scalatest._

/** Tests Buildingblocks */
// http://www.scalatest.org/user_guide/using_matchers
class TestBuildingblock extends FlatSpec with Matchers {

  "MatrixMultiply" should "pass the explicit check" in {
    val b = MatrixMultiply(3, 2)
    val params = Array(1.0, 2, 3, 4, 5, 6)
    val in = Array(1.0, 2, 3)
    val outDeriv = Array(2.0, 1.0)
    b(params)(in) should be(closeTo(Array(14.0, 32)))

    {
      val funcOfParam: Array[Double] => Double = i => b(i)(in) * outDeriv
      funcOfParam(params) should be(60)
      val paramDeriv = gradient(funcOfParam, params)
      paramDeriv should be(closeTo(Array(2.0, 4, 6, 1, 2, 3)))
      b.parameterGradient(params, in, b(params)(in), outDeriv) should be(closeTo(paramDeriv))
    }

    {
      val funcOfInputs: Array[Double] => Double = i => b(params)(i) * outDeriv
      funcOfInputs(in) should be(60)
      val inputDeriv = gradient(funcOfInputs, in)
      inputDeriv should be(closeTo(Array(6.0, 9, 12)))
      b.inputGradient(params, in, b(params)(in), outDeriv) should be(closeTo(inputDeriv))
    }

    {
      def evalFuncGrad(out: Array[Double]) = (out * outDeriv, outDeriv)

      val (eval, evalGrad) = b.gradient(params, in, evalFuncGrad)
      eval should be(60)
      evalGrad should be(closeTo(Array(2.0, 4, 6, 1, 2, 3)))
    }

  }

  "MatrixMultiply" should behave like buildingBlock(MatrixMultiply(4, 3))

  "Sigmoid" should behave like buildingBlock(Sigmoid(3))

  "Combined" should behave like buildingBlock(Combined(MatrixMultiply(2, 3), Sigmoid(3)))

  "A large combination" should behave like buildingBlock(MatrixMultiply(2, 10) + Sigmoid(10) + MatrixMultiply(10, 1))

  def buildingBlock(b: Buildingblock): Unit = {
    val in = randomVector(b.numInputs)
    val params = randomVector(b.numParameters)
    val outDeriv = randomVector(b.numOutputs)

    it should "calculate something" in {
      b(params)(in).abs shouldNot be(0.0)
    }

    if (!b.isInstanceOf[Combined]) {
      it should "have correct inputDerivative" in {
        val funcOfInputs: Array[Double] => Double = i => b(params)(i) * outDeriv
        funcOfInputs(in) shouldNot be(0.0)
        val inputDeriv = gradient(funcOfInputs, in)
        b.inputGradient(params, in, b(params)(in), outDeriv) should be(closeTo(inputDeriv))
      }

      it should "have correct parameterDerivative" in {
        val funcOfParam: Array[Double] => Double = i => b(i)(in) * outDeriv
        funcOfParam(params) shouldNot be(0.0)
        val paramDeriv = gradient(funcOfParam, params)
        paramDeriv.abs shouldNot be(0.0)
        b.parameterGradient(params, in, b(params)(in), outDeriv) should be(closeTo(paramDeriv))
      }
    }

    it should "have correct gradient" in {
      def evalFuncGrad(out: Array[Double]) = (out * outDeriv, outDeriv)

      def funcOfParam(i: Array[Double]) = b(i)(in) * outDeriv

      funcOfParam(params) shouldNot be(0.0)
      val paramDeriv = gradient(funcOfParam, params)
      paramDeriv.abs shouldNot be(0.0)

      val (eval, evalgrad) = b.gradient(params, in, evalFuncGrad)
      eval should be(b(params)(in) * outDeriv)
      evalgrad should be(closeTo(paramDeriv))
    }
  }

}
