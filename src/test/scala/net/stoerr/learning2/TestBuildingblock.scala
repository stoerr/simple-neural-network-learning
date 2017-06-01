package net.stoerr.learning2

import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import net.stoerr.learning2.common.DoubleArrayVector._

/** Tests Buildingblocks */
// http://www.scalatest.org/user_guide/using_matchers
class TestBuildingblock extends FlatSpec with Matchers {

  def closeTo(value: Array[Double]) = new BeMatcher[Array[Double]] {
    def apply(left: Array[Double]) =
      MatchResult(
        (value - left).abs < eps,
        "vectors not close enough:\n" + value.mkString(",") + "\n" + left.mkString(","),
        "vectors close enough:\n" + value.mkString(",") + "\n" + left.mkString(",")
      )
  }

  "MatrixMultiply" should "calculate values" in {
    val params = Array(1.0, 2, 3, 4, 5, 6)
    val in = Array(1.0, 2, 3)
    val b = MatrixMultiply(3, 2)
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

}
