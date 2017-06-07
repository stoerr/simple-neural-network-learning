package net.stoerr.learning2.network


import net.stoerr.learning2.common.DoubleArrayVector._
import net.stoerr.learning2.network.TestingHelper._
import org.scalatest._

class TestExampleSet extends FlatSpec with Matchers {

  "An Exampleset" should "calculate the correct derivations" in {
    val exampleSet = new ExampleSet(3, 2)
    for (i <- 0 until 5) exampleSet += randomVector(3) -> randomVector(2)
    val nn = MatrixMultiply(3, 2) + Sigmoid(2) + MatrixMultiply(2, 2)
    val params = randomVector(nn.numParameters)
    exampleSet.evaluation(nn)(params) shouldNot be(0.0)
    val (value: Double, grad: Array[Double]) = exampleSet.evaluationWithGradient(nn)(params)
    value should be(exampleSet.evaluation(nn)(params))
    val realgradient: Array[Double] = gradient(exampleSet.evaluation(nn), params)
    grad should be(closeTo(realgradient))
  }

}
