package net.stoerr.learning2

import org.scalatest._

/** Tests Buildingblocks */
// http://www.scalatest.org/user_guide/using_matchers
class TestBuildingblock extends FlatSpec with Matchers {

  "MatrixMultiply" should "calculate values" in {
    val params = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val in = Array(1.0, 2.0, 3.0)
    val b = MatrixMultiply(3, 2)
    b(in, params) should equal (Array(14.0, 32.0))
    // b.parameterDerivative(Array(2.0, 1.0), in) should equal (Array(1.0, 2.0))
  }

}
