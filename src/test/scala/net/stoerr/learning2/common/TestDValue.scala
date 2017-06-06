package net.stoerr.learning2.common

import org.scalatest.FunSuite

import scala.math._

/**
 * Tests for {@link DValue}
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.10.2014
 */
class TestDValue extends FunSuite {

  test("just print something") {
    println(DValue(1.5))
    println(DValue(2.7, "v"))
    val v1 = DValue(1.1, "v1")
    val v2 = DValue(2.3, "v2")
    println(v1 + v2)
    println(v1 + v2 + v2)
    println(v1 + v2 + v1 + v2)
    println(v1 * v2.abs / v1.log)
  }

  val eps = 1e-7

  def deriv(f: Double => Double, x: Double) = (f(x + eps) - f(x - eps)) / (2 * eps)

  test("Try to estimate value") {
    def complicated(xr: Double, yr: Double) = {
      val x = DValue(xr, "x")
      val y = DValue(yr, "y")
      (x * DValue(-3)).abs + y / (x.abs * y.log - y)
    }
    val x = 1.5
    val y = 2.7
    val res = abs(x * -3) + y / (abs(x) * log(y) - y)
    val dres: DValue = complicated(x, y)
    println(dres)
    assert(res == dres.value)
    assert(0 == dres.deriv("z"))
    assert(2 == dres.derivations.size)
    println(deriv(complicated(_, y).value, x))
    println(deriv(complicated(x, _).value, y))
    assert(abs(deriv(complicated(_, y).value, x) - dres.deriv("x")) < eps)
    assert(abs(deriv(complicated(x, _).value, y) - dres.deriv("y")) < eps)
  }

  test("Thorough test") {
    checkFunction(_ + _, _ + _)
    checkFunction(DValue(1) + _ + _, 1 + _ + _)
    checkFunction(_ - _, _ - _)
    checkFunction(_ * _, _ * _)
    checkFunction(_ / _, _ / _)
    checkFunction(_.abs + _, abs(_) + _)
    checkFunction(_.cosh + _, cosh(_) + _)
    checkFunction(_.tanh + _, tanh(_) + _)
    checkFunction((x: DValue, y: DValue) => x.abs.log, (x: Double, y: Double) => log(abs(x)))
    checkFunction((x: DValue, y: DValue) => x / x, (x: Double, y: Double) => 1)
    checkFunction((x: DValue, y: DValue) => (x + DValue(2)).abs.log / y * (x - y), (x: Double, y: Double) => log(abs(x + 2)) / y * (x - y))
  }

  def checkFunction(fd: (DValue, DValue) => DValue, f: (Double, Double) => Double): Unit = {
    checkFunctionSingle(fd, f)
    checkFunctionSingle((x: DValue, y: DValue) => fd(y, x), (x: Double, y: Double) => f(y, x))
  }

  def checkFunctionSingle(fd: (DValue, DValue) => DValue, f: (Double, Double) => Double) {
    for (xr <- Array(-1.23, -0.618, 0.834, 1.879);
         yr <- Array(-2.423, -0.917, 0.4843, 4.2343)) {
      val x = DValue(xr, "x")
      val y = DValue(yr, "y")
      val resr: Double = f(xr, yr)
      val resd: DValue = fd(x, y)
      assert(resd.value == resr)
      assert(0 == resd.deriv("z"))
      assert(2 >= resd.derivations.size)
      assert(abs(deriv(f(_, yr), xr) - resd.deriv("x")) < 100 * eps)
      assert(abs(deriv(f(xr, _), yr) - resd.deriv("y")) < 100 * eps)
    }
  }

}
