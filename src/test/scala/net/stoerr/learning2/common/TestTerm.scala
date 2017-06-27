package net.stoerr.learning2.common

import net.stoerr.learning2.common.DoubleArrayVector.{derivation, derivation2, eps}
import net.stoerr.learning2.common.Term._
import net.stoerr.learning2.network.TestingHelper._
import org.scalatest._

import scala.language.implicitConversions
import scala.util.Random

class TestTerm extends FlatSpec with Matchers {

  "A Term" should "generate a sensible string representation" in {
    ('a + 1).toString should be("(a + 1.0)")
    ('a + 'b + 'c + (1 * 2 * Var('b) * 'c) / 5 - 7).toString should be("((a + b + c + ((2.0 * b * c) / 5.0)) - 7.0)")
  }

  it should "normalize properly" in {
    (('a + 'b) + (('c + ('x + 'y) + 'd) + ('e + 'f))).normalize.toString should be("(a + b + c + x + y + d + e + f)")
    (('a * 'b) * (('c * ('x * 'y) * 'd) * ('e * 'f))).normalize.toString should be("(a * b * c * x * y * d * e * f)")
  }

  it should "have an evaluation" in {
    val t = 'a / 'e + 'b * 'c - 5
    t.toString should be("(((a / e) + (b * c)) - 5.0)")
    eval('a / 'e + 'b * 'c - 5, Map('a -> 3.0, 'b -> 5.0, 'c -> 7.0, 'e -> 1.5).map(k => Var(k._1) -> k._2)) should be(3.0 / 1.5 + 5.0 * 7.0 - 5)
    an[NoSuchElementException] should be thrownBy eval('a + 'b, Map('a -> 1.0).map(k => Var(k._1) -> k._2))
  }

  it should "implement derive" in {
    derive('a + 'b * 2, 'b).toString should be("2.0")
    derive('a * 'b, 'b).toString should be("a")
    derive('a / 'b, 'b).toString should be("((-1.0 * a) / (b * b))")
    derive('a * 'a * 'a * 'b, 'a).toString should be("((a * a * b) + (a * a * b) + (a * a * b))")
    simplify(derive('a * 'a * 'a * 'b, 'a)).toString should be("(3.0 * a * a * b)")
    val t: Term = 'a / 'b + 'c * 'c
    t.subterms.toList.mkString(", ") should be("a, b, (a / b), c, c, (c * c), ((a / b) + (c * c))")
    t.variables.mkString(", ") should be("a, b, c")
    t.variables.map(v => simplify(simplify(derive(t, v)))).mkString(", ") should
      be("(b / (b * b)), ((-1.0 * a) / (b * b)), (2.0 * c)")
  }

  it should "implement evalWithGradient" in {
    val t = 'a / 'b + 'c * 'c * 'd
    val valu = Map('a -> 2.0, 'b -> 3.0, 'c -> 5.0, 'd -> 0.0).map(k => Var(k._1) -> k._2)
    val eg = evalWithGradient(t, valu)
    eg._1 should be(eval(t, valu))
    val vars = t.variables

    def f(v: Var)(x: Double) = eval(t, valu + (v -> (valu(v) + x)))

    val grad = vars.map(v => derivation(f(v.name), 0.0))
    vars.map(v => eg._2(v.name)).toArray should be(closeTo(grad.toArray))
  }

  ignore should "perform fast enough" in {
    val upper = 20
    val t = (for (i <- 0 until upper; j <- 0 until upper) yield Symbol("x" + i) * Symbol("p" + i + "t" + j)) reduce (_ + _)
    val vars = t.variables
    val valu = vars.map(v => v -> Random.nextGaussian()).toMap
    Timer.timing("egalWithGradient")(evalWithGradient(t, valu))
    Timer.timing("egalWithGradient")(evalWithGradient(t, valu))
    Timer.timing("egalWithGradient")(evalWithGradient(t, valu))
  }

  it should behave like dgradcheck('a + 2.5)
  it should behave like dgradcheck('a * 2.5)
  it should behave like dgradcheck('a + 'b)
  it should behave like dgradcheck('a * 'b)
  it should behave like dgradcheck('a - 'b)
  it should behave like dgradcheck('a / 'b)
  it should behave like dgradcheck('a + 'b + 2.5)
  it should behave like dgradcheck('a * 'b * 2.5)
  it should behave like dgradcheck(Term.random(3, 3))

  def dgradcheck(term: Term): Unit = {
    val vars = term.variables
    val valu = vars.map(v => (v, Random.nextInt(10) / 5.0 - 0.9)).toMap
    val (tval, tgrad, tderiv, tderiv2) = evalWithGradientAndDirectionalDerivations(term, valu)

    def f(v: Var)(x: Double) = eval(term, valu + (v -> (valu(v) + x)))

    val grad = vars.map(v => derivation(f(v.name), 0.0))

    def fdir(x: Double) = eval(term, vars.map(v => v -> (valu(v) + x * tgrad(v))).toMap)

    val fd1 = derivation(fdir, 0.0)
    val fd2 = derivation2(fdir, 0.0)

    term.toString + " - evalWithGradientAndDirectionalDerivations" should "have correct value" in (tval should be(f(vars.head)(0.0)))
    it should "have correct gradient" in (vars.map(v => tgrad(v.name)).toArray should be(closeTo(grad.toArray)))
    it should "have correct 1st deriv" in (tderiv should be(fd1 +- eps))
    it should "have correct 2nd deriv" in (tderiv2 should be(fd2 +- eps))
  }

}
