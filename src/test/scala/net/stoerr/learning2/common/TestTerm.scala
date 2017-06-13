package net.stoerr.learning2.common

import net.stoerr.learning2.common.Term._
import org.scalatest._
import DoubleArrayVector.derivation
import net.stoerr.learning2.network.TestingHelper._

import scala.language.implicitConversions

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
    eval('a / 'e + 'b * 'c - 5, Map('a -> 3, 'b -> 5, 'c -> 7, 'e -> 1.5)) should be(3.0 / 1.5 + 5.0 * 7.0 - 5)
    an[NoSuchElementException] should be thrownBy eval('a + 'b, Map('a -> 1))
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
    val valu = Map('a -> 2.0, 'b -> 3.0, 'c -> 5.0, 'd -> 7.0)
    val eg = evalWithGradient(t, valu)
    eg._1 should be(eval(t, valu))
    val vars = t.variables

    def f(v: Symbol)(x: Double) = eval(t, valu + (v -> (valu(v) + x)))

    val grad = vars.map(v => derivation(f(v.name), 0.0))
    vars.map(v => eg._2(v.name)).toArray should be(closeTo(grad.toArray))
  }

}
