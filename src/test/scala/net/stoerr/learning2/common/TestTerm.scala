package net.stoerr.learning2.common

import net.stoerr.learning2.common.Term._
import org.scalatest._

import scala.language.implicitConversions

class TestTerm extends FlatSpec with Matchers {

  "A Term" should "generate a sensible string representation" in {
    ('a + 1).toString should be("(a + 1.0)")
    ('a + 'b + 'c + (1 * 2 * Var('b) * 'c) / 5 - 7).toString should be("((a + b + c + ((2.0 * b * c) / 5.0)) - 7.0)")
  }

  it should "normalize properly" in {
    (('a + 'b) + (('c + ('x + 'y) + 'd) + ('e + 'f))).normalize.toString should be ("(a + b + c + x + y + d + e + f)")
    (('a * 'b) * (('c * ('x * 'y) * 'd) * ('e * 'f))).normalize.toString should be ("(a * b * c * x * y * d * e * f)")
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
    derive('a / 'b, 'b).toString should be("((0.0 - a) / (b * b))")
    derive('a * 'a * 'a * 'b, 'a).toString should be("((a * a * b) + (a * a * b) + (a * a * b))")
  }

}
