package net.stoerr.learning2.common

import scala.language.implicitConversions

import org.scalatest._
import Term.apply

class TestTerm extends FlatSpec with Matchers {

  "A Term" should "generate a sensible string representation" in {
    (Var("a") + 1).toString should be("(a + 1.0)")
    (Term("a") + 1).toString should be("(a + 1.0)")
    (Term("a") + "b" + "c" + (1 * Var("b") * "c") / 5 - 7).toString should be("((a + b + c + ((1.0 * b * c) / 5.0)) - 7.0)")
  }

}
