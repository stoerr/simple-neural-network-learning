package net.stoerr.learning2.common

import scala.language.implicitConversions

/** Models a term. */
sealed trait Term {
  def +(o: Term): Term = Sum(List(this, o)).normalize

  def -(o: Term): Term = Minus(this, o)

  def *(o: Term): Term = Product(List(this, o)).normalize

  def /(o: Term): Term = Quotient(this, o)

  def normalize: Term = this
}

/** Various term functions */
object Term {
  implicit def apply(constant: Double): Const = Const(constant)

  implicit def apply(constant: Int): Const = Const(constant)

  implicit def apply(name: Symbol): Var = Var(name)

  def apply[T <: Term](term: T): T = term
}

case class Const(value: Double) extends Term {
  override def toString: String = value.toString
}

case class Var(name: Symbol) extends Term {
  override def toString: String = name.name
}

case class Sum(summands: List[Term]) extends Term {
  override def toString: String = "(" + summands.mkString(" + ") + ")"

  override def normalize: Term = {
    val flattened = summands map (_.normalize) flatMap {
      case t: Sum => t.summands
      case other => List(other)
    }
    if (flattened.isEmpty) this else if (flattened.length == 1) flattened(0) else Sum(flattened)
  }
}

case class Minus(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " - " + value2 + ")"
}

case class Product(factors: List[Term]) extends Term {
  override def toString: String = "(" + factors.mkString(" * ") + ")"

  override def normalize: Term = {
    val flattened = factors map (_.normalize) flatMap {
      case t: Product => t.factors
      case other => List(other)
    }
    if (flattened.isEmpty) this else if (flattened.length == 1) flattened(0) else Product(flattened)
  }
}

case class Quotient(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " / " + value2 + ")"
}
