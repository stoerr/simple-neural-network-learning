package net.stoerr.learning2.common

import scala.language.implicitConversions

/** Models a term. */
sealed trait Term {
  def +(o: Term): Term = Sum(List(this, o)).normalize

  def -(o: Term): Term = Minus(this, o)

  def *(o: Term): Term = Product(List(this, o)).normalize

  def /(o: Term): Term = Quotient(this, o)

  def normalize: Term = this

  def immediateSubterms: Iterator[Term]

  def subterms: Iterator[Term] = Iterator(this) ++ immediateSubterms

  def variables = subterms.filter(_.isInstanceOf[Var])
}

/** Various term functions */
object Term {
  implicit def apply(constant: Double): Const = Const(constant)

  implicit def apply(constant: Int): Const = Const(constant)

  implicit def apply(name: Symbol): Var = Var(name)

  def apply[T <: Term](term: T): T = term

  def eval(term: Term, valuation: Map[Symbol, Double]): Double = term match {
    case Const(c) => c
    case Var(n) => valuation(n)
    case Sum(summands) => summands.map(eval(_, valuation)).sum
    case Minus(val1, val2) => eval(val1, valuation) - eval(val2, valuation)
    case Product(factors) => factors.map(eval(_, valuation)).product
    case Quotient(val1, val2) => eval(val1, valuation) / eval(val2, valuation)
  }
}

case class Const(value: Double) extends Term {
  override def toString: String = value.toString

  override def immediateSubterms: Iterator[Term] = Iterator.empty
}

case class Var(name: Symbol) extends Term {
  override def toString: String = name.name

  override def immediateSubterms: Iterator[Term] = Iterator.empty
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

  override def immediateSubterms: Iterator[Term] = summands.toIterator
}

case class Minus(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " - " + value2 + ")"

  override def immediateSubterms: Iterator[Term] = Iterator(value1, value2)
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

  override def immediateSubterms: Iterator[Term] = factors.iterator
}

case class Quotient(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " / " + value2 + ")"

  override def immediateSubterms: Iterator[Term] = Iterator(value1, value2)
}
