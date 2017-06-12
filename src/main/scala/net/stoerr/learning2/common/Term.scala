package net.stoerr.learning2.common

import scala.language.implicitConversions

/** Models a term. */
sealed trait Term extends Comparable[Term] {
  def +(o: Term): Term = Sum(List(this, o)).normalize

  def -(o: Term): Term = Minus(this, o)

  def *(o: Term): Term = Product(List(this, o)).normalize

  def /(o: Term): Term = Quotient(this, o)

  def normalize: Term = this

  def immediateSubterms: Iterator[Term]

  def subterms: Iterator[Term] = immediateSubterms ++ Iterator(this)

  def variables = subterms.filter(_.isInstanceOf[Var])

  def subst(f: Term => Term): Term

  def substRules(f: PartialFunction[Term, Term]): Term = subst(t => if (f.isDefinedAt(t)) f(t) else t)

  override def compareTo(o: Term): Int = {
    var res = Term.types.indexOf(o.getClass) - Term.types.indexOf(this.getClass)
    if (res != 0) res else internalCompare(o.asInstanceOf[this.type])
  }

  protected def internalCompare(o: this.type): Int
}

/** Various term functions */
object Term {
  implicit def apply(constant: Double): Const = Const(constant)

  implicit def apply(constant: Int): Const = Const(constant)

  implicit def apply(name: Symbol): Var = Var(name)

  protected val types: List[Class[_]] = List(Const.getClass, Var.getClass, Sum.getClass, Minus.getClass, Product.getClass, Quotient.getClass)

  def apply[T <: Term](term: T): T = term

  def eval(term: Term, valuation: Map[Symbol, Double]): Double = term match {
    case Const(c) => c
    case Var(n) => valuation(n)
    case Sum(summands) => summands.map(eval(_, valuation)).sum
    case Minus(val1, val2) => eval(val1, valuation) - eval(val2, valuation)
    case Product(factors) => factors.map(eval(_, valuation)).product
    case Quotient(val1, val2) => eval(val1, valuation) / eval(val2, valuation)
  }

  def derive(term: Term, v: Var): Term = {
    def d(t: Term): Term = t match {
      case Const(c) => Const(0)
      case vn: Var => if (v == vn) Const(1) else Const(0)
      case Sum(summands) => Sum(summands.map(d))
      case Minus(val1, val2) => d(val1) - d(val2)
      case Product(factors) => factors match {
        case Nil => Const(0) // empty product means 1
        case a :: Nil => d(a)
        case a :: rest => d(a) * Product(rest) + a * d(Product(rest))
      }
      case Quotient(val1, val2) => (d(val1) * val2 - val1 * d(val2)) / (val2 * val2)
    }

    expand(d(term))
  }

  def expand(term: Term) = term.normalize.substRules({
    case Product(factors) if (factors.exists(_.isInstanceOf[Sum])) =>
      val sumlist: List[List[Term]] = factors map {
        case Sum(summands) => summands
        case t => List(t)
      }
      val summandCombinations: List[List[Term]] = sumlist
        .foldRight(List[List[Term]](Nil))((el, rest) => el.flatMap(p => rest.map(p :: _)))
      Sum(summandCombinations.map(Product(_)))
  }).normalize
}

case class Const(value: Double) extends Term {
  override def toString: String = value.toString

  override def immediateSubterms: Iterator[Term] = Iterator.empty

  override def subst(f: Term => Term): Term = f(this)

  override protected def internalCompare(o: this.type): Int = value.compareTo(o.value)
}

case class Var(name: Symbol) extends Term {
  override def toString: String = name.name

  override def immediateSubterms: Iterator[Term] = Iterator.empty

  override def subst(f: Term => Term): Term = f(this)

  override protected def internalCompare(o: this.type): Int = name.name.compareTo(o.name.name)
}

case class Sum(summands: List[Term]) extends Term {
  override def toString: String = "(" + summands.mkString(" + ") + ")"

  override def normalize: Term = {
    val flattened = summands map (_.normalize) flatMap {
      case t: Sum => t.summands
      case other => List(other)
    } filterNot (_ == Const(0))
    if (flattened.isEmpty) this else if (flattened.length == 1) flattened(0) else Sum(flattened)
  }

  override def immediateSubterms: Iterator[Term] = summands.toIterator

  override def subst(f: Term => Term): Term = f(Sum(summands map f))

  override protected def internalCompare(o: this.type): Int =
    (summands.view, o.summands.view).zipped.map(_.compareTo(_)).find(_ != 0).getOrElse(0)
}

case class Minus(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " - " + value2 + ")"

  override def immediateSubterms: Iterator[Term] = Iterator(value1, value2)

  override def subst(f: Term => Term): Term = f(Minus(f(value1), f(value2)))

  override protected def internalCompare(o: this.type): Int =
    Ordering[(Term, Term)].compare((this.value1, this.value2), (o.value1, o.value2))
}

case class Product(factors: List[Term]) extends Term {
  override def toString: String = "(" + factors.mkString(" * ") + ")"

  override def normalize: Term = {
    val flattened = factors map (_.normalize) flatMap {
      case t: Product => t.factors
      case other => List(other)
    } filterNot (_ == Const(1))
    if (flattened.contains(Const(0))) Const(0)
    else if (flattened.isEmpty) this
    else if (flattened.length == 1) flattened(0)
    else Product(flattened)
  }

  override def immediateSubterms: Iterator[Term] = factors.iterator

  override def subst(f: Term => Term): Term = f(Product(factors map f))

  override protected def internalCompare(o: this.type): Int =
    (factors.view, o.factors.view).zipped.map(_.compareTo(_)).find(_ != 0).getOrElse(0)
}

case class Quotient(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " / " + value2 + ")"

  override def immediateSubterms: Iterator[Term] = Iterator(value1, value2)

  override def subst(f: Term => Term): Term = f(Quotient(f(value1), f(value2)))

  override protected def internalCompare(o: this.type): Int =
    Ordering[(Term, Term)].compare((this.value1, this.value2), (o.value1, o.value2))
}
