package net.stoerr.learning2.common

import scala.language.implicitConversions

/** Models a term. */
sealed trait Term extends Comparable[Term] {
  def +(o: Term): Term = Sum(Vector(this, o)).normalize

  def -(o: Term): Term = Minus(this, o)

  def *(o: Term): Term = Product(Vector(this, o)).normalize

  def /(o: Term): Term = Quotient(this, o)

  def normalize: Term = this

  def immediateSubterms: Iterator[Term]

  def subterms: Iterator[Term] = immediateSubterms.flatMap(_.subterms) ++ Iterator(this)

  def variables: Vector[Var] = subterms.filter(_.isInstanceOf[Var]).toSet.toVector.sorted.map(_.asInstanceOf[Var])

  def subst(f: Term => Term): Term

  def substRules(f: PartialFunction[Term, Term]): Term = subst(t => if (f.isDefinedAt(t)) f(t) else t)

  override def compareTo(other: Term): Int = {
    val res = Term.types.indexOf(this.getClass) - Term.types.indexOf(other.getClass)
    if (res != 0) res else internalCompare(other.asInstanceOf[this.type])
  }

  protected def internalCompare(o: this.type): Int
}

/** Various term functions */
object Term {

  implicit def apply(constant: Double): Const = Const(constant)

  implicit def apply(constant: Int): Const = Const(constant)

  implicit def apply(name: Symbol): Var = Var(name)

  protected val types: Vector[Class[_]] = Vector(classOf[Const], classOf[Var], classOf[Sum], classOf[Minus], classOf[Product], classOf[Quotient])

  def apply[T <: Term](term: T): T = term

  def eval(term: Term, valuation: Map[Symbol, Double]): Double = term match {
    case Const(c) => c
    case Var(n) => valuation(n)
    case Sum(summands) => summands.map(eval(_, valuation)).sum
    case Minus(val1, val2) => eval(val1, valuation) - eval(val2, valuation)
    case Product(factors) => factors.map(eval(_, valuation)).product
    case Quotient(val1, val2) => eval(val1, valuation) / eval(val2, valuation)
  }

  def evalWithGradient(term: Term, valuation: Map[Symbol, Double]): (Double, Map[Symbol, Double]) = term match {
    case Const(c) => (c, Map())
    case Var(n) => (valuation(n), Map(n -> 1.0))
    case Sum(summands) =>
      val parts = summands.map(evalWithGradient(_, valuation))
      (parts.map(_._1).sum, valuation.keys.map(k => k -> parts.map(_._2.getOrElse(k, 0.0)).sum).toMap)
    case Minus(val1, val2) =>
      val (p1, p2) = (evalWithGradient(val1, valuation), evalWithGradient(val2, valuation))
      (p1._1 - p2._1, valuation.keys.map(k => k -> (p1._2.getOrElse(k, 0.0) - p2._2.getOrElse(k, 0.0))).toMap)
    case Product(factors) =>
      val fg: Vector[(Double, Map[Symbol, Double])] = factors.map(evalWithGradient(_, valuation))
      (fg.map(_._1).product, valuation.keys.map(k => k ->
        alternateChoose(fg.map(_._1), fg.map(_._2.getOrElse(k, 0.0))).map(_.product).sum).toMap)
    case Quotient(val1, val2) =>
      val (p1, p2) = (evalWithGradient(val1, valuation), evalWithGradient(val2, valuation))
      (p1._1 / p2._1, valuation.keys.map(k => k -> ((p1._2.getOrElse(k, 0.0) * p2._1 - p2._2.getOrElse(k, 0.0) * p1._1) / (p2._1 * p2._1))).toMap)
  }

  def derive(term: Term, v: Var): Term = {
    def d(t: Term): Term = t match {
      case Const(_) => Const(0)
      case vn: Var => if (v == vn) Const(1) else Const(0)
      case Sum(summands) => Sum(summands.map(d))
      case Minus(val1, val2) => d(val1) - d(val2)
      case Product(factors) => Sum(alternateChoose(factors, factors.map(derive(_, v))).map(Product))
      case Quotient(val1, val2) => (d(val1) * val2 - val1 * d(val2)) / (val2 * val2)
    }

    expand(d(term))
  }

  private def alternateChoose[U](v1: Vector[U], v2: Vector[U]): Vector[Vector[U]] = {
    val v = v1.zip(v2).zipWithIndex
    v.indices.map(i => v.map(t => if (i == t._2) t._1._2 else t._1._1)).toVector
  }

  def expand(term: Term): Term = term.normalize.substRules({
    case Product(factors) if factors.exists(_.isInstanceOf[Sum]) =>
      val sumlist: Vector[Vector[Term]] = factors map {
        case Sum(summands) => summands
        case t => Vector(t)
      }
      val summandCombinations: Vector[List[Term]] = sumlist
        .foldRight(Vector[List[Term]](Nil))((el, rest) => el.flatMap(p => rest.map(p :: _)))
      Sum(summandCombinations.map(p => Product(p.toVector)))
  }).normalize

  def simplify(term: Term): Term = expand(term).substRules({
    case Sum(summands) =>
      val duplicates: Map[Term, Vector[Term]] = summands.groupBy((t: Term) => t)
      if (duplicates.exists(_._2.length > 1))
        Sum((summands.filterNot(duplicates.contains) ++ duplicates.map(p => simplify(Const(p._2.length) * p._1))).sorted)
      else Sum(summands.sorted)
    case Product(factors) => Product(factors.sorted)
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

case class Sum(summands: Vector[Term]) extends Term {
  override def toString: String = "(" + summands.mkString(" + ") + ")"

  override def normalize: Term = {
    val flattened = summands map (_.normalize) flatMap {
      case t: Sum => t.summands
      case other => Vector(other)
    } filterNot (_ == Const(0))
    if (flattened.isEmpty) this else if (flattened.length == 1) flattened.head else Sum(flattened)
  }

  override def immediateSubterms: Iterator[Term] = summands.toIterator

  override def subst(f: Term => Term): Term = f(Sum(summands map f))

  override protected def internalCompare(o: this.type): Int =
    (summands, o.summands).zipped.map(_.compareTo(_)).find(_ != 0).getOrElse(0)
}

case class Minus(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " - " + value2 + ")"

  override def immediateSubterms: Iterator[Term] = Iterator(value1, value2)

  override def subst(f: Term => Term): Term = f(Minus(f(value1), f(value2)))

  override protected def internalCompare(o: this.type): Int =
    Ordering[(Term, Term)].compare((this.value1, this.value2), (o.value1, o.value2))

  override def normalize: Term = Minus(value1.normalize, value2.normalize) match {
    case Minus(Const(a), Const(b)) => Const(a - b)
    case Minus(Const(0), value) => (Const(-1) * value).normalize
    case Minus(value, Const(0)) => value
    case other => other
  }
}

case class Product(factors: Vector[Term]) extends Term {
  override def toString: String = "(" + factors.mkString(" * ") + ")"

  override def normalize: Term = {
    val flattened = factors map (_.normalize) flatMap {
      case t: Product => t.factors
      case other => Vector(other)
    } filterNot (_ == Const(1))
    if (flattened.contains(Const(0))) Const(0)
    else if (flattened.isEmpty) this
    else if (flattened.length == 1) flattened.head
    else Product(flattened)
  }

  override def immediateSubterms: Iterator[Term] = factors.iterator

  override def subst(f: Term => Term): Term = f(Product(factors map f))

  override protected def internalCompare(o: this.type): Int =
    (factors, o.factors).zipped.map(_.compareTo(_)).find(_ != 0).getOrElse(0)
}

case class Quotient(value1: Term, value2: Term) extends Term {
  override def toString: String = "(" + value1 + " / " + value2 + ")"

  override def immediateSubterms: Iterator[Term] = Iterator(value1, value2)

  override def subst(f: Term => Term): Term = f(Quotient(f(value1), f(value2)))

  override protected def internalCompare(o: this.type): Int =
    Ordering[(Term, Term)].compare((this.value1, this.value2), (o.value1, o.value2))

  override def normalize: Term = Quotient(value1.normalize, value2.normalize) match {
    case Quotient(Const(0), _) => Const(0)
    case Quotient(value, Const(1)) => value
    case Quotient(Const(a), Const(b)) => Const(a / b)
    case other => other
  }
}
