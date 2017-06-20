package net.stoerr.learning2.common

import scala.language.implicitConversions
import scala.util.Random

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

  def eval(term: Term, valuation: Map[Var, Double]): Double = term match {
    case Const(c) => c
    case t: Var => valuation(t)
    case Sum(summands) => summands.map(eval(_, valuation)).sum
    case Minus(val1, val2) => eval(val1, valuation) - eval(val2, valuation)
    case Product(factors) => factors.map(eval(_, valuation)).product
    case Quotient(val1, val2) => eval(val1, valuation) / eval(val2, valuation)
  }

  def derive(term: Term, v: Var): Term = {
    def d(t: Term): Term = t match {
      case Const(_) => Const(0)
      case vn: Var => if (v == vn) Const(1) else Const(0)
      case Sum(summands) => Sum(summands.map(d))
      case Minus(val1, val2) => d(val1) - d(val2)
      case Product(factors) => Sum(deriveProductRaw(factors, factors.map(derive(_, v))).map(Product))
      case Quotient(val1, val2) => (d(val1) * val2 - val1 * d(val2)) / (val2 * val2)
    }

    expand(d(term))
  }

  private def sqr(x: Double) = x * x

  private def deriveProductRaw[U](v1: Vector[U], v2: Vector[U]): Vector[Vector[U]] = {
    val v = v1.zip(v2).zipWithIndex
    v.indices.map(i => v.map(t => if (i == t._2) t._1._2 else t._1._1)).toVector
  }

  def deriveProduct(v: Vector[Double], d: Vector[Double]): Double = {
    val vproduct = v.product
    if (0 != vproduct) v.indices.toIterator.map(i => d(i) / v(i)).sum * vproduct
    else v.indices.toIterator.map(i => v.indices.toIterator.map(j => if (i == j) d(j) else v(j)).product).sum
  }

  private def secondDeriveProduct(v: Vector[Double], d: Vector[Double], d2: Vector[Double]) = {
    val vprod = v.product
    val sum1 = v.indices.map(i => d(i) / v(i)).sum
    val sum2 = v.indices.map(i => (d2(i) * v(i) - sqr(d(i))) / sqr(d(i))).sum
    (sqr(sum1) + sum2) / vprod
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

  def evalWithGradient(term: Term, valuation: Map[Var, Double]): (Double, Var => Double) = term match {
    case Const(c) => (c, _ => 0.0)
    case v: Var => (valuation(v), k => if (v == k) 1.0 else 0.0)
    case Sum(summands) =>
      val parts = summands.map(evalWithGradient(_, valuation))
      (parts.map(_._1).sum, k => parts.map(_._2(k)).sum)
    case Minus(val1, val2) =>
      val (p1, p2) = (evalWithGradient(val1, valuation), evalWithGradient(val2, valuation))
      (p1._1 - p2._1, k => p1._2(k) - p2._2(k))
    case Product(factors) =>
      val fg: Vector[(Double, (Var) => Double)] = factors.map(evalWithGradient(_, valuation))
      val fgevals = fg.map(_._1)
      (fgevals.product, k => deriveProduct(fgevals, fg.map(_._2(k))))
    case Quotient(val1, val2) =>
      val (p1, p2) = (evalWithGradient(val1, valuation), evalWithGradient(val2, valuation))
      (p1._1 / p2._1, k => (p1._2(k) * p2._1 - p1._1 * p2._2(k)) / (p2._1 * p2._1))
  }

  /** Returns the value for the valuation, the gradient, the directional derivation in the direction of the gradient and its second derivation. */
  def evalWithGradientAndDirectionalDerivations(term: Term, valuation: Map[Var, Double]): (Double, Var => Double, Double, Double) = {
    def quotrule2(f: Double, fd: Double, fdd: Double, g: Double, gd: Double, gdd: Double): Double =
      (fdd * g * g - 2 * f * g * gd + 2 * f * gd * gd - f * g * gdd) / (g * g * g)

    def chain(term: Term): (Double, Var => Double, (Var => Double) => (Double, Double))
    = term match {
      case Const(c) => (c, _ => 0.0, _ => (0, 0))
      case v: Var => (valuation(v), k => if (v == k) 1.0 else 0.0, grad => (grad(v), 0))
      case Sum(summands) =>
        val (evals: Vector[Double], grads: Vector[(Var) => Double], dirderiv: Vector[((Var) => Double) => (Double, Double)]) = summands.map(chain).unzip3
        (evals.sum, k => grads.map(_ (k)).sum, (grad) => dirderiv.map(_ (grad)).reduce((p1, p2) => (p1._1 + p2._1, p1._2 + p2._2)))
      case Minus(val1, val2) =>
        val (eval1, grad1, dirderiv1) = chain(val1)
        val (eval2, grad2, dirderiv2) = chain(val2)
        (eval1 - eval2, k => grad1(k) - grad2(k), (grad) => {
          val (dd1, dd2) = (dirderiv1(grad), dirderiv2(grad))
          (dd1._1 - dd2._1, dd1._2 - dd2._2)
        })
      case Product(factors) =>
        val (evals: Vector[Double], grads: Vector[(Var) => Double], dirderivs: Vector[((Var) => Double) => (Double, Double)]) = factors.map(chain).unzip3
        (evals.product, k => deriveProduct(evals, grads.map(_ (k))), (grad) => {
          val (ds, d2s) = dirderivs.map(_ (grad)).unzip
          (deriveProduct(evals, ds), secondDeriveProduct(evals, ds, d2s))
        })
      case Quotient(val1, val2) =>
        val (eval1, grad1, dirderiv1) = chain(val1)
        val (eval2, grad2, dirderiv2) = chain(val2)
        (eval1 / eval2, k => (grad1(k) * eval2 - eval1 * grad2(k)) / (eval2 * eval2), (grad) => {
          val ((dd1d, dd1dd), (dd2d, dd2dd)) = (dirderiv1(grad), dirderiv2(grad))
          ((dd1d * eval2 - eval1 * dd2d) / (eval2 * eval2), quotrule2(eval1, dd1d, dd1dd, eval2, dd2d, dd2dd))
        })
    }

    val (eval, grad, dirderiv) = chain(term)
    val (d1, d2) = dirderiv(grad)
    (eval, grad, d1, d2)
  }

  def random(maxDepth: Int, numVars: Int): Term =
    if (maxDepth <= 0) Random.nextInt(2) match {
      case 0 => Const(Random.nextGaussian())
      case 1 => Var(Symbol("v" + (1 + Random.nextInt(numVars))))
    } else {
      val subterm = () => random(maxDepth - 1, numVars)
      Random.nextInt(4) match {
        case 0 => subterm() + subterm()
        case 1 => subterm() - subterm()
        case 2 => subterm() * subterm()
        case 3 => subterm() / subterm()
      }
    }

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
