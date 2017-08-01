package net.stoerr.learning2.exploration

import scala.language.implicitConversions

/** Term replacement system for some experiments in symbolical differentation */

sealed trait Term extends Ordered[Term] {
  def +(o: Term) = new `+`(this, o)

  def *(o: Term) = new `*`(this, o)

  def ordernumber: Int

  protected def compareInt(that: Term): Int

  override def compare(that: Term): Int =
    if (this.ordernumber != that.ordernumber) this.ordernumber - that.ordernumber
    else if (equals(that)) 0
    else this.compareInt(that)
}

object Term {
  implicit def toTerm(n: Int): N = N(n)

  implicit def toTerm(v: Symbol): V = V(v)
}

case class N(number: Double) extends Term {
  override def toString: String = number.toString

  override def ordernumber: Int = 1

  override def compareInt(that: Term): Int = number.compareTo(that.asInstanceOf[N].number)
}

case class V(name: Symbol) extends Term {
  override def toString: String = name.name

  override def ordernumber: Int = 2

  override def compareInt(that: Term): Int = name.name compare that.asInstanceOf[V].name.name
}

case class F(functor: Symbol, derivations: List[Int], args: Term*) extends Term {
  override def toString: String =
    if (derivations.nonEmpty) functor.name + "^{" + derivations.mkString(",") + "}" + "(" + args.mkString(", ") + ")"
    else functor.name + "(" + args.mkString(", ") + ")"

  override def ordernumber: Int = 3

  override def compareInt(that: Term): Int = {
    def cmpList[T](a1: Seq[T], a2: Seq[T])(implicit ordering: Ordering[T]): Int = {
      if (a1.isEmpty && a2.isEmpty) return 0
      val res = ordering.compare(a1.head, a2.head)
      if (res != 0) res else cmpList(a1.tail, a2.tail)
    }

    val other = that.asInstanceOf[F]
    if (functor != other.functor) return functor.name compare other.functor.name
    if (derivations.length != other.derivations.length) return derivations.length.compareTo(other.derivations.length)
    val derivationsCmp = cmpList(derivations, other.derivations)
    if (derivationsCmp != 0) return derivationsCmp
    if (args.length != other.args.length) return args.length.compareTo(other.args.length)
    cmpList(args, other.args)
  }
}

case class D(v: Symbol, term: Term) extends Term {
  override def toString: String =
    if (term.ordernumber < 100) "d/d" + v + " " + term
    else "d/d" + v + " " + "(" + term + ")"

  override def ordernumber: Int = 5

  override def compareInt(that: Term): Int =
    (v.toString(), term) compare(that.asInstanceOf[D].v.toString(), that.asInstanceOf[D].term)
}

case class `*`(arg1: Term, arg2: Term) extends Term {
  override def toString: String = arg1 + " * " + arg2

  override def ordernumber: Int = 100

  override def compareInt(that: Term): Int =
    (arg1, arg2) compare(that.asInstanceOf[`*`].arg1, that.asInstanceOf[`*`].arg2)
}

case class `+`(arg1: Term, arg2: Term) extends Term {
  override def toString: String = "(" + collectSummands().mkString(" + ") + ")"

  override def ordernumber: Int = 110

  override def compareInt(that: Term): Int
  = (arg1, arg2) compare(that.asInstanceOf[`+`].arg1, that.asInstanceOf[`+`].arg2)

  def collectSummands(): List[Term] = {
    def collector(term: Term): List[Term] = term match {
      case x + y => collector(x) ++ collector(y)
      case t => List(t)
    }

    collector(this)
  }
}

class TermReplacer(rules: PartialFunction[Term, Term]) extends Function[Term, Term] {
  def apply(term: Term): Term = {
    var replacedTerm = replaceSubterms(term)
    while (rules.isDefinedAt(replacedTerm)) {
      val newTerm = rules(replacedTerm)
      if (newTerm.equals(replacedTerm)) throw new IllegalStateException("Bug: replaced to itself: " + newTerm)
      // println(replacedTerm + " -> " + newTerm)
      replacedTerm = newTerm
      replacedTerm = replaceSubterms(replacedTerm)
    }
    replacedTerm
  }

  protected def replaceSubterms(term: Term): Term = term match {
    case V(_) => term
    case N(_) => term
    case F(functor, derivations, args@_*) => F(functor, derivations, args.map(apply): _*)
    case t1 + t2 => apply(t1) + apply(t2)
    case t1 * t2 => apply(t1) * apply(t2)
    case D(v, t) => D(v, apply(t))
  }

  def latex(term: Term): String = term match {
    case N(value) => value.toString
    case V(name) => name.name
    case F(functor, derivations, args@_*) => "{" + functor.name + "}ˆ{(" + derivations.mkString(",") + ")}"
    case D(v, t) => "\\frac{d}{d" + v.name + "}\\left(" + latex(t) + "\\right)"
    case `*`(arg1, arg2) => latex(arg1) + " " + latex(arg2)
    case `+`(arg1, arg2) => "\\left(" + collectSummands(term).mkString(" + ") + "\\right)"
  }

  def latex(term1: Term, term2: Term): String = "$$ " + latex(term1) + " = " + latex(term2) + " $$"

  protected def collectSummands(term: Term): Seq[Term] = term match {
    case `+`(arg1, arg2) => collectSummands(arg1) ++ collectSummands(arg2)
    case _ => Seq(term)
  }
}

object Simplify extends TermReplacer({
  case N(0) + t => t
  case t + N(0) => t
  case N(a) + N(b) + t => N(a + b) + t

  case N(1) * t => t
  case t * N(1) => t
  case N(0) * _ => N(0)
  case t * N(0) => N(0)
  case N(a) * N(b) * t => N(a * b) * t

  case t + x + y if x == y => t + N(2) * x

  case x + (y + z) => (x + y) + z
  case x * (y * z) => (x * y) * z
  case (x + y) * z => x * z + y * z
  case x * (y + z) => x * y + x * z

  case x * y if x > y && !x.isInstanceOf[`*`] => y * x
  case (x * y) * z if y > z => (x * z) * y
  case x + y if x > y && !x.isInstanceOf[`+`] => y + x
  case (x + y) + z if y > z => (x + z) + y

  case D(v, x + y) => D(v, x) + D(v, y)
  case D(v, x * y) => D(v, x) * y + x * D(v, y)
  case D(v1, V(v2)) => if (v1 == v2) N(1) else N(0)
  case D(_, N(_)) => N(0)
  case D(v, F(s, d, args@_*)) =>
    val z: Seq[Term] = args.zipWithIndex.map(p => F(s, p._2 :: d, args: _*) * D(v, p._1))
    z.reduce(_ + _)
  /*
  rule(d(V,X + Y), d(V,X) + d(V,Y)).
rule(d(V,X - Y), d(V,X) - d(V,Y)).
rule(d(V,-X), -d(V,X)).
rule(d(V,X*Y), d(V,X) * Y + X * d(V,Y)).
rule(d(V,V), 1).
rule(d(V,N), 0) :- number(N); atom(N), V \= N.
rule(d(V,sum(I,T)), sum(I,d(V,T))).
rule(d(V,X^N),N*d(V,X)*X^N1) :- number(N), N1 is N-1.
   */
})

object TermReplacerRun extends App {
  def replaceAndPrint(term: Term): Unit = {
    println("Term: " + term)
    val simplified = Simplify(term)
    println("Yields: " + simplified)
    println(Simplify.latex(term, simplified))
  }

  replaceAndPrint(V('x) + V('y) + (N(1) + F('f, List(), 'x) + N(2)) + V('a) * N(2))
  replaceAndPrint(D('x, F('f, Nil, V('x) * 'x, 'x)))
  val f = F('f, Nil, F('b, Nil, F('v, Nil, 'x), 'x))
  val d = D('x, f)
  replaceAndPrint(d)
  Simplify(d).asInstanceOf[`+`].collectSummands().foreach(println)
  println()
  private val d2 = D('x, D('x, f))
  replaceAndPrint(d2)
  Simplify(d2).asInstanceOf[`+`].collectSummands().foreach(println)
  // f(x, b(v(x),x))
}
