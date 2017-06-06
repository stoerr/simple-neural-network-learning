package net.stoerr.learning2.common

import scala.collection.immutable.{SortedMap, TreeMap}

/** Constructors for DValue - see there. */
object DValue {
  def apply(value: Double) = new DValue(value, SortedMap.empty)

  def apply(value: Double, name: String) = new DValue(value, TreeMap(name -> 1.0))

  val ONE = DValue(1)

  def asDoubleFunction(func: Array[DValue] => DValue)(args: Array[Double]): Double =
    func(args.map(DValue(_))).value

  def asDoubleFunctionWithGradient(func: Array[DValue] => DValue)(args: Array[Double]): (Double, Array[Double]) = {
    val varnames = (0 until args.length).map("v" + _).toArray
    val dargs = (args, varnames).zipped.map(DValue(_, _))
    val fval = func(dargs)
    (fval.value, varnames.map(fval.deriv(_)))
  }


}

/**
 * Calculation where the partial derivations from an arbitrary number of variables are automatically calculated as well.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.10.2014
 */
case class DValue(value: Double, derivations: SortedMap[String, Double]) {

  def deriv(name: String): Double = derivations.getOrElse(name, 0)

  private def combineDerivations(other: DValue, func: (Double, Double) => Double) = {
    val entries = (this.derivations.keySet ++ other.derivations.keySet).toSeq.map(key =>
      key -> func(this.deriv(key), other.deriv(key))
    )
    SortedMap(entries: _*)
  }

  def +(other: DValue): DValue = DValue(value + other.value, combineDerivations(other, _ + _))

  def -(other: DValue): DValue = DValue(value - other.value, combineDerivations(other, _ - _))

  def *(other: DValue): DValue = DValue(value * other.value, combineDerivations(other,
    (dthis, dother) => dthis * other.value + value * dother))

  def /(other: DValue): DValue = DValue(value / other.value, combineDerivations(other,
    (dthis, dother) => (dthis * other.value - value * dother) / (other.value * other.value)))

  def abs: DValue = if (value < 0) this * DValue(-1) else this

  def log: DValue = DValue(math.log(value), derivations.mapValues(_ * 1 / value))

  def cosh: DValue = DValue(math.cosh(value), derivations.mapValues(_ * math.sinh(value)))

  def tanh: DValue = {
    val tanh = math.tanh(value)
    val derivation = 1 - tanh * tanh
    DValue(tanh, derivations.mapValues(_ * derivation))
  }

}
