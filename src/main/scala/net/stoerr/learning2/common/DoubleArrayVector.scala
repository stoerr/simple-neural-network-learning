package net.stoerr.learning2.common

import scala.language.implicitConversions
import scala.util.Random

object DoubleArrayVector {
  implicit def doubleArrayVector(v: Array[Double]): DoubleArrayVector = new DoubleArrayVector(v)

  /** step for numerical calcuations */
  val eps = 1e-6

  def derivation(f: Double => Double, x: Double): Double = (-f(x + 2 * eps) + 8 * f(x + eps) - 8 * f(x - eps) + f(x - 2 * eps)) / (12 * eps)

  def gradient(f: Array[Double] => Double, x: Array[Double]): Array[Double] = x.indices.map { i =>
    val fprojected = x.baseFunction(i) andThen f
    derivation(fprojected, 0)
  }.toArray

  def randomVector(length: Int): Array[Double] = Array.fill(length)(Random.nextGaussian())

}

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 12.11.2014
 */
final class DoubleArrayVector(val self: Array[Double]) {
  import DoubleArrayVector._

  def abs: Double = math.sqrt(this * self)

  def elem_abs: Array[Double] = self.map(math.abs)

  def +(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ + _)

  def -(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ - _)

  /** scalar product */
  def *(other: Array[Double]): Double = (self, other).zipped.map(_ * _).sum

  def *(other: Double): Array[Double] = self.map(_ * other)

  /** elementwise product */
  def elem_*(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ * _)

  def /(other: Double): Array[Double] = self.map(_ / other)

  def signum: Array[Double] = self.map(math.signum)

  /** function arg => self + Array(0,...,arg, ...,0) , arg is put in n-th place. */
  def baseFunction(n: Int): Double => Array[Double] = { arg: Double =>
    val basePlusArg = self.clone()
    basePlusArg(n) += arg
    basePlusArg
  }

  def directionalFunction(func: Array[Double] => Double, dx: Array[Double]): (Double) => Double =
    (v: Double) => func(self + dx * v)

  def toRep: String = "Array(" + self.mkString(", ") + ")"

  def normalize: Array[Double] = this / abs

}
