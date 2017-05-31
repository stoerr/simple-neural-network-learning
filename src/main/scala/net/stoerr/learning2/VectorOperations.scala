package net.stoerr.learning2

import language.postfixOps

/** Various matrix and vector operations. */
object VectorOperations {

  def dot(v1: Array[Double], v2: Array[Double]) : Double = {
    require(v1.length == v2.length)
    (v1 zip v2) map Function.tupled(_ * _) sum
  }

  def scalarproduct(v: Array[Double], factor: Double) : Array[Double] = v.map(_ * factor)

}
