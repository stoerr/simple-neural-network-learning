package net.stoerr.learning2.network

import org.scalatest.matchers.{BeMatcher, MatchResult}
import net.stoerr.learning2.common.DoubleArrayVector._

// http://www.scalatest.org/user_guide/using_matchers
object TestingHelper {

  def closeTo(value: Array[Double]) = new BeMatcher[Array[Double]] {
    def apply(left: Array[Double]) =
      MatchResult(
        (value - left).abs < eps,
        "vectors not close enough:\n" + value.mkString(",") + "\n" + left.mkString(","),
        "vectors close enough:\n" + value.mkString(",") + "\n" + left.mkString(",")
      )
  }

}
