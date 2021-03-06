package net.stoerr.learning2.network

import net.stoerr.learning2.common.DoubleArrayVector._
import org.scalatest.matchers.{BeMatcher, MatchResult}

// http://www.scalatest.org/user_guide/using_matchers
object TestingHelper {

  def closeTo(value: Array[Double]) = new BeMatcher[Array[Double]] {
    def apply(left: Array[Double]): MatchResult = {
      if (value.length != left.length) MatchResult(
        false,
        "vectors have different length: " + value.length + " != " + left.length + "\n" + value.mkString(",") + "\n" + left.mkString(","),
        "vectors have same length: " + value.length + " == " + left.length + "\n" + value.mkString(",") + "\n" + left.mkString(",")
      )
      else MatchResult(
        (value - left).abs < eps,
        "vectors not close enough:\n" + value.mkString(",") + "\n" + left.mkString(","),
        "vectors close enough:\n" + value.mkString(",") + "\n" + left.mkString(",")
      )
    }
  }

}
