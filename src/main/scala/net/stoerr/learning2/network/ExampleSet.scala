package net.stoerr.learning2.network

import net.stoerr.learning2.common.DoubleArrayVector._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.{Codec, Source}
import scala.language.postfixOps

/** Set of examples for a neural net or something. */
class ExampleSet(val numInputs: Int, val numOutputs: Int) {

  val examples: mutable.Buffer[(Array[Double], Array[Double])]
  = new ArrayBuffer[(Array[Double], Array[Double])]()

  def +=(inAndOut: (Array[Double], Array[Double])): this.type = {
    examples += inAndOut;
    this
  }

  private def sqr(x: Double) = x * x

  def evaluation(nn: Buildingblock)(params: Array[Double]): Double =
    examples map { case (in, out) => nn(params)(in) - out } map (_.sqr) sum

  def evaluationWithGradient(nn: Buildingblock)(params: Array[Double]): (Double, Array[Double]) = {
    val exampleGrads = examples map { case (in, out) =>
      def gradFunc(nnout: Array[Double]): (Double, Array[Double]) = {
        val outDif = nnout - out
        (outDif.sqr, outDif * 2)
      }

      nn.gradient(params, in, gradFunc)
    }
    (exampleGrads.map(_._1).sum, exampleGrads.map(_._2).reduce(_ + _))
  }

}

object ExampleSet {

  /** Read an exampleset from a file; for now we assume that all attributes are double and the last attribute
    * is a category represented as string. */
  def fromDatafile(datafile: String): ExampleSet = {
    val stream = getClass.getResourceAsStream(datafile)
    assert(null != stream, s"Could not find $datafile")
    val parsedLines = Source.fromInputStream(stream)(Codec.UTF8).getLines()
      .filterNot(_.startsWith("#")).filterNot(_.isEmpty).map {
      line => line.trim.split("\\s*,\\s*")
    }.toArray
    val categories = parsedLines.map(_.last).distinct.sorted.zipWithIndex.toMap
    assert(categories.size < 10, s"Too many categories: $categories")
    val fieldcount = parsedLines(0).size
    println(s"$datafile has ${fieldcount - 1} fields, ${parsedLines.size} lines and ${categories.size} categories $categories")
    val exampleSet = new ExampleSet(fieldcount - 1, categories.size)
    val booleanToDouble = Map(false -> -0.5, true -> 0.5)
    parsedLines foreach { example =>
      assert(fieldcount == example.size, s"Line has not $fieldcount fields: ${example.mkString(",")}")
      val categoryNum = categories(example.last)
      val outputs = (0 until categories.size).map(_ == categoryNum).map(booleanToDouble).toArray
      exampleSet += example.dropRight(1).map(_.toDouble) -> outputs
    }
    exampleSet
  }

}
