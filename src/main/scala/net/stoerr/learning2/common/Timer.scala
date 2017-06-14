package net.stoerr.learning2.common

/**
 * Utility for timing stuff.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 06.11.2014
 */
class Timer(name: String = "timer") {

  private val begin: Long = System.nanoTime()

  def time(): Float = 1e-9f * (System.nanoTime() - begin)

  override def toString = name + ": " + time() + "s"

}

object Timer {

  def timing[T](name: String)(block: => T) = {
    val timer = new Timer(name)
    val res = block
    println(timer)
    res
  }

  def apply(name: String) = new Timer(name)

}
