import math.abs

object fixed {
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x) / x < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(next, guess)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def sqrt(x: Double): Double = {
    fixedPoint(y => (y + x/y) / 2.0)(1.0)
  }

  def main(args: Array[String]) = {
    val g = fixedPoint(x => 1 + x/2)(1)
    println(g)

    val s = sqrt(9)
    println(s)
  }
}
