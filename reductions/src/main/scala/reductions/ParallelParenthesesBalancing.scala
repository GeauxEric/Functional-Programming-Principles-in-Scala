package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceOut(l: List[Char], c: Char): List[Char] = {
      if (c == ')' || c == '(') {
        if (l.isEmpty) List(c)
        else if (l.head == '(' && c == ')') l.tail
        else c :: l
      }
      else
        l
    }

    chars.foldLeft(List[Char]())(balanceOut(_, _)).isEmpty
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = idx
      var num_right_paren = arg1
      var num_left_paren = arg2
      while (i < until) {
        val c = chars(i)
        if (c == ')' || c == '(') {
          if (c == ')' && num_left_paren > 0)
            num_left_paren -= 1
          else if (c == ')' && num_left_paren == 0)
            num_right_paren += 1
          else if (c == '(')
            num_left_paren +=1
          else {}
        }
        i += 1
      }

      (num_right_paren, num_left_paren)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from > threshold) {
        val mid = (until - from) / 2
        val ((a1, a2), (b1, b2)) = parallel(reduce(from, from + mid), reduce(from + mid, until))
        if (a2 > b1)
          (a1, a2 - b1 + b2)
        else
          (b1 - a2 + a1, b2)
      }
      else {
        traverse(from, until, 0, 0)
      }
    }

    val t = reduce(0, chars.length)
    t._1 == 0 && t._2 == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
