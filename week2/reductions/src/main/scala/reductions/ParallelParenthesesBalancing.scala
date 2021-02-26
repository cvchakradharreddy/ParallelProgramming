package reductions

import scala.annotation._
import org.scalameter._

import scala.util.control.Breaks._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var acc = 0
    breakable {
      for (c <- chars) {
        if (c == '(') acc += 1
        else if (c == ')') acc -= 1
        if (acc < 0)
          break
      }
    }
    acc == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Boolean) /*: ???*/ = {
      val neg = if (arg1 < 0 && arg2 == 0) 1 else arg2
      if (idx >= until) (arg1, neg == 0)
      else if (chars(idx) == '(') traverse(idx + 1, until, arg1 + 1, neg)
      else if (chars(idx) == ')') traverse(idx + 1, until, arg1 - 1, neg)
      else traverse(idx + 1, until, arg1, neg)
    }

    def reduce(from: Int, until: Int): (Int, Boolean) /*: ???*/ = {
      if ((until - from) <= threshold) traverse(from, until, 0, 0)
      else {
        val m = (from + until) / 2
        val (left, right) = parallel(reduce(from, m), reduce(m, until))
        (left._1 + right._1, (left._2) && !(left._1 == 0 || (right._2)))
      }

    }

    reduce(0, chars.length) == (0, true)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
