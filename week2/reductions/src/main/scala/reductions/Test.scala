package reductions

import reductions.ParallelParenthesesBalancing.parBalance
import reductions.LineOfSight._
import reductions.ParallelCountChange._

object Test extends App {
  //assert(parBalance("())(()".toArray, 2) == false)
  assert(parBalance("()()()())(()".toArray, 2) == false)
  assert(parBalance("()()()".toArray, 3) == true)
  assert(parBalance(")(".toArray, 2) == false)
  assert(parBalance("())(".toArray, 1) == false)
  assert(parBalance("()())(()".toArray, 1) == false)
  assert(parBalance(".".toArray, 1) == true)

  assert(parBalance("(".toArray, 5) == false)
  assert(parBalance(")".toArray, 5) == false)
  assert(parBalance(".".toArray, 5) == true)
  assert(parBalance("()".toArray, 5) == true)
  assert(parBalance(")(".toArray, 5) == false)
  assert(parBalance("((".toArray, 5) == false)
  assert(parBalance(".)".toArray, 5) == false)
  assert(parBalance(".(".toArray, 5) == false)
  assert(parBalance("(.".toArray, 5) == false)
  assert(parBalance(").".toArray, 5) == false)

  assert(parBalance("((()))".toArray, 5) == true)
  assert(parBalance(")(".toArray, 1) == false)

  /*val input = Array[Float](0f, 1f, 8f, 9f)
  val seqOutput = new Array[Float](4)
  val parOutput = new Array[Float](4)
  lineOfSight(input, seqOutput)
  parLineOfSight(input, parOutput, 2)
  println(seqOutput.mkString(" "), parOutput.mkString(" "))
  assert(List(0f, 1f, 4f, 4f) == seqOutput.toList)
  assert(parOutput.toList == seqOutput.toList)*/

  //parCountChange(16, List(1), moneyThreshold(16))

  /*val input = Array.fill[Float](17)(0)
  val parOutput = new Array[Float](input.length)
  parLineOfSight(input, parOutput, 1)*/

}
