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
    var i: Int = 0
    var acc: Int = 0
    while(i < chars.length) {
      if(chars(i) == '(') acc = acc+1
      if (chars(i) == ')') if(acc==0) acc = acc - 1000000000 else acc = acc - 1
      i+=1
    }
    acc == 0
    // def fun(chars: Array[Char], acc: Int): Boolean = {
    //   if (chars.isEmpty) acc==0
    //   else if (chars.head == '(') fun(chars.tail, acc+1)
    //   else if (chars.head == ')') {
    //     if (acc==0) false
    //     else fun(chars.tail, acc-1)
    //   }
    //   else fun(chars.tail, acc)
    // }
    // fun(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else if (chars(idx) == '(') traverse(idx+1, until, arg1+1, arg2)
      else if (chars(idx) == ')') {
        if(arg1>0) traverse(idx+1, until, arg1-1, arg2)
        else traverse(idx+1, until, arg1, arg2+1)
      }
      else traverse(idx+1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(from == until) (0, 0)
      else if (until-from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid: Int = (from + until)/2
        val ans: ((Int, Int), (Int, Int)) = parallel(reduce(from, mid),
                                                     reduce(mid, until))
        val r1: (Int, Int) = ans._1
        val r2: (Int, Int) = ans._2
        val min: Int = if(r1._1 < r2._2) r1._1 else r2._2

        (r1._1 - min + r2._1, r1._2 + r2._2 - min)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
