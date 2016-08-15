package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length more than 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check("(asdhb())", true)
    check("(asdhb())(", false)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance should work for string of length more than 2") {
    def check(input: String, expected: Boolean, threshold: Int) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"balance($input) should be $expected")

    check("()", true, 4)
    check("((()))", true, 1)
    check("()", true, 4)
    check("(asdhb())", true, 3)
    check("(asdhb())(", false, 4)
    check(")(", false, 2)
    check("((", false, 2)
    check("))", false, 1)
    check(".)", false, 1)
    check(".(", false, 1)
    check("(.", false, 1)
    check(").", false, 1)
  }

}
