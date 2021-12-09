package rpt.udemy
package section2

import scala.annotation.tailrec

object Functions extends App {

  /**
   * Repeats a String n times.
   *
   * @param aString String to repeat.
   * @param n       Number of times to repeat the input string.
   * @return The concatenatenation of n times aString.
   */
  def repeatString(aString: String, n: Int): String = {
    @tailrec
    def helper(output: String, aString: String, n: Int): String = {
      if (aString.isEmpty) {
        ""
      } else {
        if (n == 1) {
          output + aString
        } else {
          helper(output + aString, aString, n - 1)
        }
      }
    }

    helper("", aString, n)
  }

  /**
   * Computes the factorial of n.
   * @param n Number to compute its factorial.
   * @return The factorial of n.
   */
  def factorialFunc(n:Int) : Int = {
    @tailrec
    def helper(accumulator:Int, n: Int): Int = {
      if (n <= 1){
        accumulator
      } else {
        val n_1 = n-1
        helper(accumulator*n, n_1)
      }
    }
    helper(n,n-1)
  }

  println(repeatString("Hello", 3))
  println(factorialFunc(5))

}
