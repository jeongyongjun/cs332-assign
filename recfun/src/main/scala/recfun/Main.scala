package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else {
      val triangle = Array.ofDim[Int](r + 1, r + 1)
      for (i <- 0 to r) {
        triangle(i)(0) = 1
        triangle(i)(i) = 1
        for (j <- 1 until i) {
          triangle(i)(j) = triangle(i - 1)(j - 1) + triangle(i - 1)(j)
        }
      }
      triangle(r)(c)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalancedIter(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) stack.isEmpty
      else chars.head match {
        case '(' => isBalancedIter(chars.tail, '(' :: stack)
        case ')' => stack.nonEmpty && isBalancedIter(chars.tail, stack.tail)
        case _ => isBalancedIter(chars.tail, stack)
      }
    }
    isBalancedIter(chars, Nil)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      val dp = Array.fill(money + 1)(0)
      dp(0) = 1
      for (coin <- coins; j <- coin to money) {
        dp(j) += dp(j - coin)
      }
      dp(money)
    }
  }
}