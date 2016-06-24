package recfun

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
      if (c > r || c < 0) 0
      else if (c == 0 && r == 0) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def fun(chars: List[Char], acc: Int): Int = {
        if (chars.isEmpty || acc < 0) acc
        else if (chars.head == '(') fun(chars.tail, acc + 1) 
        else if (chars.head == ')') fun(chars.tail, acc - 1)
        else fun(chars.tail, acc)
      }
      if (fun(chars, 0) == 0) true
      else false
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money <= 0 || coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
