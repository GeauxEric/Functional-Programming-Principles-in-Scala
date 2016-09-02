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
    if ((c == 0) || (r == c)) {
      1
    }
    else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(left: List[Char], right: List[Char]): List[Char] = {
      if (right.length == 0) {
        left
      }
      else {
        if (right.head == '(') {
          loop('(' :: left, right.tail)
        }
        else if (right.head == ')') {
          if (left.length != 0 && left.head == '(') {
            loop(left.tail, right.tail)
          }
          else {
            loop(')' :: left, right.tail)
          }
        }
        else
          loop(left, right.tail)
      }
    }

    val left = List[Char]()
    val after_loop = loop(left, chars)
    after_loop.length == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) {
      0
    }
    else if (money == 0) {
      1
    }
    else {
      val first_coin = coins.head
      if (first_coin > money) {
        countChange(money, coins.tail)
      }
      else {
        countChange(money - first_coin, coins) + countChange(money, coins.tail)
      }
    }
  }
}
