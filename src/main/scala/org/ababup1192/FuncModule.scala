package org.ababup1192

object FuncModule {
  def abs(n: Int) = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

}
