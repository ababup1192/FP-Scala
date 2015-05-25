package org.ababup1192

import scala.annotation.tailrec

object FuncModule {
  def abs(n: Int) = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else loop(n - 1, n * acc)
    }
    loop(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, p: Int, c: Int): Int = {
      if (n < 1) p
      else loop(n - 1, c, p + c)
    }
    loop(n, 0, 1)
  }

}
