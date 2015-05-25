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

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 == as.length - 1) ordered(as(n), as(n + 1))
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    }
    loop(0)
  }

}
