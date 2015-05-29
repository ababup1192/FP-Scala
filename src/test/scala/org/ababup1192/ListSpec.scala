package org.ababup1192

import datastructure._
import datastructure.List._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "Empty List" should "be equal Nil" in {
    List() should be(Nil)
  }

  "List" should "construct Cons and Nil" in {
    List("a", "b", "c") should be(Cons("a", Cons("b", Cons("c", Nil))))
  }

  "Cons" should "have values with several type" in {
    List(1.0, 2, 3.0, 4) should be(Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil)))))
  }

  "Int List" should "be summed" in {
    sum(List(1, 2, 3, 4)) should be(10)
  }

  "Int List" should "be multiplied" in {
    product(List(1.0, 2.0, 3.0, 4.0)) should be(24.0)
  }

  "The tail function" should "return new List" in {
    tail(List(1, 2, 3)) should be(List(2, 3))
  }

  "The drop function" should "return new List" in {
    drop(List(1, 2, 3), 1) should be(List(2, 3))
  }

  "The length function" should "return size of list" in {
    List.length(List(1, 2, 3, 4)) should be(4)
  }


  "The reverse function" should "return a new list" in {
    reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  "The dropWhile function" should "return new List" in {
    dropWhile(List(8, 9, 10, 11, 12, 13))(_ < 10) should be(List(10, 11, 12, 13))
  }

  "The append function" should "connect two lists" in {
    append(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
  }

  "The concat function" should "return new list" in {
    concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "The plusOne function" should "return new list" in {
    plusOne(List(1, 2, 3, 4, 5)) should be(List(2, 3, 4, 5, 6))
  }

  "The double2String function" should "return new String list" in {
    double2String(List(1.0, 2.0, 3.0)) should be(List("1.0", "2.0", "3.0"))
  }

  "The filter function" should "return new List" in {
    filter(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0) should be(List(2, 4, 6, 8))
  }

  "The flatMap function" should "return new List" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  "The zipWith function" should "return new List" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
  }

  "The init function" should "return new list" in {
    init(List(1, 2, 3, 4)) should be(List(1, 2, 3))
  }

  "The setHead function" should "return new List" in {
    setHead(List(1, 2, 3), 5) should be(List(5, 2, 3))
  }

  "The hasSubsequence function" should "find sub sequence" in {
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be(right = true)
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be(right = true)
    hasSubsequence(List(1, 2, 3, 4), List(4)) should be(right = true)
    hasSubsequence(List(1, 2, 3, 4), List(5)) should be(right = false)
  }

}


