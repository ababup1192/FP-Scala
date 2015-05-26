package org.ababup1192

import org.ababup1192.datastructure._
import datastructure.List._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "Empty List" should "be equal Nil" in {
    datastructure.List() should be(datastructure.Nil)
  }

  "List" should "construct Cons and Nil" in {
    datastructure.List("a", "b", "c") should be(Cons("a", Cons("b", Cons("c", Nil))))
  }

  "Cons" should "have values with several type" in {
    datastructure.List(1.0, 2, 3.0, 4) should be(Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, datastructure.Nil)))))
  }

  "Int List" should "be summed" in {
    sum(datastructure.List(1, 2, 3, 4)) should be(10)
  }

  "Int List" should "be multiplied" in {
    product(datastructure.List(1.0, 2.0, 3.0, 4.0)) should be(24.0)
  }

  "The tail function" should "return new List" in {
    tail(datastructure.List(1, 2, 3)) should be(datastructure.List(2, 3))
  }

  "The drop function" should "return new List" in {
    drop(datastructure.List(1, 2, 3), 1) should be(datastructure.List(2, 3))
  }

  "The length function" should "return size of list" in {
    datastructure.List.length(datastructure.List(1, 2, 3, 4)) should be(4)
  }


  "The reverse function" should "return a new list" in {
    reverse(datastructure.List(1, 2, 3)) should be(datastructure.List(3, 2, 1))
  }

  "The dropWhile function" should "return new List" in {
    dropWhile(datastructure.List(8, 9, 10, 11, 12, 13))(_ < 10) should be(datastructure.List(10, 11, 12, 13))
  }

  "The append function" should "connect two lists" in {
    append(datastructure.List(1, 2, 3), datastructure.List(4, 5, 6)) should be(datastructure.List(1, 2, 3, 4, 5, 6))
  }

  "The concat function" should "return new list" in {
    concat(datastructure.List(datastructure.List(1, 2, 3), datastructure.List(4, 5, 6), datastructure.List(7, 8, 9))) should be(datastructure.List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "The plusOne function" should "return new list" in {
    plusOne(datastructure.List(1, 2, 3, 4, 5)) should be(datastructure.List(2, 3, 4, 5, 6))
  }

  "The double2String function" should "return new String list" in {
    double2String(datastructure.List(1.0, 2.0, 3.0)) should be(datastructure.List("1.0", "2.0", "3.0"))
  }

  "The filter function" should "return new List" in {
    filter(datastructure.List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0) should be(datastructure.List(2, 4, 6, 8))
  }

  "The flatMap function" should "return new List" in {
    flatMap(datastructure.List(1, 2, 3))(i => datastructure.List(i, i)) should be(datastructure.List(1, 1, 2, 2, 3, 3))
  }

  "The zipWith function" should "return new List" in {
    zipWith(datastructure.List(1, 2, 3), datastructure.List(4, 5, 6))(_ + _) should be(datastructure.List(5, 7, 9))
  }

  "The init function" should "return new list" in {
    init(datastructure.List(1, 2, 3, 4)) should be(datastructure.List(1, 2, 3))
  }

  "The setHead function" should "return new List" in {
    setHead(datastructure.List(1, 2, 3), 5) should be(datastructure.List(5, 2, 3))
  }

  "The hasSubsequence function" should "find sub sequence" in {
    hasSubsequence(datastructure.List(1, 2, 3, 4), datastructure.List(1, 2)) should be(right = true)
    hasSubsequence(datastructure.List(1, 2, 3, 4), datastructure.List(2, 3)) should be(right = true)
    hasSubsequence(datastructure.List(1, 2, 3, 4), datastructure.List(4)) should be(right = true)
    hasSubsequence(datastructure.List(1, 2, 3, 4), datastructure.List(5)) should be(right = false)
  }

}


