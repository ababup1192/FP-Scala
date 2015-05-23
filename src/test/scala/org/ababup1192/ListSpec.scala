package org.ababup1192

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "Empty List" should "be equal Nil" in {
    List() should be === Nil
  }

  "List" should "construct Cons and Nil" in {
    List("a", "b", "c") should be === Cons("a", Cons("b", Cons("c", Nil)))
  }

  "Cons" should "have values with several type" in {
    List(1.0, 2, 3.0, 4) should be === Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))
  }

  "Int List" should "be summed" in {
    List.sum(List(1, 2, 3, 4)) should be === 10
  }

  "Int List" should "be multiplied" in {
    List.product(List(1.0, 2.0, 3.0, 4.0)) should be === 24.0
  }

  "The tail function" should "return new List" in {
    List.tail(List(1, 2, 3)) should be === List(2, 3)
  }

  "The setHead function" should "return new List" in {
    List.setHead(List(1, 2, 3), 5) should be === List(5, 2, 3)
  }


}


