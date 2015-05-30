package org.ababup1192

import org.ababup1192.stream._
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "The toList function" should "return a new List" in {
    Stream(1, 2, 3).toList should be(List(1, 2, 3))
  }

  "The take function" should "return a new Stream" in {
    Stream(1, 2, 3).take(2).toList should be(List(1, 2))
    Stream(1, 2, 3).take(3).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).take(4).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).take(0).toList should be(List())
    Stream(1, 2, 3).take(-1).toList should be(List())
  }

  "The drop function" should "return a new Stream" in {
    Stream(1, 2, 3).drop(2).toList should be(List(3))
    Stream(1, 2, 3).drop(3).toList should be(List())
    Stream(1, 2, 3).drop(4).toList should be(List())
    Stream(1, 2, 3).drop(0).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).drop(-1).toList should be(List(1, 2, 3))
  }

  "The dropWhile function" should "return a new Stream" in {
    Stream(1, 2, 3, 4).dropWhile(_ < 3).toList should be(List(3, 4))
  }

  "The takeWhile function" should "return a new Stream" in {
    Stream(1, 2, 3, 4).takeWhile(_ < 3).toList should be(List(1, 2))
  }

  "The empty function" should "return a Boolean value" in {
    Stream(1, 3, 5, 7, 9).exists(_ % 2 == 0) should be(right = false)
    Stream(1, 3, 5, 7, 9).exists(_ % 2 != 0) should be(right = true)
  }

  "The forAll function" should "return a Boolean value" in {
    Stream(2, 4, 5, 6, 8, 10).forAll(_ % 2 == 0) should be(right = false)
    Stream(2, 4, 6, 8, 10).forAll(_ % 2 == 0) should be(right = true)
  }

  "The map function" should "return a Stream value" in {
    Stream(1, 2, 3, 4).map(_ * 2).toList should be(List(2, 4, 6, 8))
  }

  "The filter function" should "return a Stream value" in {
    Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList should be(List(2, 4, 6))
  }

  "The append function" should "return a Stream value" in {
    Stream(1, 2, 3).append(Stream(4, 5, 6)).toList should be((1 to 6).toList)
  }

  "The find function" should "return a Option value" in {
    Stream(1, 2, 3).find(_ % 2 == 0) should be(Some(2))
  }

  "The constant function" should "return an infinite Stream value" in {
    Stream.constant(1).take(4).toList should be(List(1, 1, 1, 1))
  }

  "The from function" should "return an infinite Stream value" in {
    Stream.from(3).take(4).toList should be(List(3, 4, 5, 6))
  }

  "The fib function" should "return an infinite Stream value" in {
    Stream.fib(6).toList should be(List(0, 1, 1, 2, 3, 5))
  }

  "The zipAll function" should "return an infinite Stream value" in {
    Stream(1, 2, 3).zipAll(Stream(5, 6, 7, 8)).toList should
      be(List(Some(1) -> Some(5), Some(2) -> Some(6), Some(3) -> Some(7), None -> Some(8)))
  }

  "The startsWith function" should "return a boolean value" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(right = true)
    Stream(1, 1, 2).startsWith(Stream(1, 2)) should be(right = false)
  }

  "The tails function" should "return a Stream[Stream]] value" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) should be(List(
      List(1, 2, 3), List(2, 3), List(3), List()))
  }

}


