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
    Stream(1, 2, 3).append(Stream(4,5,6)).toList should be((1 to 6).toList)
  }

}


