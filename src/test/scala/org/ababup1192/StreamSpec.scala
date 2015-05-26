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

}


