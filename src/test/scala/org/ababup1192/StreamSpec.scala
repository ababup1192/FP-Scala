package org.ababup1192

import org.ababup1192.stream._
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "The toList function" should "return a new List" in {
    Stream(1, 2, 3).toList should be(List(1, 2, 3))
  }

  "The take function" should "return a new Stream" in {
    Stream(1, 2, 3).take(2).toList should be(List(1, 2))
    Stream(1, 2, 3).take(0).toList should be(List())
    Stream(1, 2, 3).take(-1).toList should be(List())
  }

}


