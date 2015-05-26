package org.ababup1192

import org.ababup1192.stream._
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "The toList function" should "return a new List" in {
    Stream(1, 2, 3).toList should be(List(1, 2, 3))
  }

}


