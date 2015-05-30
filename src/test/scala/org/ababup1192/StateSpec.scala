package org.ababup1192

import org.ababup1192.state._
import org.scalatest._

class StateSpec extends FlatSpec with Matchers {

  "The SimpleRNG function" should "return a (Int, RNG) value" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, _) = rng2.nextInt
    n1 -> n2 should be(16159453 -> -1281479697)
  }


}


