package org.ababup1192

import org.ababup1192.state._
import org.scalatest._

class StateSpec extends FlatSpec with Matchers {

  "The nextInt function" should "return a (Int, RNG) value" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, _) = rng2.nextInt
    n1 -> n2 should be(16159453 -> -1281479697)
  }

  "The nonNegativeInt function" should "return a (Int, RNG) value" in {
    val rng = SimpleRNG(42)
    val (n, _) = SimpleRNG.nonNegativeInt(rng)
    n should be(16159453)
  }

  "The double function" should "return a (double, RNG) value" in {
    val rng = SimpleRNG(42)
    val (n, _) = SimpleRNG.double(rng)
    n should be(0.0075248316826648865)
  }

  "The intDouble function" should "return a ((Int, Double), RNG) value" in {
    val rng = SimpleRNG(42)
    val ((i, d), _) = SimpleRNG.intDouble(rng)
    (i -> d) should be(16159453 -> 0.5967354846202138)
  }

  "The doubleInt function" should "return a ((Double, Int), RNG) value" in {
    val rng = SimpleRNG(42)
    val ((i, d), _) = SimpleRNG.intDouble(rng)
    (i -> d) should be(16159453 -> 0.5967354846202138)
  }

  "The double3 function" should "return a ((Double,Double,Double), RNG) value" in {
    val rng = SimpleRNG(42)
    val ((d1, d2, d3), _) = SimpleRNG.double3(rng)
    (d1, d2, d3) should be(0.0075248316826648865,
      0.5967354846202138, 0.158467283864288)
  }

  "The ints function" should "return a (List[Int], RNG) value" in {
    val rng = SimpleRNG(42)
    val (list, _) = SimpleRNG.ints(5)(rng)
    list should be(List(16159453, -1281479697, -340305902,
      -2015756020, 1770001318))
  }

}


