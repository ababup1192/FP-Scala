package org.ababup1192

import org.ababup1192.Option._
import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  "The mean function" should "return Option[Double] value" in {
    mean(Seq()) should be(None)
    mean(Seq(1.0, 2.0, 3.0)) should be(Some(2.0))
  }

  "The map function" should "return a new Option value" in {
    def overTen(n: Int) = if (n > 10) Some(n) else None

    overTen(11).map(_ * 2) should be(Some(22))
    overTen(5).map(_ * 2) should be(None)
  }

  "The flatMap function" should "return a new Option value" in {
    def overTen(n: Int) = if (n > 10) Some(n) else None

    overTen(11).flatMap(x => Some(x * 2)) should be(Some(22))
    overTen(5).flatMap(x => Some(x * 2)) should be(None)
  }

  /*
  "The flatMap function" should "return a new Option value" in {
    Some(10).map(_ * 2) should be(Some(20))
  }
  */


}


