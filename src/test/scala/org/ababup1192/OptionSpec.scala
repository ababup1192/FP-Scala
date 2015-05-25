package org.ababup1192

import org.ababup1192.Option._
import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  "The mean function" should "return Option[Double] value" in {
    mean(Seq()) should be(None)
    mean(Seq(1.0, 2.0, 3.0)) should be(Some(2.0))
  }

}


