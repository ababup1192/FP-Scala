package org.ababup1192

import org.ababup1192.Either._
import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {

  "The mean function" should "return Either[String, Double] value" in {
    mean(IndexedSeq(1.0, 2.0, 3.0)) should be(Right(2.0))
    mean(IndexedSeq()) should be(Left("mean of empty list!"))
  }


}


