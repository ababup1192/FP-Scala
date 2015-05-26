package org.ababup1192

import org.ababup1192.Either._
import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {

  "The mean function" should "return Either[String, Double] value" in {
    mean(IndexedSeq(1.0, 2.0, 3.0)) should be(Right(2.0))
    mean(IndexedSeq()) should be(Left("mean of empty list!"))
  }

  "The safeDiv function" should "return Either[Exception, Int] value" in {
    safeDiv(4, 2) should be(Right(2))
    (safeDiv(4, 0) match {
      case Left(_) => true
      case Right(_) => false
    }) should be(right = true)
  }

  "The parseInsuranceRateQuote function" should "return a new Either[Exception, Double] value" in {
    parseInsuranceRateQuote("12", "13") should be(Right(12 * 13 * 0.1))

    (parseInsuranceRateQuote("age", "13") match {
      case Left(_) => true
      case Right(_) => false
    }) should be(right = true)

    (parseInsuranceRateQuote("12", "ticket") match {
      case Left(_) => true
      case Right(_) => false
    }) should be(right = true)
  }


}


