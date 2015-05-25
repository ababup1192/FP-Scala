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

  "The getOrElse function" should "return a new value" in {
    def overTen(n: Int) = if (n > 10) Some(n) else None

    overTen(11).map(_ * 2).getOrElse(0) should be(22)
    overTen(5).map(_ * 2).getOrElse(0) should be(0)
  }

  "The orElse function" should "return a new Option value" in {
    def overTen(n: Int) = if (n > 10) Some(n) else None

    overTen(11).map(_ * 2).orElse(Some(0)) should be(Some(22))
    overTen(5).map(_ * 2).getOrElse(Some(0)) should be(Some(0))
  }

  "The filter function" should "return a new Option value" in {
    def overTen(n: Int) = if (n > 10) Some(n) else None

    overTen(12).map(_ * 3).filter(_ % 2 == 0) should be(Some(36))
    overTen(11).map(_ * 3).filter(_ % 2 == 0) should be(None)
  }

  "The variance function" should "return a new Int value" in {
    variance(Seq(50, 50, 50)) should be(Some(0))
    variance(Seq()) should be(None)
    variance(Seq(0, 0, 0)) should be(Some(0))
  }

  "The parseInsuranceRateQuote function" should "return a new Option[Double] value" in {
    parseInsuranceRateQuote("12", "13") should be(Some(12 * 13 * 0.1))
    parseInsuranceRateQuote("age", "13") should be(None)
    parseInsuranceRateQuote("12", "num") should be(None)
  }

  "The sequence function" should "return a new Option[List[A]] value" in {
    sequence(List(Some(1), Some(2), None, Some(3), None, Some(4))) should be(Some(List(1, 2, 3, 4)))
  }

  "The parseInts function" should "return a new Option[List[Int]]" in {
    parseInts(List("123", "hage", "456", "hoge")) should be(Some(List(123, 456)))
    parseInts(List("hage", "hoge")) should be(Some(Nil))
  }

}


