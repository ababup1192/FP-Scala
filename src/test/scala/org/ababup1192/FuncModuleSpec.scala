package org.ababup1192

import org.ababup1192.FuncModule._
import org.scalatest._

class FuncModuleSpec extends FlatSpec with Matchers {

  "The abs function" should "return an absolute value" in {
    abs(-1) should be(1)
    abs(0) should be(0)
    abs(1) should be(1)
  }

  "The factorial function" should "return Int value" in {
    factorial(5) should be(120)
  }

  "The fib function" should "return Int value" in {
    fib(5) should be(5)
  }

}


