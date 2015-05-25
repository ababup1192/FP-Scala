package org.ababup1192

import org.scalatest._

class FuncModuleSpec extends FlatSpec with Matchers {

  "The abs function" should "return an absolute value" in {
    FuncModule.abs(-1) should be(1)
    FuncModule.abs(0) should be(0)
    FuncModule.abs(1) should be(1)
  }

}


