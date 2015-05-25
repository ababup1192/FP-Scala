package org.ababup1192

import org.scalatest._
import FuncModule._

class FuncModuleSpec extends FlatSpec with Matchers {

  "The abs function" should "return an absolute value" in {
    abs(-1) should be(1)
    abs(0) should be(0)
    abs(1) should be(1)
  }

}


