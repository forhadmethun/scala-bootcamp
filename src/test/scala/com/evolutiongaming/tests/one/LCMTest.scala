package com.evolutiongaming.tests.one

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class LCMTest extends AnyFreeSpec {
  "LCM" - {
    "lcm(4, 6)" in {
      LCM.lcm(4, 6) shouldEqual 12
    }
  }
}
