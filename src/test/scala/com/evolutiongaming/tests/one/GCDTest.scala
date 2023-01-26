package com.evolutiongaming.tests.one

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class GCDTest extends AnyFreeSpec {
  "GCD" - {
    "gcd(54, 24)" in {
      GCD.gcd(54, 24) shouldEqual 6
    }
  }
}
