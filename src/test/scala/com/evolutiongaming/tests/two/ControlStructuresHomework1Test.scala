package com.evolutiongaming.tests.two

import ControlStructuresHomework1._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.EitherValues._

class ControlStructuresHomework1Test extends AnyFreeSpec {

  def isAdultTests(isAdult: Int => Either[Error, Boolean]) = {
    "should return boolean for valid input" - {
      "for input 20" in {
        isAdult(20).value shouldBe true
      }

      "for input 13" in {
        isAdult(13).value shouldBe false
      }
    }

    "should return error for invalid input" - {
      "for input 99999" in {
        isAdult(99999).left.value shouldBe "99999 is too high, are you human?"
      }

      "for input -20" in {
        isAdult(-20).left.value shouldBe "-20 is negative, we do not serve unborn people"
      }
    }
  }

  "isAdult_if" - {
    isAdultTests(isAdultIf)
  }

  "isAdult_match" - {
    isAdultTests(isAdultMatch)
  }

  "isValidTriangle" - {
    "should determine if triangle is valid" - {
      "for input (2, 2, 3)" in {
        isValidTriangle(2, 2, 3) shouldBe true
      }

      "for input (2, 2, 0)" in {
        isValidTriangle(2, 2, 0) shouldBe false
      }

      "for input (2, 2, 4)" in {
        isValidTriangle(2, 2, 4) shouldBe false
      }
    }
  }

  "isValidCandidate" - {
    "should determine that candidate is valid" - {
      "for input (\"Wakanda\", 10, 10, true, 100)" in {
        isValidCandidate("Wakanda", 10, 10, true, 100) shouldBe true
      }
    }

    "should determine that candidate is not valid" - {
      "for input (\"US\", 5, 1, false, 0)" in {
        isValidCandidate("US", 5, 1, false, 0) shouldBe false
      }
    }
  }

}
