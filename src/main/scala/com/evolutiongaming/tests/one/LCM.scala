package com.evolutiongaming.tests.one

object LCM {
  /**
    * Implement https://en.wikipedia.org/wiki/Least_common_multiple for integers.
    *
    * Return 0 if LCM is undefined.
    */
  def lcm(a: Int, b: Int): Int = {
    val gcd = GCD.gcd(a, b)
    if (gcd == 0 ) 0 else (math.abs(a) / gcd ) * math.abs(b)
  }
}
