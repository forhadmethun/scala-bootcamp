package com.evolutiongaming.tests.one

object GCD {
  /**
    * Implement https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.
    *
    * Return 0 if GCD is undefined.
    */
  def gcd(a: Int, b: Int): Int = {
    val a_pos = math.abs(a)
    val b_pos = math.abs(b)
    if (b_pos == 0) a_pos else gcd(b_pos, a_pos % b_pos)
  }
}
