package advent.shared

object Math {

  def gcd(a: Int, b: Int): Int =
    if (a < 0 || b < 0) gcd(a.abs, b.abs)
    else if (b < a) gcd(b, a)
    else if (b == 0) a
    else gcd(b, a % b)

  def lcm(a: Int, b: Int): Int = (a * b) / gcd(a, b)
}
