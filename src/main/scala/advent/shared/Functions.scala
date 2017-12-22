package advent.shared

object Functions {
  def repeatedly[A](n: Int)(f: A => A): A => A = a => Stream.fill(n)(f).foldLeft(a)((a, f) => f(a))
}
