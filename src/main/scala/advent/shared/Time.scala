package advent.shared

object Time {
  def timed[A](block: => A): A = {
    val startMillis = System.currentTimeMillis()
    try block
    finally println(s"elapsed ${System.currentTimeMillis() - startMillis} ms")
  }
}
