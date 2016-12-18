package advent

import scala.io.{BufferedSource, Source}

package object y2016 {
  def dailyResource(day: Int): BufferedSource =
    Source.fromInputStream(getClass.getResourceAsStream(s"day$day.input"))

  def timed[A](block: => A): A = {
    val startMillis = System.currentTimeMillis()
    try block
    finally println(s"elapsed ${System.currentTimeMillis() - startMillis} ms")
  }
}
