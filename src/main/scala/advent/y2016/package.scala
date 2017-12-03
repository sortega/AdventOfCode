package advent

import scala.io.{BufferedSource, Source}

package object y2016 {
  def dailyResource(day: Int): BufferedSource =
    Source.fromInputStream(getClass.getResourceAsStream(s"day$day.input"))
}
