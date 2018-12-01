package advent

import scala.io.{BufferedSource, Source}

package object y2018 {
  def inputResource(day: Int): BufferedSource =
    Source.fromInputStream(getClass.getResourceAsStream(s"day$day.input"))
}
