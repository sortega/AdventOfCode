package advent.y2016

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day22Test extends FlatSpec with ShouldMatchers {

  "Part 1" should "count the number of viable pairs" in {
    Day22.part1("""/dev/grid/node-x0-y0     85T   64T    21T   75%
                  |/dev/grid/node-x0-y1     92T   64T    28T   69%
                  |/dev/grid/node-x0-y2     89T    0T    89T    0%
                  |""".stripMargin) shouldBe 2
  }

  "Part 2" should "???" in {}
}
