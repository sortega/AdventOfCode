package advent

import advent.Day9.DistanceTable
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day9Test extends FlatSpec with ShouldMatchers {

  "A distance table" should "be parsed" in {
    DistanceTable.parse(
      """London to Dublin = 464
        |London to Belfast = 518
        |Dublin to Belfast = 141""".stripMargin) // shouldBe ???
  }

  it should "list its cities" in {
    ???
  }

  it should "measure itinerary distances" in {
    ???
  }

  "Part 1" should "compute the shortest itinerary" in {
    Day9.part1(
      """London to Dublin = 464
        |London to Belfast = 518
        |Dublin to Belfast = 141""".stripMargin) shouldBe 605
  }

  "Part 2" should "compute the longest itinerary" in {
    Day9.part2(
      """London to Dublin = 464
        |London to Belfast = 518
        |Dublin to Belfast = 141""".stripMargin) shouldBe 982
  }
}
