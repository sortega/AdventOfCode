package advent

import advent.Day9.DistanceTable
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day9Test extends FlatSpec with ShouldMatchers {

  private val table = DistanceTable(
    "London" -> "Dublin" -> 464,
    "London" -> "Belfast" -> 518,
    "Dublin" -> "Belfast" -> 141
  )

  "A distance table" should "be parsed" in {
    DistanceTable.parse(
      """London to Dublin = 464
        |London to Belfast = 518
        |Dublin to Belfast = 141""".stripMargin) shouldBe table
  }

  it should "list its cities" in {
    table.cities shouldBe Set("Belfast", "Dublin", "London")
  }

  it should "measure itinerary distances" in {
    table.distanceFor(Seq("Dublin", "London", "Belfast")) shouldBe 982
    table.distanceFor(Seq("Belfast", "Dublin", "London")) shouldBe 605
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
