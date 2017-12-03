package advent.y2016

import advent.geom.Point
import advent.y2016.Day24._
import org.scalatest.{FlatSpec, Matchers}

class Day24Test extends FlatSpec with Matchers {

  private val sampleMaze = Maze.parse("""###########
                                        |#0.1.....2#
                                        |#.#######.#
                                        |#4.......3#
                                        |###########
                                        |""".stripMargin)

  "A maze" should "be parsed" in {
    sampleMaze shouldBe Maze(
      spots = Map(0 -> Point(1, 1),
                  1 -> Point(3, 1),
                  2 -> Point(9, 1),
                  3 -> Point(9, 3),
                  4 -> Point(1, 3)),
      openSpaces = (1 to 9).toSet
          .flatMap((x: Int) => Set(Point(x, 1), Point(x, 3))) ++ Set(Point(1, 2), Point(9, 2))
    )
  }

  it should "compute distance between two points" in {
    sampleMaze.distance(Point(1, 1), Point(4, 3)) shouldBe 5
  }

  it should "provide the distance map for spots" in {
    sampleMaze.spotDistances shouldBe Map(
      (0, 2) -> 8,
      (0, 0) -> 0,
      (4, 0) -> 2,
      (3, 4) -> 8,
      (3, 1) -> 8,
      (4, 1) -> 4,
      (2, 0) -> 8,
      (0, 3) -> 10,
      (4, 4) -> 0,
      (3, 0) -> 10,
      (1, 1) -> 0,
      (1, 4) -> 4,
      (0, 4) -> 2,
      (3, 2) -> 2,
      (1, 3) -> 8,
      (2, 2) -> 0,
      (4, 2) -> 10,
      (2, 4) -> 10,
      (0, 1) -> 2,
      (3, 3) -> 0,
      (2, 3) -> 2,
      (1, 2) -> 6,
      (2, 1) -> 6,
      (4, 3) -> 8,
      (1, 0) -> 2
    )

  }

  "Part 1" should "get the shortest tourné" in {
    Day24.part1("""###########
                  |#0.1.....2#
                  |#.#######.#
                  |#4.......3#
                  |###########
                  |""".stripMargin) shouldBe 14
  }

  "Part 2" should "get the shortest tourné coming back to the starting point" in {
    Day24.part2("""###########
                  |#0.1.....2#
                  |#.#######.#
                  |#4.......3#
                  |###########
                  |""".stripMargin) shouldBe 20
  }
}
