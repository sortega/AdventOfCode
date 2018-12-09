package advent.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day9Test extends FlatSpec with Matchers {

  "Part 1" should "compute the points of the winner player" in {
    Day9.part1(numPlayers = 9, lastMarble = 25) should ===(32)
    Day9.part1(numPlayers = 10, lastMarble = 1618) should ===(8317)
    Day9.part1(numPlayers = 17, lastMarble = 1104) should ===(2764)
    Day9.part1(numPlayers = 21, lastMarble = 6111) should ===(54718)
    Day9.part1(numPlayers = 30, lastMarble = 5807) should ===(37305)
  }

  "Part 2" should "do 100 times more work than part 1" in {
    Day9.part2(numPlayers = 30, lastMarble = 5807) should ===(
      Day9.part1(numPlayers = 30, lastMarble = 580700))
  }
}
