package advent.y2018

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import advent.shared.Time.timed

object Day9 {
  type Player = Int
  type Marble = Int

  final case class ImmutableCircle(marbles: List[Marble]) extends AnyVal {
    def current: Marble = marbles.head

    def insert(elem: Marble): ImmutableCircle = ImmutableCircle(elem :: marbles)

    def extract: (ImmutableCircle, Marble) = (ImmutableCircle(marbles.tail), current)

    def rotate(offset: Int): ImmutableCircle = {
      val (before, after) = marbles.splitAt(normalizeOffset(offset))
      ImmutableCircle(after ++ before)
    }

    private def normalizeOffset(offset: Int): Int =
      if (offset < 0) normalizeOffset(offset + marbles.size)
      else offset % marbles.size
  }

  object ImmutableCircle {
    val Initial = ImmutableCircle(List(0))

    def apply(marble: Marble, others: Marble*): ImmutableCircle =
      ImmutableCircle(marble :: others.toList)
  }

  final class Circle private (private var current: Circle.Node) {
    def this() = this {
      val node = new Circle.Node(prev = null, marble = 0, next = null)
      node.prev = node
      node.next = node
      node
    }

    def rotate(offset: Int): Unit = {
      if (offset > 0) {
        (1 to offset).foreach { _ =>
          current = current.next
        }
      } else if (offset < 0) {
        (1 to -offset).foreach { _ =>
          current = current.prev
        }
      }
    }

    def insert(marble: Marble): Unit = {
      val node = new Circle.Node(prev = current.prev, marble, next = current)
      current.prev.next = node
      current.prev = node
      current = node
    }

    def extract(): Marble = {
      val result = current.marble
      current.next.prev = current.prev
      current.prev.next = current.next
      current = current.next
      result
    }

    def toList: List[Int] = {
      val list   = ListBuffer.empty[Marble]
      var cursor = current
      list += cursor.marble
      while (cursor.next != current) {
        cursor = cursor.next
      }
      list.toList
    }
  }

  object Circle {
    private final class Node(var prev: Node, val marble: Marble, var next: Node)
  }

  final class Game(circle: Circle = new Circle,
                   scores: mutable.Map[Player, Long] = mutable.Map.empty.withDefaultValue(0)) {

    def play(player: Player, marble: Marble): Unit = {
      if (special(marble)) playExtraction(player, marble) else playInsertion(marble)
    }

    def playerScores: Map[Player, Long] = scores.toMap

    private def special(marble: Marble): Boolean = marble % 23 == 0

    private def playExtraction(player: Player, marble: Marble): Unit = {
      circle.rotate(offset = -7)
      val extraMarble = circle.extract()
      scores.update(player, scores(player) + marble + extraMarble)
    }

    private def playInsertion(marble: Marble): Unit = {
      circle.rotate(offset = 2)
      circle.insert(marble)
    }
  }

  object Game {
    def playFor(numPlayers: Int, lastMarble: Int): Map[Player, Long] = {
      val game = new Game
      (1 to lastMarble).foreach { marble =>
        val player = marble % numPlayers
        game.play(player, marble)
      }
      game.playerScores
    }
  }

  def part1(numPlayers: Int, lastMarble: Int): Long =
    Game.playFor(numPlayers, lastMarble).values.max

  def part2(numPlayers: Int, lastMarble: Int): Long =
    Game.playFor(numPlayers, lastMarble * 100).values.max

  def main(args: Array[String]): Unit = {
    val numPlayers = 493
    val lastMarble = 71863
    timed(println("Part 1 result: " + part1(numPlayers, lastMarble)))
    timed(println("Part 2 result: " + part2(numPlayers, lastMarble)))
  }
}
