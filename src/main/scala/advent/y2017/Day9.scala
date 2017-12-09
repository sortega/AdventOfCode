package advent.y2017

import advent.shared.Time.timed
import fastparse.all._

object Day9 {

  sealed trait Content {
    def score: Int = score(nesting = 1)
    def score(nesting: Int): Int
    def garbageAmount: Int
  }

  case class Garbage(size: Int) extends Content {
    override def score(nesting: Int): Int = 0
    override def garbageAmount: Int       = size
  }

  case class Group(children: Seq[Content]) extends Content {
    def score(nesting: Int): Int =
      nesting + children.map(_.score(nesting + 1)).sum
    override def garbageAmount: Int = children.map(_.garbageAmount).sum
  }

  object Content {
    private object Grammar {
      val group: P[Group] =
        P("{" ~/ content.rep(sep = "," ~/ Pass) ~ "}").map(children => Group(children))

      val garbage: P[Garbage] =
        P("<" ~/ (regularChars | cancelledChar).rep ~ ">").map(sizes => Garbage(sizes.sum))
      val regularChars: P[Int]  = CharsWhile(c => c != '>' && c != '!').!.map(_.length)
      val cancelledChar: P[Int] = P("!" ~ AnyChar).map(_ => 0)

      val content: P[Content] = P(group | garbage)
    }

    def parse(text: String): Content = Grammar.content.parse(text).get.value
  }

  def part1(input: String): Int = Content.parse(input).score

  def part2(input: String): Int = Content.parse(input).garbageAmount

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 9).getLines().next()
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
