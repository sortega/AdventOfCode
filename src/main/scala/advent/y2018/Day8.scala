package advent.y2018

import advent.shared.Time.timed

object Day8 {

  type Input     = List[Int]
  type Parser[A] = Input => (Input, A)

  final case class Tree(metadata: List[Int], children: List[Tree] = Nil) {
    def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum

    def value: Int =
      if (children.isEmpty) metadata.sum
      else {
        val lookup = children.lift
        metadata.flatMap(pos => lookup(pos - 1)).map(_.value).sum
      }
  }

  object Tree {
    def parse(input: String): Tree = fastparse.parse(input, Parser.tree(_)).get.value

    private object Parser {
      import fastparse._, JavaWhitespace._

      def tree[_: P]: P[Tree] = P(node ~ End)

      def node[_: P]: P[Tree] =
        P(header)
          .flatMap {
            case (numChildren, metadataSize) =>
              body(numChildren, metadataSize)
          }
          .map {
            case (children, metadata) =>
              Tree(metadata.toList, children.toList)
          }

      def header[_: P]: P[(Int, Int)] = P(number ~ number)

      def body[_: P](numChildren: Int, metadataSize: Int): P[(Seq[Tree], Seq[Int])] =
        P(
          node.rep(min = numChildren, max = numChildren) ~
            number.rep(min = metadataSize, max = metadataSize)
        )

      def number[_: P]: P[Int] = P(CharsWhileIn("0-9").!.map(_.toInt))
    }
  }

  def part1(tree: Tree): Int = tree.metadataSum

  def part2(tree: Tree): Int = tree.value

  def main(args: Array[String]): Unit = {
    val input = Tree.parse(inputResource(day = 8).mkString)
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
