package advent.y2017

import scalaz.Scalaz._

import advent.shared.Time.timed
import advent.shared.graphs.UndirectedAdjacencyGraph

object Day12 {
  type Node  = Int
  type Graph = UndirectedAdjacencyGraph[Node]

  object Graph {
    private object Grammar {
      import fastparse._, SingleLineWhitespace._
      def node[_: P]: P[Node]          = P(CharsWhileIn("0-9").!).map(_.toInt)
      def nodeList[_: P]: P[Set[Node]] = P(node.rep(min = 1, sep = ",").map(_.toSet))
      def adjacency[_: P]: P[Map[Node, Set[Node]]] = P(node ~ "<->" ~ nodeList ~ "\n").map {
        case (node, adjacent) => Map(node -> adjacent)
      }
      def graph[_: P]: P[Graph] =
        P(adjacency.rep ~ End).map(adj => UndirectedAdjacencyGraph.fromAdjacency(adj.toList.suml))
    }

    def parse(text: String): Graph = fastparse.parse(text, Grammar.graph(_)).get.value
  }

  def part1(input: String): Int = {
    val graph = Graph.parse(input)
    graph.connectedComponentOf(0).size
  }

  def part2(input: String): Int = Graph.parse(input).connectedComponents.size

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 12).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
