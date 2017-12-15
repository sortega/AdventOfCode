package advent.y2017

import scalaz.Scalaz._

import advent.shared.Time.timed
import advent.shared.graphs.UndirectedAdjacencyGraph
import fastparse.WhitespaceApi
import fastparse.all._

object Day12 {
  type Node  = Int
  type Graph = UndirectedAdjacencyGraph[Node]

  object Graph {
    private object Grammar {
      val White = WhitespaceApi.Wrapper(NoTrace(" ".rep))
      import White._
      val node: P[Node]          = P(CharsWhileIn('0' to '9').!).map(_.toInt)
      val nodeList: P[Set[Node]] = P(node.rep(min = 1, sep = ",").map(_.toSet))
      val adjacency: P[Map[Node, Set[Node]]] = P(node ~ "<->" ~ nodeList ~ "\n").map {
        case (node, adjacent) => Map(node -> adjacent)
      }
      val graph: P[Graph] =
        P(adjacency.rep).map(adj => UndirectedAdjacencyGraph.fromAdjacency(adj.toList.suml))
    }

    def parse(text: String): Graph = Grammar.graph.parse(text).get.value
  }

  def part1(input: String): Int = Graph.parse(input).connectedComponentOf(0).size

  def part2(input: String): Int = Graph.parse(input).connectedComponents.size

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 12).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
