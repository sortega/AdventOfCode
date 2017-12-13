package advent.y2017

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz.std.stream.unfold

import advent.shared.Time.timed
import fastparse.WhitespaceApi
import fastparse.all._

object Day12 {
  type Node = Int

  case class Graph(adjacentTo: Map[Node, Set[Node]]) {
    def connectedComponentOf(node: Node): Set[Node] = {
      @tailrec
      def go(component: Set[Node], queue: List[Node]): Set[Node] =
        queue match {
          case Nil                                      => component
          case node :: rest if component.contains(node) => go(component, rest)
          case node :: rest =>
            go(component + node, rest ++ adjacentTo(node))
        }

      go(component = Set.empty, queue = List(node))
    }

    def connectedComponents: List[Set[Node]] = {
      val allNodes = (0 to adjacentTo.flatMap {
        case (key, values) => key +: values.toSeq
      }.max).toSet
      unfold(allNodes) { remainingNodes =>
        remainingNodes.headOption.map { node =>
          val component = connectedComponentOf(node)
          (component, remainingNodes -- component)
        }
      }.toList
    }
  }

  object Graph {
    private object Grammar {
      val White = WhitespaceApi.Wrapper(NoTrace(" ".rep))
      import White._
      val node: P[Node]          = P(CharsWhileIn('0' to '9').!).map(_.toInt)
      val nodeList: P[Set[Node]] = P(node.rep(min = 1, sep = ",").map(_.toSet))
      val adjacency: P[Map[Node, Set[Node]]] = P(node ~ "<->" ~ nodeList ~ "\n").map {
        case (node, adjacent) =>
          // Generate the reciprocals
          Map(node -> adjacent) ++ adjacent.map(a => a -> Set(node))
      }
      val graph: P[Graph] =
        P(adjacency.rep).map(adj => Graph(adj.toList.suml.withDefaultValue(Set.empty)))
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
