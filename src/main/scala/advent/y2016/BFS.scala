package advent.y2016

import scala.collection.mutable
import scalaz._
import Scalaz._
import scala.collection.immutable.Queue

object BFS {

  def shortestPaths[Node](root: Node,
                          neighbors: Node => Set[Node]): (Map[Node, Double], Map[Node, Node]) = {
    val distance = mutable.Map(root -> 0d).withDefaultValue(Double.PositiveInfinity)
    val cameFrom = mutable.Map.empty[Node, Node]
    val queue = mutable.Queue[Node](root)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      for (candidate <- neighbors(current) if !distance(candidate).isInfinity) {
        distance.update(candidate, distance(candidate) + 1)
        cameFrom.update(candidate, current)
        queue.enqueue(candidate)
      }
    }

    (distance.toMap, cameFrom.toMap)
  }

  def traverse[Node](root: Node, neighbors: Node => Set[Node]): Stream[List[Node]] = {
    case class State(queue: Queue[List[Node]], seen: Set[Node])

    val seed = State(Queue(List(root)), Set.empty)
    unfold(seed) { state =>
      state.queue.dequeueOption.map {
        case (current :: path, remaining) =>
          val candidates = neighbors(current).map(node => node :: current :: path)
          ((current :: path).reverse, State(remaining.enqueue(candidates), state.seen + current))
      }
    }
  }
}
