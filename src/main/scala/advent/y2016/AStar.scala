package advent.y2016

import scala.collection.mutable
import scala.math.Ordering
import scalaz.Scalaz.some

class AStar[Node](distance: (Node, Node) => Double,
                  neighbors: Node => Set[Node],
                  heuristic: (Node, Node) => Double) {
  type Path = List[Node]

  def search(start: Node, goal: Node): Option[Path] = {
    // Cost of getting to a node from the starting point
    val gScore = mutable.Map(start -> 0d).withDefaultValue(Double.PositiveInfinity)

    // Estimated cost of getting to the goal from a point
    val fScore =
      mutable.Map(start -> heuristic(start, goal)).withDefaultValue(Double.PositiveInfinity)

    val cameFrom = mutable.Map.empty[Node, Node]

    // Discarded nodes
    val closed = mutable.HashSet.empty[Node]

    // Paths being searched
    val openQueue =
      mutable.PriorityQueue[Node](start)(Ordering.by[Node, Double](node => -fScore(node)))
    val openSet = mutable.HashSet.empty[Node]

    while (openQueue.nonEmpty) {
      val current = openQueue.dequeue()
      openSet.remove(current)

      if (current == goal) return Some(reconstructPath(cameFrom, current))
      else {
        closed += current
        for (neighbor <- neighbors(current) if !closed.contains(neighbor)) {
          val newGScore = gScore(current) + distance(current, neighbor)

          if (!openSet.contains(neighbor)) {
            openQueue.enqueue(neighbor)
            openSet.add(neighbor)
          }
          if (newGScore < gScore(neighbor)) {
            cameFrom.put(neighbor, current)
            gScore.put(neighbor, newGScore)
            fScore.put(neighbor, newGScore + heuristic(neighbor, goal))
          }
        }
      }
    }

    None
  }

  private def reconstructPath(cameFrom: mutable.Map[Node, Node], current: Node): Path =
    Stream
      .iterate(some(current))(node => node.flatMap(cameFrom.get))
      .takeWhile(_.nonEmpty)
      .flatten
      .toList
      .reverse
}