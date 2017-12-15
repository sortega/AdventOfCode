package advent.shared.graphs

import advent.shared.Point

case class PointGraph(nodes: Set[Point]) extends UndirectedGraph[Point] {
  override def adjacentTo(node: Point): Set[Point] = node.adjacent4
}
