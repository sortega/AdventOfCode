package advent.shared.graphs

import scalaz.Scalaz._

final case class UndirectedAdjacencyGraph[Node] private (adjacency: Map[Node, Set[Node]])
    extends UndirectedGraph[Node] {
  override def nodes: Set[Node]                  = adjacency.keySet
  override def adjacentTo(node: Node): Set[Node] = adjacency(node)
}

object UndirectedAdjacencyGraph {
  def fromAdjacency[Node](adjacency: Map[Node, Set[Node]]): UndirectedAdjacencyGraph[Node] = {
    val reciprocalAdjacency = adjacency.toStream.foldMap {
      case (source, targets) =>
        targets.map(target => target -> Set(source)).toMap
    }
    new UndirectedAdjacencyGraph[Node](adjacency |+| reciprocalAdjacency)
  }
}
