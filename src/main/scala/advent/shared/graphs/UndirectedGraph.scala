package advent.shared.graphs

import scala.annotation.tailrec

trait UndirectedGraph[Node] {
  def nodes: Set[Node]
  def adjacentTo(node: Node): Set[Node]

  def connectedComponentOf(node: Node): Set[Node] = {
    @tailrec
    def expandComponent(open: List[Node], closed: Set[Node]): Set[Node] = open match {
      case Nil => closed
      case node :: remainingOpen if nodes.contains(node) && !closed.contains(node) =>
        expandComponent(remainingOpen ++ adjacentTo(node), closed + node)
      case _ :: remainingOpen => expandComponent(remainingOpen, closed)
    }

    expandComponent(open = List(node), closed = Set.empty)
  }

  def connectedComponents: List[Set[Node]] = {
    @tailrec
    def allConnectedComponents(unassignedNodes: Set[Node],
                               components: List[Set[Node]] = Nil): List[Set[Node]] =
      if (unassignedNodes.isEmpty) components
      else {
        val component = connectedComponentOf(unassignedNodes.head)
        allConnectedComponents(unassignedNodes.diff(component), component +: components)
      }

    allConnectedComponents(nodes)
  }
}
