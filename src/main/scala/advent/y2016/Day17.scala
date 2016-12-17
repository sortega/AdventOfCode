package advent.y2016

object Day17 {

  case class State(pos: Point, path: String = "") {
    def adjacentStates(seed: String): Set[State] = {
      val hash = Hash.md5(seed + path)
      for {
        (index, dir, delta) <- Set(
          (0, 'U', Point(x = 0, y = -1)),
          (1, 'D', Point(x = 0, y = 1)),
          (2, 'L', Point(x = -1, y = 0)),
          (3, 'R', Point(x = 1, y = 0))
        )
        nextPos = pos + delta
        if nextPos.x >= 0 && nextPos.x < 4 && nextPos.y >= 0 && nextPos.y < 4 && isOpen(
          hash(index))
      } yield State(nextPos, path + dir)
    }

    private val isOpen = "bcdef".toSet

    def isGoal: Boolean = pos == Point(3, 3)

    override def equals(obj: scala.Any): Boolean =
      obj match {
        case other: State =>
          isGoal && other.isGoal || (pos == other.pos && path == other.path)
        case _ => false
      }

    override def hashCode(): Int = pos.hashCode()
  }

  object State {
    val Initial = State(Point(0, 0))
    val Goal = State(Point(3, 3))
  }

  class Solver(seed: String) {

    private def heuristic(l: State, r: State): Double = (l.pos - r.pos).norm1

    private val searcher =
      new AStar[State](distance = (_, _) => 1, neighbors = _.adjacentStates(seed), heuristic)

    def shortestPath: String = searcher.search(State.Initial, State.Goal).get.last.path
  }

  def part1(seed: String): String = new Solver(seed).shortestPath

  def part2(seed: String): Int =
    BFS
      .traverse[State](State.Initial, _.adjacentStates(seed).filter(!_.isGoal))
      .filter(_.last.adjacentStates(seed).contains(State.Goal))
      .map(_.size)
      .max

  def main(args: Array[String]): Unit = {
    val seed = "pxxbnzuo"
    println("Part 1 result: " + part1(seed))
    println("Part 2 result: " + part2(seed))
  }
}
