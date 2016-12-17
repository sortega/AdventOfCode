package advent.y2016

class Day13(seed: Int) {

  private def adjacentPoints(point: Point): Set[Point] =
    point.adjacent.filter(p => (p.x >= 0) && (p.y >= 0) && openSpace(p))

  private def openSpace(point: Point): Boolean = {
    import point._
    bits(x * x + 3 * x + 2 * x * y + y + y * y + seed).count(identity) % 2 == 0
  }

  private def bits(x: Int): Stream[Boolean] =
    Stream.iterate(x)(_ / 2).takeWhile(_ > 0).map(_ % 2 == 1)

  private def manhattanDistance(l: Point, r: Point): Double = (l - r).norm1

  private val searcher = new AStar[Point](
    distance = manhattanDistance,
    neighbors = adjacentPoints,
    heuristic = manhattanDistance
  )

  private def printMap(rows: Int, cols: Int, path: Set[Point]): Unit = {
    (0 until rows).foreach { row =>
      (0 until cols).foreach { col =>
        val point = Point(col, row)
        print(
          if (path.contains(point) && !openSpace(point)) "@"
          else if (path.contains(point)) "x"
          else if (openSpace(point)) "."
          else "#"
        )
      }
      println()
    }
  }

  private val start = Point(1, 1)

  def part1: Int = {
    val path = searcher.search(start, goal = Point(x = 31, y = 39)).get
    printMap(rows = 39, cols = 31, path.toSet)
    path.size - 1
  }

  def part2: Int = {
    val reachable =
      Stream
        .iterate(Set(start))(reached => reached.union(reached.flatMap(adjacentPoints)))
        .apply(50)
    printMap(rows = 39, cols = 31, reachable)
    reachable.size
  }
}

object Day13 extends App {
  val day13 = new Day13(1358)
  println("Part 1 result: " + day13.part1)
  println("Part 2 result: " + day13.part2)
}
