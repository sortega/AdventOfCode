package advent.y2016

import scalaz._, Scalaz._

object Day22 {

  case class Node(pos: Point, size: Int, used: Int) {
    require(used <= size)
    def available: Int = size - used
    def isEmpty: Boolean = used == 0
    def empty: Node = copy(used = 0)
  }

  object Node {
    private val Pattern = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+\d+T\s+\d+%""".r

    def parse(line: String): Node = {
      val Pattern(x, y, size, used) = line
      Node(Point(x.toInt, y.toInt), size.toInt, used.toInt)
    }
  }

  def part1(input: String): Int = {
    val nodes = input.lines.map(Node.parse).toVector
    (for {
      a <- nodes if !a.isEmpty
      b <- nodes if (a != b) && a.used <= b.available
    } yield 1).sum
  }

  private def visualizeMap(nodes: Vector[Node]) = {
    val nodeMap = nodes.map(n => n.pos -> n).toMap
    val maxX = nodeMap.values.map(_.pos.x).max
    val maxY = nodeMap.values.map(_.pos.y).max
    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        val node = nodeMap(Point(x, y))
        print(f"${node.used}%03d/${node.size}%03d ")
      }
      println()
    }

    println("Size hist: " + nodes.foldMap(n => Map(n.size -> 1)).toSeq.sortBy(-_._1))
    println("Avail hist: " + nodes.foldMap(n => Map(n.available -> 1)).toSeq.sortBy(-_._1))
    println("Used hist: " + nodes.foldMap(n => Map(n.used -> 1)).toSeq.sortBy(_._1))
  }

  val MaxX = 29
  val MaxY = 34
  val TargetPos = Point.Origin

  case class State(goalData: Point, hole: Point, nodes: Set[Point]) {
    def nextStates: Set[State] =
      for {
        to <- hole.adjacent if nodes.contains(to)
      } yield
        copy(
          goalData = if (to == goalData) hole else goalData,
          hole = to
        )

    def heuristic: Double = {
      val moveGoalData = goalData.norm1 * 5
      val getToTheGoalData = (hole - goalData).norm1 - 1
      moveGoalData + getToTheGoalData
    }

    override def toString: String =
      (for (y <- 0 to MaxY) yield {
        (for (x <- 0 to MaxX) yield {
          val pos = Point(x, y)
          if (pos == goalData) 'G'
          else if (pos == hole) '_'
          else if (nodes.contains(pos)) '.'
          else '#'
        }).mkString
      }).mkString("", "\n", "\n")

//    override def toString: String = s"Hole at $hole, data at $goalData"
  }

  def part2(input: String): Int = {
    val nodes = input.lines.map(Node.parse).toVector
    visualizeMap(nodes)

    val searcher =
      new AStar[State](distance = (_, _) => 1d, neighbors = _.nextStates, heuristic = _.heuristic)

    val initialState =
      State(goalData = Point(MaxX, y = 0),
            hole = Point(x = 28, y = MaxY),
            nodes.filter(n => n.used < 100).map(_.pos).toSet)
    val goalState = initialState.copy(goalData = TargetPos, hole = Point(x = 1, y = 0))

    val path = searcher.search(initialState, _.goalData == TargetPos).get
    path.foreach(s => println(s + "\n"))
    path.size - 1
  }

  def main(args: Array[String]): Unit = {
    val input = dailyResource(22).getLines().drop(2).mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input))) // 182 is too low
  }
}
