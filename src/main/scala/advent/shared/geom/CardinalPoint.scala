package advent.shared.geom

sealed abstract class CardinalPoint(val delta: Point, val char: Char) {
  def turnRight: CardinalPoint
  def turnLeft: CardinalPoint
}

object CardinalPoint {
  case object North extends CardinalPoint(delta = Point(0, 1), char = '^') {
    override def turnRight: CardinalPoint = East
    override def turnLeft: CardinalPoint = West
  }
  case object East extends CardinalPoint(delta = Point(1, 0), char = '>') {
    override def turnRight: CardinalPoint = South
    override def turnLeft: CardinalPoint = North
  }
  case object South extends CardinalPoint(delta = Point(0, -1), char = 'v') {
    override def turnRight: CardinalPoint = West
    override def turnLeft: CardinalPoint = East
  }
  case object West extends CardinalPoint(delta = Point(-1, 0), char = '<') {
    override def turnRight: CardinalPoint = North
    override def turnLeft: CardinalPoint = South
  }

  val All: Set[CardinalPoint] = ca.mrvisser.sealerate.values[CardinalPoint]

  def parse(char: Char): Option[CardinalPoint] = All.find(_.char == char)
}
