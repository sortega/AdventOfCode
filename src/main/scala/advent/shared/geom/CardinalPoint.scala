package advent.shared.geom

trait CardinalPoint {
  def delta: Point
  def turnRight: CardinalPoint
  def turnLeft: CardinalPoint
}

object CardinalPoint {
  case object North extends CardinalPoint {
    override val delta = Point(0, 1)
    override def turnRight: CardinalPoint = East
    override def turnLeft: CardinalPoint = West
  }
  case object East extends CardinalPoint {
    override val delta = Point(1, 0)
    override def turnRight: CardinalPoint = South
    override def turnLeft: CardinalPoint = North
  }
  case object South extends CardinalPoint {
    override val delta = Point(0, -1)
    override def turnRight: CardinalPoint = West
    override def turnLeft: CardinalPoint = East
  }
  case object West extends CardinalPoint {
    override val delta = Point(-1, 0)
    override def turnRight: CardinalPoint = North
    override def turnLeft: CardinalPoint = South
  }

  val All = Set[CardinalPoint](North, East, South, West)
}
