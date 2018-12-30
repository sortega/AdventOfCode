package advent.shared.geom

sealed abstract class Steering {
  def apply(dir: CardinalPoint): CardinalPoint
}

object Steering {
  case object Left extends Steering {
    override def apply(dir: CardinalPoint): CardinalPoint = dir.turnLeft
  }

  case object Straight extends Steering {
    override def apply(dir: CardinalPoint): CardinalPoint = dir
  }

  case object Right extends Steering {
    override def apply(dir: CardinalPoint): CardinalPoint = dir.turnRight
  }
}
