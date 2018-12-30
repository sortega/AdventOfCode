package advent.y2018

import advent.shared.Time.timed
import advent.shared.geom._
import scalaz.Scalaz._

import advent.shared.geom.CardinalPoint._

object Day13 {

  final case class Track(segments: Map[Point, Track.Segment]) {
    val rect: Rect = Rect.enclosing(segments.keys.toList).get

    def charAt(point: Point): Char = segments.get(point).fold(' ')(_.chars.head)
  }

  object Track {
    sealed abstract class Segment(val chars: Char*)
    case object Horizontal   extends Segment('-', '<', '>')
    case object Vertical     extends Segment('|', 'v', '^')
    case object Intersection extends Segment('+')
    case object Curve1       extends Segment('/')
    case object Curve2       extends Segment('\\')

    val Segments: Set[Segment] = ca.mrvisser.sealerate.values[Segment]
  }

  final case class Cart(pos: Point, facing: CardinalPoint, timesTurned: Int = 0) {
    import Track._

    def tick(track: Track): Cart = {
      val nextPos = pos + facing.delta.hadamardProduct(Point(1, -1))
      val (nextFacing, turned) = track.segments(nextPos) match {
        case Horizontal | Vertical => (facing, false)
        case Intersection          => (nextSteering.apply(facing), true)
        case Curve1 =>
          facing match {
            case North => (East, false)
            case South => (West, false)
            case East  => (North, false)
            case West  => (South, false)
          }
        case Curve2 =>
          facing match {
            case North => (West, false)
            case South => (East, false)
            case East  => (South, false)
            case West  => (North, false)
          }
      }
      Cart(nextPos, nextFacing, timesTurned + (if (turned) 1 else 0))
    }

    private def nextSteering: Steering = timesTurned % 3 match {
      case 0 => Steering.Left
      case 1 => Steering.Straight
      case 2 => Steering.Right
    }
  }

  object Cart {
    def parse(char: Char, point: Point): Option[Cart] = CardinalPoint.parse(char).map { dir =>
      Cart(pos = point, facing = dir)
    }
  }

  final case class State(track: Track, carts: List[Cart]) {
    def tick: State         = copy(carts = tickCarts)
    def crashes: Set[Point] = carts.groupBy(_.pos).filter(_._2.size > 1).keySet
    def crashed: Boolean    = crashes.nonEmpty
    def removeCrashed: State =
      copy(carts = carts.groupBy(_.pos).filter(_._2.size == 1).values.flatten.toList)

    private def tickCarts: List[Cart] = {
      val cartsInOrder = carts.sortBy(cart => (cart.pos.y, cart.pos.x)).toArray
      for (i <- cartsInOrder.indices) {
        val current = cartsInOrder(i)
        if (cartsInOrder.count(_.pos == current.pos) == 1) {
          cartsInOrder(i) = current.tick(track)
        }
      }
      cartsInOrder.toList
    }

    override def toString: String = {
      val builder = new StringBuilder
      builder ++= track.rect.xRange.map(col => col / 10 % 10).mkString(" ", "", "\n")
      builder ++= track.rect.xRange.map(col => col      % 10).mkString(" ", "", "\n")

      for (y <- track.rect.yRange) {
        builder ++= track.rect.xRange
          .map { x =>
            val point     = Point(x, y)
            val cartsHere = carts.filter(_.pos == point)
            cartsHere match {
              case Nil            => track.charAt(point)
              case List(onlyCart) => onlyCart.facing.char
              case _              => 'X'
            }
          }
          .mkString((y % 10).toString, "", "")
          .trim
        builder += '\n'
      }

      builder.toString
    }
  }

  object State {
    def parse(lines: List[String]): State = {
      val (segments, carts) = (for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex
        point = Point(x, y)
      } yield {
        val maybeSegment = Track.Segments.find(_.chars.contains(char))
        val maybeCart    = Cart.parse(char, point)
        (maybeSegment.toList.map(segment => point -> segment), maybeCart.toList)
      }).suml
      State(Track(segments.toMap), carts)
    }
  }

  def part1(initial: State): Point = {
    val crashes = Stream
      .iterate(initial)(_.tick)
      .map(_.crashes)
      .filter(_.nonEmpty)
      .head
    crashes.head
  }

  def part2(initial: State): Point =
    Stream
      .iterate(initial)(_.tick.removeCrashed)
      .collect {
        case State(_, List(lastCart)) => lastCart.pos
      }
      .head

  def main(args: Array[String]): Unit = {
    val initial = State.parse(inputResource(day = 13).getLines().toList)
    timed(println("Part 1 result: " + part1(initial)))
    timed(println("Part 2 result: " + part2(initial)))
  }
}
