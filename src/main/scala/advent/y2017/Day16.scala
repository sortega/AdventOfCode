package advent.y2017

import advent.shared.Streams
import advent.shared.Time.timed
import fastparse.all._

object Day16 {

  val DefaultPrograms = 16
  val Iterations      = 1000000000

  sealed trait Move

  object Move {
    case class Spin(offset: Int)         extends Move
    case class Exchange(a: Int, b: Int)  extends Move
    case class Partner(a: Char, b: Char) extends Move

    private object Grammar {
      val id: P[Char] = P(CharIn('a' to 'z').!).map(_.head).opaque("identifier")
      val int: P[Int] = P(CharIn('0' to '9').rep(1).!).map(_.toInt)

      val move: P[Move]         = P(spin | exchange | partner)
      val spin: P[Spin]         = P("s" ~ int).map(Spin.apply)
      val exchange: P[Exchange] = P("x" ~ int ~ "/" ~ int).map(Exchange.tupled)
      val partner: P[Partner]   = P("p" ~ id ~ "/" ~ id).map(Partner.tupled)

      val dance: P[Seq[Move]] = P(move.rep(sep = ",") ~ End)
    }

    def parseDance(input: String): Seq[Move] = Grammar.dance.parse(input).get.value
  }

  case class Queue(programs: Vector[Char]) extends AnyVal {
    def update(move: Move): Queue = move match {
      case Move.Spin(offset) =>
        val (start, end) = programs.splitAt(programs.length - offset % programs.length)
        Queue(end ++ start)

      case Move.Exchange(a, b) =>
        copy(programs.updated(a, programs(b)).updated(b, programs(a)))

      case Move.Partner(a, b) =>
        copy(programs.map {
          case `a`   => b
          case `b`   => a
          case other => other
        })
    }

    override def toString: String = programs.mkString
  }

  object Queue {
    def ofSize(n: Int): Queue = Queue(('a' to 'z').take(n).toVector)
  }

  def part1(input: String, size: Int = DefaultPrograms): String = {
    val dance = Move.parseDance(input)
    val queue = Queue.ofSize(size)
    dance.foldLeft(queue)(_.update(_)).toString
  }

  def part2(input: String, size: Int = DefaultPrograms): String = {
    val queue   = Queue.ofSize(size)
    val dance   = Move.parseDance(input)
    val states  = Stream.continually(dance).flatten.scanLeft(queue)(_.update(_))
    val indices = Stream.from(0).map(_ % dance.length)
    val cycle   = Streams.detectCycle(states.zip(indices))
    states(cycle.equivalentIndexTo(Iterations)).toString
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 16).getLines().next()
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
