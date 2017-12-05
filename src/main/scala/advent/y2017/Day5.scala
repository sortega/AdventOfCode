package advent.y2017

import scala.annotation.tailrec

import advent.shared.Time.timed

object Day5 {

  case class State(instructions: Vector[Int], next: Int = 0, update: Int => Int = inc) {
    def step: Option[State] =
      if (instructions.indices.contains(next)) Some {
        val offset = instructions(next)
        copy(instructions = instructions.updated(next, update(offset)), next = next + offset)
      } else None

    def states: Stream[State] = {
      def iterate(seed: State): Stream[State] =
        seed #:: seed.step.fold(Stream.empty[State])(iterate)
      iterate(this)
    }
  }

  object State {
    def apply(instructions: Int*): State = State(instructions.toVector)
  }

  def part1(input: Vector[Int]): Int = stepsUntilEscaping(State(input))

  def part2(input: Vector[Int]): Int = stepsUntilEscaping(State(input, next = 0, part2update _))

  private def inc(n: Int): Int = n + 1

  private def part2update(n: Int): Int = if (n >= 3) n - 1 else n + 1

  private def stepsUntilEscaping(initialState: State): Int =
    streamLength(initialState.states) - 1

  @tailrec
  private def streamLength[A](stream: Stream[A], length: Int = 0): Int = stream match {
    case Stream.Empty => length
    case _ #:: tail   => streamLength(tail, length + 1)
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(5).getLines().map(_.toInt).toVector
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
