package advent.shared

import scala.annotation.tailrec

object Streams {

  case class Cycle[A](repeatedState: A, firstSeenAt: Int, repeatedAt: Int)

  /** Iterate until detecting an infinite loop. */
  def iterateUntilCycling[A](a: A)(f: A => A): Cycle[A] = {
    val indexedStates = Stream.iterate(a)(f).zipWithIndex
    val seenStates = indexedStates.scanLeft(Map.empty[A, Int]) {
      case (seen, (a, index)) => seen + (a -> index)
    }
    (indexedStates, seenStates).zipped.collectFirst {
      case ((state, index), seenAt) if seenAt.contains(state) => Cycle(state, seenAt(state), index)
    }.get
  }

  def length(stream: Stream[_]): Int = {
    @tailrec def go(stream: Stream[_], accum: Int = 0): Int =
      stream match {
        case Stream.Empty => accum
        case _ #:: rest   => go(rest, accum + 1)
      }
    go(stream)
  }
}
