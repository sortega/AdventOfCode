package advent.shared

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
}
