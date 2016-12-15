package advent.y2016

object Day14 {

  case class Stats(firstTriplet: Option[Char], quintets: Set[Char])

  object Stats {
    def apply(hash: String): Stats =
      Stats(firstTriplet = uniformSubStrings(hash, windowSize = 3).headOption,
            quintets = uniformSubStrings(hash, windowSize = 5).toSet)

    private def uniformSubStrings(string: String, windowSize: Int) =
      string
        .sliding(windowSize, step = 1)
        .collect { case group if group.toSet.size == 1 => group.head }
        .toSeq
  }

  private def validKey(candidate: Stats, context: Stream[Stats]): Boolean =
    candidate.firstTriplet.exists { char =>
      context.exists(stats => stats.quintets.contains(char))
    }

  private def validKeyIndices(stats: Stream[Stats]) =
    stats.sliding(size = 1001, step = 1).toStream.zipWithIndex.collect {
      case (candidate #:: context, index) if validKey(candidate, context) => index
    }

  private def statsStream(seed: String, rounds: Int): Stream[Stats] =
    Stream.from(0).map { i =>
      Stats(List.fill(rounds)(Hash.md5 _).foldLeft(s"$seed$i")((value, f) => f(value)))
    }

  def part1(seed: String): Int = validKeyIndices(statsStream(seed, rounds = 1)).apply(63)

  def part2(seed: String): Int = validKeyIndices(statsStream(seed, rounds = 2017)).apply(63)

  def main(args: Array[String]): Unit = {
    val seed = "jlmsuwbz"
    println("Part 1 result: " + part1(seed))
    println("Part 2 result: " + part2(seed))
  }
}
