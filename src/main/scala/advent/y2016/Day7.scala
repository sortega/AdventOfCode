package advent.y2016

object Day7 {

  case class IPv7(entries: Vector[IPv7.Entry]) {
    def tlsCompliant: Boolean = {
      val (squaredEntries, notSquaredEntries) = entries.partition(_.squared)
      notSquaredEntries.exists(_.abba) && squaredEntries.forall(!_.abba)
    }

    def sslCompliant: Boolean = {
      val (squaredEntries, notSquaredEntries) = entries.partition(_.squared)
      val candidateBabs = notSquaredEntries.toStream.flatMap(_.abas).map(toBab)
      candidateBabs.exists { bab =>
        squaredEntries.exists(_.content.contains(bab))
      }
    }

    private def toBab(aba: String): String = "%c%c%c".format(aba(1), aba(0), aba(1))
  }

  object IPv7 {
    case class Entry(squared: Boolean, content: String) {
      def abba: Boolean = content.sliding(size = 4, step = 1).exists(nonUniformPalindrome)

      def abas: Stream[String] =
        content.sliding(size = 3, step = 1).toStream.filter(nonUniformPalindrome)
    }

    private def nonUniformPalindrome(string: String) =
      string.length > 1 && string == string.reverse && string(0) != string(1)

    def parse(input: String): IPv7 = {
      val entries =
        """\[?([a-z]+)\]?""".r.findAllMatchIn(input).map { m =>
          Entry(squared = m.matched.startsWith("["), content = m.group(1))
        }
      IPv7(entries.toVector)
    }
  }

  def part1(input: Seq[String]): Int = input.map(IPv7.parse).count(_.tlsCompliant)

  def part2(input: Seq[String]): Int = input.map(IPv7.parse).count(_.sslCompliant)

  def main(args: Array[String]): Unit = {
    val input = dailyResource(7).getLines().toSeq
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
