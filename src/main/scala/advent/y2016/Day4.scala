package advent.y2016

object Day4 {

  val Alphabet = ('a' to 'z').toVector

  case class Room(id: Int, name: String, checksum: String) {
    def isValid: Boolean = checksum == generateChecksum

    private def generateChecksum: String = {
      val counts = name.groupBy(identity).mapValues(-_.length) - '-'
      val mostFrequent = counts.toList.map(_.swap).sorted.take(5)
      mostFrequent.map(_._2).mkString
    }

    def decrypt: String = name.map {
      case '-' => ' '
      case char => toChar(toIndex(char) + id)
    }

    def toIndex(c: Char): Int = Alphabet.indexOf(c)
    def toChar(i: Int): Char = Alphabet(i % Alphabet.length)
  }

  object Room {
    private val Pattern = """([-a-z]*?)-(\d+)\[([a-z]{5})\]""".r

    def parseAll(input: String): Seq[Room] = input.split("\n").map {
      case Pattern(name, id, checksum) => Room(id.toInt, name, checksum)
    }
  }

  def part1(input: String): Int = Room.parseAll(input).filter(_.isValid).map(_.id).sum

  def part2(input: String): Int =
    Room.parseAll(input).find(_.decrypt == "northpole object storage").get.id

  def main(args: Array[String]): Unit = {
    val input = dailyResource(4).mkString
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
