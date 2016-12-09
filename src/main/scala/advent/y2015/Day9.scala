package advent.y2015

object Day9 {

  case class DistanceTable(entries: Map[(String, String), Int]) {
    val cities: Set[String] = entries.keys.flatMap { case (from, to) => Seq(from, to) }.toSet

    def distanceFor(itinerary: Seq[String]): Int = itinerary
      .sliding(size = 2, step = 1)
      .map { case Seq(f, t) => stepDistance(f, t) }
      .sum

    private def stepDistance(from: String, to: String): Int =
      entries.getOrElse(from -> to, entries(to -> from))
  }

  object DistanceTable {
    def apply(entries: ((String, String), Int)*): DistanceTable = DistanceTable(entries.toMap)

    def parse(input: String) = DistanceTable(input.lines.map(parseLine).toMap)

    private val LinePattern = """(\w+) to (\w+) = (\d+)""".r

    private def parseLine(line: String) = line match {
      case LinePattern(from, to, distance) => from -> to -> distance.toInt
    }
  }

  def part1(input: String): Int = allDistances(input).min

  private def allDistances(input: String): Iterator[Int] = {
    val table = DistanceTable.parse(input)
    val itineraries = table.cities.toSeq.permutations
    itineraries.map(table.distanceFor)
  }

  def part2(input: String): Int = allDistances(input).max

  def main(args: Array[String]): Unit = {
    val input = "Faerun to Norrath = 129\nFaerun to Tristram = 58\nFaerun to AlphaCentauri = 13\nFaerun to Arbre = 24\nFaerun to Snowdin = 60\nFaerun to Tambi = 71\nFaerun to Straylight = 67\nNorrath to Tristram = 142\nNorrath to AlphaCentauri = 15\nNorrath to Arbre = 135\nNorrath to Snowdin = 75\nNorrath to Tambi = 82\nNorrath to Straylight = 54\nTristram to AlphaCentauri = 118\nTristram to Arbre = 122\nTristram to Snowdin = 103\nTristram to Tambi = 49\nTristram to Straylight = 97\nAlphaCentauri to Arbre = 116\nAlphaCentauri to Snowdin = 12\nAlphaCentauri to Tambi = 18\nAlphaCentauri to Straylight = 91\nArbre to Snowdin = 129\nArbre to Tambi = 53\nArbre to Straylight = 40\nSnowdin to Tambi = 15\nSnowdin to Straylight = 99\nTambi to Straylight = 70"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
