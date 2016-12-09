package advent.y2015

object Day13 {

  type Preferences = Map[(String, String), Int]

  object Preferences {
    private val LinePattern =
      """(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).""".r
    private val ActionSigns = Map("gain" -> 1, "lose" -> -1)

    def parse(input: String): Preferences = input.trim.lines
      .map(parseLine)
      .toMap
      .withDefaultValue(0)

    private def parseLine(line: String): ((String, String), Int) = line match {
      case LinePattern(subject, action, amount, company) =>
        (subject, company) -> (ActionSigns(action) * amount.toInt)
    }
  }

  def part1(input: String): Int = {
    val preferences = Preferences.parse(input)
    val persons = uniquePersons(preferences)
    maxHappiness(preferences, persons)
  }

  def part2(input: String): Int = {
    val preferences = Preferences.parse(input)
    val persons = uniquePersons(preferences) :+ "me"
    maxHappiness(preferences, persons)
  }

  private def uniquePersons(preferences: Preferences): Seq[String] =
    preferences.keySet.map(_._1).toSeq

  private def maxHappiness(preferences: Preferences, persons: Seq[String]): Int =
    persons.permutations.map(arrangement => happiness(preferences, arrangement)).max

  private def happiness(preferences: Preferences, arrangement: Seq[String]): Int =
    (arrangement :+ arrangement.head)
      .sliding(size = 2, step = 1)
      .map { case Seq(p1, p2) =>
        preferences(p1 -> p2) + preferences(p2 -> p1)
      }
      .sum

  def main(args: Array[String]): Unit = {
    val input = "Alice would lose 57 happiness units by sitting next to Bob.\nAlice would lose 62 happiness units by sitting next to Carol.\nAlice would lose 75 happiness units by sitting next to David.\nAlice would gain 71 happiness units by sitting next to Eric.\nAlice would lose 22 happiness units by sitting next to Frank.\nAlice would lose 23 happiness units by sitting next to George.\nAlice would lose 76 happiness units by sitting next to Mallory.\nBob would lose 14 happiness units by sitting next to Alice.\nBob would gain 48 happiness units by sitting next to Carol.\nBob would gain 89 happiness units by sitting next to David.\nBob would gain 86 happiness units by sitting next to Eric.\nBob would lose 2 happiness units by sitting next to Frank.\nBob would gain 27 happiness units by sitting next to George.\nBob would gain 19 happiness units by sitting next to Mallory.\nCarol would gain 37 happiness units by sitting next to Alice.\nCarol would gain 45 happiness units by sitting next to Bob.\nCarol would gain 24 happiness units by sitting next to David.\nCarol would gain 5 happiness units by sitting next to Eric.\nCarol would lose 68 happiness units by sitting next to Frank.\nCarol would lose 25 happiness units by sitting next to George.\nCarol would gain 30 happiness units by sitting next to Mallory.\nDavid would lose 51 happiness units by sitting next to Alice.\nDavid would gain 34 happiness units by sitting next to Bob.\nDavid would gain 99 happiness units by sitting next to Carol.\nDavid would gain 91 happiness units by sitting next to Eric.\nDavid would lose 38 happiness units by sitting next to Frank.\nDavid would gain 60 happiness units by sitting next to George.\nDavid would lose 63 happiness units by sitting next to Mallory.\nEric would gain 23 happiness units by sitting next to Alice.\nEric would lose 69 happiness units by sitting next to Bob.\nEric would lose 33 happiness units by sitting next to Carol.\nEric would lose 47 happiness units by sitting next to David.\nEric would gain 75 happiness units by sitting next to Frank.\nEric would gain 82 happiness units by sitting next to George.\nEric would gain 13 happiness units by sitting next to Mallory.\nFrank would gain 77 happiness units by sitting next to Alice.\nFrank would gain 27 happiness units by sitting next to Bob.\nFrank would lose 87 happiness units by sitting next to Carol.\nFrank would gain 74 happiness units by sitting next to David.\nFrank would lose 41 happiness units by sitting next to Eric.\nFrank would lose 99 happiness units by sitting next to George.\nFrank would gain 26 happiness units by sitting next to Mallory.\nGeorge would lose 63 happiness units by sitting next to Alice.\nGeorge would lose 51 happiness units by sitting next to Bob.\nGeorge would lose 60 happiness units by sitting next to Carol.\nGeorge would gain 30 happiness units by sitting next to David.\nGeorge would lose 100 happiness units by sitting next to Eric.\nGeorge would lose 63 happiness units by sitting next to Frank.\nGeorge would gain 57 happiness units by sitting next to Mallory.\nMallory would lose 71 happiness units by sitting next to Alice.\nMallory would lose 28 happiness units by sitting next to Bob.\nMallory would lose 10 happiness units by sitting next to Carol.\nMallory would gain 44 happiness units by sitting next to David.\nMallory would gain 22 happiness units by sitting next to Eric.\nMallory would gain 79 happiness units by sitting next to Frank.\nMallory would lose 16 happiness units by sitting next to George."
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
