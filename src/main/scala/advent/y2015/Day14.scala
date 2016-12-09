package advent.y2015

object Day14 {

  case class Reindeer(speed: Int, flyTime: Int, restTime: Int) {

    private val cycleTime = flyTime + restTime
    private val cycleDistance = speed * flyTime

    def distanceAt(time: Int): Int =
      if (time < cycleTime) speed * (time min flyTime)
      else cycleDistance * (time / cycleTime) + distanceAt(time % cycleTime)

    def distances: Stream[Int] = Stream.from(1).map(distanceAt)
  }

  object Reindeer {
    private val LinePattern =
      """\w+ can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

    def parse(line: String): Reindeer = {
      val LinePattern(speed, flyTime, restTime) = line
      Reindeer(speed.toInt, flyTime.toInt, restTime.toInt)
    }

    def parseList(input: String): Seq[Reindeer] = input.trim.lines.map(Reindeer.parse).toSeq
  }

  def part1(input: String, raceDuration: Int): Int = Reindeer.parseList(input)
    .map(_.distanceAt(raceDuration))
    .max

  def part2(input: String, raceDuration: Int): Int = {
    val racers = Reindeer.parseList(input)
    val racerDistances = racers.map(_.distances.take(raceDuration).toSeq)
    val maxDistances = racerDistances.transpose.map(_.max)

    def points(distances: Seq[Int]): Int =
      distances.zip(maxDistances).count { case (l, r) => l == r }

    racerDistances.map(points).max
  }

  def main(args: Array[String]): Unit = {
    val input = "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.\nCupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.\nPrancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.\nDonner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.\nDasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.\nComet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.\nBlitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.\nVixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.\nDancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds."
    println("Part 1 result: " + part1(input, 2503))
    println("Part 2 result: " + part2(input, 2503))
  }
}
