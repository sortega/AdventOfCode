package advent.y2016

object Day15 {

  case class Disc(number: Int, positions: Int, startsAt: Int) {
    def aligned(time: Int): Boolean = (number + startsAt + time) % positions == 0
  }

  def apply(discs: Seq[Disc]): Int = Stream.from(0).find(t => discs.forall(_.aligned(t))).get

  def main(args: Array[String]): Unit = {
    val input1 = Seq(
      Disc(1, positions = 13, startsAt = 10),
      Disc(2, positions = 17, startsAt = 15),
      Disc(3, positions = 19, startsAt = 17),
      Disc(4, positions = 7, startsAt = 1),
      Disc(5, positions = 5, startsAt = 0),
      Disc(6, positions = 3, startsAt = 1)
    )
    println("Part 1 result: " + apply(input1))

    val input2 = input1 :+ Disc(7, positions = 11, startsAt = 0)
    println("Part 2 result: " + apply(input2))
  }
}
