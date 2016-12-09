package advent.y2015

object Day17 {

  def part1(amount: Int, containers: List[Int]): Int = combinations(amount, containers)

  private def combinations(amount: Int, containers: List[Int]): Int =
    if (amount == 0) 1
    else containers match {
      case Nil => 0
      case c :: cs if c > amount => combinations(amount, cs)
      case c :: cs => combinations(amount - c, cs) + combinations(amount, cs)
    }

  def part2(amount: Int, containers: List[Int]): Int =
    Stream.from(1)
      .map(k => kCombinations(k, amount, containers))
      .dropWhile(_ == 0)
      .head

  def kCombinations(k: Int, amount: Int, containers: List[Int]): Int =
    if (amount == 0 && k == 0) 1
    else if (amount == 0 || k == 0) 0
    else containers match {
      case Nil => 0
      case c :: cs if c > amount => kCombinations(k, amount, cs)
      case c :: cs => kCombinations(k - 1, amount - c, cs) + kCombinations(k, amount, cs)
    }

  def main(args: Array[String]): Unit = {
    val amount = 150
    val containers = List(43, 3, 4, 10, 21, 44, 4, 6, 47, 41, 34, 17, 17, 44, 36, 31, 46, 9, 27, 38)
    println("Part 1 result: " + part1(amount, containers))
    println("Part 2 result: " + part2(amount, containers))
  }
}
