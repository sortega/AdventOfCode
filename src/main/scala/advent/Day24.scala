package advent

object Day24 {

  def subsetsInAscendingSize(elems: Set[Int]): Stream[Set[Int]] =
    (1 to elems.size).toStream.flatMap(size => subsetsOfSize(size, elems))

  private def subsetsOfSize(size: Int, elems: Set[Int]): Stream[Set[Int]] =
    if (size == 0) Stream(Set.empty)
    else for {
      chosen <- elems.toSeq.sorted.toStream
      notChosen = elems.filter(_ > chosen)
      notChosenSubset <- subsetsOfSize(size - 1, notChosen)
    } yield notChosenSubset + chosen

  def isDivisibleIntoEqualParts(elems: Set[Int], n: Int): Boolean = {
    val size = elems.sum
    if (size % n != 0) false
    else isDivisibleInGroups(elems.toSeq.sorted, Seq.fill(n)(size / n))
  }

  private def isDivisibleInGroups(elems: Seq[Int], groups: Seq[Int]): Boolean =
    if (elems.isEmpty && groups.sum == 0) true
    else groups.indices
      .filter(groupIndex => elems.head <= groups(groupIndex))
      .exists { groupIndex =>
        isDivisibleInGroups(elems.tail, groups.updated(groupIndex, groups(groupIndex) - elems.head))
      }


  private def balance(input: Set[Int], totalGroups: Int): Long = {
    val groupWeight = input.sum / totalGroups
    val group1 = subsetsInAscendingSize(input)
      .filter(_.sum == groupWeight)
      .filter(group1 => isDivisibleIntoEqualParts(input diff group1, totalGroups - 1))
      .head
    entanglement(group1)
  }

  private def entanglement(elems: Set[Int]): Long = elems.map(_.toLong).product

  def part1(input: Set[Int]): Long = balance(input, 3)

  def part2(input: Set[Int]): Long = balance(input, 4)

  def main(args: Array[String]): Unit = {
    val input = Set(1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79,
      83, 89, 97, 101, 103, 107, 109, 113)
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
