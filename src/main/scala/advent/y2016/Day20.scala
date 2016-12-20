package advent.y2016

import scala.annotation.tailrec

object Day20 {
  val Max = 4294967295L

  case class Range(from: Long, to: Long) {
    def size: Long = to - from + 1
    def union(other: Range): Range = Range(from min other.from, to max other.to)
    def compatibleWith(other: Range): Boolean = overlaps(other) || touches(other)
    def overlaps(other: Range): Boolean =
      contains(other.from) || contains(other.to) || other.contains(from) || other.contains(to)
    def contains(point: Long): Boolean = point >= from && point <= to
    def touches(other: Range): Boolean = (to + 1) == other.from || (other.to + 1) == from
  }

  object Range {
    def parse(line: String): Range = {
      val Array(from, to) = line.split("-").map(_.toLong)
      Range(from, to)
    }
  }

  private def fuseRanges(ranges: List[Range]): List[Range] = {
    @tailrec
    def go(ranges: List[Range], fusedRanges: List[Range]): List[Range] =
      (ranges, fusedRanges) match {
        case (Nil, _) => fusedRanges.reverse
        case (range :: remaining, Nil) => go(remaining, List(range))
        case (range :: remaining, compatibleRange :: otherFusedRanges)
            if range.compatibleWith(compatibleRange) =>
          go(remaining, range.union(compatibleRange) :: otherFusedRanges)
        case (range :: remaining, _) => go(remaining, range :: fusedRanges)
      }

    go(ranges, List.empty)
  }

  private def invert(ranges: List[Range]): List[Range] = {
    def go(ranges: List[Range], available: Range): List[Range] = ranges match {
      case Nil => Nil
      case _ if available.size <= 0 => Nil
      case range :: other if available.from < range.from =>
        Range(available.from, range.from - 1) :: go(other, Range(range.to + 1, available.to))
      case range :: other if available.from == range.from =>
        go(other, Range(range.to + 1, available.to))
    }

    go(ranges, Range(0, Max))
  }

  def part1(blacklist: List[Range]): Long =
    invert(fuseRanges(blacklist.sortBy(_.from))).head.from

  def part2(blacklist: List[Range]): Long =
    invert(fuseRanges(blacklist.sortBy(_.from))).map(_.size).sum

  def main(args: Array[String]): Unit = {
    val blacklist = dailyResource(20).getLines().map(Range.parse).toList
    println("Part 1 result: " + part1(blacklist))
    println("Part 2 result: " + part2(blacklist))
  }
}
