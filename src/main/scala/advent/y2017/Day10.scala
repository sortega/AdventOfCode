package advent.y2017

import advent.shared.Time.timed

object Day10 {

  case class Cycle(elems: Vector[Int], offset: Int, skipSize: Int) {
    def pinchAndTwist(length: Int): Cycle = {
      val (toReverse, toKeep) = elems.splitAt(length)
      val reversed            = toReverse.reverse ++ toKeep
      val advance             = length + skipSize
      Cycle(elems = rotate(advance, reversed),
            offset = (offset + advance) % elems.size,
            skipSize = skipSize + 1)
    }

    def toVector: Vector[Int] = {
      val (end, start) = elems.splitAt(elems.size - offset)
      start ++ end
    }

    private def rotate[A](n: Int, elems: Vector[A]): Vector[A] = {
      val index        = n % elems.size
      val (start, end) = elems.splitAt(index)
      end ++ start
    }
  }

  object Cycle {
    def withLength(length: Int) = Cycle(Vector.range(0, length), offset = 0, skipSize = 0)
  }

  def part1(input: Vector[Int], length: Int = 256): Int =
    input.foldLeft(Cycle.withLength(length))(_.pinchAndTwist(_)).toVector.take(2).product

  def part2(input: String): String = toHex(denseHash(sparseHash(decodeAscii(input))))

  private def decodeAscii(text: String) = text.map(_.toInt).toVector

  private def sparseHash(seed: Vector[Int]): Vector[Int] =
    Vector
      .fill(64)(seed ++ Vector(17, 31, 73, 47, 23))
      .flatten
      .foldLeft(Cycle.withLength(256))(_.pinchAndTwist(_))
      .toVector

  private def denseHash(sparseHash: Vector[Int]): Vector[Int] =
    sparseHash.grouped(size = 16).map(_.reduce(_ ^ _)).toVector

  private def toHex(bytes: Vector[Int]): String = bytes.map(_.formatted("%02x")).mkString

  def main(args: Array[String]): Unit = {
    val input = Vector(83, 0, 193, 1, 254, 237, 187, 40, 88, 27, 2, 255, 149, 29, 42, 100)
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input.mkString(","))))
  }
}
