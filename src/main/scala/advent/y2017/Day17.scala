package advent.y2017

import advent.shared.Time.timed

object Day17 {

  case class CircularBuffer(elems: Vector[Int], offset: Int, step: Int) {
    def insert(elem: Int): CircularBuffer = {
      val newOffset    = (offset + step) % elems.size + 1
      val (start, end) = elems.splitAt(newOffset)
      copy(elems = (start :+ elem) ++ end, offset = newOffset)
    }

    override def toString: String =
      elems.zipWithIndex.map {
        case (elem, `offset`) => s"($elem)"
        case (elem, _)        => s" $elem "
      }.mkString
  }

  object CircularBuffer {
    def withStep(step: Int): CircularBuffer = CircularBuffer(Vector(0), offset = 0, step)
  }

  /** Like CircularBuffer but just tracks the element to the right of zero */
  case class ChosenElementBuffer(chosenElem: Int, size: Int, offset: Int, step: Int) {
    def insert(elem: Int): ChosenElementBuffer = {
      val newOffset = (offset + step) % size + 1
      copy(chosenElem = if (newOffset == 1) elem else chosenElem,
           size = size + 1,
           offset = newOffset)
    }
  }

  object ChosenElementBuffer {
    def withStep(step: Int): ChosenElementBuffer =
      ChosenElementBuffer(chosenElem = 0, size = 1, offset = 0, step)
  }

  def part1(input: Int): Int = {
    val buffer      = (1 to 2017).foldLeft(CircularBuffer.withStep(input))(_.insert(_))
    val answerIndex = (buffer.elems.indexOf(2017) + 1) % buffer.elems.size
    buffer.elems(answerIndex)
  }

  def part2(input: Int): Int =
    (1 to 50000000).foldLeft(ChosenElementBuffer.withStep(input))(_.insert(_)).chosenElem

  def main(args: Array[String]): Unit = {
    val input = 343
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
