package advent.y2015

import scala.annotation.tailrec

object Day11 {

  val Alphabet = "abcdefghijklmnopqrstuvwxyz"
  val NumLetters = Alphabet.length

  def toNumber(password: String): Long = toLetterIndices(password).foldLeft(0L)(_ * NumLetters + _)

  private def toLetterIndices(password: String) = password.map(c => Alphabet.indexOf(c.toInt))

  def toPassword(number: Long): String =
    Seq.iterate(number, 8)(_ / NumLetters)
      .map(n => (n % NumLetters).toInt)
      .map(index => Alphabet.charAt(index))
      .reverse
      .mkString

  def isCompliant(password: String): Boolean =
    hasAnIncreasingTriplet(password) &&
      hasNoForbiddenChar(password) &&
      hasTwoDoubledLetters(password)

  private def hasAnIncreasingTriplet(password: String): Boolean =
    toLetterIndices(password)
      .toList
      .sliding(size = 3, step = 1)
      .exists(isIncreasing)

  @tailrec
  private def isIncreasing(seq: List[Int]): Boolean = seq match {
    case Nil => true
    case _ :: Nil => true
    case first :: second :: rest if first + 1 == second => isIncreasing(second :: rest)
    case _ => false
  }

  private def hasNoForbiddenChar(password: String): Boolean =
    "i|o|l".r.unanchored.findFirstIn(password).isEmpty

  private def hasTwoDoubledLetters(password: String): Boolean =
    "(.)\\1.*(.)\\2".r.unanchored.findFirstIn(password).isDefined

  def part1(input: String): String = (for {
    number <- Stream.iterate(toNumber(input) + 1)(_ + 1)
    password = toPassword(number)
    if isCompliant(password)
  } yield password).head

  def part2(input: String): String = part1(part1(input))

  def main(args: Array[String]): Unit = {
    val input = "cqjxjnds"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
