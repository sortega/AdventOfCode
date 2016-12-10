package advent.y2016

import scala.util.parsing.combinator.RegexParsers

object Day9 {

  object Grammar extends RegexParsers {
    case class Marker(size: BigInt, repetitions: BigInt)

    private def repeatedSection: Parser[BigInt] = marker.flatMap { marker =>
      textOf(marker.size) ^^ { _.length * marker.repetitions }
    }

    private def recursivelyRepeatedSection: Parser[BigInt] = marker.flatMap { marker =>
      textOf(marker.size) ^^ { nestedText =>
        Grammar.parseAll(Grammar.recursivelyCompressedText, nestedText).get * marker.repetitions
      }
    }

    private def marker: Parser[Marker] = "(" ~> number ~ "x" ~ number <~ ")" ^^ {
      case size ~ _ ~ repetitions => Marker(size, repetitions)
    }

    private def textOf(size: BigInt): Parser[String] = s""".{$size}""".r

    private def number: Parser[BigInt] = """\d+""".r ^^ { _.toInt }

    private def plainText: Parser[BigInt] = """[^(]+""".r ^^ { _.length }

    def compressedText: Parser[BigInt] = (repeatedSection | plainText).* ^^ { _.sum }

    def recursivelyCompressedText: Parser[BigInt] =
      (recursivelyRepeatedSection | plainText).* ^^ { _.sum }
  }

  def part1(input: String): BigInt = Grammar.parseAll(Grammar.compressedText, input).get

  def part2(input: String): BigInt = Grammar.parseAll(Grammar.recursivelyCompressedText, input).get

  def main(args: Array[String]): Unit = {
    val input = dailyResource(9).mkString
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
