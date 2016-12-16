package advent.y2016

import scala.annotation.tailrec

object Day16 {

  private def dragon(a: String): String = a + "0" + a.reverse.map {
    case '0' => '1'
    case '1' => '0'
  }

  private def dragons(initial: String) = Stream.iterate(initial)(dragon)

  @tailrec
  private def checksum(data: String): String =
    if (data.length % 2 == 1) data
    else checksum(data.grouped(2).map(pair => if (pair(0) == pair(1)) '1' else '0').mkString)

  def solve(input: String, diskSize: Int): String =
    checksum(dragons(input).filter(_.length >= diskSize).head.take(diskSize))

  def main(args: Array[String]): Unit = {
    val input = "10111011111001111"
    println("Part 1 result: " + solve(input, 272))
    println("Part 2 result: " + solve(input, 35651584))
  }
}
