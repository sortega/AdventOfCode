package advent.y2016

import java.security.MessageDigest
import scala.util.Try

object Day5 {

  private val digester = MessageDigest.getInstance("MD5")

  private def md5(str: String): String = {
    val hash = digester.digest(str.getBytes).map(_.formatted("%02X")).mkString
    digester.reset()
    hash
  }

  private def leadingZeroes(hexHash: String): Int = hexHash.prefixLength(_ == '0')

  private def doors(seed: String): Stream[String] = Stream.from(0).map(i => s"$seed$i")

  private def mine(seed: String, zeroes: Int) =
    doors(seed).map(md5).filter(hash => leadingZeroes(hash) >= zeroes).map(_.toLowerCase)

  def part1(seed: String, zeroes: Int = 5, passwordSize: Int = 8): String =
    mine(seed, zeroes).map(_.apply(zeroes)).take(passwordSize).mkString

  private def formatPassword(digits: Map[Int, Char], size: Int): String =
    (0 until size).map(i => digits.getOrElse(i, '*')).mkString

  private def toPartialPassword(hash: String, zeroes: Int, size: Int): Map[Int, Char] =
    Try(hash.substring(zeroes, zeroes + 1).toInt).toOption
      .filter(_ < size)
      .fold(Map.empty[Int, Char]) { index =>
        Map(index -> hash(zeroes + 1))
      }

  def part2(seed: String, zeroes: Int = 5, passwordSize: Int = 8): String = {
    val passwordDigits = mine(seed, zeroes)
      .scanLeft(Map.empty[Int, Char]) { (partialPassword, hash) =>
        val nextPassword = toPartialPassword(hash, zeroes, passwordSize) ++ partialPassword
        println(formatPassword(nextPassword, passwordSize))
        nextPassword
      }
      .find(_.size == passwordSize)
      .get
    formatPassword(passwordDigits, passwordSize)
  }

  def main(args: Array[String]): Unit = {
    val input = "cxdnnyjw"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
