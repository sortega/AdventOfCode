package advent.y2015

import java.security.MessageDigest

object Day4 {

  def part1(seed: String): Int = mineAdventCoin(seed, 5)

  private def mineAdventCoin(seed: String, minZeroes: Int): Int = {
    Stream.from(1).filter { index => leadingZeroes(md5(seed + index)) >= minZeroes }.head
  }

  def part2(seed: String): Int = mineAdventCoin(seed, 6)

  private val digester = MessageDigest.getInstance("MD5")

  private def md5(str: String): String = {
    val hash = digester.digest(str.getBytes).map(_.formatted("%02X")).mkString
    digester.reset()
    hash
  }

  private def leadingZeroes(hexHash: String): Int = hexHash.takeWhile(_ == '0').length

  def main(args: Array[String]): Unit = {
    val seed = "bgvyzdsv"
    println("Part 1 result: " + part1(seed))
    println("Part 2 result: " + part2(seed))
  }
}
