package advent.y2016

import java.security.MessageDigest

object Hash {
  private val digester = MessageDigest.getInstance("MD5")

  def md5(str: String): String = {
    val hash = digester.digest(str.getBytes).map(_.formatted("%02x")).mkString
    digester.reset()
    hash
  }

  def md5Stream(seed: String): Stream[String] =
    Stream.from(0).map(i => s"$seed$i").map(Hash.md5)
}
