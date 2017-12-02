package advent.y2016

import scala.util.Random

import advent.y2016.Day21._
import org.scalatest.{FlatSpec, Matchers}

class Day21Test extends FlatSpec with Matchers {

  "Part 1" should "execute operations" in {
    Operation.SwapIndices(4, 0).apply("abcde") shouldBe "ebcda"
    Operation.SwapLetters('d', 'b').apply("ebcda") shouldBe "edcba"
    Operation.ReverseRange(0, 4).apply("edcba") shouldBe "abcde"
    Operation.ReverseRange(1, 3).apply("edcba") shouldBe "ebcda"
    Operation.RotateSteps(Left, 1).apply("abcde") shouldBe "bcdea"
    Operation.RotateSteps(Right, 1).apply("abcde") shouldBe "eabcd"
    Operation.RotateSteps(Left, 6).apply("abcde") shouldBe "bcdea"
    Operation.Move(1, 4).apply("bcdea") shouldBe "bdeac"
    Operation.Move(3, 0).apply("bdeac") shouldBe "abdec"
    Operation.RotateByLetter('b').apply("abdec") shouldBe "ecabd"
    Operation.RotateByLetter('d').apply("ecabd") shouldBe "decab"
  }

  "Part 2" should "invert operations" in {
    Operation.SwapIndices(4, 0).reverse("ebcda") shouldBe "abcde"
    Operation.SwapLetters('d', 'b').reverse("edcba") shouldBe "ebcda"
    Operation.ReverseRange(0, 4).reverse("abcde") shouldBe "edcba"
    Operation.ReverseRange(1, 3).reverse("ebcda") shouldBe "edcba"
    Operation.RotateSteps(Left, 1).reverse("bcdea") shouldBe "abcde"
    Operation.RotateByLetter('b').reverse("ecabd") shouldBe "abdec"
    Operation.RotateByLetter('d').reverse("decab") shouldBe "ecabd"

    val input = Random.shuffle("abcdefgh".toSeq).mkString
    val rotate = Operation.RotateByLetter('a')
    rotate.reverse(rotate.apply(input)) shouldBe input
  }
}
