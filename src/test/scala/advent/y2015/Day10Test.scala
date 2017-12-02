package advent.y2015

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {

  "A number" should "be 'pronounced'" in {
    Day10.pronounce("1") shouldBe "11"
    Day10.pronounce("11") shouldBe "21"
    Day10.pronounce("21") shouldBe "1211"
    Day10.pronounce("211") shouldBe "1221"
  }

}
