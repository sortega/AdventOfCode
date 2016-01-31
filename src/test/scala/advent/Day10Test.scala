package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day10Test extends FlatSpec with ShouldMatchers {

  "A number" should "be 'pronounced'" in {
    Day10.pronounce("1") shouldBe "11"
    Day10.pronounce("11") shouldBe "21"
    Day10.pronounce("21") shouldBe "1211"
    Day10.pronounce("211") shouldBe "1221"
  }
  
}
