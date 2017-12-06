package advent.shared

import org.scalatest.{FlatSpec, Matchers}

class MutationTest extends FlatSpec with Matchers {

  "Local mutation using an array" should "be possible over a list" in {
    val result = Mutation.mutate(List(1, 2, 3)) { buffer =>
      buffer(1) = 10
    }
    result shouldBe List(1, 10, 3)
  }

  it should "be possible over a vector" in {
    val result = Mutation.mutate(Vector(1, 2, 3)) { buffer =>
      for (i <- buffer.indices) {
        buffer(i) *= 2
      }
    }
    result shouldBe List(2, 4, 6)
  }
}
