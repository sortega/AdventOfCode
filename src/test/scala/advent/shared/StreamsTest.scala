package advent.shared

import advent.shared.Streams.Cycle
import org.scalatest.{FlatSpec, Matchers}

class StreamsTest extends FlatSpec with Matchers {

  "Detecting iteration cycles" should "return the repeated state and its index" in {
    Streams.iterateUntilCycling(0)(i => (i + 1) % 10) shouldBe Cycle(repeatedState = 0,
                                                                     firstSeenAt = 0,
                                                                     repeatedAt = 10)
  }
}
