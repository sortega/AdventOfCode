package advent.y2015

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day20Test extends FlatSpec with ShouldMatchers {

  "Primes" should "contain the sequence of primer numbers" in {
    Day20.primes.take(10) shouldBe Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }

  "Numbers" should "be factorized" in {
    Day20.factorize(1) shouldBe 'empty
    Day20.factorize(2) shouldBe Seq(2)
    Day20.factorize(3) shouldBe Seq(3)
    Day20.factorize(4) shouldBe Seq(2, 2)
    Day20.factorize(6) shouldBe Seq(2, 3)
    Day20.factorize(9) shouldBe Seq(3, 3)
    Day20.factorize(1996) shouldBe Seq(2, 2, 499)
  }

  "Subseqs" should "explode" in {
    Day20.allSubSeqs(Seq.empty) shouldBe Set(Seq.empty)
    Day20.allSubSeqs(Seq(2)) shouldBe Set(Seq(2), Seq.empty)
    Day20.allSubSeqs(Seq(2, 3)) shouldBe Set(Seq(2, 3), Seq(2), Seq(3), Seq.empty)
  }

  "Numbers" should "have divisors" in {
    Day20.divisors(1) shouldBe Set(1)
    Day20.divisors(6) shouldBe Set(1, 2, 3, 6)
    Day20.divisors(64) shouldBe Set(1, 2, 4, 8, 16, 32, 64)
  }

  "Houses" should "have 10*i presents for each ith elf visiting" in {
    Day20.presentsAtPart1(1) shouldBe 10
    Day20.presentsAtPart1(2) shouldBe 30
    Day20.presentsAtPart1(3) shouldBe 40
    Day20.presentsAtPart1(4) shouldBe 70
    Day20.presentsAtPart1(5) shouldBe 60
    Day20.presentsAtPart1(6) shouldBe 120
    Day20.presentsAtPart1(64) shouldBe 1270
    Day20.presentsAtPart1(1080) shouldBe 36000
  }

  it should "have a different criterion for part 2" in {
    Day20.presentsAtPart2(1) shouldBe 10
    Day20.presentsAtPart2(2) shouldBe 30
    Day20.presentsAtPart2(3) shouldBe 40
    Day20.presentsAtPart2(64) shouldBe 1260
    Day20.presentsAtPart2(1080) shouldBe 34870
  }

  "Part 1" should "find the lowest house with a given number of presents" in {
    Day20.part1(35000) shouldBe 1080
  }

  "Part 2" should "???" in {
  }
}
