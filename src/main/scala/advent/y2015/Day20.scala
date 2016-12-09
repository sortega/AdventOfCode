package advent.y2015

object Day20 {

  lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(isPrime)

  private def isPrime(n: Int): Boolean = {
    val bound = Math.sqrt(n)
    primes.takeWhile(_ <= bound).forall(p => n % p != 0)
  }

  def factorize(n: Int): Seq[Int] = {
    def accumulateFactors(n: Int, primes: Stream[Int], factors: Seq[Int]): Seq[Int] =
      if (n == 1) factors
      else if (n % primes.head == 0)
        accumulateFactors(n / primes.head, primes, factors :+ primes.head)
      else accumulateFactors(n, primes.tail, factors)

    accumulateFactors(n, primes, Seq.empty)
  }

//  def divisors(n: Int): Seq[Int] = (1 to n).filter(divisor => n % divisor == 0)
  def divisors(n: Int): Set[Int] = allSubSeqs(factorize(n)).map(_.product)

  def allSubSeqs(elems: Seq[Int]): Set[Seq[Int]] =
    if (elems.isEmpty) Set(Seq.empty)
    else {
      val tailSubseqs = allSubSeqs(elems.tail)
      tailSubseqs.map(subseq => elems.head +: subseq) ++ tailSubseqs
    }

  def presentsAtPart1(house: Int): Int = 10 * divisors(house).sum

  def part1(input: Int): Int = Stream.from(1)
    .find(house => presentsAtPart1(house) >= input)
    .get

  def presentsAtPart2(house: Int): Int =
    10 * divisors(house).filter(d => d * 50 >= house).sum

  def part2(input: Int): Int = Stream.from(786240)
    .find(house => presentsAtPart2(house) >= input)
    .get

  def main(args: Array[String]): Unit = {
//     println("Part 1 result: " + part1(34000000))
    println("Part 2 result: " + part2(34000000))
  }
}
