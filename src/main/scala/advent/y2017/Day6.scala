package advent.y2017

import scala.reflect.ClassTag

import advent.shared.Streams
import advent.shared.Streams.Cycle
import advent.shared.Time.timed

object Day6 {

  case class MemoryBank(blocks: Vector[Int]) {
    def redistribute: MemoryBank = {
      val bankIndex   = chooseBankToRedistribute
      val blockNumber = blocks(bankIndex)
      MemoryBank(mutate(blocks) { buffer =>
        buffer(bankIndex) = 0
        for (i <- bankIndex + 1 to bankIndex + blockNumber) {
          buffer(i % buffer.length) += 1
        }
      })
    }

    private def chooseBankToRedistribute: Int = {
      val max = blocks.max
      blocks.indexWhere(_ == max)
    }

    private def mutate[A: ClassTag](vector: Vector[A])(block: Array[A] => Unit): Vector[A] = {
      val array = vector.toArray
      block(array)
      array.toVector
    }
  }

  object MemoryBank {
    def apply(blocks: Int*): MemoryBank = MemoryBank(blocks.toVector)
  }

  def part1(input: Vector[Int]): Int =
    detectLoop(input).repeatedAt

  private def detectLoop(input: Vector[Int]): Cycle[MemoryBank] =
    Streams.iterateUntilCycling(MemoryBank(input))(_.redistribute)

  def part2(input: Vector[Int]): Int = {
    val loop = detectLoop(input)
    loop.repeatedAt - loop.firstSeenAt
  }

  def main(args: Array[String]): Unit = {
    val input = Vector(10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6)
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
