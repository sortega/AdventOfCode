package advent.y2016

import scala.annotation.tailrec

import advent.shared.Time.timed
import advent.y2016.AssemblyBunny._

object Day25 {

  private def generatesClockSignal(listing: Vector[Instruction], initialA: Int): Boolean =
    validCycle(Computer(listing).update(Register.A, initialA).states)

  private def validCycle(states: Stream[Computer]): Boolean = {

    @tailrec
    def findCycleStart(unseen: Stream[(Computer, Int)], seen: Set[Computer] = Set.empty): Int =
      unseen match {
        case (next, index) #:: _ if seen.contains(next.clearOutput) => index
        case (next, _) #:: others => findCycleStart(others, seen + next.clearOutput)
      }

    val output = states(findCycleStart(states.zipWithIndex) * 2).output
    isClockSignalPrefix(output)
  }

  private def isClockSignalPrefix(signal: Vector[Int]) =
    Stream.from(0).map(_ % 2).startsWith(signal)

  def part1(input: String): Int = {
    val listing = AssemblyGrammar.parse(input)
    Stream.from(0).filter(i => generatesClockSignal(listing, i)).head
  }

  def main(args: Array[String]): Unit = {
    val input = dailyResource(25).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
  }
}
