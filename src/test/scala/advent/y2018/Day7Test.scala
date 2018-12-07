package advent.y2018

import advent.shared.test.UnitTest
import advent.y2018.Day7._

final class Day7Test extends UnitTest {

  private val sampleRawInput = """Step C must be finished before step A can begin.
                                 |Step C must be finished before step F can begin.
                                 |Step A must be finished before step B can begin.
                                 |Step A must be finished before step D can begin.
                                 |Step B must be finished before step E can begin.
                                 |Step D must be finished before step E can begin.
                                 |Step F must be finished before step E can begin.
                                 |""".stripMargin

  private val sampleInput = sampleRawInput.lines.map(Instruction.parse).toList

  "Part 1" should "find the inverse topological order" in {
    Day7.part1(sampleInput) should ===("CABDFE")
  }

  "Part 2" should "find the time it will take to build" in {
    Day7.part2(sampleInput, workers = 2, fixedTime = 0) should ===(15)
  }
}
