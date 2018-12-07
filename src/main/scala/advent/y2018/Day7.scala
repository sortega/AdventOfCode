package advent.y2018

import scalaz.EphemeralStream
import scalaz.Scalaz._

import advent.shared.Time.timed

object Day7 {
  type Step = Char
  final case class Instruction(requiredStep: Step, enabledStep: Step)

  object Instruction {
    private val Pattern = """Step (.) must be finished before step (.) can begin.""".r

    def parse(s: String): Instruction = s match {
      case Pattern(required, enabled) => Instruction(required.head, enabled.head)
    }
  }

  def part1(instructions: List[Instruction]): String =
    topologicalSort(Job(instructions)).mkString

  final case class Job(steps: Map[Step, Set[Step]]) extends AnyVal {
    def executableSteps: Set[Step] =
      steps.collect {
        case (step, noReqs) if noReqs.isEmpty => step
      }.toSet

    def execute(executedStep: Step): Job =
      Job(steps.collect {
        case (step, stepReqs) if step != executedStep => step -> (stepReqs - executedStep)
      })

    def done: Boolean = steps.isEmpty
  }
  object Job {
    def apply(instructions: List[Instruction]): Job =
      Job(instructions.foldMap { instruction =>
        Map(instruction.enabledStep  -> Set(instruction.requiredStep),
            instruction.requiredStep -> Set.empty[Step])
      })
  }

  private def topologicalSort(job: Job): List[Step] =
    EphemeralStream
      .unfold(job) { job =>
        (!job.done).option {
          val step = job.executableSteps.min
          (step, job.execute(step))
        }
      }
      .collapse[List]

  def part2(instructions: List[Instruction], workers: Int = 15, fixedTime: Int = 60): Int =
    Stream
      .iterate(ParallelJob(Job(instructions), workers, fixedTime))(_.tick)
      .find(_.done)
      .get
      .time

  final case class ParallelJob(job: Job,
                               workers: Int,
                               fixedTime: Int,
                               time: Int = 0,
                               workInProgress: Map[Step, Int] = Map.empty) {
    def done: Boolean = job.done

    def tick: ParallelJob = {
      val maybeStep = executableStep
      if (workInProgress.size < workers && maybeStep.nonEmpty) start(maybeStep.get)
      else waitNextCompletion
    }

    private def executableStep: Option[Step] =
      (job.executableSteps diff workInProgress.keySet).minimum

    private def start(step: Step): ParallelJob = {
      println(s"Starting $step at $time (will take ${timeFor(step)})")
      copy(workInProgress = workInProgress + (step -> timeFor(step)))
    }

    private def timeFor(step: Step): Int = fixedTime + (step.toInt - 'A'.toInt + 1)

    private def waitNextCompletion: ParallelJob =
      wait(workInProgress.values.min)

    private def wait(waitTime: Int): ParallelJob = {
      val remainingWorkInProgress = workInProgress.map {
        case (step, stepTime) => (step, stepTime - waitTime)
      }
      val completedJobs = remainingWorkInProgress.collect {
        case (step, 0) => step
      }.toSet
      val nextTime = time + waitTime
      println(s"Waiting until $nextTime: ${completedJobs.mkString(", ")} completed")
      copy(completedJobs.foldLeft(job)(_.execute(_)),
           time = nextTime,
           workInProgress = remainingWorkInProgress -- completedJobs)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 7).getLines().map(Instruction.parse).toList
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
