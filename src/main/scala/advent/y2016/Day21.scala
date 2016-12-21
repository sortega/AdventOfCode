package advent.y2016

object Day21 {
  sealed trait Direction {
    def reverse: Direction = this match {
      case Left => Right
      case Right => Left
    }
  }
  case object Left extends Direction
  case object Right extends Direction

  sealed trait Operation {
    def apply(input: String): String
    def reverse(input: String): String = apply(input)
  }

  object Operation {
    case class SwapIndices(x: Int, y: Int) extends Operation {
      override def apply(input: String): String =
        input.updated(y, input(x)).updated(x, input(y))
    }

    case class SwapLetters(x: Char, y: Char) extends Operation {
      override def apply(input: String): String =
        input.updated(input.indexOf(y), x).updated(input.indexOf(x), y)
    }

    case class RotateSteps(dir: Direction, steps: Int) extends Operation {
      override def apply(input: String): String = {
        val effectiveSteps = dir match {
          case Left => steps % input.length
          case Right => input.length - steps % input.length
        }
        val (before, after) = input.splitAt(effectiveSteps)
        after + before
      }

      override def reverse(input: String): String = copy(dir = dir.reverse).apply(input)
    }

    case class ReverseRange(from: Int, to: Int) extends Operation {
      override def apply(input: String): String = {
        val (before, rest) = input.splitAt(from)
        val (selected, after) = rest.splitAt(to - from + 1)
        before + selected.reverse + after
      }
    }

    case class Move(from: Int, to: Int) extends Operation {
      override def apply(input: String): String = {
        val (beforeFrom, afterFrom) = input.splitAt(from)
        val intermediate = beforeFrom + afterFrom.tail
        val (beforeTo, afterTo) = intermediate.splitAt(to)
        beforeTo + input(from) + afterTo
      }

      override def reverse(input: String): String = copy(from = to, to = from).apply(input)
    }

    case class RotateByLetter(c: Char) extends Operation {
      override def apply(input: String): String = {
        val index = input.indexOf(c)
        val stepsToTheRight = (1 + index + (if (index >= 4) 1 else 0)) % input.length
        val (before, after) = input.splitAt(input.length - stepsToTheRight)
        after + before
      }

      override def reverse(input: String): String = {
        val steps = input.indexOf(c) match {
          case 0 => 1
          case even if even % 2 == 0 => 5 + even / 2
          case odd => odd / 2 + 1
        }
        RotateSteps(Left, steps).apply(input)
      }
    }

    private val SwapIndicesPattern = """swap position (\d+) with position (\d+)""".r
    private val SwapLettersPattern = """swap letter (.) with letter (.)""".r
    private val RotateStepsPattern = """rotate (left|right) (\d+) steps?""".r
    private val RotateByLetterPattern = """rotate based on position of letter (.)""".r
    private val ReverseRangePattern = """reverse positions (\d+) through (\d+)""".r
    private val MovePattern = """move position (\d+) to position (\d+)""".r

    def parse(line: String): Operation = line match {
      case SwapIndicesPattern(x, y) => SwapIndices(x.toInt, y.toInt)
      case SwapLettersPattern(x, y) => SwapLetters(x.head, y.head)
      case RotateStepsPattern("left", steps) => RotateSteps(Left, steps.toInt)
      case RotateStepsPattern("right", steps) => RotateSteps(Right, steps.toInt)
      case RotateByLetterPattern(letter) => RotateByLetter(letter.head)
      case ReverseRangePattern(from, to) => ReverseRange(from.toInt, to.toInt)
      case MovePattern(from, to) => Move(from.toInt, to.toInt)
    }
  }

  def part1(operations: Vector[Operation]): String =
    operations.foldLeft("abcdefgh") { (password, op) =>
      op(password)
    }

  def part2(operations: Vector[Operation]): String =
    operations.reverse.foldLeft("fbgdceah") { (password, op) =>
      op.reverse(password)
    }

  def main(args: Array[String]): Unit = {
    val input = dailyResource(21).getLines().map(Operation.parse).toVector
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
