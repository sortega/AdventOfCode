package advent.y2016

object Day8 {

  case class Screen(pixels: Vector[Vector[Boolean]]) {
    def lightsOn: Int = pixels.flatten.count(identity)

    def turnOn(rows: Int, cols: Int): Screen =
      Screen(
        pixels.zipWithIndex.map {
          case (row, rowIndex) =>
            row.zipWithIndex.map {
              case (pixel, colIndex) =>
                if (rowIndex < rows && colIndex < cols) true
                else pixel
            }
        }
      )

    def updateRow(row: Int)(f: Vector[Boolean] => Vector[Boolean]): Screen =
      Screen(pixels.updated(row, f(pixels(row))))

    def updateCol(col: Int)(f: Vector[Boolean] => Vector[Boolean]): Screen =
      transpose.updateRow(col)(f).transpose

    def transpose: Screen = Screen(pixels.transpose)

    override def toString: String =
      pixels
        .map(row =>
          row.map {
            case true => '#'
            case false => '.'
          }.mkString)
        .mkString("", "\n", "\n")
  }

  object Screen {
    def apply(rows: Int, cols: Int): Screen = Screen(Vector.fill(rows)(Vector.fill(cols)(false)))
  }

  sealed trait Instruction
  object Instruction {
    case class Rect(rows: Int, cols: Int) extends Instruction
    case class RotateRow(row: Int, by: Int) extends Instruction
    case class RotateCol(col: Int, by: Int) extends Instruction

    val RectPattern = """rect (\d+)x(\d+)""".r
    val RotateRowPattern = """rotate row y=(\d+) by (\d+)""".r
    val RotateColPattern = """rotate column x=(\d+) by (\d+)""".r

    def parse(line: String): Instruction = line match {
      case RectPattern(cols, rows) => Rect(rows.toInt, cols.toInt)
      case RotateRowPattern(row, by) => RotateRow(row.toInt, by.toInt)
      case RotateColPattern(col, by) => RotateCol(col.toInt, by.toInt)
    }
  }

  private def rotate[A](by: Int, elems: Vector[A]): Vector[A] = {
    val (before, after) = elems.splitAt(elems.size - by)
    after ++ before
  }

  private def runInstructions(rows: Int, cols: Int, instructions: Seq[Instruction]) =
    instructions.foldLeft(Screen(rows, cols)) { (screen, instruction) =>
      instruction match {
        case Instruction.Rect(rows, cols) => screen.turnOn(rows, cols)
        case Instruction.RotateRow(row, by) => screen.updateRow(row)(rotate(by, _))
        case Instruction.RotateCol(col, by) => screen.updateCol(col)(rotate(by, _))
      }
    }

  def part1(input: Seq[String], rows: Int = 6, cols: Int = 50): Int =
    runInstructions(rows, cols, input.map(Instruction.parse)).lightsOn

  def part2(input: Seq[String], rows: Int = 6, cols: Int = 50): String =
    runInstructions(rows, cols, input.map(Instruction.parse)).toString

  def main(args: Array[String]): Unit = {
    val input = dailyResource(8).getLines().toSeq
    println("Part 1 result: " + part1(input))
    println("Part 2 result:\n" + part2(input))
  }
}
