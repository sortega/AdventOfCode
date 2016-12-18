package advent.y2016

object Day18 {

  def trapAutomaton(row: String): Stream[String] = Stream.iterate(row)(nextRow)

  private def nextRow(row: String): String =
    s".$row."
      .sliding(size = 3, step = 1)
      .map {
        case "^^." | ".^^" | "^.." | "..^" => '^'
        case _ => '.'
      }
      .mkString

  def countSafeTiles(input: String, rows: Int): Int =
    trapAutomaton(input).take(rows).map(_.count(_ == '.')).sum

  def main(args: Array[String]): Unit = {
    val input = ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^."
    println("Part 1 result: " + countSafeTiles(input, rows = 40))
    println("Part 2 result: " + countSafeTiles(input, rows = 400000))
  }
}
