package advent.y2016

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers
import scalaz._, Scalaz._

object Day10 {

  sealed trait Target
  case class Bot(index: Int) extends Target
  case class Output(index: Int) extends Target

  sealed trait Connection
  object Connection {
    case class Input(value: Int, target: Bot) extends Connection
    case class BotResults(bot: Bot, low: Target, high: Target) extends Connection
  }

  object Grammar extends RegexParsers {
    private val number: Parser[Int] = """\d+""".r ^^ { _.toInt }
    private val bot: Parser[Bot] = "bot" ~> number ^^ Bot.apply
    private val output: Parser[Output] = "output" ~> number ^^ Output.apply
    private def target: Parser[Target] = bot | output

    private def connections: Parser[List[Connection]] = connection *

    private def connection: Parser[Connection] = input | botResults

    private def input: Parser[Connection.Input] = "value" ~> number ~ "goes" ~ "to" ~ bot ^^ {
      case number ~ _ ~ _ ~ bot => Connection.Input(number, bot)
    }

    private def botResults: Parser[Connection.BotResults] =
      (bot <~ "gives" <~ "low" <~ "to") ~ (target <~ "and" <~ "high" <~ "to") ~ target ^^ {
        case bot ~ low ~ high => Connection.BotResults(bot, low, high)
      }

    def parse(input: String): List[Connection] = parseAll(connections, input).get
  }

  case class Comparison(bot: Bot, numbers: Seq[Int])

  def interpret(connections: List[Connection]): (Map[Target, Vector[Int]], Vector[Comparison]) = {
    @tailrec
    def run(connections: List[Connection],
            values: Map[Target, Vector[Int]],
            comparisons: Vector[Comparison]): (Map[Target, Vector[Int]], Vector[Comparison]) = {

      def isExecutable(connection: Connection): Boolean = connection match {
        case _: Connection.Input => true
        case Connection.BotResults(bot, _, _) => values.getOrElse(bot, Seq.empty).size == 2
      }

      def execute(connection: Connection): (Map[Target, Vector[Int]], Vector[Comparison]) =
        connection match {
          case Connection.Input(value, target) => (Map(target -> Vector(value)), Vector.empty)
          case Connection.BotResults(bot, lowTarget, highTarget) =>
            val numbers @ Vector(low, high) = values(bot).sorted
            (Map(lowTarget -> Vector(low), highTarget -> Vector(high)),
             Vector(Comparison(bot, numbers)))
        }

      val (executables, otherConnections) = connections.partition(isExecutable)

      if (executables.isEmpty) (values, comparisons)
      else {
        val (newValues, newComparisons) = executables.map(execute).suml
        run(otherConnections, values |+| newValues, comparisons ++ newComparisons)
      }
    }

    run(connections, values = Map.empty, comparisons = Vector.empty)
  }

  def part1(input: String, numbers: Vector[Int] = Vector(17, 61)): Int = {
    val (_, trace) = interpret(Grammar.parse(input))
    val bot = trace.collectFirst {
      case Comparison(bot, numbers) if numbers.sorted == numbers => bot
    }.get
    bot.index
  }

  def part2(input: String): Int = {
    val (values, _) = interpret(Grammar.parse(input))
    values.collect {
      case (Output(index), numbers) if index <= 2 => numbers
    }.flatten.product
  }

  def main(args: Array[String]): Unit = {
    val input = dailyResource(10).mkString
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
