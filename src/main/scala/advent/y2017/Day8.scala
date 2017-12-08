package advent.y2017

import advent.shared.Time.timed
import fastparse.WhitespaceApi
import fastparse.all._

object Day8 {

  class Registers(val entries: Map[Symbol, Int]) extends AnyVal {
    def update(reg: Symbol)(f: Int => Int): Registers =
      new Registers(entries.updated(reg, f(entries(reg))))
  }

  object Registers {
    val Empty = new Registers(Map.empty[Symbol, Int].withDefaultValue(0))
  }

  sealed abstract class Relation(val apply: (Int, Int) => Boolean)
  object Relation {
    case object LT extends Relation(_ < _)
    case object GT extends Relation(_ > _)
    case object EQ extends Relation(_ == _)
    case object LE extends Relation(_ <= _)
    case object GE extends Relation(_ >= _)
    case object NE extends Relation(_ != _)
  }

  case class Condition(reg: Symbol, rel: Relation, constant: Int) {
    def evaluate(regs: Registers): Boolean =
      rel.apply(regs.entries(reg), constant)
  }

  case class Instruction(reg: Symbol, incrementBy: Int, condition: Condition) {
    def evaluate(regs: Registers): Registers =
      if (condition.evaluate(regs)) regs.update(reg)(_ + incrementBy) else regs
  }

  object Instruction {
    private object Grammar {
      val White = WhitespaceApi.Wrapper(NoTrace(" ".rep))
      import White._

      val id: P[Symbol] = P(CharsWhileIn('a' to 'z').!).map(Symbol.apply).opaque("identifier")

      val int: P[Int] = P(("-".? ~ CharIn('0' to '9').rep(1)).!).map(_.toInt)

      val operation: P[Int] = mapping(P("inc") -> 1, P("dec") -> -1)

      val relation: P[Relation] = mapping(
        P("<=") -> Relation.LE,
        P("<")  -> Relation.LT,
        P(">=") -> Relation.GE,
        P(">")  -> Relation.GT,
        P("==") -> Relation.EQ,
        P("!=") -> Relation.NE
      )

      def mapping[A](pairs: (P0, A)*): P[A] =
        pairs.map { case (p, a) => p.map(_ => a) }.reduce(_ | _)

      val condition: P[Condition] = P("if" ~/ id ~ relation ~ int).map {
        case (reg, rel, constant) => Condition(reg, rel, constant)
      }

      val instruction: P[Instruction] = P(id ~/ operation ~/ int ~/ condition ~ "\n".?).map {
        case (reg, sign, increment, cond) =>
          Instruction(reg, sign * increment, cond)
      }
      val input: P[Vector[Instruction]] = P(instruction.rep).map(_.toVector)
    }

    def parseAll(text: String): Vector[Instruction] = Grammar.input.parse(text).get.value
  }

  def part1(rawInput: String): Int = {
    val input = Instruction.parseAll(rawInput)
    val finalRegisters = input.foldLeft(Registers.Empty) { (regs, ins) =>
      ins.evaluate(regs)
    }
    finalRegisters.entries.values.max
  }

  def part2(rawInput: String): Int = {
    val input = Instruction.parseAll(rawInput)
    val allStates = input.toStream.scanLeft(Registers.Empty) { (regs, ins) =>
      ins.evaluate(regs)
    }
    allStates.flatMap(_.entries.values).max
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 8).getLines().filter(_.nonEmpty).mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
