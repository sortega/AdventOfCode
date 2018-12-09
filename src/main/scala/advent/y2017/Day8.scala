package advent.y2017

import advent.shared.Time.timed

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
      import fastparse._, JavaWhitespace._

      def id[_: P]: P[Symbol] = P(CharsWhileIn("a-z").!).map(Symbol.apply).opaque("identifier")

      def int[_: P]: P[Int] = P(("-".? ~ CharsWhileIn("0-9")).!).map(_.toInt)

      def operation[_: P]: P[Int] = P("inc").map(_ => 1) | P("dec").map(_ => -1)

      def relation[_: P]: P[Relation] =
        P("<=").map(_ => Relation.LE) |
          P("<").map(_ => Relation.LT) |
          P(">=").map(_ => Relation.GE) |
          P(">").map(_ => Relation.GT) |
          P("==").map(_ => Relation.EQ) |
          P("!=").map(_ => Relation.NE)

      def condition[_: P]: P[Condition] = P("if" ~/ id ~ relation ~ int).map {
        case (reg, rel, constant) => Condition(reg, rel, constant)
      }

      def instruction[_: P]: P[Instruction] = P(id ~/ operation ~/ int ~/ condition ~ "\n".?).map {
        case (reg, sign, increment, cond) =>
          Instruction(reg, sign * increment, cond)
      }
      def input[_: P]: P[Vector[Instruction]] = P(instruction.rep).map(_.toVector)
    }

    def parseAll(text: String): Vector[Instruction] =
      fastparse.parse(text, Grammar.input(_)).get.value
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
