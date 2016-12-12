package advent.y2016

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day12 {

  sealed trait Register
  object Register {
    case object A extends Register
    case object B extends Register
    case object C extends Register
    case object D extends Register
  }

  sealed trait Value
  case class Immediate(value: Int) extends Value
  case class Reference(register: Register) extends Value

  sealed trait Instruction
  case class CPY(from: Value, to: Register) extends Instruction
  case class INC(r: Register) extends Instruction
  case class DEC(r: Register) extends Instruction
  case class JNZ(cond: Value, offset: Int) extends Instruction

  object AssemblyGrammar extends RegexParsers {
    private def listing: Parser[Vector[Instruction]] = instruction.* ^^ { _.toVector }
    private def instruction: Parser[Instruction] = cpy | inc | dec | jnz
    private def cpy = "cpy" ~> value ~ register ^^ {
      case from ~ to => CPY(from, to)
    }
    private def value: Parser[Value] = number ^^ Immediate.apply | register ^^ Reference.apply
    private def number = """-?\d+""".r ^^ { _.toInt }
    private def register: Parser[Register] =
      "a" ^^^ Register.A | "b" ^^^ Register.B | "c" ^^^ Register.C | "d" ^^^ Register.D
    private def inc = "inc" ~> register ^^ INC.apply
    private def dec = "dec" ~> register ^^ DEC.apply
    private def jnz = "jnz" ~> value ~ number ^^ {
      case value ~ offset => JNZ(value, offset)
    }

    def parse(input: String): Vector[Instruction] = parseAll(listing, input).get
  }

  case class Computer(listing: IndexedSeq[Instruction],
                      registers: Map[Register, Int] = Map.empty.withDefaultValue(0),
                      programCounter: Int = 0) {
    @tailrec
    final def run: Computer = if (halted) this else step.run

    def halted: Boolean = programCounter >= listing.size

    def step: Computer =
      listing(programCounter) match {
        case CPY(value, to) => update(to, lookup(value)).updateProgramCounter()
        case INC(r) => update(r)(_ + 1).updateProgramCounter()
        case DEC(r) => update(r)(_ - 1).updateProgramCounter()
        case JNZ(cond, offset: Int) => updateProgramCounter(if (lookup(cond) != 0) offset else 1)
      }

    private def lookup(v: Value): Int = v match {
      case Immediate(i) => i
      case Reference(r) => registers(r)
    }

    def updateProgramCounter(by: Int = 1): Computer = copy(programCounter = programCounter + by)
    def update(r: Register, v: Int): Computer = copy(registers = registers.updated(r, v))
    def update(r: Register)(f: Int => Int): Computer = update(r, f(registers(r)))
  }

  def part1(input: String): Int = Computer(AssemblyGrammar.parse(input)).run.registers(Register.A)

  def part2(input: String): Int = {
    val initialRegisters = Map[Register, Int](Register.C -> 1).withDefaultValue(0)
    Computer(AssemblyGrammar.parse(input), initialRegisters).run.registers(Register.A)
  }

  def main(args: Array[String]): Unit = {
    val input = """cpy 1 a
                  |cpy 1 b
                  |cpy 26 d
                  |jnz c 2
                  |jnz 1 5
                  |cpy 7 c
                  |inc d
                  |dec c
                  |jnz c -2
                  |cpy a c
                  |inc a
                  |dec b
                  |jnz b -2
                  |cpy c b
                  |dec d
                  |jnz d -6
                  |cpy 14 c
                  |cpy 14 d
                  |inc a
                  |dec d
                  |jnz d -2
                  |dec c
                  |jnz c -5
                  |""".stripMargin
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
